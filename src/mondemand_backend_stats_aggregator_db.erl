-module (mondemand_backend_stats_aggregator_db).

%% For aggregate statistics I want all statistics to represent a count
%% for a time interval.  In terms of mondemand this means I want to
%% turn counters into gauges.  In order to do this, I keep track of
%% two values and use their difference.
%%
%% Toward that end I keep an ets table of 4-tuples with entries
%%
%%  { key, index, value1, value2 }
%%
%% Whenever a value is added it adds it at index + 1 wrapping at 1, so
%% as values are added the index goes 0, 1, 0, 1, 0, 1, etc.
%%
%% Then when the current is asked for it either returns
%%   value1 - value2
%% or
%%   value2 - value1
%%
%% A negative value is considered a wrap and reset to 0.
%%
-behaviour (gen_server).

%% API
-export ([start_link/0,
          insert/2,
          current/1]).

%% gen_server callbacks
-export ( [ init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3
          ]).

-record (state, {}).
-record (entry, { key,
                  index = 0,
                  value1 = undefined,
                  value2 = undefined
                }).

-define (KEY_INDEX, #entry.key).
-define (INDEX_INDEX, #entry.index).
-define (VAL1_INDEX, #entry.value1).
-define (VAL2_INDEX, #entry.value2).
-define (SIZE, 2).
-define (TABLE, md_be_agg_c2g).

%-=====================================================================-
%-                                  API                                -
%-=====================================================================-
start_link() ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

insert (Key, Value) when is_integer (Value) ->
  Idx =
    try ets:update_counter (?TABLE, Key, {?INDEX_INDEX, 1, 1, 0}) of
      I -> I
    catch
      error:badarg ->
        ets:insert_new (?TABLE, #entry { key = Key }),
        0
    end,
  ets:update_element (?TABLE, Key, {?VAL1_INDEX + Idx, Value}).

current (Key) ->
  case ets:lookup (?TABLE, Key) of
    [] -> 0;
    [#entry {value2 = undefined}] -> 0;  % make sure we have at least 2 values
    [#entry {index = I, value1 = V1, value2 = V2}] ->
      PossibleValue =
        case I of
          0 -> V1 - V2;
          1 -> V2 - V1
        end,
      case PossibleValue >= 0 of
        true -> PossibleValue;
        false -> 0  % counter reset
      end
  end.

%-=====================================================================-
%-                        gen_server callbacks                         -
%-=====================================================================-
init([]) ->
  ets:new (?TABLE, [ set,
                     public,
                     named_table,
                     {write_concurrency, true},
                     {read_concurrency, false},
                     {keypos, ?KEY_INDEX}
                   ]),
  { ok, #state{} }.

handle_call (_Request, _From, State = #state { }) ->
  {reply, ok, State }.

handle_cast (_Request, State = #state { }) ->
  {noreply, State}.

handle_info (_Info, State = #state {}) ->
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup () ->
  case start_link() of
    {ok, Pid} -> Pid;
    {error, {already_started, _}} -> already_started
  end.

cleanup (already_started) -> ok;
cleanup (Pid) -> exit (Pid, normal).

db_test_ () ->
  { setup,
    fun setup/0,
    fun cleanup/1,
    [
      % check the case where it doesn't exist
      ?_assertEqual (0, current(foo)),
      % insert a large value the first time, which often happens with counters
      % after a restart
      ?_assertEqual (true, insert(foo, 10000)),
      % the first value will then be ignored
      ?_assertEqual (0, current(foo)),
      % so inserting another slightly larger number will give a difference
      ?_assertEqual (true, insert(foo, 10005)),
      ?_assertEqual (5, current(foo)),
      % and another to see that both directions work
      ?_assertEqual (true, insert(foo, 10013)),
      ?_assertEqual (8, current(foo)),
      % now insert a much smaller number representing a counter reset
      ?_assertEqual (true, insert(foo, 2)),
      ?_assertEqual (0, current(foo)),  % we should have a 0 after reset
      ?_assertEqual (true, insert(foo, 4)),
      ?_assertEqual (2, current(foo)),
      ?_assertEqual (true, insert(foo, 4)),
      ?_assertEqual (0, current(foo)),
      % insert a key
      ?_assertEqual (true, insert(foo,0)),
      % check the difference
      ?_assertEqual (0, current(foo)),
      % add another
      ?_assertEqual (true, insert(foo,10)),
      % check the difference
      ?_assertEqual (10, current(foo)),
      % and another
      ?_assertEqual (true, insert(foo,20)),
      % check the difference
      ?_assertEqual (10, current(foo)),
      % and a zero (so the counter reset case)
      ?_assertEqual (true, insert(foo,0)),
      % check the difference
      ?_assertEqual (0, current(foo))
    ]
  }.

gen_server_coverage_test_ () ->
  [
    ?_assertEqual ({reply, ok, #state{}}, handle_call(ok, ok, #state{})),
    ?_assertEqual ({noreply, #state{}}, handle_cast(ok, #state{})),
    ?_assertEqual ({noreply, #state{}}, handle_info(ok, #state{})),
    ?_assertEqual (ok, terminate (ok, #state{})),
    ?_assertEqual ({ok, #state{}}, code_change(ok, #state{}, ok))
  ].

eunit_converage_test_ () ->
  { setup,
    fun setup/0,
    fun cleanup/1,
    [
      ?_assertEqual (already_started, setup()),
      ?_assertEqual (ok, cleanup(already_started))
    ]
  }.

-endif.
