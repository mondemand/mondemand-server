-module (mondemand_backend_stats_aggregator_db).

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
                  value1 = 0,
                  value2 = 0
                }).

-define (KEY_INDEX, #entry.key).
-define (INDEX_INDEX, #entry.index).
-define (VAL1_INDEX, #entry.value1).
-define (VAL2_INDEX, #entry.value2).
-define (SIZE, 2).

%-=====================================================================-
%-                                  API                                -
%-=====================================================================-
start_link() ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

insert (Key, Value) ->
  Idx =
    try ets:update_counter (md_be_agg_c2g, Key, {?INDEX_INDEX, 1, 1, 0}) of
      I -> I
    catch
      error:badarg ->
        ets:insert_new (md_be_agg_c2g, #entry { key = Key }),
        0
    end,
  ets:update_element (md_be_agg_c2g, Key, {?VAL1_INDEX + Idx, Value}).

current (Key) ->
  case ets:lookup (md_be_agg_c2g, Key) of
    [] -> 0;
    [#entry {value1 = 0}] -> 0;
    [#entry {value2 = 0}] -> 0;
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
  ets:new (md_be_agg_c2g, [ set,
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
