-module (mondemand_server_time_db).

-behaviour (gen_server).

% min/max calculations using ets:update_counter/2

%% API
-export ([ start_link/1,  % ([Minute]) -> {ok, Pid}
           find/1,        % (Minute) -> Pid | undefined
           add_value/4    % (Pid, Timestamp, Key, Value) -> ok
         ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, {timer, table, minute}).
-record (entry, {key,
                 sum = 0,
                 count = 0,
                 min = infinity,
                 max = 0}).

start_link (EpochMinute) ->
  gen_server:start_link ({via, gproc, {n, l, {?MODULE, EpochMinute}}},
                         ?MODULE, [EpochMinute], []).

find (EpochMinute) ->
  gproc:where ({n, l, {?MODULE, EpochMinute}}).

add_value (Pid, Timestamp, Key, Value) ->
  gen_server:cast (Pid, {add, Timestamp, Key, Value}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([Minute]) ->
  error_logger:info_msg ("Start db for ~p",[Minute]),
  Tid = meta_db (),
  % send a flush event about a minute and 15 seconds from when the
  % db is created
  TimerRef = erlang:send_after (75000, self(), flush),
  {ok, #state { timer = TimerRef, table = Tid, minute = Minute }}.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({add, _Timestamp, Key, Value},
             State = #state { minute = Minute }) ->
  upsert (Minute, Key, Value),
  {noreply, State};
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p ~p~n",[?MODULE, Request, State]),
  { noreply, State }.

handle_info (flush, State) ->
  error_logger:info_msg ("~p : Received Flush~n",[?MODULE]),
  { noreply, State };
handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  { ok, State }.

%%====================================================================
%% internal functions
%%====================================================================

meta_db () ->
  ets:new (md_sum_meta,
           [public, {read_concurrency, true}, named_table, {keypos, 1}]).

get_or_create_tid (Minute) ->
  case ets:lookup (md_sum_meta, Minute) of
    [{_,Tid}] -> Tid;
    [] -> new_db (Minute)
  end.

new_db (Minute) ->
  Tid =
    ets:new (md_sum_min,
             [ public,
               {write_concurrency, false},
               {read_concurrency, true},
               named_table,
               {keypos, 2}
             ]),
  FinalTid =
    case ets:insert_new (md_sum_meta, {Minute, Tid}) of
      false -> % someone else must have created it, so delete the new table
               ets:delete (Tid),
               % and attempt to fetch the pid
               get_or_create_tid (Minute);
      true -> Tid
    end,
  FinalTid.

get_val (Tid, Key) ->
  case ets:lookup (Tid, Key) of
    [V] -> V;
    [] -> undefined
  end.

upsert (Minute, Key, Value) ->
  Tid = get_or_create_tid (Minute),
  OldEntry =
    % TODO: still need to make this concurrent safe, currently if 2 processes
    % were to call this, you could have a problem, maybe use the
    % update_element method?
    case get_val (Tid, Key) of
      undefined -> #entry {};
      V -> V
    end,
  NewEntry =
    #entry { key = Key,
             sum = OldEntry#entry.sum + Value,
             count = OldEntry#entry.count + 1,
             min = min (Value, OldEntry#entry.min),
             max = max (Value, OldEntry#entry.max) },
  ets:insert (Tid, NewEntry).
