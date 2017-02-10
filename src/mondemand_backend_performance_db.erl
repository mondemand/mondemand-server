-module (mondemand_backend_performance_db).

-behaviour (gen_server).

%% API
-export ([start_link/2,
          add_trace/3,
          clear_all/0,
          stats/0
         ]).

%% gen_server callbacks
-export ( [ init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3
          ]).

-record (state, { timer, age_millis, flush_interval_millis }).
-define (TIME_TABLE, md_be_perf_id2ts).
-define (PERF_TABLE, md_be_perf_ids).

-record (time, {id, timestamp}).
-record (traces, {id, trace}).

%-=====================================================================-
%-                                  API                                -
%-=====================================================================-
start_link(AgeSeconds, FlushIntervalSeconds) ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [AgeSeconds, FlushIntervalSeconds], []).

add_trace (PerfId, Timestamp, Trace) ->
  ets:insert (?TIME_TABLE, #time {id = PerfId, timestamp = Timestamp}),
  ets:insert (?PERF_TABLE, #traces { id = PerfId, trace = Trace }).

clear_all () ->
  ets:delete_all_objects (?TIME_TABLE),
  ets:delete_all_objects (?PERF_TABLE).

stats () ->
  [ ets:info(?TIME_TABLE,size), ets:info (?PERF_TABLE, size) ].

%-=====================================================================-
%-                        gen_server callbacks                         -
%-=====================================================================-
init([AgeSeconds, FlushIntervalSeconds]) ->
  ets:new (?TIME_TABLE, [ set,
                          public,
                          named_table,
                          {write_concurrency, true},
                          {read_concurrency, true},
                          {keypos, 2}
                        ]),
  ets:new (?PERF_TABLE, [ bag,
                          public,
                          named_table,
                          {write_concurrency, true},
                          {read_concurrency, true},
                          {keypos, 2}
                        ]),
  timer:send_after (
     mondemand_util:millis_to_next_round_second(),
     ?MODULE,
     flush),

  {ok, #state{ timer = undefined,
               age_millis = AgeSeconds * 1000,
               flush_interval_millis = FlushIntervalSeconds * 1000 }}.

handle_call (_Request, _From, State = #state { }) ->
  {reply, ok, State }.

handle_cast (_Request, State = #state { }) ->
  {noreply, State}.

handle_info (flush, State = #state { timer = undefined,
                                     age_millis = MaxAge,
                                     flush_interval_millis = FlushInterval
                                   }) ->
  {ok, TRef} = timer:send_interval (FlushInterval, ?MODULE, flush),
  flush (MaxAge),
  {noreply, State#state { timer = TRef }};
handle_info (flush, State = #state { age_millis = MaxAge }) ->
  flush (MaxAge),
  {noreply, State};
handle_info (_Info, State = #state {}) ->
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

flush(MaxAge) ->
  Now = mondemand_util:millis_since_epoch(),
  Then = Now - MaxAge,
  OldIds =
    ets:select (?TIME_TABLE,
                [{{time,'$1','$2'},[{'<','$2',Then}],['$1']}]),
  NumberToFlush = length(OldIds),
  clear (OldIds),
  Finished = mondemand_util:millis_since_epoch(),
  case NumberToFlush > 0 of
    true ->
      io:format ("flushing ~p in ~p millis~n",[NumberToFlush, Finished - Now]);
    false ->
      ok
  end,
  ok.

clear ([]) ->
  ok;
clear ([Id | Ids]) ->
  case ets:lookup (?PERF_TABLE, Id) of
    [] ->
      ok;
    L when is_list (L) ->
%      [ io:format ("~s",[E]) || #traces { trace = E } <- L ],
      ets:delete (?PERF_TABLE, Id),
      ets:delete (?TIME_TABLE, Id)
  end,
  clear (Ids).
