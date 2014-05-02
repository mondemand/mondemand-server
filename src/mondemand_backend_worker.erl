-module (mondemand_backend_worker).

-behaviour (gen_server).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-export ([behaviour_info/1]).

behaviour_info(callbacks) ->
  [ {create, 1},
    {send, 2},
    {destroy, 1}
  ];
behaviour_info(_) ->
  undefined.

-record (state, {
                  name,
                  handler_mod,
                  worker,
                  worker_mod,
                  events_processed = 0,
                  stats_sent_count = 0,
                  stats_dropped_count = 0,
                  stats_sent_micros = 0,
                  connection_errors = 0,
                  send_errors = 0
                }).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([Name]) ->
  % ensure terminate is called
  process_flag( trap_exit, true ),

  ConfigKey = mondemand_backend_connection_pool:sidejob_unname (Name),

  % config is grabbed from the server
  Config = mondemand_server_config:backend_config (ConfigKey),

  HandlerMod = proplists:get_value (handler_mod, Config, undefined),
  WorkerMod = proplists:get_value (worker_mod, Config, undefined),

  {ok, #state { name = Name,
                handler_mod = HandlerMod,
                worker = WorkerMod:create (Config),
                worker_mod = WorkerMod }}.

handle_call ({stats}, _From,
             State = #state { events_processed = EventsProcessed,
                              stats_sent_count = StatsSent,
                              stats_dropped_count = StatsDropped,
                              stats_sent_micros = StatsSentMicros,
                              connection_errors = ConnectionErrors,
                              send_errors = SendErrors }) ->
  { reply, dict:from_list ([ {events_processed, EventsProcessed},
                             {stats_sent_count, StatsSent},
                             {stats_dropped_count, StatsDropped},
                             {stats_sent_micros, StatsSentMicros},
                             {connection_errors, ConnectionErrors},
                             {send_errors, SendErrors } ]), State };
handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Binary},
             State = #state { handler_mod = HandlerMod,
                              worker = Worker,
                              worker_mod = WorkerMod,
                              events_processed = EventProcessed,
                              stats_sent_count = StatsSent,
                              stats_dropped_count = StatsDropped,
                              send_errors = SendErrors
                            }) ->
  {NumBad, NumGood, Lines} =
    mondemand_backend_stats_formatter:process_event (undefined,
                                                     Binary, HandlerMod),
  case WorkerMod:send (Worker, Lines) of
    ok ->
      { noreply, State#state {
          events_processed = EventProcessed + 1,
          stats_dropped_count = StatsDropped + NumBad,
          stats_sent_count = StatsSent + NumGood
        }
      };
    error ->
      { noreply, State#state {
          events_processed = EventProcessed + 1,
          stats_dropped_count = StatsDropped + NumBad + NumGood,
          send_errors = SendErrors + 1
        }
      }
  end;
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, #state { worker = Worker,
                             worker_mod = WorkerMod }) ->
  WorkerMod:destroy (Worker),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

