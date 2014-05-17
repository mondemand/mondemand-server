-module (mondemand_backend_worker).

-behaviour (gen_server).

%% API
-export ([start_link/4]).

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
                  worker_mod
                }).

start_link (SupervisorName, WorkerAtom, WorkerName, WorkerModule) ->
  gen_server:start_link ({local, WorkerAtom},?MODULE,
                         [SupervisorName, WorkerName, WorkerModule],[]).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([SupervisorName, WorkerName, WorkerModule]) ->
  % ensure terminate is called
  process_flag( trap_exit, true ),

  % init stats
  mondemand_server_stats:init_backend (WorkerModule, events_processed),
  mondemand_server_stats:init_backend (WorkerModule, stats_sent_count),
  mondemand_server_stats:init_backend (WorkerModule, stats_dropped_count),
  mondemand_server_stats:init_backend (WorkerModule, stats_sent_micros),
  mondemand_server_stats:init_backend (WorkerModule, connection_errors),
  mondemand_server_stats:init_backend (WorkerModule, send_errors),

  % config is grabbed from the server
  Config = mondemand_server_config:backend_config (WorkerModule),

  HandlerMod = proplists:get_value (handler_mod, Config, undefined),
  WorkerMod = proplists:get_value (worker_mod, Config, undefined),

  gproc_pool:connect_worker (SupervisorName, WorkerName),
  {ok, #state { name = WorkerName,
                handler_mod = HandlerMod,
                worker = WorkerMod:create (Config),
                worker_mod = WorkerMod }}.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Binary},
             State = #state { handler_mod = HandlerMod,
                              worker = Worker,
                              worker_mod = WorkerModule
                            }) ->
  {NumBad, NumGood, Lines} =
    mondemand_backend_stats_formatter:process_event (undefined,
                                                     Binary, HandlerMod),

  mondemand_server_stats:increment_backend (WorkerModule, events_processed),

  case WorkerModule:send (Worker, Lines) of
    ok ->
      mondemand_server_stats:increment_backend
        (WorkerModule, stats_dropped_count, NumBad),
      mondemand_server_stats:increment_backend
        (WorkerModule, stats_sent_count, NumGood),

      { noreply, State };
    error ->
      mondemand_server_stats:increment_backend
        (WorkerModule, stats_dropped_count, NumBad + NumGood),
      mondemand_server_stats:increment_backend
        (WorkerModule, send_errors),

      { noreply, State }
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

