-module (mondemand_backend_worker).

-behaviour (gen_server).

-include_lib ("mondemand/include/mondemand.hrl").

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
                  handler_mod,
                  worker_name,
                  worker_mod,
                  worker_state,
                  prefix
                }).

start_link (SupervisorName, WorkerAtom, WorkerName, WorkerModule) ->
  gen_server:start_link ({local, WorkerAtom},?MODULE,
                         [SupervisorName, WorkerName, WorkerModule],[]).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([SupervisorName, WorkerName, WorkerModule]) ->
  error_logger:info_msg("~p:init([~p,~p,~p])",[?MODULE,SupervisorName, WorkerName, WorkerModule]),
  % ensure terminate is called
  process_flag( trap_exit, true ),

  % init stats
  mondemand_server_stats:create_backend (WorkerModule, events_processed),
  mondemand_server_stats:create_backend (WorkerModule, stats_sent_count),
  mondemand_server_stats:create_backend (WorkerModule, stats_dropped_count),
  mondemand_server_stats:create_backend (WorkerModule, stats_process_millis),
  mondemand_server_stats:create_backend (WorkerModule, stats_send_millis),
  mondemand_server_stats:create_backend (WorkerModule, connection_errors),
  mondemand_server_stats:create_backend (WorkerModule, send_errors),

  % config is grabbed from the server
  Config = mondemand_server_config:backend_config (WorkerModule),

  HandlerMod = proplists:get_value (handler_mod, Config, undefined),
  WorkerMod = proplists:get_value (worker_mod, Config, undefined),
  Prefix = proplists:get_value (prefix, Config, undefined),

  gproc_pool:connect_worker (SupervisorName, WorkerName),
  case WorkerMod:create (Config) of
    {error, E} ->
      {stop, {error, E} };
    WorkerState ->
      {ok, #state {
             handler_mod = HandlerMod,
             worker_name = WorkerName,
             worker_mod = WorkerMod,
             worker_state = WorkerState,
             prefix = Prefix
          }
      }
  end.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, UDP = {udp,_,_,_,_}},
             State) ->
  Event = mondemand_event:from_udp (UDP),
  handle_cast ({process, Event}, State);
handle_cast ({process, Event = #md_event {}},
             State = #state { handler_mod = HandlerMod,
                              worker_state = Worker,
                              worker_mod = WorkerModule,
                              prefix = Prefix
                            }) ->
  PreProcess = os:timestamp (),
  % if the handler_mod is undefined, just pass through the event unchanged
  {NumGood, Data} =
    case HandlerMod of
      undefined ->
        {1, Event};
      _ ->
        {NumBad, G, Lines} =
          mondemand_backend_stats_formatter:process_event (Prefix,
                                                           Event, HandlerMod),
        mondemand_server_stats:increment_backend
          (WorkerModule, stats_dropped_count, NumBad),
        {G, Lines}
    end,
  PostProcess = os:timestamp (),
  ProcessMillis =
    webmachine_util:now_diff_milliseconds (PostProcess, PreProcess),
  mondemand_server_stats:increment_backend
    (WorkerModule, stats_process_millis, ProcessMillis),

  mondemand_server_stats:increment_backend (WorkerModule, events_processed),

  send_data (WorkerModule, Worker, NumGood, Data),

  { noreply, State };
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, #state { worker_state = Worker,
                             worker_mod = WorkerMod }) ->
  WorkerMod:destroy (Worker),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


send_data (WorkerModule, Worker, NumData, Data) ->
  SendStart = os:timestamp (),
  case WorkerModule:send (Worker, Data) of
    ok ->
      mondemand_server_stats:increment_backend
        (WorkerModule, stats_sent_count, NumData);
    error ->
      mondemand_server_stats:increment_backend
        (WorkerModule, stats_dropped_count, NumData),
      mondemand_server_stats:increment_backend
        (WorkerModule, send_errors)
  end,
  SendFinish = os:timestamp (),
  SendMillis = webmachine_util:now_diff_milliseconds (SendFinish, SendStart),
  mondemand_server_stats:increment_backend
    (WorkerModule, stats_send_millis, SendMillis),
  ok.
