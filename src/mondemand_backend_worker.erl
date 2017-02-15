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
  [ {create, 1},      % (Config) -> State
    {connected, 1},   % (State) -> true | false
    {connect, 1},     % (State) -> NewState
    {send, 2},        % (State, Data) -> {ok, NewState} | {{error,_}, NewState}
    {destroy, 1}      % (State) -> ok
  ];
behaviour_info(_) ->
  undefined.

-record (state, { prefix,
                  worker_name,
                  worker_mod,
                  worker_state,
                  handler_mod,
                  reconnect_min,
                  reconnect_max,
                  reconnect_time = 0,
                  pass_raw_data = false
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
  mondemand_server_stats:create_backend (WorkerModule, events_processed),
  mondemand_server_stats:create_backend (WorkerModule, sent_count),
  mondemand_server_stats:create_backend (WorkerModule, dropped_count),
  mondemand_server_stats:create_backend (WorkerModule, process_millis),
  mondemand_server_stats:create_backend (WorkerModule, send_millis),
  mondemand_server_stats:create_backend (WorkerModule, connection_errors),
  mondemand_server_stats:create_backend (WorkerModule, send_errors),

  % config is grabbed from the server
  Config = mondemand_server_config:backend_config (WorkerModule,
                                                   mondemand_server_config:all()
                                                  ),

  WorkerMod = proplists:get_value (worker_mod, Config, undefined),
  Prefix = proplists:get_value (prefix, Config, undefined),
  ReconnectMin = proplists:get_value (reconnect_min, Config, 10),
  ReconnectMax = proplists:get_value (reconnect_max, Config, 30000),
  HandlerMod = proplists:get_value (handler_mod, Config, undefined),
  PassRawData = proplists:get_value (pass_raw_data, Config, false),

  case WorkerMod:create (Config) of
    {ok, WorkerState} ->
      gproc_pool:connect_worker (SupervisorName, WorkerName),
      State = #state {
                 worker_name = WorkerName,
                 worker_mod = WorkerMod,
                 worker_state = WorkerState,
                 prefix = Prefix,
                 handler_mod = HandlerMod,
                 reconnect_min = ReconnectMin,
                 reconnect_max = ReconnectMax,
                 pass_raw_data = PassRawData
              },
      {ok, try_connect (State) };
    E ->
      {stop, E}
  end.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, UDP = {udp,_,_,_,_}},
             State = #state { pass_raw_data = true }) ->
  send_data (State, 0, 1, UDP);
handle_cast ({process, UDP = {udp,_,_,_,_}},
             State) ->
  Event = mondemand_event:from_udp (UDP),
  handle_cast ({process, Event}, State);
handle_cast ({process, Event = #md_event {}},
             State = #state { handler_mod = HandlerMod,
                              worker_mod = WorkerModule,
                              prefix = Prefix
                            }) ->
  PreProcess = os:timestamp (),
  % if the handler_mod is undefined, just pass through the event unchanged
  {NumBad, NumGood, Data} =
    case HandlerMod of
      undefined ->
        {0, 1, Event};
      _ ->
        {B, G, Lines} =
          mondemand_backend_stats_formatter:process_event (Prefix,
                                                           Event, HandlerMod),
        {B, G, Lines}
    end,
  PostProcess = os:timestamp (),
  ProcessMillis =
    webmachine_util:now_diff_milliseconds (PostProcess, PreProcess),
  mondemand_server_stats:increment_backend
    (WorkerModule, process_millis, ProcessMillis),

  send_data (State, NumBad, NumGood, Data);
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (try_connect, State) ->
  { noreply, try_connect (State) };
handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, #state { worker_state = Worker,
                             worker_mod = WorkerModule }) ->
  WorkerModule:destroy (Worker),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


send_data (State = #state { worker_mod = WorkerModule, worker_state = Worker},
           NumBad, NumGood, Data) ->

  mondemand_server_stats:increment_backend (WorkerModule, events_processed),

  case WorkerModule:connected (Worker) of
    false ->
      mondemand_server_stats:increment_backend
          (WorkerModule, dropped_count, NumBad + NumGood),
      % Not connected, let the reconnect logic reconnect, just drop for now
      { noreply, State };
    true ->
      SendStart = os:timestamp (),
      case WorkerModule:send (Worker, Data) of
        {ok, NewState} ->
          SendFinish = os:timestamp (),
          SendMillis =
            webmachine_util:now_diff_milliseconds (SendFinish, SendStart),

          mondemand_server_stats:increment_backend
            (WorkerModule, send_millis, SendMillis),
          mondemand_server_stats:increment_backend
            (WorkerModule, dropped_count, NumBad),
          mondemand_server_stats:increment_backend
            (WorkerModule, sent_count, NumGood),

          { noreply, State#state { worker_state = NewState } };
        {_, NewState} ->
          SendFinish = os:timestamp (),
          SendMillis =
            webmachine_util:now_diff_milliseconds (SendFinish, SendStart),

          mondemand_server_stats:increment_backend
            (WorkerModule, send_millis, SendMillis),
          mondemand_server_stats:increment_backend
            (WorkerModule, dropped_count, NumBad + NumGood),
          mondemand_server_stats:increment_backend
            (WorkerModule, send_errors),

          { noreply, try_connect (State#state { worker_state = NewState }) }
      end
  end.

%%====================================================================
%% internal functions
%%====================================================================
reconnect_time ( #state { reconnect_min = Min, reconnect_time = 0 } ) ->
  Min;  % initially always return the minimum reconnect
reconnect_time ( #state { reconnect_max = Max, reconnect_time = Max } ) ->
  Max;  % if we've already maxxed out our time keep returning the max
reconnect_time ( #state { reconnect_max = Max, reconnect_time = T } ) ->
  Backoff = 2 * T,  % otherwise we backoff in an exponentialy fashion up to max
  case Backoff > Max of
    true -> Max;
    false -> Backoff
  end.

try_connect (State = #state { worker_mod = WorkerModule,
                              worker_state = WorkerState } ) ->
  % close the old connection
  WorkerModule:destroy (WorkerState),

  case WorkerModule:connect (WorkerState) of
    {ok, NewState} ->
      State#state { worker_state = NewState, reconnect_time = 0 };
    Error ->
      ReconnectTime = reconnect_time (State),
      error_logger:error_msg (
        "~p : connection with ~p failed with ~p, "
        "trying again in ~p milliseconds",
        [self(), WorkerModule, Error, ReconnectTime]),
      mondemand_server_stats:increment_backend
        (WorkerModule, connection_errors),
      erlang:send_after (ReconnectTime, self(), try_connect),

      State#state { reconnect_time = ReconnectTime }
  end.
