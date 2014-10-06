-module (mondemand_backend_connection).

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

-record (state, { transport,
                  transport_mod,
                  prefix,
                  worker_name,
                  worker_mod,
                  handler_mod,
                  response_state,
                  reconnect_min,
                  reconnect_max,
                  reconnect_time = 0
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
  mondemand_server_stats:init_backend (WorkerModule, stats_process_millis),
  mondemand_server_stats:init_backend (WorkerModule, stats_send_millis),
  mondemand_server_stats:init_backend (WorkerModule, connection_errors),
  mondemand_server_stats:init_backend (WorkerModule, send_errors),

  % config is grabbed from the server
  Config = mondemand_server_config:backend_config (WorkerModule),

  TransportMod = proplists:get_value (transport_mod, Config, undefined),
  Prefix = proplists:get_value (prefix, Config),
  ReconnectMin = proplists:get_value (reconnect_min, Config, 10),
  ReconnectMax = proplists:get_value (reconnect_max, Config, 30000),
  HandlerMod = proplists:get_value (handler_mod, Config, undefined),

  case TransportMod:init (Config) of
    {ok, Transport} ->
      gproc_pool:connect_worker (SupervisorName, WorkerName),
      State = #state {
                transport = Transport,
                transport_mod = TransportMod,
                prefix = Prefix,
                worker_name = WorkerName,
                worker_mod = WorkerModule,
                handler_mod = HandlerMod,
                reconnect_min = ReconnectMin,
                reconnect_max = ReconnectMax
              },
      { ok, try_connect (State) };
    E ->
      {stop, E}
  end.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.


% when we don't have a connection we don't want keep trying the backend
% thus the reconnecting logic, so we still process the event to figure
% out how many we are dropping then just increment a counter
handle_cast ({process, Binary, Timestamp},
             State = #state { transport = Transport,
                              transport_mod = TransportMod,
                              worker_mod = WorkerModule,
                              prefix = Prefix,
                              handler_mod = HandlerMod
                            }) ->
  PreProcess = os:timestamp (),
  {NumBad, NumGood, Lines} =
    mondemand_backend_stats_formatter:process_event (Prefix,
                                                     Binary,
                                                     Timestamp,
                                                     HandlerMod),
  PostProcess = os:timestamp (),
  ProcessMillis =
    webmachine_util:now_diff_milliseconds (PostProcess, PreProcess),

  mondemand_server_stats:increment_backend
    (WorkerModule, stats_process_millis, ProcessMillis),
  mondemand_server_stats:increment_backend (WorkerModule, events_processed),

  case TransportMod:connected (Transport) of
    false ->
      mondemand_server_stats:increment_backend
        (WorkerModule, stats_dropped_count, NumBad + NumGood),
      % Not connected, let the reconnect logic reconnect, just drop for now
      { noreply, State };
    true ->
      SendStart = os:timestamp (),
      case TransportMod:send (Transport, Lines) of
        {ok, NewTransport} ->
          SendFinish = os:timestamp (),
          SendMillis =
            webmachine_util:now_diff_milliseconds (SendFinish, SendStart),

          mondemand_server_stats:increment_backend
            (WorkerModule, stats_send_millis, SendMillis),
          mondemand_server_stats:increment_backend
            (WorkerModule, stats_dropped_count, NumBad),
          mondemand_server_stats:increment_backend
            (WorkerModule, stats_sent_count, NumGood),

          { noreply, State#state { transport = NewTransport } };
        {_, NewTransport} ->
          SendFinish = os:timestamp (),
          SendMillis =
            webmachine_util:now_diff_milliseconds (SendFinish, SendStart),

          mondemand_server_stats:increment_backend
            (WorkerModule, stats_send_millis, SendMillis),
          mondemand_server_stats:increment_backend
            (WorkerModule, stats_dropped_count, NumBad + NumGood),
          mondemand_server_stats:increment_backend
            (WorkerModule, send_errors),

          { noreply, try_connect (State#state { transport = NewTransport }) }
      end
  end;
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (try_connect, State) ->
  { noreply, try_connect (State) };
handle_info (Other, State = #state { transport = Transport,
                                     transport_mod = TransportMod,
                                     handler_mod = HandlerMod,
                                     worker_mod = WorkerModule,
                                     response_state = PreviousResponseState
                                   }) ->
  % first we let the transport a chance to handle the response, mostly
  % to unpack and handle errors, then it's handed to the handler to
  % do something
  case TransportMod:handle_info (Transport, Other) of
    { ok, NewTransport, TransportResponse } ->
      { Errors, NewResponseState } =
        HandlerMod:handle_response (TransportResponse, PreviousResponseState),

      mondemand_server_stats:increment_backend
        (WorkerModule, stats_dropped_count, Errors),
      mondemand_server_stats:increment_backend
        (WorkerModule, stats_sent_count, -Errors),

      { noreply, State#state { transport = NewTransport,
                               response_state = NewResponseState
                             }
      };
    { error, NewTransport } ->

      mondemand_server_stats:increment_backend
        (WorkerModule, connection_errors),

      { noreply, try_connect (
                   State#state {
                     transport = NewTransport,
                     response_state = undefined
                   }
                 )
      }
  end;
handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, #state { transport = Transport,
                             transport_mod = TransportMod }) ->
  TransportMod:close (Transport),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

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

try_connect (State = #state { transport = Transport,
                              transport_mod = TransportMod,
                              worker_mod = WorkerModule
                            } ) ->
  % close the old connection
  TransportMod:close (Transport),

  % then attempt a new connection
  case TransportMod:connect (Transport) of
    {ok, NewTransport} ->
      State#state { transport = NewTransport, reconnect_time = 0 };
    Error ->
      ReconnectTime = reconnect_time (State),
      error_logger:error_msg (
        "~p : connection with ~p failed with ~p, "
        "trying again in ~p milliseconds",
        [self(), TransportMod, Error, ReconnectTime]),
      mondemand_server_stats:increment_backend
        (WorkerModule, connection_errors),
      erlang:send_after (ReconnectTime, self(), try_connect),

      State#state { reconnect_time = ReconnectTime }
  end.


