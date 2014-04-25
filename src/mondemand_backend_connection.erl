-module (mondemand_backend_connection).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (gen_server).

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
                  name,
                  stat_modfun,
                  reconnect_min,
                  reconnect_max,
                  reconnect_time = 0,
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

  TransportMod = proplists:get_value (transport_mod, Config, undefined),

  Prefix = proplists:get_value (prefix, Config),
  ReconnectMin = proplists:get_value (reconnect_min, Config, 10),
  ReconnectMax = proplists:get_value (reconnect_max, Config, 30000),
  ModFun = proplists:get_value (stat_modfun, Config, undefined),

  { ValidModFun, {Mod, Fun} } =
    case is_tuple (ModFun) andalso tuple_size (ModFun) =:= 2 of
      true ->
        {M, F} = ModFun,
        code:ensure_loaded (M),
        case erlang:function_exported (M, F, 8) of
          true -> { true, {M, F} };
          false -> { false, {undefined, undefined} }
        end;
      false ->
        {false, {undefined, undefined} }
    end,

  case ValidModFun of
    true ->
      case TransportMod:init (Config) of
        {ok, Transport} ->
          State = #state {
                    transport = Transport,
                    transport_mod = TransportMod,
                    prefix = Prefix,
                    name = Name,
                    stat_modfun = fun Mod:Fun/8,
                    reconnect_min = ReconnectMin,
                    reconnect_max = ReconnectMax
                  },
          { ok, try_connect (State) };
        E ->
          {stop, E}
      end;
    false ->
      { stop, bad_stat_modfun }
   end.

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


% when we don't have a connection we don't want keep trying the backend
% thus the reconnecting logic, so we still process the event to figure
% out how many we are dropping then just increment a counter
handle_cast ({process, Binary},
             State = #state { transport = Transport,
                              transport_mod = TransportMod,
                              prefix = Prefix,
                              stat_modfun = ModFun,
                              events_processed = EventProcessed,
                              stats_sent_count = StatsSent,
                              stats_dropped_count = StatsDropped,
                              send_errors = SendErrors

                            }) ->
  {Num, Lines} = process_event (Prefix, Binary, ModFun),
  case TransportMod:connected (Transport) of
    false ->
      % Not connected, let the reconnect logic reconnect, just drop for now
      { noreply,
        State#state { events_processed = EventProcessed + 1,
                      stats_dropped_count = StatsDropped + Num
                    }
      };
    true ->
      case TransportMod:send (Transport, Lines) of
        {ok, NewTransport} ->
          { noreply, State#state {
                       transport = NewTransport,
                       events_processed = EventProcessed + 1,
                       stats_sent_count = StatsSent + Num
                     }
          };
        {_, NewTransport} ->
          { noreply, try_connect (
                       State#state {
                         transport = NewTransport,
                         events_processed = EventProcessed + 1,
                         stats_dropped_count = StatsDropped + Num,
                         send_errors = SendErrors + 1
                       }
                     )
          }
      end
  end;
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (try_connect, State) ->
  { noreply, try_connect (State) };
%handle_info ({tcp_closed, _}, State = #state {
%                                        socket = Socket,
%                                        transport = Transport,
%                                        connection_errors = ConnectErrors
%                                      }) ->
%  close (Transport, Socket),
%  { noreply, try_connect (
%               State#state {
%                 socket = undefined,
%                 connection_errors = ConnectErrors + 1
%               }
%             )
%  };
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
                              connection_errors = ConnectErrors
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
      erlang:send_after (ReconnectTime, self(), try_connect),

      State#state { reconnect_time = ReconnectTime,
                    connection_errors = ConnectErrors + 1  }
  end.

process_event (Prefix, Binary, LineFormatter) ->
  % deserialize the event as a dictionary
  Event =  lwes_event:from_udp_packet (Binary, dict),

  % grab out the attributes
  #lwes_event { attrs = Data } = Event,

  % here's the timestamp
  Timestamp = trunc (dict:fetch (<<"ReceiptTime">>, Data) / 1000),

  % here's the name of the program which originated the metric
  ProgId = dict:fetch (<<"prog_id">>, Data),

  ContextWithHost = mondemand_server_util:construct_context (Event),

  % here's the host, and the rest of the context as a proplist
  {SenderHost, Context} =
    case lists:keytake (<<"host">>, 1, ContextWithHost) of
      false -> {"unknown", ContextWithHost};
      {value, {<<"host">>, Host}, ContextOut} -> {Host, ContextOut}
    end,

  Num = dict:fetch (<<"num">>, Data),
  { Num,
    lists:map (
      fun (E) ->
        MetricType = dict:fetch (mondemand_server_util:stat_type (E), Data),
        MetricName = dict:fetch (mondemand_server_util:stat_key (E), Data),
        MetricValue = dict:fetch (mondemand_server_util:stat_val (E), Data),
        LineFormatter (Prefix, ProgId, SenderHost, MetricType, MetricName,
                       MetricValue, Timestamp, Context)
      end,
      lists:seq (1, Num)
    )
  }.
