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

-record (state, { socket,
                  host,
                  port,
                  transport,
                  prefix,
                  connect_timeout,
                  send_timeout,
                  proto,
                  name,
                  modfun,
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

  Host = proplists:get_value (host, Config, undefined),
  Port = proplists:get_value (port, Config, undefined),
  Transport = proplists:get_value (transport, Config, tcp),
  ConnectTimeout = proplists:get_value (connect_timeout, Config, 5000),
  SendTimeout = proplists:get_value (send_timeout, Config, 5000),
  Prefix = proplists:get_value (prefix, Config),
  Proto = proplists:get_value (proto, Config, 1),
  ReconnectMin = proplists:get_value (reconnect_min, Config, 10),
  ReconnectMax = proplists:get_value (reconnect_max, Config, 30000),
  ModFun = proplists:get_value (modfun, Config, undefined),

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

  case ValidModFun andalso Host =/= undefined andalso Port =/= undefined of
    true ->
      State = #state { host = Host,
                       port = Port,
                       transport = Transport,
                       connect_timeout = ConnectTimeout,
                       send_timeout = SendTimeout,
                       prefix = Prefix,
                       proto = Proto,
                       name = Name,
                       modfun = fun Mod:Fun/8,
                       reconnect_min = ReconnectMin,
                       reconnect_max = ReconnectMax
                      },
      { ok, try_connect (State) };
    false ->
      { stop, config_missing_host_or_port_or_modfun }
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
             State = #state { socket = undefined,
                              prefix = Prefix,
                              modfun = ModFun,
                              events_processed = EventProcessed,
                              stats_dropped_count = StatsDropped
                            }) ->
  {Num, _} = process_event (Prefix, Binary, ModFun),
  { noreply,
    State#state { events_processed = EventProcessed + 1,
                  stats_dropped_count = StatsDropped + Num
                }
  };
handle_cast ({process, Binary},
             State = #state { socket = Socket,
                              host = Host,
                              port = Port,
                              transport = Transport,
                              prefix = Prefix,
                              modfun = ModFun,
                              events_processed = EventProcessed,
                              stats_sent_count = StatsSent,
                              stats_dropped_count = StatsDropped,
                              send_errors = SendErrors
                            }) ->
  {Num, Lines} = process_event (Prefix, Binary, ModFun),
  case send (Transport, Socket, Host, Port, Lines) of
    ok ->
      { noreply, State#state {
                   events_processed = EventProcessed + 1,
                   stats_sent_count = StatsSent + Num
                 }
      };
    _ ->
      { noreply, try_connect (
                   State#state {
                     events_processed = EventProcessed + 1,
                     stats_dropped_count = StatsDropped + Num,
                     send_errors = SendErrors + 1
                   }
                 )
      }
  end;
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (try_connect, State) ->
  { noreply, try_connect (State) };
handle_info ({tcp_closed, _}, State = #state {
                                        socket = Socket,
                                        transport = Transport,
                                        connection_errors = ConnectErrors
                                      }) ->
  close (Transport, Socket),
  { noreply, try_connect (
               State#state {
                 socket = undefined,
                 connection_errors = ConnectErrors + 1
               }
             )
  };
handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, #state { socket = Socket, transport = Transport }) ->
  close (Transport, Socket),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% internal functions
%%====================================================================
connect (tcp, Host, Port, ConnectTimeout, SendTimeout) ->
  gen_tcp:connect (Host, Port,
                   [{mode, list}, {send_timeout, SendTimeout}],
                   ConnectTimeout);
connect (udp, _Host, _Port, _ConnectTimeout, _SendTimeout) ->
  gen_udp:open (0, [{sndbuf, 2 * 1024 *1024 }]).

send (udp, Socket, Host, Port, Line) ->
  gen_udp:send (Socket, Host, Port, Line);
send (tcp, Socket, _Host, _Port, Line) ->
  gen_tcp:send (Socket, Line).

close (_, undefined) ->
  ok;
close (udp, Socket) ->
  gen_udp:close (Socket);
close (tcp, Socket) ->
  gen_tcp:close (Socket).

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

try_connect (State = #state { socket = OldSocket,
                              host = Host,
                              port = Port,
                              connect_timeout = ConnectTimeout,
                              send_timeout = SendTimeout,
                              transport = Transport,
                              connection_errors = ConnectErrors
                            } ) ->
  % close the old connection
  close (Transport, OldSocket),

  % then attempt a new connection
  case connect (Transport, Host, Port, ConnectTimeout, SendTimeout) of
    {ok, Socket} ->
      State#state { socket = Socket, reconnect_time = 0 };
    Error ->
      ReconnectTime = reconnect_time (State),
      error_logger:error_msg (
        "~p : connection to ~s:~p failed with ~p, "
        "trying again in ~p milliseconds",
        [self(), Host, Port, Error, ReconnectTime]),
      erlang:send_after (ReconnectTime, self(), try_connect),

      State#state { socket = undefined,
                    reconnect_time = ReconnectTime,
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
