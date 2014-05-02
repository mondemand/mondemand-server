-module (mondemand_backend_stats_formatter).

-include_lib ("lwes/include/lwes.hrl").

-export ([process_event/3]).

process_event (Prefix, Binary, HandlerMod) ->
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
  Separator = HandlerMod:separator(),
  { NumBad, NumGood, _, Entries } =
    lists:foldl (
      fun (E, { Errors, Okay, Current, List } ) ->
        MetricType = dict:fetch (mondemand_server_util:stat_type (E), Data),
        MetricName = dict:fetch (mondemand_server_util:stat_key (E), Data),
        MetricValue = dict:fetch (mondemand_server_util:stat_val (E), Data),
        case HandlerMod:format_stat (Prefix, ProgId, SenderHost,
                                     MetricType, MetricName, MetricValue,
                                     Timestamp, Context)
        of
          error ->
            % if not on the last one, just keep going
            case Current =/= Num of
              true -> { Errors + 1, Okay, Current + 1, List };
              false ->
                % otherwise we need to remove the trailing separator
                [ Separator | Rest ] = List,
                { Errors + 1, Okay, Current + 1, Rest }
            end;
          Other ->
            case Current =/= Num of
              true -> { Errors, Okay + 1, Current + 1, [Separator, Other | List] };
              false -> { Errors, Okay + 1, Current + 1, [Other | List ] }
            end
        end
      end,
      { 0, 0, 1, [] },
      lists:seq (1, Num)
    ),
  { NumBad, NumGood, [ HandlerMod:header(), Entries, HandlerMod:footer() ] }.
