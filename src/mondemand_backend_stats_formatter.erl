-module (mondemand_backend_stats_formatter).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("mondemand/include/mondemand.hrl").

-export ([process_event/3]).

process_event (Prefix, Event, HandlerMod) ->
  % grab out the statsmsg
  StatsMsg = mondemand_event:msg (Event),

  % calculate timestamps for different metrics
  CollectTime =
    case mondemand_statsmsg:collect_time (StatsMsg) of
      undefined -> mondemand_event:receipt_time (Event);
      CT -> CT
    end,
  CollectTimestamp = trunc ( CollectTime / 1000),
  SendTime =
     case mondemand_statsmsg:send_time (StatsMsg) of
       undefined -> mondemand_event:receipt_time (Event);
       ST -> ST
     end,
  SendTimestamp = trunc ( SendTime / 1000),

  % here's the name of the program which originated the metric
  ProgId = mondemand_statsmsg:prog_id (StatsMsg),

  Host = mondemand_statsmsg:host (StatsMsg),
  Context = mondemand_statsmsg:context (StatsMsg),
  Metrics = mondemand_statsmsg:metrics (StatsMsg),

  Num = mondemand_statsmsg:num_metrics (StatsMsg),
  Separator = HandlerMod:separator(),
  { NumBad, NumGood, _, Entries } =
    lists:foldl (
      fun (E, { Errors, Okay, Current, List } ) ->
        { MetricType, MetricName, MetricValue } = mondemand_statsmsg:metric (E),

        Timestamp =
          case MetricType of
            statset -> CollectTimestamp;
            _ -> SendTimestamp
          end,

        case HandlerMod:format_stat (Current, Num, Prefix, ProgId, Host,
                                     MetricType, MetricName, MetricValue,
                                     Timestamp, Context)
        of
          error ->
            error_logger:error_msg (
                "Bad Data in format Prefix=~p,ProgId=~p,SenderHost=~p,"
                "MetricType=~p,MetricName=~p,MetricValue=~p,Timestamp=~p,"
                "Context=~p",[Prefix, ProgId, Host, MetricType,
                              MetricName, MetricValue, Timestamp, Context]),
            % if not on the last one, just keep going
            case Current =/= Num of
              true -> { Errors + 1, Okay, Current + 1, List };
              false ->
                % otherwise we need to remove the trailing separator
                Rest =
                  case List of
                    [] -> [];  % an error at the beginning can result in an
                               % empty list
                    [ Separator | R ] -> R
                  end,
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
      Metrics
    ),
  case NumGood > 0 of
    true ->
      { NumBad, NumGood, [ HandlerMod:header(), Entries, HandlerMod:footer() ] };
    false ->
%      error_logger:error_msg ("No Good Data ~p",[Event]),
      { NumBad, NumGood, [] }
  end.
