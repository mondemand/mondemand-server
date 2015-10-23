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

        {ok, Other, NG, NB} =
          HandlerMod:format_stat (Current, Num, Prefix, ProgId, Host,
                                  MetricType, MetricName, MetricValue,
                                  Timestamp, Context),
        case {Separator =/= undefined, Current =/= Num} of
          {true, true} ->
            { Errors + NB, Okay + NG, Current + 1, [Separator, Other | List] };
          {false, true} ->
            { Errors + NB, Okay + NG, Current + 1, [ Other | List ] };
          {_, false} -> { Errors + NB, Okay + NG, Current + 1, [Other | List ] }
        end
      end,
      { 0, 0, 1, [] },
      Metrics
    ),
  case NumGood > 0 of
    true ->
      Header = HandlerMod:header(),
      Footer = HandlerMod:footer(),
      case {Header =/= undefined, Footer =/= undefined} of
        {true, true} -> { NumBad, NumGood, [ Header, Entries, Footer ] };
        {true, false} -> { NumBad, NumGood, [ Header, Entries ] };
        {false, true} -> { NumBad, NumGood, [ Entries, Footer ] };
        {false, false} -> { NumBad, NumGood, Entries }
      end;
    false ->
      { NumBad, NumGood, [] }
  end.
