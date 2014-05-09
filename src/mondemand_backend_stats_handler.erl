-module (mondemand_backend_stats_handler).

-export ([behaviour_info/1]).

behaviour_info(callbacks) ->
  [ {header, 0},
    {separator, 0},
    {format_stat, 8},
    {footer, 0},
    {handle_response, 2}
  ];
behaviour_info(_) ->
  undefined.
