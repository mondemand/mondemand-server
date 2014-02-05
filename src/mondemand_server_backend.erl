-module (mondemand_server_backend).

-export ([behaviour_info/1]).

behaviour_info(callbacks) ->
  [ {start_link, 1},
    {process, 1},
    {stats, 0},
    {required_apps, 0}
  ];
behaviour_info(_) ->
  undefined.
