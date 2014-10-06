-module (mondemand_server_backend).

-export ([behaviour_info/1]).

behaviour_info(callbacks) ->
  [ {start_link, 1},
    {process, 2},
    {required_apps, 0},
    {type, 0}
  ];
behaviour_info(_) ->
  undefined.
