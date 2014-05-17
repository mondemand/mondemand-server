-module (mondemand_server_stats).

-export ([
           prog_id/0,
           init/1,
           increment/1,
           increment/2,
           init_backend/2,
           increment_backend/2,
           increment_backend/3
         ]).

prog_id () ->
  mondemand_server.

init (Metric) ->
  mondemand:increment (prog_id(), Metric, 0).

increment (Metric) ->
  mondemand:increment (prog_id(), Metric, 1).

increment (Metric, Value) ->
  mondemand:increment (prog_id(), Metric, Value).

init_backend (Backend, Metric) ->
  mondemand:increment (prog_id(), Metric, 0, [{backend, Backend}]).

increment_backend (Backend, Metric) ->
  mondemand:increment (prog_id(), Metric, 1, [{backend, Backend}]).

increment_backend (Backend, Metric, Value) ->
  mondemand:increment (prog_id(), Metric, Value, [{backend, Backend}]).
