-module (mondemand_server_backend).

-callback start_link(Config :: list()) -> supervisor:startlink_ret().
-callback process(Event :: term()) -> ok.
-callback required_apps () -> list().
-callback type() -> supervisor | worker.
