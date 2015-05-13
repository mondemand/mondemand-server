-module (mondemand_backend_transport_tcp).

-export ([init/1,            % (Config)
          connected/1,       % (State) -> true | false
          connect/1,         % (Config)
          send/2,            % (State, Data)
          close/1,           % (State)
          handle_info/2      % (State, Response)
         ]).

-record (md_tcp_state, { host, port, connect_timeout, send_timeout, socket }).

init (Config) ->
  Host = proplists:get_value (host, Config),
  Port = proplists:get_value (port, Config),
  ConnectTimeout = proplists:get_value (connect_timeout, Config, 5000),
  SendTimeout = proplists:get_value (send_timeout, Config, 5000),
  case Host =/= undefined andalso Port =/= undefined of
    true ->
      {ok, #md_tcp_state {
            host = Host,
            port = Port,
            connect_timeout = ConnectTimeout,
            send_timeout = SendTimeout,
            socket = undefined
          } };
    false ->
      {error, bad_host_or_port}
  end.

connected (#md_tcp_state { socket = undefined }) -> false;
connected (_) -> true.

connect (State = #md_tcp_state {
                   host = Host,
                   port = Port,
                   connect_timeout = ConnectTimeout,
                   send_timeout = SendTimeout }) ->
  case gen_tcp:connect (Host, Port,
                        [{mode, list}, {send_timeout, SendTimeout}],
                        ConnectTimeout) of
    {ok, Socket} ->
      {ok, State#md_tcp_state { socket = Socket }};
    {error, Error} ->
      {{error, Error}, State}
  end.

send (State, []) ->
  {error, State};
send (State = #md_tcp_state { socket = Socket}, Data) ->
  { gen_tcp:send (Socket, Data), State }.

close (#md_tcp_state { socket = undefined}) ->
  ok;
close (#md_tcp_state { socket = Socket}) ->
  gen_tcp:close (Socket).

handle_info (State, {tcp_closed, _}) ->
  { error, State };
handle_info (State, {tcp, _, Response}) ->
  { ok, State, Response }.
