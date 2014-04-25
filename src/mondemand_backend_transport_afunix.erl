-module (mondemand_backend_transport_afunix).

-export ([init/1,            % (Config) -> State
          connected/1,       % (State) -> true | false
          connect/1,         % (State) -> State
          send/2,            % (State, Data) -> State
          close/1,           % (State) -> ok
          handle_response/2  % (State, Response) -> State
         ]).

-record (md_afunix_state, { path, send_timeout, socket }).

init (Config) ->
  Path = proplists:get_value (path, Config, undefined),
  SendTimeout = proplists:get_value (send_timeout, Config, 5000),
  case Path =/= undefined of
    true ->
      #md_afunix_state { path = Path,
                         send_timeout = SendTimeout };
    false ->
      {error, bad_path}
  end.

connected (#md_afunix_state { socket = undefined }) -> false;
connected (_) -> true.

connect (State = #md_afunix_state {
                    path = Path
                 }) ->
  case afunix:connect (Path,[]) of
    {ok, Socket} ->
      {ok, State#md_afunix_state { socket = Socket }};
    {error, Error} ->
      {{error, Error}, State}
  end.

send (State = #md_afunix_state { socket = Socket}, Data) ->
  {afunix:send (Socket, Data), State}.

close (#md_afunix_state { socket = Socket}) ->
  afunix:close (Socket).

handle_response (State, Response) ->
  io:format ("got ~p~n",[Response]),
  {ok, State}.
