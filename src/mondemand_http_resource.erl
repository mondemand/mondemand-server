-module (mondemand_http_resource).

-export ([ init/1,
           finish_request/2,
           allowed_methods/2,
           process_post/2
         ]).

-include_lib ("webmachine/include/webmachine.hrl").

init (_) -> { ok, []}.

allowed_methods (ReqData, State) ->
  {['POST'], ReqData, State}.

process_post (ReqData, State) ->
  mondemand_server_dispatcher_sup:dispatch
    ( { udp, none,
        request_ip (ReqData),
        server_port (),
        wrq:req_body (ReqData)
      }
    ),
  {true, ReqData, State}.

finish_request (ReqData, State) ->
  { true, ReqData, State }.

%%---------------------------------
%%% Internal functions
%%---------------------------------
request_ip (ReqData) -> 
  {ok, Ip} = inet_parse:address (wrq:peer (ReqData)),
  Ip.

server_port () ->
  proplists:get_value (port,
    mondemand_server_config:web_config()).

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
