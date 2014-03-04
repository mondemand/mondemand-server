-module (mondemand_backend_http_trace_resource).

-export ([ init/1,
           content_types_provided/2,
           finish_request/2,
           allowed_methods/2,
           process_post/2,
           to_html/2
         ]).

-include_lib ("webmachine/include/webmachine.hrl").

init (_) -> { ok, []}.

allowed_methods (ReqData, State) ->
  {['POST'], ReqData, State}.

content_types_provided (ReqData, State) ->
  {[{"text/html", to_html}], ReqData, State}.

to_html (ReqData, State) ->
  { <<"Get Called">>, ReqData , State }.

process_post (ReqData, State) ->
  mondemand_server:process
    ( { udp, none, 
        request_ip (ReqData), 
        server_port (),  
        wrq:req_body (ReqData) },
      mondemand_server_config:get_dispatch (dict)),
  {true, ReqData, State}.

finish_request (ReqData, State) ->
  { true, ReqData, State }.

%%---------------------------------
%%% Internal functions
%%---------------------------------
%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------

request_ip (ReqData) -> 
  {ok, Ip} = inet_parse:address (wrq:peer (ReqData)),
  Ip.

server_port () ->
  proplists:get_value (port, 
    mondemand_server_config:web_config()).

-ifdef(HAVE_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

-endif.
