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
  mondemand_backend_trace_file:process_tcp
    (request_metadata (ReqData), 
     wrq:req_body(ReqData)),
  {true, ReqData, State}.

finish_request (ReqData, State) ->
  { true, ReqData, State }.

%%---------------------------------
%%% Internal functions
%%---------------------------------
%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
request_metadata (ReqData) ->
 [{ <<"SenderIP">>, list_to_binary(wrq:peer(ReqData)) },
  { <<"SenderPort">>, server_port() },
  { <<"ReceiptTime">>, millisecond_since_epoch () }].

server_port () ->
  proplists:get_value (port, 
    mondemand_server_config:web_config()).

millisecond_since_epoch () ->
  {Meg, Sec, Mic} = os:timestamp(),
  trunc (Meg * 1000000000 + Sec * 1000 + Mic / 1000).


-ifdef(HAVE_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

-endif.
