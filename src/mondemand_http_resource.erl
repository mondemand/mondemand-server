-module (mondemand_http_resource).

-export ([ init/1,
           finish_request/2,
           allowed_methods/2,
           process_post/2
         ]).

-include_lib ("webmachine/include/webmachine.hrl").

-record (state, {port, num_dispatchers}).

init (_) -> { ok, initial_state() }.

allowed_methods (ReqData, State) ->
  {['POST'], ReqData, State}.

process_post (ReqData, State = #state { port = Port, num_dispatchers = Num}) ->
  mondemand_server_dispatcher_sup:dispatch
    ( Num,
      { udp, 0,
        request_ip (ReqData),
        Port,
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

initial_state () ->
  All = mondemand_server_config:all(),
  Port = proplists:get_value (port, mondemand_server_config:web_config(All)),
  Num = mondemand_server_config:num_dispatchers (All),
  #state { port = Port, num_dispatchers = Num}.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
