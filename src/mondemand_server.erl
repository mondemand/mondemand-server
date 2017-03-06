-module (mondemand_server).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (gen_server).

%% API
-export ([ start_link/2,
           process_event/2 ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, { listeners }).
-record (listener_state, { dispatchers }).

%%====================================================================
%% API
%%====================================================================
start_link (ListenerConfig, DispatchConfig) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE,
                          [ListenerConfig, DispatchConfig], []).

process_event (Event = {udp, P, SenderIp, SenderPort, Data},
               State = #listener_state { dispatchers = Dispatchers }) ->
  % TODO: move this into lwes library?
  %
  % The lwes library will see if the second element of the udp packet is a
  % port or an integer and treat integer as the ReceiptTime.  By setting the
  % receipt time before we forward, this should ensure that backends all get
  % the same receipt time, or don't end up with duplicate's because of delays
  % and quick processing.  This could probably be done in the lwes library
  % since the port is normally not useful to consumers anyway.
  NewEvent =
    case is_port (P) of
      true ->
        {udp, mondemand_util:millis_since_epoch(), SenderIp, SenderPort, Data};
      false ->
        Event
    end,
  % just ignore errors, so the whole server doesn't crash
  try mondemand_server_dispatcher_sup:dispatch (Dispatchers, NewEvent) of
    DispatchersOut -> State#listener_state { dispatchers = DispatchersOut }
  catch
    E1:E2 ->
      error_logger:error_msg ("Error dispatching : ~p:~p",[E1,E2]),
      State
  end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([ListenerConfig, DispatchConfig]) ->
  % I want terminate to be called
  process_flag (trap_exit, true),

  % lwes listener config
  case ListenerConfig of
    undefined ->
      { stop, missing_listener_config };
    L when is_list (L) ->
      Channels = [ open_lwes_channel (Config, DispatchConfig) || Config <- L ],
      { ok, #state {listeners = Channels } };
    Config ->
      Channel = open_lwes_channel (Config, DispatchConfig),
      { ok, #state {listeners = [Channel] } }
  end.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",
                            [?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",
                            [?MODULE, Request]),
  {noreply, State}.

terminate (_Reason, #state { listeners = Channels }) ->
  [ lwes:close (Channel) || Channel <- Channels ],
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
open_lwes_channel (LwesConfig, DispatchConfig) ->
  {ok, Channel} = lwes:open (listener, LwesConfig),
  ok = lwes:listen (
         Channel,
         fun process_event/2,
         raw,
         #listener_state{ dispatchers = DispatchConfig }
       ),
  Channel.

%%====================================================================
%% Test functions
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% include test code here

-endif.
