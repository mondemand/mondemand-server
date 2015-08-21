-module (mondemand_server).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (gen_server).

%% API
-export ([ start_link/0,
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
-record (listener_state, {}).

%%====================================================================
%% API
%%====================================================================
start_link () ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, [], []).

process_event (Event, State) ->
  % just ignore errors, so the whole server doesn't crash
  try mondemand_server_dispatcher_sup:dispatch (Event) of
    _ -> ok
  catch
    E1:E2 ->
      error_logger:error_msg ("Error dispatching : ~p:~p",[E1,E2])
  end,
  State.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([]) ->
  % I want terminate to be called
  process_flag (trap_exit, true),

  % lwes listener config
  case mondemand_server_config:listener_config () of
    undefined ->
      { stop, missing_listener_config };
    { ok, L } when is_list (L) ->
      Channels = [ open_lwes_channel (Config) || Config <- L ],
      { ok, #state {listeners = Channels } };
    { ok, Config} ->
      Channel = open_lwes_channel (Config),
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
open_lwes_channel (LwesConfig) ->
  {ok, Channel} = lwes:open (listener, LwesConfig),
  ok = lwes:listen (Channel, fun process_event/2, raw, #listener_state{ }).

%%====================================================================
%% Test functions
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% include test code here

-endif.
