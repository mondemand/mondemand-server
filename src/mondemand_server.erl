-module (mondemand_server).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (gen_server).

%% API
-export ([ start_link/1,
           process_event/2 ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, { listener, dispatch }).
-record (listener_state, { }).

%%====================================================================
%% API
%%====================================================================
start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, [Config], []).

process_event (Event, State) ->
  mondemand_server_dispatcher_sup:dispatch (Event),
  State.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([Dispatch]) ->
  % lwes listener config
  { ok, LwesConfig } = application:get_env (mondemand_server, listener),

  % I want terminate to be called
  process_flag (trap_exit, true),

  % open lwes channel
  {ok, Channel} = lwes:open (listener, LwesConfig),
  ok = lwes:listen (Channel, fun process_event/2, raw, #listener_state{ }),

  { ok, #state { dispatch = Dispatch, listener = Channel } }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("Unrecognized call ~p from ~p~n",[Request, From]),
  { reply, ok, State }.

handle_cast (Request, State) ->
  error_logger:warning_msg ("Unrecognized cast ~p~n",[Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("Unrecognized info ~p~n",[Request]),
  {noreply, State}.

terminate (_Reason, #state { listener = Channel }) ->
  lwes:close (Channel),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

