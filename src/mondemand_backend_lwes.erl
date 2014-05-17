-module (mondemand_backend_lwes).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").

-behaviour (mondemand_server_backend).
-behaviour (gen_server).

%% mondemand_backend callbacks
-export ([ start_link/1,
           process/1,
           required_apps/0,
           type/0
         ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, { config,
                  channels
                }).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================
start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

required_apps () ->
  [ lwes ].

type () ->
  worker.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (Config) ->
  LwesConfig = proplists:get_value (lwes, Config, undefined),

  case lwes:open (emitter, LwesConfig) of
    {ok, Channels} ->
      mondemand_server_stats:init_backend (?MODULE, events_processed),
      {ok, #state { config = LwesConfig,
                    channels = Channels }};
    {error, E} ->
      {stop, {error, E}}
  end.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, {udp,_Port,_SenderIp,_SenderPort,Event}},
             State = #state { channels = ChannelsIn }) ->
  ChannelsOut = lwes:emit (ChannelsIn, Event),
  mondemand_server_stats:increment_backend (?MODULE, events_processed),
  { noreply, State#state { channels = ChannelsOut } };

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  { ok, State }.

%%====================================================================
%% Internal
%%====================================================================
