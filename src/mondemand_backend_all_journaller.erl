-module (mondemand_backend_all_journaller).

-include_lib ("lwes/include/lwes.hrl").

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

-record (state, { config, journal }).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================

start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

required_apps () ->
  [ ].

type () ->
  worker.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (Config) ->

  % ensure directories exist
  Dir = proplists:get_value (root, Config, "."),
  mondemand_server_util:mkdir_p (Dir),
  NewConfig = lists:keystore (root, 1, Config, {root, filename:join (Dir)}),

  % open journal file
  {ok, Journal} = lwes_journaller:start_link (NewConfig),

  % initialize all stats to zero
  mondemand_server_stats:create_backend (?MODULE, events_processed),

  {ok, #state { config = NewConfig, journal = Journal }}.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Event}, #state { journal = Journal }) ->
  JournalOut = lwes_journaller:process_event (
                 mondemand_event:to_udp (Event),
                 Journal),
  mondemand_server_stats:increment_backend (?MODULE, events_processed),
  {noreply, #state { journal = JournalOut }};

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal
%%====================================================================


