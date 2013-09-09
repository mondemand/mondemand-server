-module (mondemand_backend_all_journaller).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (mondemand_server_backend).
-behaviour (gen_server).

%% mondemand_backend callbacks
-export ([ start_link/1,
           process/1,
           stats/0,
           required_apps/0
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
                  journal,
                  stats = dict:new ()
                }).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================

start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

stats () ->
  gen_server:call (?MODULE, {stats}).

required_apps () ->
  [ ].

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
  InitialStats =
    mondemand_server_util:initialize_stats ([ errors, processed ] ),

  {ok, #state { config = NewConfig, journal = Journal, stats = InitialStats }}.

handle_call ({stats}, _From,
             State = #state { stats = Stats }) ->
  { reply, Stats, State };
handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Event}, #state { journal = Journal, stats = Stats}) ->
  JournalOut = lwes_journaller:process_event (Event, Journal),
  NewStats = mondemand_server_util:increment_stat (processed, Stats),
  {noreply, #state { journal = JournalOut, stats = NewStats }};

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


