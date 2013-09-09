-module (mondemand_backend_trace_couchdb).

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
                  couch,
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
  [ sasl, crypto, public_key, ssl, ibrowse, couchbeam ].

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (Config) ->
  {CouchHost,CouchPort, CouchUser, CouchPassword} =
    proplists:get_value (couch, Config),

  % open and test couch connection
  Server =
    couchbeam:server_connection (CouchHost, CouchPort, "",
                                 [{basic_auth, {CouchUser, CouchPassword}}]),

  % initialize all stats to zero
  InitialStats =
    mondemand_server_util:initialize_stats ([ errors, processed ] ),

  case couchbeam:server_info (Server) of
    {ok, _} ->
      {ok, Couch} = couchbeam:open_or_create_db (Server, "traces"),
      { ok, #state{ config = Config, couch = Couch, stats = InitialStats }};
    {error, _} ->
      io:format ("ERROR: Can't start CouchDB not running!~n"),
      {stop, no_couchdb}
  end.

handle_call ({stats}, _From,
             State = #state { stats = Stats }) ->
  { reply, Stats, State };
handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Event},
             State = #state { couch = Couch,
                              stats = Stats
                            }) ->
  Doc = lwes_event:from_udp_packet (Event, json_eep18),

  NewStats =
    case couchbeam:save_doc (Couch, Doc) of
      {ok, _Doc1} ->
         mondemand_server_util:increment_stat (processed, Stats);
      _ ->
         mondemand_server_util:increment_stat (errors, Stats)
    end,

  {noreply, State#state { stats = NewStats }};

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


