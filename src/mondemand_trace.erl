-module (mondemand_trace).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (gen_server).

%% API
-export ([ start_link/1,
           process/1,
           error_count/0
         ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, { config, couch, error_count = 0}).

%%====================================================================
%% API
%%====================================================================
start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

error_count () ->
  gen_server:call (?MODULE, {error_count}).

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
  case couchbeam:server_info (Server) of
    {ok, _} ->
      {ok, Couch} = couchbeam:open_or_create_db (Server, "traces"),
      { ok, #state{ config = Config, couch = Couch }};
    {error, _} ->
      io:format ("ERROR: Can't start CouchDB not running!~n"),
      {stop, no_couchdb}
  end.

handle_call ({error_count}, _From,
             State = #state { error_count = ErrorCount }) ->
  { reply, ErrorCount, State };
handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Event},
             State = #state { couch = Couch,
                              error_count = ErrorCount }) ->
  Doc = lwes_event:from_udp_packet (Event, json_eep18),
  NewErrorCount =
    case couchbeam:save_doc (Couch, Doc) of
      {ok, _Doc1} -> ErrorCount;
      _ -> ErrorCount + 1
    end,
  {noreply, State#state { error_count = NewErrorCount }};

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


