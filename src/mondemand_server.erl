-module (mondemand_server).

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

-record (state, {
                  journal,
                  couch,
                  listener
                }).

%%====================================================================
%% API
%%====================================================================
start_link () ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, [], []).

process_event (Event, A) ->
  gen_server:cast (?MODULE, {process, Event}),
  A.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([]) ->
  % get appication variables
  { ok, Root } = application:get_env (mondemand_server, journal_dir),
  mkdir_p (Root),
  { ok, Name } = application:get_env (mondemand_server, journal_name),
  { ok, Interval } = application:get_env (mondemand_server, rotation_interval),
  { ok, LwesConfig} = application:get_env (mondemand_server, listener),
  { ok, {CouchHost,CouchPort, CouchUser, CouchPassword}} =
    application:get_env (mondemand_server, couch),

  % I want terminate to be called
  process_flag (trap_exit, true),

  % open journal file
  {ok, Journal} = lwes_journaller:start_link ([{root, Root},
                                               {name, Name},
                                               {interval, Interval}]),

  % open and test couch connection
  Server =
    case CouchUser of
      "" -> couchbeam:server_connection (CouchHost, CouchPort, "", []);
      _ -> couchbeam:server_connection (CouchHost, CouchPort, "",
             [{basic_auth, {CouchUser, CouchPassword}}])
    end,

  {ok, _Version} = couchbeam:server_info (Server),
  {ok, Couch} = couchbeam:open_or_create_db (Server, "traces"),

  % open lwes channel
  {ok, Channel} = lwes:open (listener, LwesConfig),
  ok = lwes:listen (Channel, fun process_event/2, raw, {}),

  { ok, #state {
          listener            = Channel,
          journal             = Journal,
          couch               = Couch
        }
  }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("Unrecognized call ~p from ~p~n",[Request, From]),
  { reply, ok, State }.

handle_cast ( {process, Event},
              State = #state { journal = Journal, couch = Couch }) ->

  % couch trace message
  case lwes_event:peek_name_from_udp (Event) of
    <<"MonDemand::StatsMsg">> ->
      ok;
    <<"MonDemand::LogMsg">> ->
      ok;
    <<"MonDemand::TraceMsg">> ->
      Doc = lwes_event:from_udp_packet (Event, json_eep18),
      {ok, _Doc1} = couchbeam:save_doc (Couch, Doc);
    Name ->
      error_logger:info_msg ("Unknown message : ~p", [Name])
  end,

  % journal everything
  JournalOut = lwes_journaller:process_event (Event, Journal),

  { noreply, State#state { journal = JournalOut } };
handle_cast (Request, State) ->
  error_logger:warning_msg ("Unrecognized cast ~p~n",[Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("Unrecognized info ~p~n",[Request]),
  {noreply, State}.

terminate (_Reason, #state {
                      journal = _Journal,
                      couch   = _Couch,
                      listener = Channel
                    }) ->
  lwes:close (Channel),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

% if the directory list of the form
%  [ "foo", "bar", "baz" ]
% I want to filename:join/1 it, so to differentiate this from the
% case where I have mkdir_p("foo"), I need to check for a nested
% list of at least one character, thus [[_|_]|_]
mkdir_p (Dir = [[_|_]|_]) when is_list (Dir) ->
  mkdir_p (filename:join (Dir));
mkdir_p (Dir) ->
  % ensure_dir only seemed to create all but the final level of directories
  % so I need to make_dir right afterward to create the final directory
  ok = filelib:ensure_dir (Dir),
  case file:make_dir (Dir) of
    ok -> ok;
    {error, eexist} -> ok
  end.

