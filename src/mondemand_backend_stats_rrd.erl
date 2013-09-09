-module (mondemand_backend_stats_rrd).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").

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
                  root,
                  context_delimiter,
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
  [ erlrrd ].

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (Config) ->
  Dir = proplists:get_value (root, Config, "."),
  Delimiter = proplists:get_value (context_delimiter, Config, "-"),

  mondemand_server_util:mkdir_p (Dir),

  % initialize all stats to zero
  InitialStats =
    mondemand_server_util:initialize_stats ([ errors, processed ] ),

  { ok, #state {
          config = Config,
          root = filename:join (Dir),
          context_delimiter = Delimiter,
          stats = InitialStats
        }
  }.

handle_call ({stats}, _From,
             State = #state { stats = Stats }) ->
  { reply, Stats, State };
handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Binary},
             State = #state { root = Dir,
                              context_delimiter = Delimiter,
                              stats = Stats }) ->
  Event =  lwes_event:from_udp_packet (Binary, dict),
  #lwes_event { attrs = Data } = Event,

  SecsSinceEpoch =
   case find (<<"ReceiptTime">>, Data, undefined) of
     undefined -> mondemand_server_util:seconds_since_epoch ();
     Timestamp -> trunc (Timestamp / 1000)
   end,

  Num = find (<<"num">>, Data, 0),
  ProgId = find (<<"prog_id">>, Data, <<"unknown">>),
  {Host, ContextString} =
    mondemand_server_util:construct_context_string (Event, Delimiter),

  % perform updates keeping track of number processed and number of errors
  {TotalProcessed, TotalErrors} =
    lists:foldl (
      fun (E, {Processed, Errors}) ->
        T = find (mondemand_server_util:stat_type(E), Data,<<"counter">>),
        K = find (mondemand_server_util:stat_key(E), Data, <<"unknown">>),
        V = find (mondemand_server_util:stat_val(E), Data, 1),
        FileName = list_to_binary ([ProgId,"-",T,"-",K,"-",Host,
                                    case ContextString of
                                      [] -> [];
                                      _ -> ["-",ContextString]
                                    end,
                                    ".rrd"]),
        FilePath =
          binary_to_list (
            filename:join([Dir,
                           ProgId,
                           K,
                           FileName
                          ])
          ),
        case mondemand_server_util:mkdir_p (filename:join([Dir, ProgId, K])) of
          ok ->
             case update (FilePath, T, SecsSinceEpoch, V) of
               ok -> { Processed + 1, Errors };
               _ -> { Processed + 1, Errors + 1 }
             end;
          _ ->
            { Processed + 1, Errors + 1 }
        end
      end,
      {0, 0},
      lists:seq (1,Num)),

  NewStats =
    mondemand_server_util:increment_stat (processed, TotalProcessed,
      mondemand_server_util:increment_stat (errors, TotalErrors,
        Stats)),

  {noreply, State#state { stats = NewStats } };

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

find (Key, Data, Default) ->
  case dict:find (Key, Data) of
    error -> Default;
    {ok, Val} -> Val
  end.

% attempt an update, and return 0 if the update succeeds and 1 if it fails
update (File, Type, Timestamp, Value) ->
  case maybe_create (Type, File) of
    { ok, _ } ->
      case
        erlrrd:update ([
            io_lib:fwrite ("~s",[File]),
            io_lib:fwrite (" ~b:~b", [Timestamp,Value])
          ]) of
        {ok, _} ->
          ok;
        Err ->
          error_logger:error_msg (
            "Unable to update '~p:~p:~p:~p' because of ~p",
            [File, Type, Timestamp, Value, Err]),
          error
      end;
    {error, Error} ->
       error_logger:error_msg (
        "Unable to create '~p' because of ~p",[File, Error]),
      error
  end.

maybe_create (Type, File) ->
  case file:read_file_info (File) of
    {ok, I} -> {ok, I};
    _ ->
      case Type of
        <<"counter">> -> create_counter (File);
        <<"gauge">> -> create_gauge (File);
        _ -> create_counter (File) % default is counter
      end
  end.

create_counter (File) ->
  erlrrd:create ([
      io_lib:fwrite ("~s",[File]),
      " --step \"60\""
      " --start \"now - 90 days\""
      " \"DS:value:DERIVE:900:0:U\""
      " \"RRA:AVERAGE:0.5:1:44640\""
      " \"RRA:AVERAGE:0.5:15:9600\""
      " \"RRA:AVERAGE:0.5:1440:400\""
    ]).

create_gauge (File) ->
  erlrrd:create ([
      io_lib:fwrite ("~s",[File]),
      " --step \"60\""
      " --start \"now - 90 days\""
      " \"DS:value:GAUGE:900:U:U\""
      " \"RRA:AVERAGE:0.5:1:44640\""
      " \"RRA:AVERAGE:0.5:15:9600\""
      " \"RRA:AVERAGE:0.5:1440:400\""
    ]).
