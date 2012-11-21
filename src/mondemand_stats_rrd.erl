-module (mondemand_stats_rrd).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").

-behaviour (gen_server).

%% API
-export ([ start_link/1,
           process/1 ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, {config, root, context_delimiter}).

%%====================================================================
%% API
%%====================================================================
start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (Config) ->
  Dir = proplists:get_value (root, Config, "."),
  Delimiter = proplists:get_value (context_delimiter, Config, "-"),

  mondemand_util:mkdir_p (Dir),

  { ok, #state {
          config = Config,
          root = filename:join (Dir),
          context_delimiter = Delimiter
        }
  }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Binary},
             State = #state { root = Dir,
                              context_delimiter = Delimiter }) ->

  Event =  lwes_event:from_udp_packet (Binary, dict),
  #lwes_event { attrs = Data } = Event,

  Timestamp = dict:fetch (<<"ReceiptTime">>, Data),
  SecsSinceEpoch = trunc (Timestamp / 1000),

  Num = dict:fetch (<<"num">>, Data),
  ProgId = dict:fetch (<<"prog_id">>, Data),
  {Host, ContextString} =
    mondemand_util:construct_context_string (Event, Delimiter),

  [ begin
      T = dict:fetch (mondemand_util:stat_type (E), Data),
      K = dict:fetch (mondemand_util:stat_key (E), Data),
      V = dict:fetch (mondemand_util:stat_val (E), Data),
      FilePath =
        binary_to_list (
          filename:join([Dir,
                         ProgId,K,
                         [ProgId,"-",T,"-",K,"-",Host,
                          case ContextString of
                            [] -> [];
                            _ -> ["-",ContextString]
                          end,
                          ".rrd"]])),

      ok = mondemand_util:mkdir_p (filename:join([Dir, ProgId, K])),
      update (FilePath, T, SecsSinceEpoch, V)
    end
    || E
    <- lists:seq (1,Num)
  ],
  {noreply, State};

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

update (File, Type, Timestamp, Value) ->
  case file:read_file_info (File) of
    {ok, _} -> ok;
    _ ->
      {ok, _} =
        case Type of
          <<"counter">> -> create_counter (File);
          <<"gauge">> -> create_gauge (File);
          E ->
            error_logger:error_msg (
              "mondemand_stats_rrd:update/4 : Got ~p for Type ~p",[E,Type])
        end
  end,
  case
    erlrrd:update ([
        io_lib:fwrite ("~s",[File]),
        io_lib:fwrite (" ~b:~b", [Timestamp,Value])
      ]) of
    {ok, _} -> ok;
    Err ->
      error_logger:error_msg (
        "mondemand_stats_rrd:update/4 : Got ~p from update",[Err])
  end,
  ok.

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
