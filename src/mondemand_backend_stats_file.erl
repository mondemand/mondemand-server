-module (mondemand_backend_stats_file).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("mondemand/include/mondemand.hrl").
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
                  root,
                  context_delimiter
                }).

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
  Dir = proplists:get_value (root, Config, "."),
  Delimiter = proplists:get_value (context_delimiter, Config, "-"),

  mondemand_server_util:mkdir_p (Dir),

  % initialize all stats to zero
  mondemand_server_stats:create_backend (?MODULE, events_processed),
  mondemand_server_stats:create_backend (?MODULE, stats_sent_count),

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

handle_cast ({process, UDP = {udp,_,_,_,_}},
             State) ->
  Event = mondemand_event:from_udp (UDP),
  handle_cast ({process, Event}, State);
handle_cast ({process, Event = #md_event {}},
             State = #state { root = Dir,
                              context_delimiter = Delimiter
                            }) ->
  StatsMsg = mondemand_event:msg (Event),

  CollectTime =
    mondemand_server_util:epoch_to_mdyhms (
      mondemand_statsmsg:collect_time (StatsMsg)
    ),
  SendTime =
    mondemand_server_util:epoch_to_mdyhms (
      mondemand_statsmsg:send_time (StatsMsg)
    ),

  % here's the name of the program which originated the metric
  ProgId = mondemand_statsmsg:prog_id (StatsMsg),

  Host = mondemand_statsmsg:host (StatsMsg),
  Context = mondemand_statsmsg:context (StatsMsg),
  Metrics = mondemand_statsmsg:metrics (StatsMsg),
  ContextString =
    mondemand_server_util:construct_context_string (Context, "=", Delimiter),

  mondemand_server_stats:increment_backend (?MODULE, events_processed),

  TotalProcessed =
    lists:foldl (
      fun (E, A) ->
        {T, K, V} = mondemand_statsmsg:metric (E),
        DateTime =
          case T of
            statset -> CollectTime;
            _ -> SendTime
          end,
        RawLogLines =
          case T of
            statset ->
              [
                format_line (DateTime, Host, ProgId, ST, K, SV, ContextString)
                || {ST,SV}
                <- mondemand_statsmsg:statset_to_list (V)
              ];
            _ ->
              [ format_line (DateTime, Host, ProgId, T, K, V, ContextString) ]
          end,

        RawDir =
          filename:join (
            [Dir,
             mondemand_server_util:mdyhms_to_dir_prefix (DateTime),
             ProgId]),
        ok = mondemand_server_util:mkdir_p (RawDir),
        RawFileName = filename:join ([RawDir, Host]),
        {ok, RawFile} = file:open (RawFileName, [append]),
        [ io:format (RawFile, "~s",[RawLogLine]) || RawLogLine <- RawLogLines ],
        ok = file:close (RawFile),
        A + 1
      end,
      0,
      Metrics
    ),

  mondemand_server_stats:increment_backend
    (?MODULE, stats_sent_count, TotalProcessed),

  { noreply, State };

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
%%
format_line (DateTime, Host, ProgId, T, K, V, ContextString) ->
  io_lib:format ("~s\t~s\t~s\t~s\t~s\t~p\t~s\n",
    [ mondemand_server_util:mdyhms_to_log_string (DateTime),
      Host,
      ProgId,
      mondemand_util:stringify (T),
      mondemand_util:stringify (K),
      V,
      ContextString]).

