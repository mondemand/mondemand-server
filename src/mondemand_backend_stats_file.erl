-module (mondemand_backend_stats_file).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").

-behaviour (mondemand_server_backend).
-behaviour (gen_server).

%% mondemand_backend callbacks
-export ([ start_link/1,
           process/2,
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

process (Event, Timestamp) ->
  gen_server:cast (?MODULE, {process, Event, Timestamp}).

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
  mondemand_server_stats:init_backend (?MODULE, events_processed),
  mondemand_server_stats:init_backend (?MODULE, stats_sent_count),

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

handle_cast ({process, Binary, Timestamp},
             State = #state { root = Dir,
                              context_delimiter = Delimiter
                            }) ->
  Event =  lwes_event:from_udp_packet (Binary, dict),
  #lwes_event { attrs = Data } = Event,

  DateTime = mondemand_server_util:now_to_mdyhms (Timestamp),

  Num = dict:fetch (<<"num">>, Data),
  ProgId = dict:fetch (<<"prog_id">>, Data),
  {Host, ContextString} =
    mondemand_server_util:construct_context_string (Event, Delimiter),

  mondemand_server_stats:increment_backend (?MODULE, events_processed),

  TotalProcessed =
    lists:foldl (
      fun (E, A) ->
        K = dict:fetch (mondemand_server_util:stat_key (E), Data),
        V = dict:fetch (mondemand_server_util:stat_val (E), Data),
        T = dict:fetch (mondemand_server_util:stat_type (E), Data),

        RawLogLine = io_lib:format ("~s\t~s\t~s\t~p\t~s\n",
          [ mondemand_server_util:mdyhms_to_log_string (DateTime),
            binary_to_list (T),
            binary_to_list (K),
            V,
            ContextString]),

        RawDir =
          filename:join (
            [Dir,
             mondemand_server_util:mdyhms_to_dir_prefix (DateTime),
             ProgId]),
        ok = mondemand_server_util:mkdir_p (RawDir),
        RawFileName = filename:join ([RawDir, Host]),
        {ok, RawFile} = file:open (RawFileName, [append]),
        io:format (RawFile, "~s",[RawLogLine]),
        ok = file:close (RawFile),
        A + 1
      end,
      0,
      lists:seq (1,Num)
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
