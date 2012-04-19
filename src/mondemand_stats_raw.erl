-module (mondemand_stats_raw).

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

-record (state, {config, root}).

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

  mondemand_util:mkdir_p (Dir),

  { ok, #state {config = Config, root = filename:join (Dir)} }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Binary}, State = #state { root = Dir }) ->

  Event =  lwes_event:from_udp_packet (Binary, dict),
  #lwes_event { attrs = Data } = Event,

  Timestamp = dict:fetch (<<"ReceiptTime">>, Data),
  DateTime = mondemand_util:epoch_to_mdyhms (Timestamp),

  Num = dict:fetch (<<"num">>, Data),
  ProgId = dict:fetch (<<"prog_id">>, Data),
  {Host, ContextString} = mondemand_util:construct_context_string (Event),
  lists:foreach (
    fun(E) ->
      K = dict:fetch (mondemand_util:stat_key (E), Data),
      V = dict:fetch (mondemand_util:stat_val (E), Data),

      RawLogLine = io_lib:format ("~s\t~s\t~p\t~s\n",
        [ mondemand_util:mdyhms_to_log_string (DateTime),
          binary_to_list (K),
          V,
          ContextString]),

      RawDir =
        filename:join ([Dir,
                        mondemand_util:mdyhms_to_dir_prefix (DateTime),
                        ProgId]),
      ok = mondemand_util:mkdir_p (RawDir),
      RawFileName = filename:join ([RawDir, Host]),
      {ok, RawFile} = file:open (RawFileName, [append]),
      io:format (RawFile, "~s",[RawLogLine]),
      ok = file:close (RawFile)
    end, lists:seq (1,Num)),
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
