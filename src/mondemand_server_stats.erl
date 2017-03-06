-module (mondemand_server_stats).

-include_lib ("mondemand/include/mondemand.hrl").

-behaviour (gen_server).

%% API
-export ([ start_link/0,
           prog_id/0,
           create/1,
           increment/1,
           increment/2,
           create_backend/2,
           increment_backend/2,
           increment_backend/3,
           flush_init/0,
           flush_one/2
         ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, { }).

%%====================================================================
%% API
%%====================================================================
start_link () ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, [], []).


prog_id () ->
  mondemand_server.

create (Metric) ->
  mondemand:increment (prog_id(), Metric, 0).

increment (Metric) ->
  mondemand:increment (prog_id(), Metric, 1).

increment (Metric, Value) ->
  mondemand:increment (prog_id(), Metric, Value).

create_backend (Backend, Metric) ->
  mondemand:increment (prog_id(), Metric, 0, [{backend, Backend}]).

increment_backend (Backend, Metric) ->
  mondemand:increment (prog_id(), Metric, 1, [{backend, Backend}]).

increment_backend (Backend, Metric, Value) ->
  mondemand:increment (prog_id(), Metric, Value, [{backend, Backend}]).

flush_init () ->
  All = mondemand_server_config:all(),
  Num = mondemand_server_config:num_dispatchers(All),
  Num.

% The flush here is doing 2 things.
%
% 1. flushing the mondemand_statdb back through the server (except mondemand_server
%    internal statistics) which causes aggregates to get written to rrd's as well
%    as forwarded
% 2. flushing the internal stats of the mondemand server to the mondemand
%    library configured channel
flush_one (Num, StatsMsg = #md_stats_msg { prog_id = ProgId, host = Host }) ->
  % the server returns all "strings" as binary, so even though we use
  % atoms above to set the values, we check against a binary here, this
  % is grabbing all mondemand_server stats that are not aggregated (which
  % would have a host of 'all'
  case ProgId =:= <<"mondemand_server">> andalso Host =/= <<"all">> of
    true ->
      mondemand:flush_one_stat (undefined, StatsMsg);
    false ->
      % this will resend everything back through this server, setting the port
      % to 0 allows the backends to determine that these are self sent
      Event = mondemand_event:new (
                "127.0.0.1", 0,
                mondemand_util:millis_since_epoch(),
                ?MD_STATS_EVENT, StatsMsg),
      mondemand_server_dispatcher_sup:dispatch (Num, Event)
  end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([]) ->
  % I want terminate to be called
  process_flag (trap_exit, true),

  { ok, #state {} }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",
                            [?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",
                            [?MODULE, Request]),
  {noreply, State}.

terminate (_Reason, #state { }) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

