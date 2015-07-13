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
           flush/0
         ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, { stats_flush_timer }).

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

flush () ->
  mondemand_statdb:flush (1,
    fun (StatsMsg) ->
      Event = mondemand_event:new ("127.0.0.1", 0, 0,?MD_STATS_EVENT,StatsMsg),
      mondemand_server_dispatcher_sup:dispatch (Event)
    end
  ).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([]) ->
  % I want terminate to be called
  process_flag (trap_exit, true),

  {ok, TRef} = timer:send_interval (60000, ?MODULE, flush),
  { ok, #state {stats_flush_timer = TRef} }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",
                            [?MODULE, Request]),
  { noreply, State }.

handle_info (flush, State) ->
  flush(),
  {noreply, State};
handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",
                            [?MODULE, Request]),
  {noreply, State}.

terminate (_Reason, #state { stats_flush_timer = TRef }) ->
  timer:cancel (TRef),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

