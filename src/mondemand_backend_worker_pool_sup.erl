-module (mondemand_backend_worker_pool_sup).

-behaviour (supervisor).

%% API
-export ([start_link/4, process/3, stats/1]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link (SupervisorName, WorkerTypeModule, NumWorkers, WorkerModule) ->
  supervisor:start_link({local, SupervisorName}, ?MODULE,
                        [ SupervisorName, WorkerTypeModule,
                          NumWorkers, WorkerModule]).

process (SupervisorName, Event, Timestamp) ->
  Pid = gproc:where (gproc_pool:pick (SupervisorName)),
  gen_server:cast (Pid, {process, Event, Timestamp}).

stats (SupervisorName) ->
  [
    begin
      { message_queue_len, L } = process_info (Pid, message_queue_len),
      { worker_atom (SupervisorName, N), gen_server:call (Pid, {stats}), L }
    end
    || {{_,N}, Pid}
    <- gproc_pool:active_workers (SupervisorName)
  ].

%%====================================================================
%% supervisor callbacks
%%====================================================================
init([SupervisorName, WorkerTypeModule, NumWorkers, WorkerModule]) ->
  ok = gproc_pool:new (SupervisorName),
  { ok,
    {
      {one_for_one, 10, 10},
      [ begin
          WorkerName = {SupervisorName, N},
          WorkerAtom = worker_atom (SupervisorName, N),
          gproc_pool:add_worker (SupervisorName, WorkerName),
          { WorkerAtom,
            {WorkerTypeModule, start_link,
              [SupervisorName, WorkerAtom, WorkerName, WorkerModule] },
            transient,
            2000,
            worker,
            [WorkerTypeModule]
          }
        end
        || N
        <- lists:seq (1, NumWorkers)
      ]
    }
  }.

worker_atom (SupervisorName, N) ->
  list_to_atom (atom_to_list (SupervisorName)++"_"++integer_to_list (N)).

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(HAVE_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

-endif.
