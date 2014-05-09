-module (mondemand_server_dispatcher_sup).

-behaviour (supervisor).

%% API
-export ([start_link/2, dispatch/1, stats/0]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link(Dispatch, NumDispatchers) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Dispatch, NumDispatchers]).

dispatch (Event) ->
  Pid = gproc:where (gproc_pool:pick (mondemand_dispatcher)),
  mondemand_server_dispatcher:process (Pid, Event).

stats () ->
  [
    begin
      { message_queue_len, L } = process_info (Pid, message_queue_len),
      { worker_atom (N), gen_server:call (Pid, {stats}), L }
    end
    || {{_,N}, Pid}
    <- gproc_pool:active_workers(mondemand_dispatcher) ].

%%====================================================================
%% supervisor callbacks
%%====================================================================
init([Dispatch, Num]) ->
  ok = gproc_pool:new (mondemand_dispatcher),
  { ok,
    {
      {one_for_one, 10, 10},
      [ begin
          WorkerName = {mondemand_dispatcher, N},
          WorkerAtom = worker_atom (N),
          gproc_pool:add_worker (mondemand_dispatcher, WorkerName),
          { WorkerAtom,
            {mondemand_server_dispatcher, start_link,
              [Dispatch, WorkerAtom, WorkerName] },
            transient,
            2000,
            worker,
            [mondemand_server_dispatcher]
          }
        end
        || N
        <- lists:seq (1, Num)
      ]
    }
  }.

worker_atom (N) ->
  list_to_atom (atom_to_list (mondemand_dispatcher)++"_"++integer_to_list (N)).

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
