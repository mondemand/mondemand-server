-module (mondemand_backend_performance_summarize).

-behaviour (supervisor).
-behaviour (mondemand_server_backend).
-behaviour (mondemand_backend_worker).

%% mondemand_backend_worker callbacks
-export ([ create/1,
           connected/1,
           connect/1,
           send/2,
           destroy/1
         ]).

%% mondemand_server_backend callbacks
-export ([ start_link/1,
           process/1,
           required_apps/0,
           type/0
         ]).

%% supervisor callbacks
-export ([ init/1 ]).

-define (POOL, md_perf_pool).
-record (state, { fixup_mod }).

%%====================================================================
%% mondemand_server_backend callbacks
%%====================================================================
start_link (Config) ->
  supervisor:start_link ( { local, ?MODULE }, ?MODULE, [Config]).

process (Event) ->
  mondemand_backend_worker_pool_sup:process (?POOL, Event).

required_apps () ->
  [ ].

type () ->
  supervisor.

%%====================================================================
% supervisor callbacks
%%====================================================================
init ([Config]) ->
  % default to one process per scheduler
  Number =
    proplists:get_value (number, Config, erlang:system_info(schedulers)),

  { ok,
   {
      {one_for_one, 10, 10},
      [
        { mondemand_backend_performance_db,
          { mondemand_backend_performance_db, start_link, [] },
          permanent,
          2000,
          worker,
          [ mondemand_backend_performance_db ]
        },
        { ?POOL,
         { mondemand_backend_worker_pool_sup, start_link,
           [ ?POOL,
             mondemand_backend_worker,
             Number,
             ?MODULE
           ]
         },
         permanent,
         2000,
         supervisor,
         [ ]
        }
      ]
    }
  }.

%%====================================================================
%% mondemand_backend_worker callbacks
%%====================================================================
create (Config) ->
  FixupModule = proplists:get_value (fixup_mod, Config, undefined),
  {ok, #state { fixup_mod = FixupModule }}.

connected (_State) ->
  true. % always connected

connect (State) ->
  {ok, State}.

send (State = #state { fixup_mod = FixupModule }, Data) ->
  PerfMsg = mondemand_event:msg (Data),
  [ mondemand:add_sample (ProgId, MetricName, Context, MetricValue)
    || { ProgId, Context, MetricName, MetricValue }
    <- FixupModule:to_stats (PerfMsg)
  ],
  { ok, State }.

destroy (_) ->
  ok.
