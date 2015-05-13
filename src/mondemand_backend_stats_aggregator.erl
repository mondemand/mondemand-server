-module (mondemand_backend_stats_aggregator).

% the purpose of this backend is to peform a few aggregations across each
% minute, then dispatch those aggregates to another backend (or backends).
% 
% the form of the key for the aggregate is
%
% program_id, metric, ctxt - host
%
% and aggregates performed are
%
% SUM - add all the values gotten in the given minute for all hosts
% CNT - count of the samples gotten in the given minute
% AVG - determined when red by SUM/CNT
% MIN - minimum value seen in the given minute
% MAX - maximum value seen in the given minute
%
% each minute is stored in a different ets table, and a certain number of
% ets tables in the past are kept, so that eventually value changes could
% be triggered
%
% something like folsom could be used if additional statistics were needed
%
-behaviour (supervisor).
-behaviour (mondemand_server_backend).
-behaviour (mondemand_backend_stats_handler).
-behaviour (mondemand_backend_worker).

%% mondemand_backend_worker callbacks
-export ([ create/1,
           send/2,
           destroy/1 ]).

%% mondemand_backend callbacks
-export ([ start_link/1,
           process/2,
           required_apps/0,
           type/0
         ]).

%% mondemand_backend_stats_handler callbacks
-export ([ header/0,
           separator/0,
           format_stat/10,
           footer/0,
           handle_response/2
         ]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% mondemand_server_backend callbacks
%%====================================================================
start_link (Config) ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, [Config]).

process (Event, Timestamp) ->
  mondemand_backend_worker_pool_sup:process
    (mdbe_stats_agg_worker_pool, Event, Timestamp).

required_apps () ->
  [ ].

type () ->
  supervisor.

%%====================================================================
%% supervisor callbacks
%%====================================================================
init ([Config]) ->
  Number = proplists:get_value (number, Config, 16), % FIXME: replace default
  { ok,
    {
      {one_for_one, 10, 10},
      [
        { mondemand_backend_stats_aggregator_db:minute_proc (M),
          { mondemand_backend_stats_aggregator_db, start_link, [M] },
          permanent,
          2000,
          worker,
          [ mondemand_backend_stats_aggregator_db ]
        }
        || M
        <- lists:seq (0, 59)
      ]
      ++
      [
        { mdbe_stats_agg_worker_pool,
          { mondemand_backend_worker_pool_sup, start_link,
            [ mdbe_stats_agg_worker_pool,
              mondemand_backend_worker,
              Number,
              ?MODULE ]
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
%% mondemand_backend_stats_handler callbacks
%%====================================================================
header () -> undefined.

separator () -> undefined.

format_stat (_Num, _Total, _Prefix, ProgId, _Host,
             _MetricType, MetricName,
             MetricValue, Timestamp,
             Context) ->
  Key = {ProgId, MetricName, Context},
  mondemand_backend_stats_aggregator_db:add_val (Timestamp, Key, MetricValue),
  undefined.

footer () -> undefined.

handle_response (_, State) ->
  {0, State}.

%%====================================================================
%% mondemand_backend_worker callbacks
%%====================================================================
create (_Config) -> {}.
send ({}, _Lines) -> ok.
destroy (_) -> ok.
