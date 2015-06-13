-module (mondemand_backend_stats_aggregator).

-behaviour (supervisor).
-behaviour (mondemand_server_backend).
%-behaviour (mondemand_backend_stats_handler).
-behaviour (mondemand_backend_worker).

%% mondemand_backend_worker callbacks
-export ([ create/1,
          send/2,
          destroy/1 ]).

%% mondemand_server_backend callbacks
-export ([ start_link/1,
          process/1,
          required_apps/0,
          type/0
         ]).

%%% mondemand_backend_stats_handler callbacks
%-export ([ header/0,
%          format_stat/10,
%          separator/0,
%          footer/0,
%          handle_response/2
%         ]).

%% supervisor callbacks
-export ([ init/1 ]).

-define (POOL, md_aggregator_pool).

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
  error_logger:info_msg ("create worker with config ~p",[Config]),
  { }.

send (_Config, Data) ->
%  error_logger:info_msg ("got ~p data ~p",[Config, Data]),
  case mondemand_event:sender_port (Data) of
    0 ->
      error_logger:info_msg ("got aggregated value");
    _ ->
      StatsMsg = mondemand_event:msg (Data),
      case mondemand_statsmsg:prog_id (StatsMsg) of
        <<"mondemand_server">> ->
          error_logger:info_msg ("need to ignore our owm metrics");
        ProgId ->
          Context = mondemand_statsmsg:context (StatsMsg),
          Metrics = mondemand_statsmsg:metrics (StatsMsg),
          lists:map (
            fun (E) ->
              {T, K, V} = mondemand_statsmsg:metric (E),
              case T of
                statset ->
                  lists:map (fun
                               ({ST,SV}) when ST=:=sum; ST=:=count ->
                                 mondemand:increment (
                                   ProgId, K,
                                   [{stat,ST}|Context], SV);
                               ({ST, SV}) ->
                                 mondemand:add_sample (
                                   ProgId, K,
                                   [{stat,ST}|Context], SV)
                             end,
                             mondemand_statsmsg:statset_to_list (V));
                _ ->
                  % counters and gauges just get added as straight samples
                  mondemand:add_sample (ProgId, K, Context, V)
    %              error_logger:info_msg ("~p:~p:~p:~p:~p",[ProgId, Context, T, K, V])
              end
            end,
            Metrics
          )
        end
    end,
  ok.

destroy (_) ->
  ok.
