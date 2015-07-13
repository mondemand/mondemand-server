-module (mondemand_backend_stats_aggregator).

-behaviour (supervisor).
-behaviour (mondemand_server_backend).
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

%% supervisor callbacks
-export ([ init/1 ]).

-define (POOL, md_aggregator_pool).
-record (state, {aggregation_keys}).

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
  AggKeys = proplists:get_value (aggregation_keys, Config, undefined),
  #state {aggregation_keys = AggKeys}.

% aggregation is currently always by host, and optionally by other keys.
% host should be normalized by just leaving it in the context which would
% simplify much of the logic below.
send (#state {aggregation_keys = AggKeys}, Data) ->
  case mondemand_event:sender_port (Data) of
    0 ->
      % got aggregated value from self
      ok;
    _ ->
      % not a self sent event so need to aggregate
      StatsMsg = mondemand_event:msg (Data),
      ProgId = mondemand_statsmsg:prog_id (StatsMsg),

      ContextIn = mondemand_statsmsg:context (StatsMsg),

      % this is meant to allow aggregating at 2 levels, but is not really
      % working as I would like, so is currently mostly a no-op
      { _OriginalType, ContextMinusUpstream } =
        case lists:keytake (<<"stat">>,1,ContextIn) of
          false -> {undefined, ContextIn};
          {value, {_,ST}, C} -> {ST, C}
        end,

      % aggregation is currently achieved by make the context value 'all'
      % for the given aggregation keys
      Context = case AggKeys of
                  undefined -> ContextMinusUpstream;
                  _ ->
                       lists:map (
                         fun ({FK,FV}) ->
                           case lists:member (FK, AggKeys) of
                             true -> {FK, <<"all">>};
                             false -> {FK, FV}
                           end
                         end,
                         ContextMinusUpstream
                       )
                end,
      Metrics = mondemand_statsmsg:metrics (StatsMsg),
      lists:map (
        fun (E) ->
          {T, K, V} = mondemand_statsmsg:metric (E),
          case T of
            statset ->
              % statset's need some special processing to work,
              % sum and count are treated as counters, while
              % all other things are treated as samples.  This
              % leads to some confusing setups, so should be avoided
              % for the moment.
              lists:map (fun
                           ({ST,SV}) when ST=:=sum; ST=:=count ->
                             mondemand:increment (
                               ProgId, K,
                               [{<<"host">>, <<"all">>},
                                {<<"stat">>,ST} | Context],
                               SV);
                           ({ST, SV}) ->
                             mondemand:add_sample (
                               ProgId, K,
                               [{<<"host">>, <<"all">>},
                                {<<"stat">>,ST} | Context],
                               SV)
                         end,
                         mondemand_statsmsg:statset_to_list (V));
            Stat ->
              % counters and gauges just get added as straight samples
              mondemand:add_sample (ProgId, K,
                                    [{<<"host">>, <<"all">>},
                                     {<<"stat">>, Stat} | Context],
                                    V)
          end
        end,
        Metrics
      )
  end,
  ok.

destroy (_) ->
  ok.
