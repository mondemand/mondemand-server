-module (mondemand_backend_lwes_global).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("mondemand/include/mondemand.hrl").

-behaviour (supervisor).
-behaviour (mondemand_server_backend).

%% mondemand_server_backend callbacks
-export ([ start_link/1,
           process/1,
           required_apps/0,
           type/0
         ]).

%% supervisor callbacks
-export ([ init/1 ]).

%% api
-export ([ filter/1 ]).

-define (POOL, mdbe_lwes_gl_pool).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================
start_link (Config) ->
  supervisor:start_link ( { local, ?MODULE }, ?MODULE, [Config]).

process (Event) ->
  mondemand_backend_worker_pool_sup:process (?POOL, Event).

required_apps () ->
  [ lwes ].

type () ->
  supervisor.

%%====================================================================
% supervisor callbacks
%%====================================================================
init ([Config]) ->
  % default to one process per scheduler
  Number =
    proplists:get_value (number, Config,
                         erlang:system_info(schedulers)),


  { ok,
   {
      {one_for_one, 10, 10},
      [
        { ?POOL,
         { mondemand_backend_worker_pool_sup, start_link,
           [ ?POOL,
             mondemand_backend_worker,
             Number,
             mondemand_backend_lwes_global
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

filter (Event = #md_event {}) ->
  case mondemand_event:peek_type_from_udp (Event) of
    undefined -> true;  % filter undefined
    stats_msg ->
      StatsMsg = mondemand_event:msg (Event),

      % if an aggregate return false, otherwise return true

      % the aggregating backend adds this context field to all aggregates, so
      % if it's undefined this is not an aggregate event
      mondemand_statsmsg:context_value (StatsMsg, <<"stat">>) =:= undefined;

      % for the moment filter out anything not an aggregate
      % or not from the mondemand-server
    _ ->
      true % don't allow any non-stats messages through
  end.

%      % for the moment filter out anything not an aggregate
%      % or not from the mondemand-server
%      ShouldSend =
%        case mondemand_statsmsg:context_value (StatsMsg, <<"stat">>) =:= undefined of
%          true ->
%            mondemand_statsmsg:prog_id (StatsMsg) =:= <<"mondemand_server">>;
%          _ ->
%            true
%        end,

%%--------------------------------------------------------------------
%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
