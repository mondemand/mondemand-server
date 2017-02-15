-module (mondemand_server_sup).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, mds_sup}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  All =
    mondemand_server_config:all (),

  BackendConfigs =
    [ begin
        { BackendModule,
          { BackendModule, start_link,
            [mondemand_server_config:backend_config (BackendModule, All)] },
          permanent,
          2000,
          BackendModule:type (),
          [ BackendModule ]
        }
      end
      || BackendModule
      <- mondemand_server_config:backends_to_start (All)
    ],

  WebConfig =
    case mondemand_server_config:web_config (All) of
      undefined -> [];
      WC ->
        [
          { webmachine_mochiweb,
            { webmachine_mochiweb, start, [WC]},
            permanent,
            5000,
            worker,
            dynamic
          }
        ]
    end,

  MappingsConfig =
    case mondemand_server_config:mappings_config (All) of
      undefined -> [];
      {Dir, ReloadSecs} ->
        [
          { mondemand_server_mappings_db,
            { mondemand_server_mappings_db, start_link, [Dir, ReloadSecs] },
            permanent,
            2000,
            worker,
            [ mondemand_server_mappings_db ]
          }
        ]
    end,

  ToStart =
    {
      { one_for_one, 10, 10 },
      MappingsConfig ++ BackendConfigs ++ WebConfig ++
      [
        { mondemand_server_stats,
          { mondemand_server_stats, start_link, [] },
          permanent,
          2000,
          worker,
          [ mondemand_server_stats ]
        },
        {
          mondemand_server_dispatcher_sup,
          { mondemand_server_dispatcher_sup, start_link,
            [mondemand_server_config:num_dispatchers(All),
             mondemand_server_config:dispatch_config(All)]
          },
          permanent,
          2000,
          supervisor,
          [ mondemand_server_dispatcher_sup ]
        },
        {
          mondemand_server,
          { mondemand_server, start_link,
            [ mondemand_server_config:listener_config(All),
              mondemand_server_dispatcher_sup:dispatchers(
                mondemand_server_config:num_dispatchers(All)
              )
            ] },
          permanent,
          2000,
          worker,
          [ mondemand_server ]
        }
      ]
    },
  { ok, ToStart }.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
