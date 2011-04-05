-module (mondemand_server_sup).

-behaviour (supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]). 

%%====================================================================
%% API functions
%%====================================================================
%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  { ok,
    {
      { one_for_one, 10, 10 },
      [
        {
          mondemand_server,
          { mondemand_server, start_link, [] },
          permanent,
          2000,
          worker,
          [ mondemand_server ]
        }
      ]
    }
  }.

