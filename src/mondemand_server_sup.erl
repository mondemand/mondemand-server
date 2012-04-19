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
  Dispatch =
    case application:get_env (mondemand_server, dispatch) of
      { ok, D } -> D;
      undefined -> exit(no_dispatch_list)
    end,

  % split the dispatch list into the wildcard rule and others
  { [{"*",AllList}], NotAllList } =
    lists:splitwith(fun({"*",_})-> true; (_) -> false end, Dispatch),

  % add the wildcard rule to the others
  FinalDispatch =
    lists:map (fun ({K,VL}) ->
                 {list_to_binary (K), lists:append (VL, AllList)}
               end,
               NotAllList),

  UniquedModules =
    lists:usort(lists:flatten(lists:foldl(fun({_,L},A)-> [L|A] end,
                                          [], FinalDispatch))),

  ToStart =
    {
      { one_for_one, 10, 10 },
      [
        {
          mondemand_server,
          { mondemand_server, start_link, [FinalDispatch] },
          permanent,
          2000,
          worker,
          [ mondemand_server ]
        }
        |
        [ begin
            C =
              case application:get_env (mondemand_server, M) of
                { ok, T } -> T;
                undefined -> []
              end,
            { M, { M, start_link, [C] }, permanent, 2000, worker, [ M ] }
          end
          || M <- UniquedModules ]
      ]
    },
  error_logger:info_msg ("Starting ~p",[ToStart]),
  { ok, ToStart }.

