-module (mondemand_server_config).

-export ([
           num_dispatchers/0,
           backends_to_start/0,
           applications_to_start/0,
           web_config/0,
           listener_config/0,
           backend_config/1,
           dispatch_config/0,
           parse_dispatch/1,
           mappings_config/0
         ]).

num_dispatchers () ->
  case application:get_env (mondemand_server, num_dispatchers) of
    {ok, C} when is_integer (C) -> C;
    undefined -> erlang:system_info(schedulers)
  end.

dispatch () ->
  % the application needs to be loaded in order to see the variables for
  % some reason
  application:load (mondemand_server),

  case application:get_env (mondemand_server, dispatch) of
    { ok, Dispatch } -> parse_dispatch (Dispatch);
    undefined -> undefined
  end.

backends_to_start () ->
  case dispatch () of
    undefined -> exit (no_dispatch_list);
    [] -> exit (empty_dispatch_list);
    Dispatch ->
      % determine the unique list of modules to start from the dispatch list
      lists:usort(lists:flatten(lists:foldl(fun({_,L},A)-> [L|A] end,
                                            [],
                                            Dispatch)))
  end.

applications_to_start () ->
  lists:append ([ Mod:required_apps()
                  || Mod <- backends_to_start () ]).

calculate_web_dispatch (InitialDispatch) ->
  OutputDispatch =
    lists:sort
      (fun dispatch_specificity/2,
       lists:foldl (fun (Mod, Acc) ->
                         case application:get_env (mondemand_server, Mod) of
                           {ok, E} ->
                             case proplists:get_value (dispatch, E) of
                               undefined -> Acc;
                               D -> D ++ Acc
                             end;
                           _ ->
                             Acc
                         end
                    end,
                    InitialDispatch,
                    backends_to_start ()
                  )
      ),
  OutputDispatch.

web_config () ->
  case application:get_env (mondemand_server, web) of
    {ok, C} ->
      InitialDispatch = proplists:get_value (dispatch , C, []),
      lists:keystore (dispatch, 1, C,
                      {dispatch, calculate_web_dispatch (InitialDispatch)});
    undefined ->
      undefined
  end.

listener_config () ->
  application:get_env (mondemand_server, listener).

backend_config (BackendModule) ->
  case application:get_env (mondemand_server, BackendModule) of
    { ok, T } -> T;
    undefined -> []
  end.

dispatch_config () ->
  dict:from_list (dispatch ()).

mappings_config () ->
  case application:get_env (mondemand_server, mappings) of
    {ok, C} ->
      Directory = proplists:get_value (directory, C, "."),
      ReloadSeconds = proplists:get_value (reload_seconds, C, 60),
      {Directory, ReloadSeconds};
    undefined ->
      undefined
  end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% functions to order a webmachine dispatch from
%% http://dukesoferl.blogspot.com/2009/08/dynamically-loading-webmachine.html
path_spec_priority ('*') -> 3;
path_spec_priority (X) when is_atom (X) -> 2;
path_spec_priority (X) when is_list (X) -> 1.

dispatch_specificity ({ PathSpecA, _, _ }, { PathSpecB, _, _ }) ->
  case erlang:length (PathSpecA) - erlang:length (PathSpecB) of
    X when X > 0 ->
      true;
    X when X < 0 ->
      false;
    _ ->
      PrioPathSpecA = [ path_spec_priority (X) || X <- PathSpecA ],
      PrioPathSpecB = [ path_spec_priority (X) || X <- PathSpecB ],

      case PrioPathSpecA =< PrioPathSpecB of
        false ->
          false;
        true ->
          FullPathSpecA = [ { path_spec_priority (X), X } || X <- PathSpecA ],
          FullPathSpecB = [ { path_spec_priority (X), X } || X <- PathSpecB ],

          FullPathSpecA =< FullPathSpecB
      end
  end.

parse_dispatch (Dispatch) ->
  % split the dispatch list into the wildcard rule and others
  { WildcardList, EventNameList } =
    case
      lists:partition (fun({"*",_})-> true; (_) -> false end, Dispatch)
    of
      { [], N } -> { [], N };
      { [{"*",A}], N } -> { A, N }
    end,

  % add the wildcard rule to each of the others
  DispatchWithEventNames =
    lists:map (fun ({K,VL}) ->
                 {list_to_binary (K), lists:append (VL, WildcardList)}
               end,
               EventNameList),

  % add a wildcard rule to the master list if one is available
  FinalDispatch =
    case WildcardList of
      [] -> DispatchWithEventNames;
      _ -> lists:append (DispatchWithEventNames ,[{"*",WildcardList}])
    end,

  FinalDispatch.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
