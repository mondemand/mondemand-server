-module (mondemand_server_config).

-export ([ dispatch/0,
           backends_to_start/0,
           applications_to_start/0,
           web_config/0
         ]).

dispatch () ->
  % the application needs to be loaded in order to see the variables for
  % some reason
  application:load (mondemand_server),

  case application:get_env (mondemand_server, dispatch) of
    { ok, Dispatch } ->
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

      FinalDispatch;
    undefined ->
      []
  end.

backends_to_start () ->
  case dispatch () of
    [] -> [];
    Dispatch ->
      % determine the unique list of modules to start from the dispatch list
      lists:usort(lists:flatten(lists:foldl(fun({_,L},A)-> [L|A] end,
                                            [],
                                            Dispatch)))
  end.

applications_to_start () ->
  lists:append ([ Mod:required_apps() || Mod <- backends_to_start () ]).

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
  {ok, C} = application:get_env (mondemand_server, web),
  InitialDispatch = proplists:get_value (dispatch , C, []),
  lists:keystore (dispatch, 1, C,
                  {dispatch, calculate_web_dispatch (InitialDispatch)}).

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

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(HAVE_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

-endif.
