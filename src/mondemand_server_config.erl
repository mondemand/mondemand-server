-module (mondemand_server_config).

-export ([ dispatch/0,
           backends_to_start/0,
           applications_to_start/0
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

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(HAVE_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

-endif.
