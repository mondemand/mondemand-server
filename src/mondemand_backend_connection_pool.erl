-module (mondemand_backend_connection_pool).

%% API
-export ([ init/1,
           cast/2,
           call/2,
           sidejob_name/1,
           sidejob_unname/1,
           stats/1
         ]).

%%====================================================================
%% API
%%====================================================================
sidejob_name (Name) when is_atom (Name) ->
  list_to_atom (atom_to_list (Name) ++ "_sidejob").

sidejob_unname (Name) when is_atom (Name) ->
  case lists:reverse (atom_to_list (Name)) of
    [ $b,$o,$j,$e,$d,$i,$s,$_ | Rest ] -> list_to_atom (lists:reverse (Rest));
    _ -> Name
  end.

init ([Module, Limit, Number]) ->
  case Number of
    undefined ->
      sidejob:new_resource (sidejob_name (Module),
                            mondemand_backend_connection,
                            Limit);
    _ ->
      sidejob:new_resource (sidejob_name (Module),
                            mondemand_backend_connection,
                            Limit, Number)
  end.

call (Module, Msg) ->
  sidejob:call (sidejob_name (Module), Msg).

cast (Module, Msg) ->
  sidejob:cast (sidejob_name (Module), Msg).

stats (Module) ->
  SideJobModule = sidejob_name (Module),
  Workers = tuple_to_list (SideJobModule:workers ()),
  [ {W, gen_server:call (W, {stats})} || W <- Workers ].
