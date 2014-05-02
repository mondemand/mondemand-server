-module (mondemand_backend_connection_pool).

%% API
-export ([ init/1,
           cast/2,
           call/2,
           sidejob_name/1,
           sidejob_unname/1,
           stats_by_worker/1,
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
  init ([Module, Limit, Number, mondemand_backend_connection]);
init ([Module, Limit, Number, WorkerMod]) ->
  case Number of
    undefined ->
      sidejob:new_resource (sidejob_name (Module),
                            WorkerMod,
                            Limit);
    _ ->
      sidejob:new_resource (sidejob_name (Module),
                            WorkerMod,
                            Limit, Number)
  end .

call (Module, Msg) ->
  sidejob:call (sidejob_name (Module), Msg).

cast (Module, Msg) ->
  sidejob:cast (sidejob_name (Module), Msg).

stats_by_worker (Module) ->
  SideJobModule = sidejob_name (Module),
  Workers = tuple_to_list (SideJobModule:workers ()),
  [ {W, gen_server:call (W, {stats})} || W <- Workers ].

stats (Module) ->
  SideJobModule = sidejob_name (Module),
  Workers = tuple_to_list (SideJobModule:workers ()),
  Stats =
    lists:foldl ( fun (Dict, Accum) ->
                    dict:merge (fun (_, Value1, Value2) ->
                                  Value1 + Value2
                                end,
                                Dict,
                                Accum)
                  end,
                  dict:new (),
                  [ gen_server:call (W, {stats}) || W <- Workers ]),
  Stats.
