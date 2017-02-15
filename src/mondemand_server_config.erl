-module (mondemand_server_config).

-export ([ all/0,
           listener_config/1,
           num_dispatchers/1,
           dispatch_config/1,
           backends_to_start/1,
           applications_to_start/1,
           web_config/1,
           backend_config/2,
           mappings_config/1
         ]).

-include_lib ("mondemand/include/mondemand.hrl").
-include ("mondemand_server_internal.hrl").

all () ->
  % the application needs to be loaded in order to see the variables for
  % some reason
  application:load (mondemand_server),
  application:get_all_env (mondemand_server).

listener_config (Config) ->
  find_in_config (listener, Config).

num_dispatchers (Config) ->
  case find_in_config (num_dispatchers, Config) of
    undefined -> erlang:system_info(schedulers);
    C when is_integer (C) -> C
  end.

dispatch_config (Config) ->
  dispatch (Config).

applications_to_start (Config) ->
  lists:append ([ Mod:required_apps()
                  || Mod
                  <- backends_to_start (Config) ]).

backends_to_start (Config) ->
  case dispatch (Config) of
    undefined -> exit (no_dispatch_list);
    #mds_dispatch { annotation_msg = A,
                    log_msg = L,
                    perf_msg = P,
                    stats_msg = S,
                    trace_msg = T } ->
      % determine the unique list of modules to start from the dispatch list
      lists:usort(lists:flatten([A,L,P,S,T]))
  end.

web_config (Config) ->
  case find_in_config (web, Config) of
    undefined -> undefined;
    C ->
      InitialWebDispatch = find_in_config (dispatch , C, []),
      BackendWebDispatch = backend_web_configs (Config),
      lists:keystore ( dispatch, 1, C,
        {dispatch,
         calculate_web_dispatch (InitialWebDispatch ++ BackendWebDispatch)
        }
      )
  end.

backend_web_configs (Config) ->
  lists:foldl (fun (Mod, Acc) ->
                 case find_in_config (Mod, Config) of
                   undefined -> Acc;
                   E ->
                     case find_in_config (dispatch, E) of
                       undefined -> Acc;
                       D -> D ++ Acc
                     end
                 end
               end,
               [],
               backends_to_start (Config)).


backend_config (BackendModule, Config) ->
  find_in_config (BackendModule, Config).


mappings_config (Config) ->
  case find_in_config (mappings, Config) of
    undefined -> undefined;
    C ->
      Directory = find_in_config (directory, C, "."),
      ReloadSeconds = find_in_config (reload_seconds, C, 60),
      {Directory, ReloadSeconds}
  end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

find_in_config (Key, Proplist) ->
  proplists:get_value (Key, Proplist).

find_in_config (Key, Proplist, Default) ->
  proplists:get_value (Key, Proplist, Default).

dispatch (Config) ->
  dispatch_to_record (find_in_config (dispatch, Config)).

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

calculate_web_dispatch (InitialDispatch) ->
  lists:sort (fun dispatch_specificity/2, InitialDispatch).

% In the application environment variable the dispatch list is of the form
%
% { dispatch,
%   [
%     { "*", [ mondemand_backend_all_journaller ] }, % add to all
%     { "MonDemand::StatsMsg", [ mondemand_backend_stats_file,
%                                mondemand_backend_stats_aggregator,
%                                mondemand_backend_lwes ] },
%     { "MonDemand::LogMsg",   [ mondemand_backend_log_file,
%                                mondemand_backend_lwes ] },
%     { "MonDemand::TraceMsg", [ mondemand_backend_trace_file ] },
%     { "MonDemand::PerfMsg",  [ mondemand_backend_lwes,
%                                mondemand_backend_performance_collate] }
%   ]
% }
%
% This will be turned into a flattened structure, and all registered names
% will be added to lists.  This should allow for the quickest dispatch.
%
% The final structure for the above will be something like
%
% #md_dispatch { annotation = [ mondemand_backend_all_journaller ],
%                log =        [ mondemand_backend_log_file,
%                               mondemand_backend_lwes,
%                               mondemand_backend_all_journaller ],
%                perf =       [ mondemand_backend_lwes,
%                               mondemand_backend_all_journaller ],
%                stats =      [ mondemand_backend_stats_file,
%                               mondemand_backend_stats_aggregator,
%                               mondemand_backend_lwes,
%                               mondemand_backend_all_journaller ],
%                trace =      [ mondemand_backend_trace_file,
%                               mondemand_backend_all_journaller ]
%             }
%
% Dispatching then becomes a lookup in this record followed by
% invoking the process function for each.
dispatch_to_record (undefined) -> undefined;
dispatch_to_record (Dispatch) when is_list (Dispatch) ->
  case find_all_for_type (<<"*">>, Dispatch) of
    {ok, Wildcard} ->
      Annotation = find_all_for_type(?MD_ANNOTATION_EVENT, Dispatch),
      Log = find_all_for_type (?MD_LOG_EVENT, Dispatch),
      Perf = find_all_for_type (?MD_PERF_EVENT, Dispatch),
      Stats = find_all_for_type (?MD_STATS_EVENT, Dispatch),
      Trace = find_all_for_type (?MD_TRACE_EVENT, Dispatch),
      case {Annotation,Log,Perf,Stats,Trace} of
        {{ok,A},{ok,L},{ok,P},{ok,S},{ok,T}} ->
          #mds_dispatch {
            annotation_msg = A ++ Wildcard,
            log_msg = L ++ Wildcard,
            perf_msg = P ++ Wildcard,
            stats_msg = S ++ Wildcard,
            trace_msg = T ++ Wildcard
          };
        O ->
          {error, {malformed_dispatch, O}}
      end;
    {error, E} ->
      {error, {malformed_wildcard, E}}
  end.

% Given a Dispatch list of the form [{EventType, [handlers]}], this will
% allow for the EventType to be either a binary, list or atom, and enforce
% that it's in the list of valid mondemand events, or is the wildcard "*"
find_all_for_type (T, Dispatch) when is_binary (T) ->
  case lists:filter (
         fun (undefined) -> false;
             (_) -> true
         end,
         [ find_dispatch_entry (erlang:binary_to_atom(T,latin1), Dispatch),
           find_dispatch_entry (T, Dispatch),
           find_dispatch_entry (binary_to_list(T), Dispatch) ]
       ) of
    [One] -> {ok, One};
    [] -> {ok, []};
    _ -> {error, multiple_entries_for_type}
  end.

find_dispatch_entry (K, Dispatch) ->
  case lists:keyfind (K,1,Dispatch) of
    false -> undefined;
    {_,L} -> L
  end.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dispatch_config_test_ () ->
  [
    ?_assertEqual (
      #mds_dispatch { annotation_msg = [j],
                      log_msg = [l, sl, j],
                      trace_msg = [tf, j],
                      perf_msg = [l, c, j],
                      stats_msg = [l, sa, sl, j] },
      dispatch_to_record ([{"*",[j]},
                           {"MonDemand::StatsMsg", [l, sa, sl]},
                           {"MonDemand::LogMsg", [l, sl]},
                           {"MonDemand::TraceMsg",[tf]},
                           {"MonDemand::PerfMsg", [l, c]}
                          ])
    ),
    ?_assertEqual (
      {error, {malformed_wildcard, multiple_entries_for_type}},
      dispatch_to_record([{"*",[a]},{'*',[b]}])),
    ?_assertEqual (
      {error,{malformed_dispatch,{{ok,[]},
                                  {ok,[]},
                                  {ok,[]},
                                  {error,multiple_entries_for_type},
                                  {ok,[]}}}},
      dispatch_to_record([{"*",[a]},
                          {'MonDemand::StatsMsg',[b]},
                          {"MonDemand::StatsMsg",[c]}]))
  ].

-endif.
