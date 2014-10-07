%% FIXME: FIXME: TODO: FIXME: TODO:
%%   This will actually be in the mondemand client lib, but I need it in the
%%   server for the moment, so copied it here.
%% FIXME: FIXME: TODO: FIXME: TODO:

%% @author Anthony Molinaro <anthonym@alumni.caltech.edu>
%%
%% @doc Mondemand Stats functions
%%
%% This module wraps various records used by mondemand and provide
%% the serialization/deserialization to lwes.
%%
%% It also provide an API which should allow easier process of metrics
%% by the mondemand-server, for instance I'd want to do something like
%%
%%  Stats = mondemand_stats:from_lwes (Event),
%%  Host = mondemand_stats:host (Stats),
%%  Context = mondemand_stats:context (Stats),
%%  Metrics = mondemand_stats:metrics (Stats),
%%  lists:foldl (fun (Metric, A) ->
%%                 MetricType = mondemand_stats:metric_type (Metric),
%%                 MetricName = mondemand_stats:metric_name (Metric),
%%                 MetricValue= mondemand_stats:metric_value(Metric)
%%                 % do stuff here with everything above
%%               end,
%%               [],
%%               Metrics)
%%

-module (mondemand_server_stats_helper).
%-include ("mondemand_internal.hrl").
-include_lib ("lwes/include/lwes.hrl").

-compile({parse_transform, ct_expand}).

-record (stats_msg, { prog_id,
                      host,
                      num_context = 0,
                      context = [],
                      num_metrics = 0,
                      metrics = []
                    }).
-record (metric, {type, name, value}).

-export ([new/3,
          prog_id/1,
          host/1,
          context/1,
          context_value/2,
          add_contexts/2,
          add_context/3,
          metrics/1,
          metric/1,
          metric_type/1,
          metric_name/1,
          metric_value/1,
          from_ets/1,
          to_lwes/1,
          from_lwes/1
        ]).

-define (STATS_EVENT, <<"MonDemand::StatsMsg">>).
-define (TRACE_EVENT, <<"MonDemand::TraceMsg">>).

% tokens in Stats message
-define (PROG_ID,    <<"prog_id">>).
-define (STATS_NUM,  <<"num">>).
-define (STATS_K,    "k").
-define (STATS_V,    "v").
-define (STATS_T,    "t").
-define (CTXT_NUM,   <<"ctxt_num">>).
-define (CTXT_K,     "ctxt_k").
-define (CTXT_V,     "ctxt_v").
-define (STATS_HOST, <<"host">>).


new (ProgId, Context, Metrics) ->
  #stats_msg { prog_id = ProgId,
               host = net_adm:localhost (),
               num_context = length (Context),
               context = Context,
               num_metrics = length (Metrics),
               metrics = Metrics
             }.

from_ets (Table) ->

  AllEts = ets:tab2list (Table),
  Host = net_adm:localhost (),

  % struct in ets is
  %  { { ProgId, Context, Type, Key }, Value }
  % but I want to send this in the fewest number of mondemand-tool calls
  % so I need to get all {ProgId, Context} pairs, then unique sort them,
  % after that send_stats for all stats which match ProgId/Context pair
  [ begin
      Metrics =
        [ #metric { type = T, name = K, value = V }
          || { { P2, C2, T, K }, V }
          <- AllEts,
             P2 =:= ProgId,
             C2 =:= Context ],
      #stats_msg {
        prog_id = ProgId,
        host = Host,
        num_context = length (Context),
        context = Context,
        num_metrics = length (Metrics),
        metrics = Metrics
      }
    end
    || { ProgId, Context }
    <- lists:usort ( [ {EtsProgId, EtsContext}
                       || { { EtsProgId, EtsContext, _, _ }, _ }
                       <- AllEts
                     ])
  ].

from_lwes (Binary) when is_binary (Binary) ->
  from_lwes (lwes_event:from_binary (Binary, dict));
from_lwes (#lwes_event { attrs = Data}) ->
  % here's the name of the program which originated the metric
  ProgId = dict:fetch (?PROG_ID, Data),
  {Host, NumContexts, Context} = construct_context (Data),
  {NumMetrics, Metrics} = construct_metrics (Data),

  #stats_msg {
    prog_id = ProgId,
    host = Host,
    num_context = NumContexts,
    context = Context,
    num_metrics = NumMetrics,
    metrics = Metrics
  }.

construct_metrics (Data) ->
  Num =
    case dict:find (?STATS_NUM, Data) of
      error -> 0;
      {ok, C} -> C
    end,
  { Num,
    lists:map (
      fun (N) ->
          K = dict:fetch (metric_name_key (N), Data),
          V = dict:fetch (metric_value_key (N), Data),
          T = dict:fetch (metric_type_key (N), Data),
          #metric { name = K, type = T, value = V}
      end,
      lists:seq (1,Num)
    )
  }.

construct_context (Data) ->
  Num =
    case dict:find (?CTXT_NUM, Data) of
      error -> 0;
      {ok, C} -> C
    end,
  { Host, Context } =
    lists:foldl ( fun (N, {H, A}) ->
                    K = dict:fetch (context_name_key (N), Data),
                    V = dict:fetch (context_value_key (N), Data),
                    case K of
                      ?STATS_HOST -> { V, A };
                      _ -> { H, [ {K, V} | A ] }
                    end
                  end,
                  { <<"unknown">>, [] },
                  lists:seq (1,Num)
                ),
  { Host, length (Context), lists:keysort (1, Context) }.

to_lwes (L) when is_list (L) ->
  lists:map (fun to_lwes/1, L);

to_lwes (#stats_msg { prog_id = ProgId,
                      host = Host,
                      num_context = NumContexts,
                      context = Context,
                      num_metrics = NumMetrics,
                      metrics = Metrics
                    }) ->
  #lwes_event {
    name  = ?STATS_EVENT,
    attrs = lists:flatten (
              [ { ?LWES_STRING, ?PROG_ID, ProgId },
                { ?LWES_U_INT_16, ?STATS_NUM, NumMetrics },
                lists:zipwith (fun metric_to_lwes/2,
                               lists:seq (1, NumMetrics),
                               Metrics),
                { ?LWES_U_INT_16, ?CTXT_NUM, NumContexts + 1},
                lists:zipwith (fun context_to_lwes/2,
                               lists:seq (1, NumContexts),
                               Context),
                context_to_lwes (NumContexts+1, { ?STATS_HOST, Host })
              ]
            )
  }.

context_to_lwes (ContextIndex, {ContextKey, ContextValue}) ->
  [ { ?LWES_STRING,
      context_name_key (ContextIndex),
      stringify (ContextKey)
    },
    { ?LWES_STRING,
      context_value_key (ContextIndex),
      stringify (ContextValue)
    }
  ].

metric_to_lwes (MetricIndex,
                #metric { name = Name, type = Type, value = Value }) ->
  [ { ?LWES_STRING,
      metric_name_key (MetricIndex),
      stringify (Name) },
    { ?LWES_STRING,
      metric_type_key (MetricIndex),
      stringify (Type)
    },
    { ?LWES_INT_64,
      metric_value_key (MetricIndex),
      Value
    }
  ].

prog_id (#stats_msg { prog_id = ProgId }) -> ProgId.
host (#stats_msg { host = Host }) -> Host.
context (#stats_msg { context = Context }) -> Context.

context_value (#stats_msg { context = Context }, ContextKey) ->
  context_find (ContextKey, Context, undefined).

context_find (Key, Context, Default) ->
  case lists:keyfind (Key, 1, Context) of
    false -> Default;
    {_, H} -> H
  end.

add_contexts (S = #stats_msg { num_context = ContextNum,
                              context = Context},
              L) when is_list (L) ->
  S#stats_msg { num_context = ContextNum + length (L),
                context = L ++ Context }.
 

add_context (S = #stats_msg { num_context = ContextNum,
                              context = Context},
             ContextKey, ContextValue) ->
  S#stats_msg { num_context = ContextNum + 1,
                context = [ {ContextKey, ContextValue} | Context ] }.

metrics (#stats_msg { metrics = Metrics }) ->
  Metrics.

metric_name (#metric { name = Name }) -> Name.
metric_type (#metric { type = Type }) -> Type.
metric_value (#metric { value = Value }) -> Value.
metric (#metric { name = Name, type = Type, value = Value }) ->
  { Type, Name, Value }.

stringify (I) when is_integer (I) ->
  integer_to_list (I);
stringify (F) when is_float (F) ->
  float_to_list (F);
stringify (A) when is_atom (A) ->
  atom_to_list (A);
stringify (L) ->
  L.

% generate lookup tables for lwes keys so save some time in production
-define (ELEMENT_OF_TUPLE_LIST(N,Prefix),
         element (N,
                  ct_expand:term (
                    begin
                      list_to_tuple (
                        [
                          list_to_binary (
                            lists:concat ([Prefix, integer_to_list(E-1)])
                          )
                          || E <- lists:seq(1,1024)
                        ]
                      )
                    end))).

metric_name_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, ?STATS_K).

metric_value_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, ?STATS_V).

metric_type_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, ?STATS_T).

context_name_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, ?CTXT_K).

context_value_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, ?CTXT_V).
%-=====================================================================-
%-                            Test Functions                           -
%-=====================================================================-
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").


-endif.
