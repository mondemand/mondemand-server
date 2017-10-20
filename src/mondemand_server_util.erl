-module(mondemand_server_util).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").

-compile({parse_transform, ct_expand}).

-export ([seconds_since_epoch/0,
          epoch_to_mdyhms/1,
          mdyhms_to_dir_prefix/1,
          mdyhms_to_log_string/1,
          mkdir_p/1,
          construct_context/1,
          construct_context_string/2,
          construct_context_string/3,
          join/2,
          stat_key/1,
          stat_val/1,
          stat_type/1,
          ctxt_key/1,
          ctxt_val/1,
          log_file_key/1,
          log_line_key/1,
          log_priority_key/1,
          log_message_key/1,
          log_repeat_key/1,
          priority_string/1,
          initialize_stats/1,
          increment_stat/2,
          increment_stat/3,
          for_each_line_in_file/4,
          module_priv_dir/1
        ]).

seconds_since_epoch () ->
  {Mega, Secs, _ } = os:timestamp(),
  Mega * 1000000 + Secs.

epoch_to_mdyhms(undefined) ->
  calendar:now_to_universal_time (os:timestamp());
epoch_to_mdyhms(Time = {_, _, _}) ->
  calendar:now_to_universal_time (Time);
epoch_to_mdyhms(MilliSeconds) ->
  calendar:now_to_universal_time (
    {MilliSeconds div 1000000000,
     MilliSeconds rem 1000000000 div 1000,
     MilliSeconds rem 1000000000 rem 1000
    }).

mdyhms_to_dir_prefix ({{Year, Month, Day}, {_, _, _}}) ->
  filename:join ([ io_lib:fwrite ("~4..0B",[Year]),
                   io_lib:fwrite ("~2..0B",[Month]),
                   io_lib:fwrite ("~2..0B",[Day]) ]).

mdyhms_to_log_string ({{Year, Month, Day}, {Hour, Min, Sec}}) ->
  lists:flatten(
    io_lib:fwrite("~4..0B/~2..0B/~2..0B ~2..0B:~2.10.0B:~2.10.0B",
                  [Year, Month, Day, Hour, Min, Sec])).

% if the directory list of the form
%  [ "foo", "bar", "baz" ]
% I want to filename:join/1 it, so to differentiate this from the
% case where I have mkdir_p("foo"), I need to check for a nested
% list of at least one character, thus [[_|_]|_]
mkdir_p (Dir = [[_|_]|_]) when is_list (Dir) ->
  mkdir_p (filename:join (Dir));
mkdir_p (Dir) ->
  % ensure_dir only seemed to create all but the final level of directories
  % so I need to make_dir right afterward to create the final directory
  case filelib:ensure_dir (Dir) of
    ok ->
      case file:make_dir (Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        E -> E
      end;
    EE -> EE
  end.

construct_context (#lwes_event { attrs = Data }) ->
  CtxtNum =
    case dict:find (<<"ctxt_num">>, Data) of
      error -> 0;
      {ok, C} -> C
    end,
  lists:map (
    fun (N) ->
      K = dict:fetch (ctxt_key (N), Data),
      V = dict:fetch (ctxt_val (N), Data),
      {K,V}
    end,
    lists:seq (1,CtxtNum)
  ).

construct_context_string (Event, Delimiter) ->
  Context = lists:keysort (1, construct_context (Event)),
  {Host, C1} =
     lists:foldl (fun ({<<"host">>,H}, {_,A}) -> {H,A};
                      ({K,V},{H,A}) -> {H,[[K,"=",V]|A]}
                  end,
                  {"unknown",[]}, Context),
  ContextString = join (C1, Delimiter),
  {Host, ContextString}.

construct_context_string (Context, InnerDelimiter, OuterDelimiter) ->
  join (lists:map (fun ({K,V}) -> [mondemand_util:stringify (K),
                                   InnerDelimiter,
                                   mondemand_util:stringify (V)
                                  ]
                   end, Context),
        OuterDelimiter).

join (L,S) when is_list (L) ->
  lists:reverse (join (L, S, [])).

join ([], _, A) ->
  A;
join ([H], _, []) ->
  [H];
join ([H], S, A) ->
  [H,S|A];
join ([H|T], S, []) ->
  join (T,S,[H]);
join ([H|T], S, A) ->
  join (T,S,[H,S|A]).

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

stat_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "k").

stat_val (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "v").

stat_type (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "t").

ctxt_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "ctxt_k").

ctxt_val (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "ctxt_v").

log_file_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "f").
log_line_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "l").
log_priority_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "p").
log_message_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "m").
log_repeat_key (N) ->
  ?ELEMENT_OF_TUPLE_LIST (N, "r").

priority_string (0) -> <<"emerg">>;
priority_string (1) -> <<"alert">>;
priority_string (2) -> <<"crit">>;
priority_string (3) -> <<"error">>;
priority_string (4) -> <<"warning">>;
priority_string (5) -> <<"notice">>;
priority_string (6) -> <<"info">>;
priority_string (7) -> <<"debug">>;
priority_string (8) -> <<"all">>.

initialize_stats (Keys) ->
  lists:foldl (fun (E, D) ->
                 dict:update_counter (E, 0, D)
               end,
               dict:new (),
               Keys).

increment_stat (Key, Stats) ->
  dict:update_counter (Key, 1, Stats).

increment_stat (Key, Amount, Stats) ->
  dict:update_counter (Key, Amount, Stats).

for_each_line_in_file (Name, Proc, Mode, Accum0) ->
  case file:open (Name, Mode) of
    {ok, Device} ->
      for_each_line (Device, Proc, 1, Accum0);
    {error, E} ->
      {error, 0, E}
  end.

for_each_line (Device, Proc, LineNumber, Accum) ->
  case io:get_line(Device, "") of
    eof  ->
      file:close(Device), Accum;
    Line ->
      case Proc(Line, Accum) of
        {error, E} ->
          file:close (Device),
          {error, LineNumber, E};
        NewAccum ->
          for_each_line (Device, Proc, LineNumber + 1, NewAccum)
      end
  end.

module_priv_dir (Module) ->
  PrivDir = case code:priv_dir(Module) of
    {error, _} ->
      EbinDir = filename:dirname(code:which(Module)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Path ->
      Path
  end,
  PrivDir.
