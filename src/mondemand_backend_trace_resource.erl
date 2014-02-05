-module (mondemand_backend_trace_resource).

-export ([ init/1 ]).
-export ([ allowed_methods/2,
           resource_exists/2,
           content_types_provided/2,
           to_json/2,
           to_javascript/2
         ]).

-record (state, {root, type, owner, id, file}).

-include_lib ("kernel/include/file.hrl").
-include_lib ("webmachine/include/webmachine.hrl").

init (Config) ->
  {root, Root} = proplists:lookup (root, Config),
  {ok, #state { root = Root } }.

allowed_methods (ReqData, State) ->
  {['GET'], ReqData, State}.

resource_exists(ReqData, State = #state { root = Root }) ->
  % because we have a dispatch rules of
  %   {["trace",owner,id], ...}
  %   {["trace",owner], ...}
  %   [{"trace"], ...}
  % we'll check the path info for the three cases
  PathInfo = wrq:path_info (ReqData),
  case PathInfo of
    [] ->
      check_trace_dir (ReqData, State, Root);
    L when length (L) =:= 1 ->
      check_trace_owner_dir (ReqData, State, Root, L);
    L when length (L) =:= 2 ->
      check_trace_owner_id_dir (ReqData, State, Root, L);
    L when length (L) =:= 3 ->
      check_trace_file (ReqData, State, Root, L);
    _ ->
      {false, ReqData, State}
  end.

check_trace_dir (ReqData, State, Root) ->
  FilePath = filename:join ([Root, "trace"]),
  case filelib:is_dir (FilePath) of
    true -> {true, ReqData, State#state { type = owners, file = FilePath }};
    false -> {false, ReqData, State}
  end.

check_trace_owner_dir (ReqData, State, Root, [{owner, Owner}]) ->
  FilePath = filename:join ([Root, "trace", Owner]),
  case filelib:is_dir (FilePath) of
    true -> {true, ReqData, State#state { type = ids, owner = Owner,
                                          file = FilePath }};
    false -> {false, ReqData, State}
  end.

check_trace_owner_id_dir (ReqData, State, Root, L) ->
  case { proplists:get_value (owner, L), proplists:get_value (id, L) } of
    { Owner, Id } when Owner =/= undefined; Id =/= undefined ->
      FilePath = filename:join ([Root, "trace", Owner, Id]),
      case filelib:is_dir (FilePath) of
        true -> {true, ReqData,
                 State#state { type = traces, owner = Owner, id = Id,
                               file = FilePath }};
        false -> {false, ReqData, State}
      end;
    _ ->
      {false, ReqData, State}
  end.

check_trace_file (ReqData, State, Root, L) ->
  case { proplists:get_value (owner, L),
         proplists:get_value (id, L),
         proplists:get_value (file, L)
       } of
    { Owner, Id, File }
      when Owner =/= undefined; Id =/= undefined ; File =/= undefined ->
        FilePath = filename:join ([Root, "trace", Owner, Id, File]),
        case filelib:is_regular (FilePath) of
          true -> {true, ReqData,
                   State#state { type = trace, owner = Owner, id = Id,
                                 file = FilePath } };
          false -> {false, ReqData, State}
        end;
      _ ->
        {false, ReqData, State}
  end.

% order matters here, if you put text/javascript first, you will get a 500
% as it will use that if a content type is not specified and it will require
% a jsonp query arg, if we list application/json first then hitting this
% with a browser and without jsonp will work
content_types_provided (ReqData, State) ->
  { [ {"application/json", to_json},
      {"text/javascript", to_javascript}
    ],
    ReqData, State
  }.

to_javascript (ReqData, State) ->
  case get_jsonp (ReqData) of
    undefined ->
      % in order to get javascript you need to specify a callback, otherwise
      % it's considered an error
      { { halt, 500}, ReqData, State };
    _ ->
      to_json (ReqData, State)
  end.

to_json (ReqData,
         State = #state { type = Type, owner = _Owner,
                          id = _Id, file = FilePath }) ->

  Prefix = get_prefix (ReqData),

  JSON =
    case Type of
      owners -> % list owners
        list_owners (Prefix, FilePath);
      ids -> % list ids for owner
        list_ids (Prefix, FilePath);
      traces -> % list files for owner and id
        list_traces (FilePath);
      trace -> % return file for owner and id and name
        get_file (FilePath)
    end,

  ResponseBody =
    case get_jsonp (ReqData) of
      undefined -> JSON;
      Callback -> [ Callback, "(", JSON, ");" ]
    end,

  { ResponseBody, ReqData, State }.

list_owners (Prefix, Root) ->
  list_files (Prefix, Root).

list_ids (Prefix, Root) ->
  list_files (Prefix, Root).

list_traces (Root) ->
  {ok, Filenames} = file:list_dir (Root),
  Entries = lists:flatten ([ process_file (Root, F) || F <- Filenames ]),
  Sorted = [ E || {_, E} <- lists:keysort (1, Entries) ],
  Index =
        [ { "total_rows", length (Filenames) },
          { "offset", 0 },
          { "rows", Sorted }
        ],
  list_to_binary (lwes_mochijson2:encode (Index)).

process_file (Root, File) ->
  FileName = filename:join ([Root, File]),
  {ok, Bin} = file:read_file (FileName),
  try
    lwes_mochijson2:decode (Bin, [{format, proplist}])
  of
    Json ->
      ProgId = proplists:get_value (<<"mondemand.prog_id">>, Json),
      TraceOwner = proplists:get_value (<<"mondemand.owner">>, Json),
      TraceId = proplists:get_value (<<"mondemand.trace_id">>, Json),
      Message = proplists:get_value (<<"mondemand.message">>, Json),
      Host = proplists:get_value (<<"mondemand.src_host">>, Json),
      ReceiptTime = proplists:get_value (<<"ReceiptTime">>, Json),
      { ReceiptTime,
        [ {"id", list_to_binary (File)},
          {"key", [ TraceOwner, TraceId ] },
          {"value", [ { "prog_id", ProgId },
                      { "host", Host },
                      { "message", Message},
                      { "timestamp", ReceiptTime } ] }
        ]
      }
  catch
    _:_ ->
      error_logger:error_msg ("Failed to parse ~p with mochijson",[FileName]),
      []
  end.

list_files (Prefix, Dir) ->
  AllFiles =
    case file:list_dir (Dir) of
      {ok, F} -> F;
      {error, _} -> []
    end,
  FilteredByPrefix =
    case Prefix of
      undefined -> AllFiles;
      _ ->
        lists:filter (
          fun (E) -> lists:prefix (Prefix, E) end,
          AllFiles)
    end,
  mochijson2:encode ([ [{ <<"label">>, list_to_binary (F)}]
                       || F <- lists:sort (FilteredByPrefix) ]).

get_file (FilePath) ->
  case file:read_file (FilePath) of
    {ok, Bin} -> Bin;
    _ -> <<"{}">>  % return empty json for error
  end.

get_jsonp (ReqData) ->
  case wrq:get_qs_value ("jsonp", ReqData) of
    undefined -> wrq:get_qs_value ("callback", ReqData);
    V -> V
  end.

get_prefix (ReqData) ->
  wrq:get_qs_value ("term", ReqData).
