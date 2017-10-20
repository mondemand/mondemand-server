-module (mondemand_server_fs_resource).

-export ([ init/1 ]).
-export ([ allowed_methods/2,
           resource_exists/2,
           content_types_provided/2,
           provide_content/2
         ]).

-record (state, {root, filepath, is_dir}).

-include_lib ("kernel/include/file.hrl").
-include_lib ("webmachine/include/webmachine.hrl").

init (Config) ->
  RootDir =
    case proplists:lookup (root, Config) of
      {root, {privdir, Module, Path} } ->
        filename:join(
          mondemand_server_util:module_priv_dir (Module),
          Path);
      {root, Root} -> Root
    end,
  {ok, #state { root = RootDir } }.

allowed_methods (ReqData, State) ->
  {['GET'], ReqData, State}.

resource_exists(ReqData, State = #state { root = Root }) ->
  FilePath = filename:join ([Root, wrq:disp_path(ReqData)]),
  case filelib:is_dir (FilePath) of
    true ->
      {true, ReqData, State#state { filepath = FilePath, is_dir = true }};
    false ->
      case filelib:is_regular (FilePath) of
        true ->
          {true, ReqData, State#state { filepath = FilePath, is_dir = false }};
        false ->
          {false, ReqData, State}
      end
  end.

content_types_provided (ReqData,
                        State = #state { root = Root, is_dir = undefined }) ->
  FilePath = filename:join ([Root, wrq:disp_path(ReqData)]),
  content_types_provided (ReqData,
                          State#state { is_dir = filelib:is_dir (FilePath) });
content_types_provided (ReqData,
                        State = #state { is_dir = IsDir }) ->
  ContentType =
    case IsDir of
      true -> "text/html";
      _ -> webmachine_util:guess_mime (wrq:disp_path(ReqData))
    end,
  {[{ContentType, provide_content}], ReqData, State}.

provide_content (ReqData,
                 State = #state { filepath = FilePath, is_dir = IsDir }) ->
  ResponseContent =
    case IsDir of
      true ->
        Path = wrq:path (ReqData),
        dir_to_html (Path, FilePath);
      false ->
        file_contents (FilePath)
    end,
  { ResponseContent, ReqData, State}.

file_contents (FilePath) ->
  case file:read_file (FilePath) of
    {ok, Bin} -> Bin;
    _ -> error
  end.

dir_to_html (Base, FilePath) ->
 RealBase =
   case lists:reverse (Base) of
     [$/, _] -> Base;
     _ -> Base ++ "/"
   end,
 [ "<!DOCTYPE html>\n",
   "<html><head><title>Index of ",Base,"</title></head>",
   "<body>"
   "<ul>",
   [ ["<li><a href='",RealBase,F,"'>",F,"</a>"]
     || F
     <- lists:sort (dir_contents (FilePath))
   ],
   "</ul>",
   "</body></html>"
 ].

dir_contents (FilePath) ->
  case file:list_dir (FilePath) of
    {ok, F} -> F;
    {error, _} -> []
  end.
