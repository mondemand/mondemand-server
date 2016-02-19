-module (mondemand_server_mappings_db).

-behaviour (gen_server).

%% API
-export ([start_link/2,
          map/2]).

%% gen_server callbacks
-export ( [ init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3
          ]).

-record (state, { dir,
                  delay,
                  last_scan
                }).
-define (TABLE, md_mappings).

%-=====================================================================-
%-                                  API                                -
%-=====================================================================-
start_link(Directory, ReloadSeconds) ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE,
                         [Directory, ReloadSeconds], []).

map (List, From) when is_list (List) ->
  map (list_to_binary(List), From);
map (List, From) when is_list (From) ->
  map (List, list_to_binary(From));
map (List, From) ->
  case ets:lookup (?TABLE, {List, From}) of
    [{_,To}] -> To;
    _ -> undefined
  end.

%-=====================================================================-
%-                        gen_server callbacks                         -
%-=====================================================================-
init([Directory, ReloadSeconds]) ->
  % ensure terminate is called
  process_flag (trap_exit, true),
  ets:new (?TABLE, [ set,
                     named_table,
                     {write_concurrency, false},
                     {read_concurrency, true},
                     {keypos, 1}
                   ]),
  Delay = ReloadSeconds * 1000, % delay is in millseconds
  State = #state {
            delay = Delay,
            dir = Directory,
            last_scan = []
          },
  case load (State) of
    { error, Reason } -> { stop, Reason };
    InitialScan -> { ok, State#state { last_scan = InitialScan }, Delay }
  end.

handle_call (_Request, _From, State = #state { delay = Delay }) ->
  {reply, ok, State, Delay}.

handle_cast (_Request, State = #state { delay = Delay }) ->
  {noreply, State, Delay}.

handle_info (timeout, State = #state { delay = Delay }) ->
  {noreply, State#state { last_scan = load (State) }, Delay};
handle_info (_Info, State = #state { delay = Delay }) ->
  {noreply, State, Delay}.

terminate (_Reason, _State) ->
  ets:delete (?TABLE),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%-====================================================================
%- Internal functions
%-====================================================================
load (#state { dir = ConfigDir, last_scan = LastScan}) ->
  NewScan = lwes_file_watcher:scan ([ConfigDir]),
  case lwes_file_watcher:changes (LastScan, NewScan) of
    [] ->
      ok; % no changes
    Changes ->
      process_change_list (Changes)
  end,
  NewScan.

process_change_list (L) ->
  [ process_file_entry (E) || E <- L],
  ok.

process_file_entry ({file, Status, File}) ->
  FileName = filename:basename (File),
  List =
    case is_list (FileName) of
      true -> list_to_binary (FileName);
      false -> FileName
    end,
  case Status of
    removed ->
      io:format("file ~p was removed!~n",[FileName]);
    _ ->
      case
        mondemand_server_util:for_each_line_in_file (File,
                                                     fun handle_line/2,
                                                     [read],
                                                     {1,List,[]})
      of
        {error, Line, Error} ->
          error_logger:error_msg ("Error ~p in mapping file ~p at line ~p"
                                  " most likely partially loaded",
                                  [Error, File, Line]);
        {_, _, Entries} ->
          KeysBefore = get_all_keys_for_list (List),
          KeysAfter = update_values (List, Entries),
          KeysToDelete =
            ordsets:subtract (
              ordsets:from_list (KeysBefore),
              ordsets:from_list (KeysAfter)),

          error_logger:info_msg ("Adding ~p entries to mapping ~s",
                                 [length (KeysAfter), List]),
          case length (KeysToDelete) of
            0 -> ok;
            D ->
             error_logger:info_msg ("Deleting ~p entries from mapping ~s",
                                    [D,List])
          end,

          delete_values (KeysToDelete)
      end
  end;
process_file_entry (_) ->
  ok.

handle_line (Line, {LineNumber, List, Entries}) ->
  % chomp newline if it exists, otherwise it might be a file without
  % a newline at the end, so just accept that last line as is
  LineWithoutNewLine =
    case lists:reverse (Line) of
      [ $\n | R ] -> lists:reverse (R);
      _ -> Line
    end,
  case re:split (LineWithoutNewLine, "\t") of
    [From, To] ->
      {LineNumber + 1, List, [{From,To} | Entries]};
    _ ->
      {error, malformed_line}
  end.

update_values (List, Entries) ->
  lists:map (fun ({From, To}) ->
               true = ets:insert (?TABLE, {{List, From}, To}),
               {List, From}  % return the list of keys added
             end,
             Entries).

delete_values (ToDelete) ->
  lists:foreach (fun (K) ->
                   ets:delete (?TABLE, K)
                 end,
                 ToDelete).

get_all_keys_for_list (List) ->
  lists:map (fun ([From]) -> {List, From} end,
             ets:match (?TABLE, {{List, '$1'}, '_'})).
