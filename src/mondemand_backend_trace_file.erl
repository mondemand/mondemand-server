-module (mondemand_backend_trace_file).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (mondemand_server_backend).
-behaviour (gen_server).

%% mondemand_backend callbacks
-export ([ start_link/1,
           process/1,
           required_apps/0,
           type/0
         ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, { config,
                  root,
                  fields
                }).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================
start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

required_apps () ->
  [ ].

type () ->
  worker.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (Config) ->
  Dir = proplists:get_value (root, Config, "."),
  Fields =
    lists:map (fun (E) when is_list (E) -> list_to_binary (E) ;
                   (E) when is_binary (E) -> E ;
                   (E) when is_atom (E) -> list_to_binary (atom_to_list (E))
               end,
               proplists:get_value (extra_fields, Config, [])),

  mondemand_server_util:mkdir_p (Dir),

  % initialize all stats to zero
  mondemand_server_stats:init_backend (?MODULE, events_processed),
  mondemand_server_stats:init_backend (?MODULE, send_errors),

  { ok, #state {
          config = Config,
          root = filename:join (Dir),
          fields = Fields
        }
  }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Binary},
             State = #state { root = Dir,
                              fields = Fields }) ->
  Event =  lwes_event:from_udp_packet (Binary, json_eep18),

  mondemand_server_stats:increment_backend (?MODULE, events_processed),

  case Event of
    {PL} when is_list (PL) ->
      Owner =
        normalize_to_binary (
          proplists:get_value (<<"mondemand.owner">>, PL, <<"unknown">>)),
      Id =
        normalize_to_binary (
          proplists:get_value (<<"mondemand.trace_id">>, PL, <<"unknown">>)),
      ProgId =
        proplists:get_value (<<"mondemand.prog_id">>, PL, <<"unknown">>),
      ReceiptTime =
        list_to_binary (integer_to_list (
            proplists:get_value (<<"ReceiptTime">>, PL))),
      ExtraFields =
        lists:flatten ([ proplists:get_value (F, PL, [])
                         || F <- Fields ]),

      case mondemand_server_util:mkdir_p ([Dir, Owner, Id]) of
        {error, E1} ->
          error_logger:error_msg ("got error ~p on mkdir for ~p",
                                  [E1,Event]),
          mondemand_server_stats:increment_backend (?MODULE, send_errors);
        ok ->
          case attempt_write (Dir, Owner, Id, ReceiptTime, ProgId,
                              ExtraFields, Event, 0) of
            ok ->
              ok;
            {error, _} ->
              mondemand_server_stats:increment_backend (?MODULE, send_errors)
          end
      end;
    _ ->
      mondemand_server_stats:increment_backend (?MODULE, send_errors)
  end,
  { noreply, State };
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  { ok, State }.

%%====================================================================
%% Internal
%%====================================================================
attempt_write (Dir, Owner, Id, ReceiptTime, ProgId, ExtraFields,
               Event, Num) ->
  attempt_write (Dir, Owner, Id, ReceiptTime, ProgId, ExtraFields,
                 Event, Num, 100).

attempt_write (_Dir, _Owner, _Id, _ReceiptTime, _ProgId, _ExtraFields,
               Event, Num, Num) ->
  error_logger:error_msg ("Failed to write ~p, ~p times", [Event, Num]),
  { error, conflict };
attempt_write (Dir, Owner, Id, ReceiptTime, ProgId, ExtraFields,
               Event, Num, Max) ->
  % Calculate the TraceFile Name,
  %
  % This is dense, but we end up with the trace file being
  %
  %   Dir/Owner/Id/trace-ReceipTime-Num-ProgId(-ExtraFields)?
  %
  % where the Num is there incase the ReceiptTime is the same and the
  % ExtraFields end up being separated by hyphens
  TraceFile =
    filename:join ([Dir, Owner, Id,
                    mondemand_server_util:join (
                      [ "trace",
                        ReceiptTime,
                        list_to_binary (integer_to_list (Num)),
                        ProgId | ExtraFields],
                      <<"-">>)]),

  case file:open (TraceFile, [raw, write, exclusive]) of
    {error, eexist} ->
      % conflict, so we recurse and try with a larger num
      % FIXME: this ends up being bad if we expect a lot of conflicts, so
      % it might be better to keep track of the last ReceiptTime and Num
      % in the state of the server, but that is complicated, as we'd need
      % to keep track of all recent traces, so we'll just iterate here,
      % most of the time there will only be one or two conflicts
      attempt_write (Dir, Owner, Id, ReceiptTime, ProgId, ExtraFields,
                     Event, Num + 1, Max);
    {ok, Dev} ->
      case file:write (Dev, lwes_mochijson2:encode (Event)) of
        { error, E3 } ->
          file:close (Dev),
          error_logger:error_msg ("got error ~p on write of ~p",
                                  [E3,Event]),
          { error, write_fail };
        ok ->
          file:close (Dev),
          ok
      end
  end.

normalize_to_binary (I) when is_integer (I) ->
  list_to_binary (integer_to_list (I));
normalize_to_binary (L) when is_list (L) ->
  list_to_binary (L);
normalize_to_binary (B) when is_binary (B) ->
  B.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(HAVE_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

-endif.
