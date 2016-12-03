-module (mondemand_backend_trace_file).

-include_lib ("lwes/include/lwes.hrl").

-behaviour (supervisor).
-behaviour (mondemand_server_backend).
-behaviour (mondemand_backend_worker).

%% mondemand_backend_worker callbacks
-export ([ create/1,
           connected/1,
           connect/1,
           send/2,
           destroy/1
         ]).

%% mondemand_backend callbacks
-export ([ start_link/1,
           process/1,
           required_apps/0,
           type/0
         ]).

%% supervisor callbacks
-export ([ init/1 ]).

-define (POOL, md_be_trace_pool).
-record (state, { config,
                  root,
                  fields
                }).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================
start_link (Config) ->
  supervisor:start_link ( { local, ?MODULE }, ?MODULE, [Config]).

process (Event) ->
  mondemand_backend_worker_pool_sup:process (?POOL, Event).

required_apps () ->
  [ ].

type () ->
  supervisor.

%%====================================================================
% supervisor callbacks
%%====================================================================
init ([Config]) ->
  % default to one process per scheduler
  Number =
    proplists:get_value (number, Config, erlang:system_info(schedulers)),

  { ok,
   {
      {one_for_one, 10, 10},
      [
        { ?POOL,
         { mondemand_backend_worker_pool_sup, start_link,
           [ ?POOL,
             mondemand_backend_worker,
             Number,
             ?MODULE
           ]
         },
         permanent,
         2000,
         supervisor,
         [ ]
        }
      ]
    }
  }.

%%====================================================================
%% mondemand_backend_worker callbacks
%%====================================================================
create (Config) ->
  Dir = proplists:get_value (root, Config, "."),
  Fields =
    lists:map (fun (E) when is_list (E) -> list_to_binary (E) ;
                   (E) when is_binary (E) -> E ;
                   (E) when is_atom (E) -> list_to_binary (atom_to_list (E))
               end,
               proplists:get_value (extra_fields, Config, [])),

  mondemand_server_util:mkdir_p (Dir),

  { ok, #state {
          config = Config,
          root = filename:join (Dir),
          fields = Fields
        }
  }.

connected (_State) ->
  true. % always connected

connect (State) ->
  {ok, State}.

send (State = #state { root = Dir,
                       fields = Fields
                     },
      Binary ) ->
  Event = mondemand_event:from_udp (Binary),
  Msg = mondemand_event:msg (Event),
  case Msg of
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
          {{error,mkdir}, State};
        ok ->
          case attempt_write (Dir, Owner, Id, ReceiptTime, ProgId,
                              ExtraFields, Msg, 0) of
            ok ->
              {ok, State};
            E = {error, _} ->
              {E, State}
          end
      end;
    Other ->
      error_logger:error_msg ("got error ~p on deserialize for ~p",
                              [Other,Event]),
      {{error,deserialize}, State}
  end.

destroy (_) ->
  ok.

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
  Json = lwes_mochijson2:encode (Event),

  case file:write_file (TraceFile, Json, [raw, exclusive]) of
    {error, eexist} ->
      % conflict, so we recurse and try with a larger num
      % FIXME: this ends up being bad if we expect a lot of conflicts, so
      % it might be better to keep track of the last ReceiptTime and Num
      % in the state of the server, but that is complicated, as we'd need
      % to keep track of all recent traces, so we'll just iterate here,
      % most of the time there will only be one or two conflicts
      attempt_write (Dir, Owner, Id, ReceiptTime, ProgId, ExtraFields,
                     Event, Num + 1, Max);
    R -> R
  end.

normalize_to_binary (I) when is_integer (I) ->
  list_to_binary (integer_to_list (I));
normalize_to_binary (F) when is_float (F) ->
  list_to_binary (float_to_list (F));
normalize_to_binary (L) when is_list (L) ->
  list_to_binary (L);
normalize_to_binary (B) when is_binary (B) ->
  B.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
