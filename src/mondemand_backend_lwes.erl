-module (mondemand_backend_lwes).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").

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

-define (POOL, md_backend_lwes_pool).
-record (state, { config,
                  channels,
                  extra_context
                }).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================
start_link (Config) ->
  supervisor:start_link ( { local, ?MODULE }, ?MODULE, [Config]).

process (Event) ->
  mondemand_backend_worker_pool_sup:process (?POOL, Event).

required_apps () ->
  [ lwes ].

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
  LwesConfig = proplists:get_value (lwes, Config, undefined),
  ExtraContext = proplists:get_value (extra_context, Config, []),

  case lwes:open (emitters, LwesConfig) of
    {ok, Channels} ->
      mondemand_server_stats:create_backend (?MODULE, events_processed),
      {ok, #state { config = LwesConfig,
                    channels = Channels,
                    extra_context = ExtraContext }
      };
    {error, E} ->
      {stop, {error, E}}
  end.

connected (_State) ->
  true. % always connected

connect (State) ->
  {ok, State}.

send (State = #state { channels = ChannelsIn,
                       extra_context = undefined },
      {udp,_,_,_,Event}) ->
  ChannelsOut = lwes:emit (ChannelsIn, Event),
  mondemand_server_stats:increment_backend (?MODULE, events_processed),
  { noreply, State#state { channels = ChannelsOut } };

send (State = #state { channels = ChannelsIn,
                       extra_context = ExtraContext},
      UDP) ->

  ChannelsOutFinal =
    case mondemand_event:peek_type_from_udp (UDP) of
      undefined ->
        mondemand_server_stats:increment_backend (?MODULE, send_errors),
        error_logger:error_msg ("Bad event ~p",[UDP]),
        ChannelsIn;
      stats_msg ->
        % we need special processing for internally generated versus normal
        % stats events
        case UDP of
          {udp,_,_,_,Packet} ->
            case ExtraContext of
              E when E =:= []; E =:= undefined -> 
                % no context defined, so just forward  it
                mondemand_server_stats:increment_backend (?MODULE, events_processed),
                lwes:emit (ChannelsIn, Packet);
              _ ->
                % otherwise we need to add extra context before forwarding
                Event = mondemand_event:from_udp (UDP),
                StatsMsg = mondemand_event:msg (Event),
                NewEvent =
                  mondemand_event:to_lwes (
                    mondemand_event:set_msg (Event,
                      mondemand_statsmsg:add_contexts (
                        StatsMsg,
                        ExtraContext
                      )
                    )
                  ),
                mondemand_server_stats:increment_backend (?MODULE, events_processed),
                lwes:emit (ChannelsIn, NewEvent)
            end;
          _ ->
            % internal events (ie, aggregates and mondemand server stats) will
            % be filtered out by this logic
            mondemand_server_stats:increment_backend (?MODULE, events_processed),
            mondemand_server_stats:increment_backend (?MODULE, events_filtered),
            ChannelsIn
        end;
      _ ->
        case UDP of
          {udp,_,_,_,Packet} ->
            ChannelsOut = lwes:emit (ChannelsIn, Packet),
            mondemand_server_stats:increment_backend
              (?MODULE, events_processed),
            ChannelsOut;
          _ ->
            mondemand_server_stats:increment_backend (?MODULE, send_errors),
            error_logger:error_msg ("Bad event ~p",[UDP]),
            ChannelsIn
        end
    end,

  { noreply, State#state { channels = ChannelsOutFinal } };

send (State, Request) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

destroy (_) ->
  ok.

