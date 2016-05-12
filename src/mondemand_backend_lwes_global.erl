-module (mondemand_backend_lwes_global).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").
-include_lib ("mondemand/include/mondemand.hrl").

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

-define (POOL, md_be_lwes_global_pool).
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
                       extra_context = ExtraContext },
      {udp,_,_,_,Event} ) 
      when ExtraContext =:= []; ExtraContext =:= undefined ->

  ChannelsOut = lwes:emit (ChannelsIn, Event),
  mondemand_server_stats:increment_backend (?MODULE, events_processed),
  { noreply, State#state { channels = ChannelsOut } };

send (State = #state { channels = ChannelsIn,
                       extra_context = ExtraContext },
      Event = #md_event {} ) 
      when ExtraContext =:= []; ExtraContext =:= undefined ->

  ChannelsOut = lwes:emit (ChannelsIn, mondemand_event:to_lwes(Event)),
  mondemand_server_stats:increment_backend (?MODULE, events_processed),
  { noreply, State#state { channels = ChannelsOut } };

send (State = #state { channels = ChannelsIn,
                       extra_context = ExtraContext},
      Data ) ->

  ChannelsOutFinal =
    case mondemand_event:peek_type_from_udp (Data) of
      undefined ->
        mondemand_server_stats:increment_backend (?MODULE, send_errors),
        error_logger:error_msg ("Bad event ~p",[Data]),
        ChannelsIn;
      stats_msg ->
        Event = mondemand_event:from_udp (Data),
        StatsMsg = mondemand_event:msg (Event),
        Context = mondemand_statsmsg:context (StatsMsg),
        ProgId = mondemand_statsmsg:prog_id (StatsMsg),

        % for the moment filter out anything not an aggregate
        % or not from the mondemand-server
        ShouldSend =
          case lists:keyfind (<<"stat">>, 1, Context) of
            {_,_} ->
              true;
            false ->
              ProgId =:= <<"mondemand_server">>
          end,

        NewEvent =
          case ShouldSend of
            true ->
              % TODO: need to split stats events, but there's not too many
              %       which are bigger at the moment
              case mondemand_statsmsg:num_metrics (StatsMsg) of
                Good when Good =< 1024 ->
                  mondemand_event:to_lwes (
                    mondemand_event:set_msg (Event,
                      mondemand_statsmsg:add_contexts (
                        StatsMsg,
                        ExtraContext
                      )
                    )
                  );
                Bad ->
                  error_logger:error_msg ("Can't send too big ~p keys",[Bad]),
                  undefined
              end;
            false ->
              undefined
          end,

        % only send if we have an event to send
        case NewEvent of
          undefined ->
            mondemand_server_stats:increment_backend (?MODULE, events_processed),
            mondemand_server_stats:increment_backend (?MODULE, events_filtered),
            ChannelsIn;
          _ ->
            ChannelsOut = lwes:emit (ChannelsIn, NewEvent),
            mondemand_server_stats:increment_backend (?MODULE, events_processed),
            ChannelsOut
        end;
      _ ->
        LwesEvent = case Data of
          #md_event {} -> mondemand_event:to_lwes(Data);
          {udp,_,_,_,Packet} -> Packet
        end,
        ChannelsOut = lwes:emit (ChannelsIn, LwesEvent),
        mondemand_server_stats:increment_backend
          (?MODULE, events_processed),
        ChannelsOut
    end,

  { noreply, State#state { channels = ChannelsOutFinal } };

send (State, Request) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

destroy (_) ->
  ok.

