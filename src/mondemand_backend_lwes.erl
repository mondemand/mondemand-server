-module (mondemand_backend_lwes).

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

-define (POOL, md_be_lwes_pool).
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
      {udp, _, SenderIp, SenderPort, Event} )
      when ExtraContext =:= []; ExtraContext =:= undefined ->

  ChannelsOut =
    lwes:emit (ChannelsIn,
               [ Event,
                 lwes_event:header_fields_to_iolist (
                   mondemand_util:millis_since_epoch(),
                   SenderIp,
                   SenderPort)
               ] ),
  { ok, State#state { channels = ChannelsOut } };

send (State = #state { channels = ChannelsIn,
                       extra_context = ExtraContext },
      Event = #md_event {} )
      when ExtraContext =:= []; ExtraContext =:= undefined ->

  ChannelsOut =
    lwes:emit (ChannelsIn,
               [ lwes_event:to_binary (mondemand_event:to_lwes(Event)),
                 lwes_event:header_fields_to_iolist (
                   mondemand_event:receipt_time (Event),
                   mondemand_event:sender_ip (Event),
                   mondemand_event:sender_port (Event)
                 )
               ]
              ),
  { ok, State#state { channels = ChannelsOut } };

send (State = #state { channels = ChannelsIn,
                       extra_context = ExtraContext},
      Data ) ->

  case mondemand_event:peek_type_from_udp (Data) of
    undefined ->
      error_logger:error_msg ("Bad event ~p",[Data]),
      {{error, bad_event}, ChannelsIn};
    stats_msg ->
      % we need special processing for internally generated versus normal
      % stats events
      %  we need to add extra context before forwarding
      Event = mondemand_event:from_udp (Data),
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
      ChannelsOut =
        lwes:emit (ChannelsIn,
                   [ lwes_event:to_binary (NewEvent),
                     lwes_event:header_fields_to_iolist (
                       mondemand_event:receipt_time (Event),
                       mondemand_event:sender_ip (Event),
                       mondemand_event:sender_port (Event)
                     )
                   ]
                  ),
      {ok, State#state { channels = ChannelsOut } };
    _ ->
      LwesEvent =
        case Data of
          #md_event {} ->
            [ lwes_event:to_binary (mondemand_event:to_lwes(Data)),
              lwes_event:header_fields_to_iolist (
                mondemand_event:receipt_time (Data),
                mondemand_event:sender_ip (Data),
                mondemand_event:sender_port (Data)
              )
            ];
          {udp,_,SenderIp,SenderPort,Packet} ->
            [ Packet,
              lwes_event:header_fields_to_iolist (
                mondemand_util:millis_since_epoch(),
                SenderIp,
                SenderPort)
            ]
        end,
      {ok, State#state { channels = lwes:emit (ChannelsIn, LwesEvent) } }
  end.

destroy (_) ->
  ok.
