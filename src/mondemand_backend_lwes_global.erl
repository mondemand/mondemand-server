-module (mondemand_backend_lwes_global).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("kernel/include/file.hrl").

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
                  channels,
                  extra_context
                }).

%%====================================================================
%% mondemand_backend callbacks
%%====================================================================
start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

required_apps () ->
  [ lwes ].

type () ->
  worker.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (Config) ->
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

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, {udp,_Port,_SenderIp,_SenderPort,Event}},
             State = #state { channels = ChannelsIn,
                              extra_context = undefined }) ->
  ChannelsOut = lwes:emit (ChannelsIn, Event),
  mondemand_server_stats:increment_backend (?MODULE, events_processed),
  { noreply, State#state { channels = ChannelsOut } };
handle_cast ({process, UDP},
             State = #state { channels = ChannelsIn,
                              extra_context = ExtraContext}) ->

  ChannelsOutFinal =
    case mondemand_event:peek_type_from_udp (UDP) of
      undefined ->
        mondemand_server_stats:increment_backend (?MODULE, send_errors),
        error_logger:error_msg ("Bad event ~p",[UDP]),
        ChannelsIn;
      stats_msg ->
        Event = mondemand_event:from_udp (UDP),
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
              mondemand_event:to_lwes (
                mondemand_event:set_msg (Event,
                  mondemand_statsmsg:add_contexts (
                    StatsMsg,
                    ExtraContext
                  )
                )
              );
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
