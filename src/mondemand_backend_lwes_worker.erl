-module (mondemand_backend_lwes_worker).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("mondemand/include/mondemand.hrl").

-behaviour (mondemand_backend_worker).

%% mondemand_backend_worker callbacks
-export ([ create/1,
           connected/1,
           connect/1,
           send/2,
           destroy/1
         ]).

-record (state, { config = undefined,
                  channels = undefined,
                  extra_context = undefined,
                  filter_func = undefined,
                  include_headers = false,
                  mondemand_filtered_key = undefined
                }).

%%====================================================================
%% mondemand_backend_worker callbacks
%%====================================================================
create (Config) ->
  LwesConfig =
    proplists:get_value (lwes, Config, undefined),
  ExtraContext =
    proplists:get_value (extra_context, Config, []),
  FilterFuncConfig =
    proplists:get_value (filter_function, Config, undefined),
  IncludeHeaders =
    proplists:get_value (include_headers, Config, false),
  MondemandKey =
    proplists:get_value (mondemand_filtered_key, Config, undefined),

  case find_function (FilterFuncConfig) of
    { error, E } ->
      {stop, {error, E}};
    FilterFunc ->
      case lwes:open (emitters, LwesConfig) of
        {ok, Channels} ->
          {ok, #state { config = LwesConfig,
                        channels = Channels,
                        extra_context = ExtraContext,
                        filter_func = FilterFunc,
                        include_headers = IncludeHeaders,
                        mondemand_filtered_key = MondemandKey
                      }
          };
        {error, E} ->
          {stop, {error, E}}
      end
  end.

connected (_State) ->
  true. % always connected

connect (State) ->
  {ok, State}.

send (State = #state { channels = ChannelsIn,
                       extra_context = ExtraContext,
                       filter_func = FilterFunc,
                       include_headers = IncludeHeaders,
                       mondemand_filtered_key = MondemandKey },
      EventOrUDP) ->

  Headers = case IncludeHeaders of
              true -> compute_headers (EventOrUDP);
              false -> []
            end,

  ChannelsOut =
    case FilterFunc =:= undefined
           andalso (ExtraContext =:= [] orelse ExtraContext =:= undefined) of
      true ->
        % straight pass through, only adding extra headers
        emit_with_headers (EventOrUDP, Headers, ChannelsIn);
      false ->
        Event = mondemand_event:from_udp (EventOrUDP),
        case FilterFunc(Event) of
          false ->
            EventWithContext = add_context (EventOrUDP, ExtraContext),
            emit_with_headers (EventWithContext, Headers, ChannelsIn);
          true ->
            case MondemandKey of
              undefined -> ok;
              _ ->
                mondemand_server_stats:increment_backend (MondemandKey, events_filtered)
            end,
            ChannelsIn
        end
    end,

  { ok, State#state { channels = ChannelsOut } }.

destroy (_) ->
  ok.

add_context (Event, undefined) ->
  Event;
add_context (Event, []) ->
  Event;
add_context (UDP = {udp, _, _, _, _}, ExtraContext) ->
  add_context (mondemand_event:from_udp (UDP), ExtraContext);
add_context (Event = #md_event {}, ExtraContext) ->
  mondemand_event:add_contexts (Event, ExtraContext).

emit_with_headers ({udp, _, _, _, Event}, Headers, ChannelsIn)
  when is_binary (Event) ->
    lwes:emit (ChannelsIn, [Event, Headers]);
emit_with_headers (Event = #md_event {}, Headers, ChannelsIn) ->
  EventOrEvents = mondemand_event:to_lwes(Event),
  case EventOrEvents of
    E = #lwes_event {} ->
      lwes:emit (ChannelsIn, [lwes_event:to_binary(E), Headers]);
    [E = #lwes_event {}] ->
      lwes:emit (ChannelsIn, [lwes_event:to_binary(E), Headers]);
    L = [#lwes_event{}|_] ->
      lists:foldl (
        fun (E, C) ->
          lwes:emit (C, [lwes_event:to_binary(E), Headers])
        end,
        ChannelsIn,
        L)
  end.

compute_headers ({udp, ReceiptTime, SenderIp, SenderPort, _}) ->
  compute_headers (ReceiptTime, SenderIp, SenderPort);
compute_headers (#md_event { receipt_time = ReceiptTime,
                             sender_ip = SenderIp,
                             sender_port = SenderPort }) ->
  compute_headers (ReceiptTime, SenderIp, SenderPort).

compute_headers (undefined,_,_) -> [];
compute_headers (_,undefined,_) -> [];
compute_headers (_,_,undefined) -> [];
compute_headers (ReceiptTime, SenderIp, SenderPort) ->
  lwes_event:header_fields_to_iolist (ReceiptTime, SenderIp, SenderPort).

find_function (undefined) ->
  undefined;
find_function ({Module, Function}) ->
  case code:which (Module) of
    non_existing -> {error, no_module};
    _ ->
      code:ensure_loaded (Module),
      case erlang:function_exported (Module, Function, 1) of
        false -> {error, no_function};
        true -> fun Module:Function/1
      end
  end.
