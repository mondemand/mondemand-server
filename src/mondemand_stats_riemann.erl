-module (mondemand_stats_riemann).

-include_lib ("lwes/include/lwes.hrl").
-include_lib ("riemann/include/riemann_pb.hrl").

-behaviour (gen_server).

%% API
-export ([ start_link/1,
           process/1 ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, {}).

%%====================================================================
%% API
%%====================================================================
start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

process (Event) ->
  gen_server:cast (?MODULE, {process, Event}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init (_Config) ->
  { ok, #state {  } }.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({process, Binary}, State) ->
  Event =  lwes_event:from_udp_packet (Binary, dict),
  #lwes_event { attrs = Data } = Event,

  Timestamp = dict:fetch (<<"ReceiptTime">>, Data),

  Num = dict:fetch (<<"num">>, Data),
  ProgId = dict:fetch (<<"prog_id">>, Data),
  { Host, Context } =
    case mondemand_util:construct_context (Event) of
      [] -> { "unknown", [] };
      C ->
        case lists:keytake (<<"host">>, 1, C) of
          false -> { "unknown", C };
          {value, {<<"host">>, H}, OC } -> {H, OC }
        end
    end,

  Events =
    lists:map (
      fun(E) ->
        K = dict:fetch (mondemand_util:stat_key (E), Data),
        V = dict:fetch (mondemand_util:stat_val (E), Data),
        error_logger:info_msg ("send ~p : ~p : ~p : ~p : ~p => ~p",
          [Timestamp, ProgId, Host, Context, K, V]),
        #riemannevent {
          service = ProgId,
          state = "ok",
          description = K,
          metric_sint64 = V,
          metric_f = V * 1.0,
          time = Timestamp,
          host = Host,
          attributes = Context
        }

%        riemann:event ([ {service, ProgId},
%                         {state, "ok"},
%                         {metric, V},
%                         {time, Timestamp},
%                         {description, K},
%                         {host, Host},
%                         {attributes, Context} ])
      end,
      lists:seq (1,Num)
    ),
  riemann:send (Events),
  {noreply, State};

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  {noreply, State}.

terminate (_Reason, #state { }) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
