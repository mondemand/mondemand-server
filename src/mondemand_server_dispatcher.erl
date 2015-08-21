-module (mondemand_server_dispatcher).

-include_lib ("mondemand/include/mondemand.hrl").

-behaviour (gen_server).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

%% API
-export ([start_link/3, dispatch/2]).

-record (state, { dispatch, name, events_processed = 0 }).

start_link (Dispatch, WorkerAtom, Name) ->
  gen_server:start_link ({local, WorkerAtom}, ?MODULE, [Dispatch, Name], []).

dispatch (Pid, Event) ->
  gen_server:cast (Pid, {dispatch, Event}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([Dispatch, Name]) ->
  gproc_pool:connect_worker (mondemand_dispatcher, Name),
  {ok, #state { dispatch = Dispatch, name = Name } }.

handle_call ({stats}, _,
             State = #state { events_processed = EventsProcessed }) ->
  { reply, EventsProcessed, State };
handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({dispatch, Event},
             State = #state { dispatch = Dispatch,
                              events_processed = EventsProcessed
                            }) ->
  dispatch_one (Event, Dispatch),
  { noreply, State#state { events_processed = EventsProcessed + 1 } };
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% internal functions
%%====================================================================

dispatch_one (Event, Dispatch) ->
  mondemand_server_stats:increment (events_received),

  % call handlers for each event type
  case mondemand_event:peek_name_from_udp (Event) of
    { error, _ } ->
      mondemand_server_stats:increment (dispatcher_errors),
      error_logger:error_msg ("Bad event ~p",[Event]);
    EventName ->
      % first we check the specific dispatch by event name, and send to all
      % the handlers for that name
      case dict:find (EventName, Dispatch) of
        {ok, V} ->
          [ M:process (Event) || M <- V ],
          mondemand_server_stats:increment (events_dispatched);
        error ->
          % if we don't find the name, we send through any "*" handlers
          case dict:find ("*", Dispatch) of
            {ok, DV} ->
              [ M:process (Event) || M <- DV ],
              mondemand_server_stats:increment (events_dispatched);
            error ->
              mondemand_server_stats:increment (dispatcher_errors),
              error_logger:error_msg ("No handler for event ~p in dispatch~n~p",
                                      [EventName, Dispatch])
          end
      end
  end.
