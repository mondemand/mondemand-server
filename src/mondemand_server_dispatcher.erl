-module (mondemand_server_dispatcher).

-include_lib ("mondemand/include/mondemand.hrl").
-include ("mondemand_server_internal.hrl").

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
-export ([start_link/2, dispatch/2]).

-record (state, { dispatch, name, events_processed = 0 }).

start_link (Name, Dispatch) ->
  gen_server:start_link ({local, Name}, ?MODULE, [Name, Dispatch], []).

dispatch (Name, Event) ->
  gen_server:cast (Name, {dispatch, Event}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([Name, Dispatch]) ->
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
    {error, malformed_event }->
      mondemand_server_stats:increment (dispatcher_errors),
      error_logger:error_msg ("Bad event ~p",[Event]);
    EventName ->
      case name_to_entry (EventName, Dispatch) of
        [] ->
          mondemand_server_stats:increment (dispatcher_errors),
          error_logger:error_msg ("No handler for event ~p in dispatch~n~p",
                                  [EventName, Dispatch]);
        V ->
          [ Module:process (Event) || Module <- V ],
          mondemand_server_stats:increment (events_dispatched)
      end
  end.

name_to_entry (?MD_ANNOTATION_EVENT, #mds_dispatch { annotation_msg = A }) -> A;
name_to_entry (?MD_LOG_EVENT,        #mds_dispatch { log_msg = A }) -> A;
name_to_entry (?MD_PERF_EVENT,       #mds_dispatch { perf_msg = A }) -> A;
name_to_entry (?MD_STATS_EVENT,      #mds_dispatch { stats_msg = A }) -> A;
name_to_entry (?MD_TRACE_EVENT,      #mds_dispatch { trace_msg = A }) -> A;
name_to_entry (_, _) -> [].
