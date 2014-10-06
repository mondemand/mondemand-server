-module (mondemand_server_cron).

-behaviour (gen_server).

%% API
-export ([start_link/0]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, {delay}).

%%====================================================================
%% API callbacks
%%====================================================================
start_link () ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([]) ->
  % ensure terminate is called
  process_flag( trap_exit, true ),
  Now = os:timestamp (),
  Next = mondemand_server_util:millis_to_next_round_second (Now),
  error_logger:info_msg ("Starting at ~p next in ~p", [Now, Next]),
  erlang:send_after (Next, self(), tick),
  {ok, #state {}}.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (tick, State) ->
  Now = os:timestamp (),
  Next = mondemand_server_util:millis_to_next_round_second (Now),
  error_logger:info_msg ("Tick at ~p next in ~p", [Now, Next]),
  erlang:send_after (Next, self(), tick),
  { noreply, State };
handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
