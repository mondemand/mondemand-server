-module (mondemand_backend_performance_db).

-behaviour (gen_server).

%% API
-export ([start_link/0]).

%% gen_server callbacks
-export ( [ init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3
          ]).

-record (state, {}).

%-=====================================================================-
%-                                  API                                -
%-=====================================================================-
start_link() ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

%-=====================================================================-
%-                        gen_server callbacks                         -
%-=====================================================================-
init([]) ->
  ets:new (md_be_perf_id2pid, [ set,
                                public,
                                named_table,
                                {write_concurrency, true},
                                {read_concurrency, true},
                                {keypos, 1}
                              ]),
  { ok, #state{} }.

handle_call (_Request, _From, State = #state { }) ->
  {reply, ok, State }.

handle_cast (_Request, State = #state { }) ->
  {noreply, State}.

handle_info (_Info, State = #state {}) ->
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
