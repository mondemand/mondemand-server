-module (mondemand_server_dispatcher_sup).

-behaviour (supervisor).

%% API
-export ([start_link/2,
          dispatchers/1,
          dispatch/2
         ]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link (Num, Dispatch) ->
  supervisor:start_link({local, mds_dispatcher_sup}, ?MODULE, [Num, Dispatch]).

dispatchers (Num) when is_integer (Num) ->
  { 1, Num, names_tuple (Num) }.

dispatch (N, Event) when is_integer (N) ->
  R = random:uniform (N),
  dispatch ({R, N, names_tuple(N)}, Event);
dispatch ({C, N, Dispatchers}, Event) when C > N ->
  dispatch ({1, N, Dispatchers}, Event);
dispatch ({C, N, Dispatchers}, Event) ->
  mondemand_server_dispatcher:dispatch (element (C, Dispatchers), Event),
  {C + 1, N, Dispatchers}.

%%====================================================================
%% supervisor callbacks
%%====================================================================
init([Num, Dispatch]) ->
  mondemand_server_stats:create (events_received),
  mondemand_server_stats:create (events_dispatched),
  mondemand_server_stats:create (dispatcher_errors),

  { ok,
    {
      {one_for_one, 10, 10},
      [
        { Name,
          {mondemand_server_dispatcher, start_link, [Name, Dispatch] },
          permanent,
          2000,
          worker,
          [mondemand_server_dispatcher]
        }
        || Name
        <- names_list (Num)
      ]
    }
  }.

names_tuple (Num) ->
  list_to_tuple (names_list(Num)).
names_list (Num) ->
  [ name_atom (N) || N <- lists:seq (1,Num) ].

name_atom (N) ->
  list_to_atom (atom_to_list (mds_dispatcher)++"_"++integer_to_list (N)).

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% include test code here

-endif.
