-module (mondemand_server_time_db_sup).
-behaviour (supervisor).

%% API
-export ([
           start_link/0,
           add_value/3
         ]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% API
%%====================================================================
start_link () ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

add_value (Timestamp, Key, Value) ->
  EpochMinute = mondemand_server_util:now_to_epoch_minutes (Timestamp),
  ChildPid =
    case mondemand_server_time_db:find (EpochMinute) of
      undefined -> start_minute (EpochMinute);
      Pid -> Pid
    end,
  % Now actually add the value to the child
  mondemand_server_time_db:add_value (ChildPid, Timestamp, Key, Value).

start_minute (EpochMinute) ->
  case supervisor:start_child (?MODULE, [EpochMinute]) of
    {ok, Pid} -> Pid;
    {error, {already_started, Pid}} -> Pid
  end.

%%====================================================================
%% supervisor callbacks
%%====================================================================
init ([]) ->
  { ok,
    {
      {simple_one_for_one, 10, 10},
      [ { mondemand_server_time_db,
          { mondemand_server_time_db, start_link, []},
          transient,
          2000,
          worker,
          [ mondemand_server_time_db ]
        }
      ]
    }
  }.

%%====================================================================
%% Test functions
%====================================================================
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

-endif.
