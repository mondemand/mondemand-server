-module (mondemand_backend_performance_collate).

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

%% mondemand_server_backend callbacks
-export ([ start_link/1,
           process/1,
           required_apps/0,
           type/0
         ]).

%% supervisor callbacks
-export ([ init/1 ]).

-define (POOL, mdbep_collate_pool).
-record (state, { }).

%%====================================================================
%% mondemand_server_backend callbacks
%%====================================================================
start_link (Config) ->
  supervisor:start_link ( { local, ?MODULE }, ?MODULE, [Config]).

process (Event) ->
  mondemand_backend_worker_pool_sup:process (?POOL, Event).

required_apps () ->
  [ ].

type () ->
  supervisor.

%%====================================================================
% supervisor callbacks
%%====================================================================
init ([Config]) ->
  % default to one process per scheduler
  Number =
    proplists:get_value (number, Config, erlang:system_info(schedulers)),
  AgeSeconds =
    proplists:get_value (max_age, Config, 300),
  FlushIntervalSeconds =
    proplists:get_value (flush_interval, Config, 10),

  { ok,
   {
      {one_for_one, 10, 10},
      [
        { mondemand_backend_performance_db,
          { mondemand_backend_performance_db, start_link,
            [AgeSeconds, FlushIntervalSeconds] },
          permanent,
          2000,
          worker,
          [ mondemand_backend_performance_db ]
        },
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
create (_Config) ->
  {ok, #state { }}.

connected (_State) ->
  true. % always connected

connect (State) ->
  {ok, State}.

send (State = #state { }, Data) ->
  % use the timestamp of the event to see if it might be time to check
  % for new indices
  Timestamp = mondemand_event:receipt_time (Data),

  % convert messages to json
  PerfMsg = mondemand_event:msg (Data),
  Id = mondemand_perfmsg:id(PerfMsg),
  Context = mondemand_server_util:construct_context_string(
              lists:keysort(1,mondemand_perfmsg:context (PerfMsg)),
              ":","\t"),
  Lines =
    list_to_binary(
      [
        begin
          Start = mondemand_perfmsg:timing_start_time (L),
          End = mondemand_perfmsg:timing_end_time (L),
          [
            io_lib:format ("~b\t"
                           "~s\t"
                           "~s\t"
                           "~b\t"
                           "~b\t"
                           "~b\t"
                           "~s\t",
                           [ Timestamp,
                             mondemand_perfmsg:caller_label (PerfMsg),
                             mondemand_perfmsg:timing_label (L),
                             Start,
                             End,
                             (End - Start),
                             Id
                           ]),
            Context,
            [ <<"\tsource_ip:">>,
              lwes_util:ip2bin(mondemand_event:sender_ip (Data)),
              "\n" ]
          ]
        end
        || L <- mondemand_perfmsg:timings (PerfMsg)
      ]
    ),

  mondemand_backend_performance_db:add_trace (Id, Timestamp, Lines),
  { ok, State }.

destroy (_) ->
  ok.
