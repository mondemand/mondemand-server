-module (mondemand_backend_stats_aggregator_db).

-behaviour (gen_server).

% min/max calculations using ets:update_counter/2

%% API
-export ([ start_link/1,  % ([Minute])
           add_val/3,     % (Timestamp, Key, Value)
           get_val/2,     % (Minute, Key)
           current/1,     % (Key)
           current_tab/0, % ()
           clear/1,       % (Minute)
           dump/1,        % (Minute)
           stats/1,       % (Minute)
           minute_proc/1, % (Minute)
           prev_proc/1,   % (Minute)
           next_proc/1    % (Minute)
         ]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, {minute, last}).
-record (entry, {key,
                 sum = 0,
                 count = 0,
                 min = infinity,
                 max = 0}).

start_link (Minute) ->
  gen_server:start_link ( { local, minute_proc (Minute) },
                          ?MODULE, [Minute], []).

add_val (Timestamp, Key, Value) ->
  DateTime = {{_,_,_},{_,Minute,_}}
    = mondemand_server_util:now_to_mdyhms (Timestamp),
  gen_server:cast (minute_proc (Minute), {add, DateTime, Key, Value}).

get_val (Minute, Key) ->
  case ets:lookup (minute_tab (Minute), Key) of
    [V] -> V;
    [] -> undefined
  end.

stats (Minute) ->
  io:format ("~-21s ~-35s ~-10s ~-10s ~-10s ~-10s ~-10s~n",
             ["prog_id", "key", "sum", "count", "min", "avg", "max"]),
  io:format ("~-21s ~-35s ~-10s ~-10s ~-10s ~-10s ~-10s~n",
             ["---------------------",
              "-----------------------------------",
              "----------",
              "----------",
              "----------",
              "----------",
              "----------"]),
  [
    begin
      io:format ("~-21s ~-35s ~-10b ~-10b ~-10b ~-10b ~-10b~n",
                 [ProgId, Key, Sum, Count, Min,
                   case Count =/= 0 of
                     true -> trunc (Sum / Count);
                     false -> 0
                   end,
                   Max ]),
      lists:foreach (fun ({CKey, CValue}) ->
                       io:format (" {~s, ~s}~n", [CKey, CValue])
                     end, Context)

    end
    || #entry { key = {ProgId, Key, Context},
                sum = Sum,
                count = Count,
                min = Min,
                max = Max }
    <- ets:tab2list (minute_tab (Minute))
  ],
  ok.

dump (Minute) ->
  gen_server:cast (minute_proc (Minute), {dump}).

clear (Minute) ->
  gen_server:cast (minute_proc (Minute), {clear}).

current (Key) ->
  get_val (current_tab(), Key).

current_tab () ->
  {_, Minute, _} = time (),
  minute_tab (Minute).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([Minute]) ->

  ets:new ( minute_tab (Minute),
            [ public,
              {write_concurrency, false},
              {read_concurrency, true},
              named_table,
              {keypos, 2}
            ]),

  {ok, #state { minute = Minute }}.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast ({dump}, State = #state { last = undefined }) ->
  error_logger:warning_msg ("dump called when no data has been set"),
  { noreply, State };
handle_cast ({dump}, State = #state { last = {Date,{Hour, Minute,_}} }) ->
  DateTime = {Date, {Hour, Minute, 0}},
  EpochSecs = mondemand_server_util:mdyhms_to_epoch_secs (DateTime),
  NowTimestamp = mondemand_server_util:epoch_secs_to_now (EpochSecs),
  error_logger:info_msg ("~s : perform dump for ~p : ~p",[
      mondemand_server_util:mdyhms_to_log_string (DateTime),
      EpochSecs,
      NowTimestamp]),
  { noreply, State };
handle_cast ({add, DateTime = {_,{_,Minute,_}}, Key, Value}, State) ->
  Entry =
    case ets:lookup (current_tab (), Key) of
      [V] -> V;
      [] -> #entry {}
    end,
  % upon first getting a value for this bin, clear the next bin
%  case Entry#entry.sum =:= 0 of
%    true ->

  NewEntry =
    #entry { key = Key,
             sum = Entry#entry.sum + Value,
             count = Entry#entry.count + 1,
             min = min (Value, Entry#entry.min),
             max = max (Value, Entry#entry.max) },
  ets:insert (minute_tab (Minute), NewEntry),
  { noreply, State#state { last = DateTime } };
handle_cast ({clear}, State = #state { minute = Minute }) ->
  ets:delete_all_objects (minute_tab (Minute)),
  { noreply, State#state { last = undefined } };
handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p ~p~n",[?MODULE, Request, State]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  { ok, State }.

%%====================================================================
%% mondemand_backend_worker callbacks
%%====================================================================
prev_proc (Min) -> minute_proc (prev_min (Min)).
prev_tab (Min) -> minute_tab (prev_min (Min)).
prev_min (Min) ->
  case Min - 1 of
    -1 -> 59;
    Previous -> Previous
  end.

next_proc (Min) -> minute_proc (next_min (Min)).
next_tab (Min) -> minute_tab (next_min (Min)).
next_min (Min) ->
  case Min + 1 of
    60 -> 0;
    Next -> Next
  end.

minute_tab (0)  -> md_min_00;
minute_tab (1)  -> md_min_01;
minute_tab (2)  -> md_min_02;
minute_tab (3)  -> md_min_03;
minute_tab (4)  -> md_min_04;
minute_tab (5)  -> md_min_05;
minute_tab (6)  -> md_min_06;
minute_tab (7)  -> md_min_07;
minute_tab (8)  -> md_min_08;
minute_tab (9)  -> md_min_09;
minute_tab (10) -> md_min_10;
minute_tab (11) -> md_min_11;
minute_tab (12) -> md_min_12;
minute_tab (13) -> md_min_13;
minute_tab (14) -> md_min_14;
minute_tab (15) -> md_min_15;
minute_tab (16) -> md_min_16;
minute_tab (17) -> md_min_17;
minute_tab (18) -> md_min_18;
minute_tab (19) -> md_min_19;
minute_tab (20) -> md_min_20;
minute_tab (21) -> md_min_21;
minute_tab (22) -> md_min_22;
minute_tab (23) -> md_min_23;
minute_tab (24) -> md_min_24;
minute_tab (25) -> md_min_25;
minute_tab (26) -> md_min_26;
minute_tab (27) -> md_min_27;
minute_tab (28) -> md_min_28;
minute_tab (29) -> md_min_29;
minute_tab (30) -> md_min_30;
minute_tab (31) -> md_min_31;
minute_tab (32) -> md_min_32;
minute_tab (33) -> md_min_33;
minute_tab (34) -> md_min_34;
minute_tab (35) -> md_min_35;
minute_tab (36) -> md_min_36;
minute_tab (37) -> md_min_37;
minute_tab (38) -> md_min_38;
minute_tab (39) -> md_min_39;
minute_tab (40) -> md_min_40;
minute_tab (41) -> md_min_41;
minute_tab (42) -> md_min_42;
minute_tab (43) -> md_min_43;
minute_tab (44) -> md_min_44;
minute_tab (45) -> md_min_45;
minute_tab (46) -> md_min_46;
minute_tab (47) -> md_min_47;
minute_tab (48) -> md_min_48;
minute_tab (49) -> md_min_49;
minute_tab (50) -> md_min_50;
minute_tab (51) -> md_min_51;
minute_tab (52) -> md_min_52;
minute_tab (53) -> md_min_53;
minute_tab (54) -> md_min_54;
minute_tab (55) -> md_min_55;
minute_tab (56) -> md_min_56;
minute_tab (57) -> md_min_57;
minute_tab (58) -> md_min_58;
minute_tab (59) -> md_min_59.

minute_proc (0)  -> md_min_00;
minute_proc (1)  -> md_min_01;
minute_proc (2)  -> md_min_02;
minute_proc (3)  -> md_min_03;
minute_proc (4)  -> md_min_04;
minute_proc (5)  -> md_min_05;
minute_proc (6)  -> md_min_06;
minute_proc (7)  -> md_min_07;
minute_proc (8)  -> md_min_08;
minute_proc (9)  -> md_min_09;
minute_proc (10) -> md_min_10;
minute_proc (11) -> md_min_11;
minute_proc (12) -> md_min_12;
minute_proc (13) -> md_min_13;
minute_proc (14) -> md_min_14;
minute_proc (15) -> md_min_15;
minute_proc (16) -> md_min_16;
minute_proc (17) -> md_min_17;
minute_proc (18) -> md_min_18;
minute_proc (19) -> md_min_19;
minute_proc (20) -> md_min_20;
minute_proc (21) -> md_min_21;
minute_proc (22) -> md_min_22;
minute_proc (23) -> md_min_23;
minute_proc (24) -> md_min_24;
minute_proc (25) -> md_min_25;
minute_proc (26) -> md_min_26;
minute_proc (27) -> md_min_27;
minute_proc (28) -> md_min_28;
minute_proc (29) -> md_min_29;
minute_proc (30) -> md_min_30;
minute_proc (31) -> md_min_31;
minute_proc (32) -> md_min_32;
minute_proc (33) -> md_min_33;
minute_proc (34) -> md_min_34;
minute_proc (35) -> md_min_35;
minute_proc (36) -> md_min_36;
minute_proc (37) -> md_min_37;
minute_proc (38) -> md_min_38;
minute_proc (39) -> md_min_39;
minute_proc (40) -> md_min_40;
minute_proc (41) -> md_min_41;
minute_proc (42) -> md_min_42;
minute_proc (43) -> md_min_43;
minute_proc (44) -> md_min_44;
minute_proc (45) -> md_min_45;
minute_proc (46) -> md_min_46;
minute_proc (47) -> md_min_47;
minute_proc (48) -> md_min_48;
minute_proc (49) -> md_min_49;
minute_proc (50) -> md_min_50;
minute_proc (51) -> md_min_51;
minute_proc (52) -> md_min_52;
minute_proc (53) -> md_min_53;
minute_proc (54) -> md_min_54;
minute_proc (55) -> md_min_55;
minute_proc (56) -> md_min_56;
minute_proc (57) -> md_min_57;
minute_proc (58) -> md_min_58;
minute_proc (59) -> md_min_59.
