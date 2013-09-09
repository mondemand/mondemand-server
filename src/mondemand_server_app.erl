-module (mondemand_server_app).

-behaviour (application).

%% API
-export([start/0]).

%% application callbacks
-export ([start/2, stop/1]).

%%====================================================================
%% API functions
%%====================================================================
start () ->
  Apps = lists:append ( [ [ sasl, lwes ],
                          mondemand_server_config:applications_to_start(),
                          [ mondemand_server ] ] ),
  error_logger:info_msg ("Start ~p",[Apps]),
  [ ensure_started (App) || App <- Apps ].

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

start (_Type, _Args) ->
  case mondemand_server_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop (_State) ->
  ok.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(HAVE_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).

-endif.
