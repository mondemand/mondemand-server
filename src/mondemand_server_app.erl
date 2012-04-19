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
  [ ensure_started (App)
    || App <- [sasl, crypto, mochiweb, lwes,
               ibrowse, couchbeam, erlrrd,
               mondemand_server]].

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

