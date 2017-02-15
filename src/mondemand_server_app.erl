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
  All = mondemand_server_config:all(),
  Apps = lists:append ( [ [ sasl, lwes, inets, mondemand, crypto, gproc,
                            afunix, asn1, public_key,
                            ssl, xmerl, compiler, syntax_tools, mochiweb,
                            webmachine ],
                          mondemand_server_config:applications_to_start(All),
                          [ mondemand_server ] ] ),
  [ ensure_started (App) || App <- Apps ].

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

start (_Type, _Args) ->
  All = mondemand_server_config:all(),
  % need to make sure that all the apps called out by plugins are started
  % prior to starting our supervisor, else we get errors
  [ ensure_started (App)
    || App <- mondemand_server_config:applications_to_start(All) ],

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
      ok;
    Other ->
      error_logger:error_msg ("Got ~p",[Other]),
      ok
  end.

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
