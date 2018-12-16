-module (mondemand_backend_stats_handler).

%-export ([behaviour_info/1]).

-callback header() -> undefined | term().
-callback separator() -> undefined | term().
-callback format_stat (term(), term(), term(), term(), term(),
                       term(), term(), term(), term(), term()) -> {ok, term(), integer(), integer()}.
-callback footer() -> undefined | term().
-callback handle_response(term(), term()) -> ok.
%behaviour_info(callbacks) ->
%  [ {header, 0},
%    {separator, 0},
%    {format_stat, 10},
%    {footer, 0},
%    {handle_response, 2}
%  ];
%behaviour_info(_) ->
%  undefined.
