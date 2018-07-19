-module (whereis_fail).
-export([start/0]).

start() ->
    result_to_int(whereis(this_is_missing)).

result_to_int(undefined) ->
    2;
result_to_int(_) ->
    3.
