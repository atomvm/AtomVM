-module(external_proplist_test).

-export([start/0, get_value/2]).

start() ->
    APropList = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],
    get_value(APropList, c).

get_value([], _N) ->
    not_found;
get_value([{PN, PV} | _Tail], N) when PN == N ->
    PV;
get_value([_Head | Tail], N) ->
    get_value(Tail, N).
