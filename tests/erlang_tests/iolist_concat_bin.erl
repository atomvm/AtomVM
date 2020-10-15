-module(iolist_concat_bin).

-export([start/0, id/1]).

start() ->
    L1 = [id(<<"Ato">>), id($m) | id(<<"VM">>)],
    L2 = [id(<<"Ato">>), {id($m)} | id(<<"VM">>)],
    Size = erlang:iolist_size(L1),
    Expected = <<"AtomVM">>,
    bool_to_int(try_to_bin(L1) == Expected) * 1000 + Size +
        try_to_bin(L2) * 10000 + try_to_bin({L1}) * 20000 +
        bool_to_int(try_to_bin([]) == <<"">>) * 40000 + erlang:iolist_size(id([])).

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.

id(I) when is_integer(I) or is_binary(I) or is_list(I) ->
    I.

try_to_bin(L) when is_list(L) or is_tuple(L) ->
    try erlang:iolist_to_binary(L) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> -2
    end.
