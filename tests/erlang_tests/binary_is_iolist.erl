-module(binary_is_iolist).

-export([start/0, id/1, try_size/1, try_to_bin/1]).

start() ->
    Size = try_size(id(<<"AtomVM">>)),
    Expected = <<"AtomVM">>,
    bool_to_int(try_to_bin(id(<<"AtomVM">>)) == Expected) * 1000 + Size.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.

id(I) when is_integer(I) or is_binary(I) or is_list(I) ->
    I.

try_to_bin(L) when is_list(L) or is_binary(L) ->
    try id(erlang:iolist_to_binary(id(L))) of
        Res -> Res
    catch
        error:badarg -> -1;
        _:_ -> -2
    end.

try_size(L) when is_list(L) or is_binary(L) ->
    try id(erlang:iolist_size(id(L))) of
        Res -> Res
    catch
        error:badarg -> -10;
        _:_ -> -20
    end.
