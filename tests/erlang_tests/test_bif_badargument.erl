-module(test_bif_badargument).

-export([start/0, id/1, byte_size1/1]).

start() ->
    byte_size1(id([])) + length1(id(not_a_list)) + hd1(id(not_a_list)) + tl1(id(not_a_list)) +
        element2(id(2), id([1, 2, 3])) + tuple_size1(id([1, 2, 3])) +
        element2(id(nan), id({1, 2, 3})) * 16 +
        element2(id(3), id({1, 2})) * 64 + element2(-1, {1, 2}) * 256 + element2(0, {}) * 1024 +
        hd1(id([])) * 65536 + tl1(id([])) * 65536.

id(X) ->
    X.

byte_size1(A) ->
    try byte_size(A) of
        Result -> Result
    catch
        error:badarg -> -1;
        _:_ -> -2
    end.

length1(A) ->
    try length(A) of
        Result -> Result
    catch
        error:badarg -> -4;
        _:_ -> -8
    end.

hd1(A) ->
    try hd(A) of
        Result -> Result
    catch
        error:badarg -> -16;
        _:_ -> -32
    end.

tl1(A) ->
    try tl(A) of
        Result -> Result
    catch
        error:badarg -> -64;
        _:_ -> -128
    end.

element2(A, B) ->
    try element(A, B) of
        Result -> Result
    catch
        error:badarg -> -256;
        _:_ -> -512
    end.

tuple_size1(A) ->
    try tuple_size(A) of
        Result -> Result
    catch
        error:badarg -> -1024;
        _:_ -> -2048
    end.
