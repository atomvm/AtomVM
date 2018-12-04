-module(test_badarith2).
-export([start/0, id/1, add2/2, mul2/2, div2/2, rem2/2, abs1/1, neg1/1]).

start() ->
    add2(id(nan), id(8)) + sub2(id(nan), id(8)) + mul2(id(-1), id(nan)) + div2(id(nan), id(5))
    + rem2(id([]), id(2)) + abs1(id([])) + neg1(id([])) + div2(id([]), id(0)) * 256 + rem2(id([]), id(0)) * 256.

id(X) ->
    X.

add2(A, B) ->
    try A + B of
        Result -> Result
    catch
        error:badarith -> -1;
        _:_ -> -2
    end.

sub2(A, B) ->
    try A - B of
        Result -> Result
    catch
        error:badarith -> -4;
        _:_ -> -8
    end.

mul2(A, B) ->
    try A * B of
        Result -> Result
    catch
        error:badarith -> -16;
        _:_ -> -32
    end.

div2(A, B) ->
    try A div B of
        Result -> Result
    catch
        error:badarith -> -64;
        _:_ -> -128
    end.

rem2(A, B) ->
    try A rem B of
        Result -> Result
    catch
        error:badarith -> -256;
        _:_ -> -512
    end.

abs1(A) ->
    try abs(A) of
        Result -> Result
    catch
        error:badarg -> -1024;
        _:_ -> -2048
    end.

neg1(A) ->
    try -A of
        Result -> Result
    catch
        error:badarith -> -4096;
        _:_ -> -8192
    end.
