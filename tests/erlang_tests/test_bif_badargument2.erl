-module(test_bif_badargument2).

-export([start/0, id/1, and2/2]).

start() ->
    not1(id(nil)) + and2(id(true), id(nil)) + or2(id(true), id(nil)) + xor2(id(false), id(nil)).

id(X) ->
    X.

not1(A) ->
    try not A of
        Result -> Result
    catch
        error:badarg -> -1;
        _:_ -> -2
    end.

and2(A, B) ->
    try A and B of
        Result -> Result
    catch
        error:badarg -> -4;
        _:_ -> -8
    end.

or2(A, B) ->
    try A or B of
        Result -> Result
    catch
        error:badarg -> -16;
        _:_ -> -32
    end.

xor2(A, B) ->
    try A xor B of
        Result -> Result
    catch
        error:badarg -> -64;
        _:_ -> -128
    end.
