-module(test_atom_to_binary).

-export([start/0, f/1, h/1, compare_bin/2]).

start() ->
    compare_bin(f(hello_world), <<"hello_world">>) - h(15) - i(test_bin).

f(A) when is_binary(A) ->
    binaries_not_ok;
f(A) ->
    atom_to_binary(A, latin1).

g(A) ->
    atom_to_binary(A, not_good).

compare_bin(Bin1, Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1).

compare_bin(_Bin1, _Bin2, -1) ->
    1;
compare_bin(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    case binary:at(Bin2, Index) of
        B1 ->
            compare_bin(Bin1, Bin2, Index - 1);
        _Any ->
            0
    end.

h(A) ->
    try f(A) of
        _AnyVal -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1024
    end.

i(A) ->
    try g(A) of
        _AnyVal -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1024
    end.
