%
% This file is part of AtomVM.
%
% Copyright 2020-2021 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(alisp_stdlib).

-export([
    append/1,
    car/1,
    cdr/1,
    cons/1,
    last/1,
    list/1,
    tuple/1,
    '='/1,
    '+'/1,
    '-'/1,
    '*'/1,
    mapcar/1,
    'remove-if'/1,
    'remove-if-not'/1,
    binaryp/1,
    listp/1,
    numberp/1,
    integerp/1,
    floatp/1,
    pidp/1,
    refp/1,
    tuplep/1,
    identity/1,
    print/1
]).

append([L1, L2]) ->
    L1 ++ L2.

car([[H | _T]]) ->
    H.

cdr([[_H | T]]) ->
    T.

cons([H, T]) ->
    [H | T].

last([List]) when is_list(List) ->
    find_last(List).

find_last([]) ->
    [];
find_last([T]) ->
    T;
find_last([_H | T]) ->
    find_last(T).

list(List) when is_list(List) ->
    List.

tuple(List) when is_list(List) ->
    erlang:list_to_tuple(List).

mapcar([Fun, List]) ->
    lists:map(Fun, List).

'remove-if'([Fun, List]) ->
    BooleanFun = fun(X) ->
        not alisp:booleanize(Fun(X))
    end,
    lists:filter(BooleanFun, List).

'remove-if-not'([Fun, List]) ->
    BooleanFun = fun(X) ->
        alisp:booleanize(Fun(X))
    end,
    lists:filter(BooleanFun, List).

'='([_E]) ->
    true;
'='([E, E]) ->
    true;
'='([E, E | T]) ->
    '='(T);
'='([_E1, _E2 | _T]) ->
    false.

'+'(L) ->
    '+'(L, 0).

'+'([], Acc) ->
    Acc;
'+'([H | T], Acc) ->
    '+'(T, Acc + H).

'-'([E]) ->
    -E;
'-'([E1, E2 | T]) ->
    '-'(T, E1 - E2).

'-'([], Acc) ->
    Acc;
'-'([H | T], Acc) ->
    '-'(T, Acc - H).

'*'(L) ->
    '*'(L, 1).

'*'([], Acc) ->
    Acc;
'*'([H | T], Acc) ->
    '*'(T, Acc * H).

binaryp([Arg]) ->
    case Arg of
        B when is_binary(B) -> true;
        _ -> false
    end.

listp([Arg]) ->
    case Arg of
        L when is_list(L) -> true;
        _ -> false
    end.

numberp([Arg]) ->
    case Arg of
        N when is_number(N) -> true;
        _ -> false
    end.

integerp([Arg]) ->
    case Arg of
        I when is_integer(I) -> true;
        _ -> false
    end.

floatp([Arg]) ->
    case Arg of
        F when is_float(F) -> true;
        _ -> false
    end.

pidp([Arg]) ->
    case Arg of
        P when is_pid(P) -> true;
        _ -> false
    end.

refp([Arg]) ->
    case Arg of
        R when is_reference(R) -> true;
        _ -> false
    end.

tuplep([Arg]) ->
    case Arg of
        T when is_tuple(T) -> true;
        _ -> false
    end.

identity([Arg]) ->
    Arg.

print([Arg]) ->
    erlang:display(Arg).
