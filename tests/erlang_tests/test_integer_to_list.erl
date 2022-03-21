%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(test_integer_to_list).

-export([start/0, some_calculation/2, concat_integers/2, compare_list/2]).

start() ->
    NewList =
        concat_integers(some_calculation(100, 1), some_calculation(100, hello)) ++
            concat_integers(a, []),
    compare_list(NewList, "6,-1").

some_calculation(N, A) when is_integer(N) and is_integer(A) ->
    N div 20 + A;
some_calculation(_N, _A) ->
    -1.

concat_integers(A, B) ->
    ListA =
        try integer_to_list(A) of
            ListValue -> ListValue
        catch
            error:badarg -> "";
            _:_ -> "error"
        end,
    ListB =
        try integer_to_list(B) of
            AListValue -> "," ++ AListValue
        catch
            error:badarg -> "";
            _:_ -> "error"
        end,
    ListA ++ ListB.

compare_list([], []) ->
    1;
compare_list([H_A | T_A], [H_B | T_B]) when H_A == H_B ->
    compare_list(T_A, T_B);
compare_list(_A, _B) ->
    0.
