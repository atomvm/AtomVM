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

-export([start/0, some_calculation/2, concat_integers/2, compare_list/2, id/1]).

start() ->
    ok = test_decimal(),
    ok = test_bases(),
    0.

test_decimal() ->
    NewList =
        concat_integers(some_calculation(100, 1), some_calculation(100, hello)) ++
            concat_integers(a, []),
    1 = compare_list(NewList, "6,-1"),
    ok.

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

test_bases() ->
    "0" = integer_to_list(?MODULE:id(0)),
    "0" = integer_to_list(?MODULE:id(0), 10),
    "-1" = integer_to_list(?MODULE:id(-1)),
    "-1010" = integer_to_list(?MODULE:id(-10), 2),
    "1010" = integer_to_list(?MODULE:id(10), 2),
    "A" = integer_to_list(?MODULE:id(10), 16),
    "123456789ABCDEF0" = integer_to_list(?MODULE:id(16#123456789ABCDEF0), 16),
    "7FFFFFFFFFFFFFFF" = integer_to_list(?MODULE:id(16#7FFFFFFFFFFFFFFF), 16),
    "-8000000000000000" = integer_to_list(?MODULE:id(-16#8000000000000000), 16),
    "A" = integer_to_list(?MODULE:id(10), 36),
    assert_badarg(fun() -> integer_to_list(?MODULE:id(10), 1) end),
    assert_badarg(fun() -> integer_to_list(?MODULE:id(10), 0) end),
    assert_badarg(fun() -> integer_to_list(?MODULE:id(10), -1) end),
    assert_badarg(fun() -> integer_to_list(?MODULE:id(10), 37) end),
    "100111000000110010111011010010" = integer_to_list(?MODULE:id(654520018), 2),
    "1200121121001200011" = integer_to_list(?MODULE:id(654520018), 3),
    "213000302323102" = integer_to_list(?MODULE:id(654520018), 4),
    "2320024120033" = integer_to_list(?MODULE:id(654520018), 5),
    "144540345134" = integer_to_list(?MODULE:id(654520018), 6),
    "22135220425" = integer_to_list(?MODULE:id(654520018), 7),
    "4700627322" = integer_to_list(?MODULE:id(654520018), 8),
    "1617531604" = integer_to_list(?MODULE:id(654520018), 9),
    "654520018" = integer_to_list(?MODULE:id(654520018), 10),
    "306506639" = integer_to_list(?MODULE:id(654520018), 11),
    "1632451AA" = integer_to_list(?MODULE:id(654520018), 12),
    "A57A7469" = integer_to_list(?MODULE:id(654520018), 13),
    "62CD99BC" = integer_to_list(?MODULE:id(654520018), 14),
    "3C6DBCCD" = integer_to_list(?MODULE:id(654520018), 15),
    "27032ED2" = integer_to_list(?MODULE:id(654520018), 16),
    "1A1GA129" = integer_to_list(?MODULE:id(654520018), 17),
    "1146H194" = integer_to_list(?MODULE:id(654520018), 18),
    "DH66IG0" = integer_to_list(?MODULE:id(654520018), 19),
    "A4AF00I" = integer_to_list(?MODULE:id(654520018), 20),
    "7D59I7J" = integer_to_list(?MODULE:id(654520018), 21),
    "5H00I1K" = integer_to_list(?MODULE:id(654520018), 22),
    "49FKFL2" = integer_to_list(?MODULE:id(654520018), 23),
    "3A4IFBA" = integer_to_list(?MODULE:id(654520018), 24),
    "2H0E70I" = integer_to_list(?MODULE:id(654520018), 25),
    "2327AMM" = integer_to_list(?MODULE:id(654520018), 26),
    "1IGG1I4" = integer_to_list(?MODULE:id(654520018), 27),
    "1A0NQQQ" = integer_to_list(?MODULE:id(654520018), 28),
    "12QBJSN" = integer_to_list(?MODULE:id(654520018), 29),
    "QS1EDS" = integer_to_list(?MODULE:id(654520018), 30),
    "MQMC6U" = integer_to_list(?MODULE:id(654520018), 31),
    "JG6BMI" = integer_to_list(?MODULE:id(654520018), 32),
    "GNTWFV" = integer_to_list(?MODULE:id(654520018), 33),
    "EDQPQQ" = integer_to_list(?MODULE:id(654520018), 34),
    "CG5R1X" = integer_to_list(?MODULE:id(654520018), 35),
    "ATOMVM" = integer_to_list(?MODULE:id(654520018), 36),
    ok.

assert_badarg(F) ->
    ok =
        try
            F(),
            fail_no_ex
        catch
            error:badarg -> ok
        end.

id(I) -> I.
