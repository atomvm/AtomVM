%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(test_binary_part).

-export([start/0, id/1, some_part/1, first_part/1, empty_part/1, compare_bin/2]).

start() ->
    B1 = some_part(id(<<"012Testxyz">>)),
    B2 = first_part(id(<<"First01234">>)),
    B3 = some_part(id(<<"XYZLast">>)),
    B4 = empty_part(id(<<"">>)),
    B5 = first_part(id(<<"01234">>)),
    B6 = not_fail2(1, -1),

    compare_bin(B1, <<"Test">>) + compare_bin(B2, <<"First">>) + compare_bin(B3, <<"Last">>) +
        byte_size(B4) + compare_bin(B5, <<"01234">>) + fail1(<<":(">>) + fail1({0, 1, 2, 3, 4}) +
        fail2(-1, 0) + fail2(0, -1) + compare_bin(<<":">>, B6) + fail2(-1, 2) + fail2(1, fail) +
        fail2(fail, 1).

empty_part(Bin1) ->
    binary:part(id(Bin1), 0, 0).

first_part(Bin1) ->
    binary:part(id(Bin1), 0, 5).

some_part(Bin1) ->
    binary:part(id(Bin1), 3, 4).

id(X) ->
    X.

fail1(X) ->
    try binary:part(X, 0, 3) of
        _Any -> 1000
    catch
        error:badarg -> 1;
        _:_ -> 2000
    end.

fail2(X, Y) ->
    try binary:part(<<":((">>, X, Y) of
        _Any -> 1000
    catch
        error:badarg -> 1;
        _:_ -> 2000
    end.

not_fail2(X, Y) ->
    try binary:part(<<":((">>, X, Y) of
        Any -> Any
    catch
        error:badarg -> 1;
        _:_ -> 2000
    end.

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
