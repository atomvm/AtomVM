%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(test_bs_int_unaligned).

-export([start/0]).

start() ->
    %% two aligned bytes
    ok = test_pack_unpack(3, 5, 1, 7, false),
    ok = test_pack_unpack(5, 3, 7, 1, false),
    %% still adds up to 2 bytes (16 bits)
    ok = test_pack_unpack(3, 1, 5, 7, false),
    ok = test_pack_unpack(5, 7, 3, 1, false),
    %% expressions that traverse a byte boundary
    ok = test_pack_unpack(13, 3, 1, 7, false),
    ok = test_pack_unpack(3, 13, 1, 7, false),
    %% expressions not aligned on 8 bit boundary (expect failure with AtomVM)
    ExpectFailure =
        case erlang:system_info(machine) of
            "BEAM" -> false;
            _ -> true
        end,
    ok = test_pack_unpack(1, 1, 1, 1, ExpectFailure),
    ok = test_pack_unpack(3, 13, 1, 1, ExpectFailure),
    0.

test_pack_unpack(ALen, BLen, CLen, DLen, ExpectFailure) ->
    try_test_pack_unpack(
        {random(pow(2, ALen)), ALen},
        {random(pow(2, BLen)), BLen},
        {random(pow(2, CLen)), CLen},
        {random(pow(2, DLen)), DLen},
        ExpectFailure
    ).

try_test_pack_unpack({A, ALen}, {B, BLen}, {C, CLen}, {D, DLen}, ExpectFailure) ->
    try
        Bin = <<A:ALen, B:BLen, C:CLen, D:DLen>>,
        <<A1:ALen, B1:BLen, C1:CLen, D1:DLen>> = id(Bin),
        ok = check(A, A1),
        ok = check(B, B1),
        ok = check(C, C1),
        ok = check(D, D1),
        if
            ExpectFailure -> expected_failure;
            true -> ok
        end
    catch
        _:E ->
            if
                ExpectFailure -> ok;
                true -> {expected_ok, E}
            end
    end.

check(A, A) ->
    ok;
check(A, B) ->
    throw({expected_equal, A, B}).

random(N) ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    ((MegaSecs bxor Secs bxor MicroSecs) rem N) + 1.

pow(B, N) ->
    pow(B, N, 1).

pow(_B, 1, Accum) ->
    Accum;
pow(B, N, Accum) ->
    pow(B, N - 1, B * Accum).

id(X) ->
    X.
