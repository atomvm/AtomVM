%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_is_integer_3).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 29).
-define(OTP29_OR_LATER, true).
-endif.
-endif.

-ifdef(OTP29_OR_LATER).

-export([
    start/0,
    id/1,
    is_integer_helper/3,
    is_0_100/1,
    is_uint32/1,
    is_uint64/1,
    is_atomvm_integer/1,
    classify/1
]).

start() ->
    ok = test_small_int_interval(),
    ok = test_swapped_interval(),
    ok = test_single_value_interval(),
    ok = test_known_bounds(),
    ok = test_bigint(),
    ok = test_errors(),
    0.

test_small_int_interval() ->
    false = is_integer(?MODULE:id(-16#FFFFFFFFFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(-16#FFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(-20), ?MODULE:id(0), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(-1), ?MODULE:id(0), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(0), ?MODULE:id(0), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(5), ?MODULE:id(0), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(10), ?MODULE:id(0), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(20), ?MODULE:id(0), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(16#FFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(16#FFFFFFFFFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(not_a_number), ?MODULE:id(0), ?MODULE:id(10)),

    false = ?MODULE:is_integer_helper(
        ?MODULE:id(-16#FFFFFFFFFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)
    ),
    false = ?MODULE:is_integer_helper(?MODULE:id(-16#FFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(-20), ?MODULE:id(0), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(-1), ?MODULE:id(0), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(0), ?MODULE:id(0), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(5), ?MODULE:id(0), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(10), ?MODULE:id(0), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(20), ?MODULE:id(0), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(16#FFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(
        ?MODULE:id(16#FFFFFFFFFFFFFFF), ?MODULE:id(0), ?MODULE:id(10)
    ),
    false = ?MODULE:is_integer_helper(
        ?MODULE:id(<<"not a number">>), ?MODULE:id(0), ?MODULE:id(10)
    ),

    false = is_integer(?MODULE:id(-16#FFFFFFFFFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(-16#FFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(-20), ?MODULE:id(-10), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(-10), ?MODULE:id(-10), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(-1), ?MODULE:id(-10), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(0), ?MODULE:id(-10), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(5), ?MODULE:id(-10), ?MODULE:id(10)),
    true = is_integer(?MODULE:id(10), ?MODULE:id(-10), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(20), ?MODULE:id(-10), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(16#FFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(16#FFFFFFFFFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)),
    false = is_integer(?MODULE:id(5.0), ?MODULE:id(-10), ?MODULE:id(10)),

    false = ?MODULE:is_integer_helper(
        ?MODULE:id(-16#FFFFFFFFFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)
    ),
    false = ?MODULE:is_integer_helper(?MODULE:id(-16#FFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(-20), ?MODULE:id(-10), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(-10), ?MODULE:id(-10), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(-1), ?MODULE:id(-10), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(0), ?MODULE:id(-10), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(5), ?MODULE:id(-10), ?MODULE:id(10)),
    true = ?MODULE:is_integer_helper(?MODULE:id(10), ?MODULE:id(-10), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(20), ?MODULE:id(-10), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(16#FFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)),
    false = ?MODULE:is_integer_helper(
        ?MODULE:id(16#FFFFFFFFFFFFFFF), ?MODULE:id(-10), ?MODULE:id(10)
    ),
    false = ?MODULE:is_integer_helper(?MODULE:id(0.0), ?MODULE:id(-10), ?MODULE:id(10)),

    ok.

test_swapped_interval() ->
    false = is_integer(?MODULE:id(-20), ?MODULE:id(10), ?MODULE:id(-10)),
    false = is_integer(?MODULE:id(-11), ?MODULE:id(10), ?MODULE:id(-10)),
    false = is_integer(?MODULE:id(-10), ?MODULE:id(10), ?MODULE:id(-10)),
    false = is_integer(?MODULE:id(0), ?MODULE:id(10), ?MODULE:id(-10)),
    false = is_integer(?MODULE:id(10), ?MODULE:id(10), ?MODULE:id(-10)),
    false = is_integer(?MODULE:id(11), ?MODULE:id(10), ?MODULE:id(-10)),
    false = is_integer(?MODULE:id(20), ?MODULE:id(10), ?MODULE:id(-10)),

    false = ?MODULE:is_integer_helper(?MODULE:id(-20), ?MODULE:id(10), ?MODULE:id(-10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(-11), ?MODULE:id(10), ?MODULE:id(-10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(-10), ?MODULE:id(10), ?MODULE:id(-10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(0), ?MODULE:id(10), ?MODULE:id(-10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(10), ?MODULE:id(10), ?MODULE:id(-10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(11), ?MODULE:id(10), ?MODULE:id(-10)),
    false = ?MODULE:is_integer_helper(?MODULE:id(20), ?MODULE:id(10), ?MODULE:id(-10)),

    ok.

test_single_value_interval() ->
    true = is_integer(?MODULE:id(0), ?MODULE:id(0), ?MODULE:id(0)),
    true = is_integer(?MODULE:id(1), ?MODULE:id(1), ?MODULE:id(1)),
    true = is_integer(?MODULE:id(-1), ?MODULE:id(-1), ?MODULE:id(-1)),
    true = is_integer(?MODULE:id(16#FFFFFFFF), ?MODULE:id(16#FFFFFFFF), ?MODULE:id(16#FFFFFFFF)),
    true = is_integer(
        ?MODULE:id(16#FFFFFFFFFFFFFFF),
        ?MODULE:id(16#FFFFFFFFFFFFFFF),
        ?MODULE:id(16#FFFFFFFFFFFFFFF)
    ),

    true = ?MODULE:is_integer_helper(?MODULE:id(0), ?MODULE:id(0), ?MODULE:id(0)),
    true = ?MODULE:is_integer_helper(?MODULE:id(1), ?MODULE:id(1), ?MODULE:id(1)),
    true = ?MODULE:is_integer_helper(?MODULE:id(-1), ?MODULE:id(-1), ?MODULE:id(-1)),
    true = ?MODULE:is_integer_helper(
        ?MODULE:id(16#FFFFFFFF), ?MODULE:id(16#FFFFFFFF), ?MODULE:id(16#FFFFFFFF)
    ),
    true = ?MODULE:is_integer_helper(
        ?MODULE:id(16#FFFFFFFFFFFFFFF),
        ?MODULE:id(16#FFFFFFFFFFFFFFF),
        ?MODULE:id(16#FFFFFFFFFFFFFFF)
    ),
    ok.

test_known_bounds() ->
    false = ?MODULE:is_0_100(?MODULE:id(-16#8000000000000000)),
    false = ?MODULE:is_0_100(?MODULE:id(-16#FFFFCAFECAFE)),
    false = ?MODULE:is_0_100(?MODULE:id(-16#CAFECAFE)),
    false = ?MODULE:is_0_100(?MODULE:id(-100)),
    false = ?MODULE:is_0_100(?MODULE:id(-1)),
    true = ?MODULE:is_0_100(?MODULE:id(0)),
    true = ?MODULE:is_0_100(?MODULE:id(1)),
    true = ?MODULE:is_0_100(?MODULE:id(50)),
    true = ?MODULE:is_0_100(?MODULE:id(99)),
    true = ?MODULE:is_0_100(?MODULE:id(100)),
    false = ?MODULE:is_0_100(?MODULE:id(101)),
    false = ?MODULE:is_0_100(?MODULE:id(500)),
    false = ?MODULE:is_0_100(?MODULE:id(16#CAFECAFE)),
    false = ?MODULE:is_0_100(?MODULE:id(16#FFFFCAFECAFE)),
    false = ?MODULE:is_0_100(?MODULE:id(16#7FFFFFFFFFFFFFFF)),
    false = ?MODULE:is_0_100(?MODULE:id(5.0)),
    false = ?MODULE:is_0_100(?MODULE:id(not_a_number)),

    false = ?MODULE:is_uint32(?MODULE:id(-16#CAFECAFE)),
    false = ?MODULE:is_uint32(?MODULE:id(-42)),
    true = ?MODULE:is_uint32(?MODULE:id(0)),
    true = ?MODULE:is_uint32(?MODULE:id(42)),
    true = ?MODULE:is_uint32(?MODULE:id(16#CAFECAFE)),
    false = ?MODULE:is_uint32(?MODULE:id(16#CAFECAFEFFF)),
    false = ?MODULE:is_uint64(?MODULE:id(16#10000000000000000)),
    false = ?MODULE:is_uint32(?MODULE:id(42.0)),
    false = ?MODULE:is_uint32(?MODULE:id(not_a_number)),

    false = ?MODULE:is_uint64(?MODULE:id(-16#FFFFFFFFFFFFFFFF)),
    false = ?MODULE:is_uint64(?MODULE:id(-16#CAFECAFE)),
    false = ?MODULE:is_uint64(?MODULE:id(-42)),
    true = ?MODULE:is_uint64(?MODULE:id(0)),
    true = ?MODULE:is_uint64(?MODULE:id(42)),
    true = ?MODULE:is_uint64(?MODULE:id(16#CAFECAFE)),
    true = ?MODULE:is_uint64(?MODULE:id(16#CAFECAFEFFF)),
    true = ?MODULE:is_uint64(?MODULE:id(16#FFFFFFFFFFFFFFFF)),
    false = ?MODULE:is_uint64(?MODULE:id(16#10000000000000000)),
    false = ?MODULE:is_uint64(?MODULE:id(0.0)),
    false = ?MODULE:is_uint64(?MODULE:id(<<"not_a_number">>)),

    true = ?MODULE:is_atomvm_integer(?MODULE:id(-16#FFFFFFFFFFFFFFFF)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(-16#CAFECAFE)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(-42)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(0)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(42)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(16#CAFECAFE)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(16#CAFECAFEFFF)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(16#FFFFFFFFFFFFFFFF)),
    true = ?MODULE:is_atomvm_integer(?MODULE:id(16#10000000000000000)),
    false = ?MODULE:is_atomvm_integer(?MODULE:id(-1.0)),
    false = ?MODULE:is_atomvm_integer(?MODULE:id(<<"not_a_number">>)),

    int8 = ?MODULE:classify(?MODULE:id(0)),
    int8 = ?MODULE:classify(?MODULE:id(42)),
    int8 = ?MODULE:classify(?MODULE:id(-100)),
    uint8 = ?MODULE:classify(?MODULE:id(255)),
    int16 = ?MODULE:classify(?MODULE:id(-30000)),
    uint16 = ?MODULE:classify(?MODULE:id(60000)),
    uint32 = ?MODULE:classify(?MODULE:id(16#FFFFFFFF)),
    int32 = ?MODULE:classify(?MODULE:id(-16#FFFFFFF)),
    uint64 = ?MODULE:classify(?MODULE:id(16#FFFFFFFFCAFEFFFF)),
    int64 = ?MODULE:classify(?MODULE:id(-16#FFFFFFFFA)),
    uint256 = ?MODULE:classify(?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)),

    ok.

is_0_100(N) when is_integer(N, 0, 100) ->
    ?MODULE:id(true);
is_0_100(_) ->
    false.

is_uint32(N) when is_integer(N, 0, 16#FFFFFFFF) ->
    ?MODULE:id(true);
is_uint32(_) ->
    false.

is_uint64(N) when is_integer(N, 0, 16#FFFFFFFFFFFFFFFF) ->
    ?MODULE:id(true);
is_uint64(_) ->
    false.

is_atomvm_integer(N) when
    is_integer(
        N,
        -16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    )
->
    ?MODULE:id(true);
is_atomvm_integer(_) ->
    false.

classify(N) when is_integer(N, -16#80, 16#7F) ->
    int8;
classify(N) when is_integer(N, 0, 16#FF) ->
    uint8;
classify(N) when is_integer(N, -16#8000, 16#7FFF) ->
    int16;
classify(N) when is_integer(N, 0, 16#FFFF) ->
    uint16;
classify(N) when is_integer(N, -16#80000000, 16#7FFFFFFF) ->
    int32;
classify(N) when is_integer(N, 0, 16#FFFFFFFF) ->
    uint32;
classify(N) when is_integer(N, -16#8000000000000000, 16#7FFFFFFFFFFFFFFF) ->
    int64;
classify(N) when is_integer(N, 0, 16#FFFFFFFFFFFFFFFF) ->
    uint64;
classify(N) when
    is_integer(N, 0, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
->
    uint256.

test_bigint() ->
    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),

    UINT64MaxBin = <<"FFFFFFFFFFFFFFFF">>,
    UINT64Max = erlang:binary_to_integer(?MODULE:id(UINT64MaxBin), 16),

    UINT64MaxNegBin = <<"-FFFFFFFFFFFFFFFF">>,
    UINT64MaxNeg = erlang:binary_to_integer(?MODULE:id(UINT64MaxNegBin), 16),

    INT63MaxP1Bin = <<"8000000000000000">>,
    INT63MaxP1 = erlang:binary_to_integer(?MODULE:id(INT63MaxP1Bin), 16),

    false = is_integer(?MODULE:id(MaxPattern), ?MODULE:id(0), ?MODULE:id(5)),
    false = is_integer(?MODULE:id(MaxPattern), ?MODULE:id(5), ?MODULE:id(0)),
    true = is_integer(?MODULE:id(MaxPattern), ?MODULE:id(0), ?MODULE:id(MaxPattern)),
    false = is_integer(?MODULE:id(MaxPattern), ?MODULE:id(MaxPattern), ?MODULE:id(0)),
    false = ?MODULE:is_integer_helper(MaxPattern, 0, 5),
    false = ?MODULE:is_integer_helper(MaxPattern, 5, 0),
    true = ?MODULE:is_integer_helper(MaxPattern, 0, MaxPattern),
    false = ?MODULE:is_integer_helper(MaxPattern, MaxPattern, 0),

    false = ?MODULE:is_integer_helper(<<"hello">>, 0, MaxPattern),
    false = ?MODULE:is_integer_helper(MaxPattern, <<"hello">>, MaxPattern),
    false = ?MODULE:is_integer_helper(MaxPattern, 0, <<"hello">>),
    false = is_integer(<<"hello">>, 0, MaxPattern),
    false = is_integer(<<"hello">>, MaxPattern, 0),

    true = is_integer(?MODULE:id(UINT64Max), ?MODULE:id(0), ?MODULE:id(MaxPattern)),
    false = is_integer(?MODULE:id(UINT64Max), ?MODULE:id(MaxPattern), ?MODULE:id(0)),
    true = is_integer(
        ?MODULE:id(INT63MaxP1), ?MODULE:id(UINT64MaxNeg), ?MODULE:id(MaxPattern)
    ),
    false = is_integer(
        ?MODULE:id(INT63MaxP1), ?MODULE:id(MaxPattern), ?MODULE:id(UINT64MaxNeg)
    ),
    true = ?MODULE:is_integer_helper(UINT64Max, 0, MaxPattern),
    false = ?MODULE:is_integer_helper(UINT64Max, MaxPattern, 0),
    true = ?MODULE:is_integer_helper(INT63MaxP1, UINT64MaxNeg, MaxPattern),
    false = ?MODULE:is_integer_helper(INT63MaxP1, MaxPattern, UINT64MaxNeg),
    ok.

test_errors() ->
    exp_err =
        try
            is_integer(42, ?MODULE:id(foo), 100)
        catch
            error:badarg -> exp_err
        end,
    exp_err =
        try
            is_integer(42, 0, ?MODULE:id(<<"bar">>))
        catch
            error:badarg -> exp_err
        end,
    exp_err =
        try
            is_integer(<<"foo">>, ?MODULE:id(foo), 100)
        catch
            error:badarg -> exp_err
        end,
    ok.

is_integer_helper(I, A, B) when is_integer(I, A, B) ->
    _ = ?MODULE:id(I),
    true;
is_integer_helper(_I, _A, _B) ->
    ?MODULE:id(false).

id(X) ->
    X.
-else.

-export([start/0]).

start() ->
    0.

-endif.
