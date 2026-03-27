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

-module(float2bin_short).

-export([start/0, id/1, negate/1]).

start() ->
    ok = test_short_basic(),
    ok = test_short_grisu3(),
    ok = test_short_edge_cases(),
    ok = test_short_combinations(),
    ok = test_short_badarg(),
    0.

test_short_basic() ->
    %% Values exactly representable in both 32 and 64 bit floats
    <<"1.5">> = erlang:float_to_binary(?MODULE:id(1.5), [short]),
    <<"1.0">> = erlang:float_to_binary(?MODULE:id(1.0), [short]),
    <<"0.0">> = erlang:float_to_binary(?MODULE:id(0.0), [short]),
    <<"-0.0">> = erlang:float_to_binary(?MODULE:negate(0.0), [short]),
    <<"-1.5">> = erlang:float_to_binary(?MODULE:id(-1.5), [short]),
    <<"100.0">> = erlang:float_to_binary(?MODULE:id(100.0), [short]),
    <<"0.5">> = erlang:float_to_binary(?MODULE:id(0.5), [short]),
    <<"0.25">> = erlang:float_to_binary(?MODULE:id(0.25), [short]),
    <<"2.5">> = erlang:float_to_binary(?MODULE:id(2.5), [short]),
    ok.

test_short_grisu3() ->
    case floatsize() of
        8 ->
            %% Verify Grisu3 produces shortest roundtrip
            <<"3.14">> = erlang:float_to_binary(?MODULE:id(3.14), [short]),
            <<"0.1">> = erlang:float_to_binary(?MODULE:id(0.1), [short]),
            <<"0.001">> = erlang:float_to_binary(?MODULE:id(0.001), [short]),
            <<"999.0">> = erlang:float_to_binary(?MODULE:id(999.0), [short]),
            <<"3.14159265">> = erlang:float_to_binary(?MODULE:id(3.14159265), [short]),
            %% Large exponents: must always have decimal point
            <<"1.2e20">> = erlang:float_to_binary(?MODULE:id(1.2e20), [short]),
            <<"1.2e-20">> = erlang:float_to_binary(?MODULE:id(1.2e-20), [short]),
            <<"1.0e3">> = erlang:float_to_binary(?MODULE:id(1000.0), [short]),
            <<"2.0e3">> = erlang:float_to_binary(?MODULE:id(2000.0), [short]),
            <<"1.0e5">> = erlang:float_to_binary(?MODULE:id(100000.0), [short]),
            <<"1.0e20">> = erlang:float_to_binary(?MODULE:id(1.0e20), [short]),
            <<"1.0e300">> = erlang:float_to_binary(?MODULE:id(1.0e300), [short]),
            <<"1.0e-300">> = erlang:float_to_binary(?MODULE:id(1.0e-300), [short]),
            %% 2^53 boundary (forces scientific)
            <<"9.007199254740992e15">> = erlang:float_to_binary(?MODULE:id(9007199254740992.0), [
                short
            ]),
            ok;
        4 ->
            %% 32-bit: best-effort, verify roundtrip and expected output
            %% 3.14 rounds to a different 32-bit float (3.1400001...)
            %% Our %.9g gives "3.1400001" — correct but not shortest
            <<"3.1400001">> = erlang:float_to_binary(?MODULE:id(3.14), [short]),
            F = ?MODULE:id(3.14),
            F = erlang:binary_to_float(erlang:float_to_binary(?MODULE:id(F), [short])),
            ok
    end.

test_short_edge_cases() ->
    case floatsize() of
        8 ->
            %% Extremes: verify no crash and roundtrip
            _ = erlang:float_to_binary(?MODULE:id(1.7976931348623157e308), [short]),
            _ = erlang:float_to_binary(?MODULE:id(5.0e-324), [short]),
            _ = erlang:float_to_binary(?MODULE:id(2.2250738585072014e-308), [short]),
            %% Classic roundtrip trap: 0.1 + 0.2
            F1 = ?MODULE:id(0.1) + ?MODULE:id(0.2),
            F1 = erlang:binary_to_float(erlang:float_to_binary(?MODULE:id(F1), [short])),
            %% Values that Grisu3 handles natively (match OTP/Ryu exactly)
            <<"55388492.622190244">> = erlang:float_to_binary(?MODULE:id(55388492.622190244), [
                short
            ]),
            <<"2.2506787569811123e-253">> = erlang:float_to_binary(
                ?MODULE:id(2.2506787569811123e-253), [short]
            ),
            <<"1103618912042992.8">> = erlang:float_to_binary(?MODULE:id(1103618912042992.8), [
                short
            ]),
            <<"2.9802322387695312e-8">> = erlang:float_to_binary(
                ?MODULE:id(2.9802322387695312e-08), [short]
            ),
            %% Values where Grisu3 falls back to %.17g (longer output than
            %% OTP/Ryu but still roundtrips correctly)
            case erlang:system_info(machine) of
                "BEAM" ->
                    %% OTP uses Ryu: shortest output guaranteed
                    <<"1.0e23">> = erlang:float_to_binary(?MODULE:id(1.0e23), [short]),
                    <<"9.0e-265">> = erlang:float_to_binary(?MODULE:id(9.0e-265), [short]),
                    <<"5.423717798060526e125">> = erlang:float_to_binary(
                        ?MODULE:id(5.423717798060526e+125), [short]
                    ),
                    <<"1.372371880954233e-288">> = erlang:float_to_binary(
                        ?MODULE:id(1.372371880954233e-288), [short]
                    ),
                    %% Near 2^53: OTP uses scientific with shortest digits
                    <<"1.979740120616623e16">> = erlang:float_to_binary(
                        ?MODULE:id(1.979740120616623e+16), [short]
                    ),
                    <<"2.204209551024847e16">> = erlang:float_to_binary(
                        ?MODULE:id(2.204209551024847e+16), [short]
                    ),
                    <<"6.3098511070304936e16">> = erlang:float_to_binary(
                        ?MODULE:id(6.3098511070304936e+16), [short]
                    ),
                    <<"-1.8830736066034812e16">> = erlang:float_to_binary(
                        ?MODULE:id(-1.8830736066034812e+16), [short]
                    );
                "ATOM" ->
                    %% AtomVM uses Grisu3: fallback gives longer but correct output
                    <<"9.9999999999999992e22">> = erlang:float_to_binary(?MODULE:id(1.0e23), [short]),
                    <<"9.0000000000000006e-265">> = erlang:float_to_binary(?MODULE:id(9.0e-265), [
                        short
                    ]),
                    <<"5.4237177980605264e125">> = erlang:float_to_binary(
                        ?MODULE:id(5.423717798060526e+125), [short]
                    ),
                    <<"1.3723718809542329e-288">> = erlang:float_to_binary(
                        ?MODULE:id(1.372371880954233e-288), [short]
                    ),
                    ok = assert_roundtrip_bin(?MODULE:id(1.0e23)),
                    ok = assert_roundtrip_bin(?MODULE:id(9.0e-265)),
                    ok = assert_roundtrip_bin(?MODULE:id(5.423717798060526e+125)),
                    ok = assert_roundtrip_bin(?MODULE:id(1.372371880954233e-288)),
                    %% Grisu3 fallback >= 2^53: must use scientific notation
                    <<"1.9797401206166232e16">> = erlang:float_to_binary(
                        ?MODULE:id(1.979740120616623e+16), [short]
                    ),
                    <<"2.2042095510248472e16">> = erlang:float_to_binary(
                        ?MODULE:id(2.204209551024847e+16), [short]
                    ),
                    <<"6.3098511070304936e16">> = erlang:float_to_binary(
                        ?MODULE:id(6.3098511070304936e+16), [short]
                    ),
                    <<"-1.8830736066034812e16">> = erlang:float_to_binary(
                        ?MODULE:id(-1.8830736066034812e+16), [short]
                    ),
                    ok = assert_roundtrip_bin(?MODULE:id(1.979740120616623e+16)),
                    ok = assert_roundtrip_bin(?MODULE:id(2.204209551024847e+16)),
                    ok = assert_roundtrip_bin(?MODULE:id(6.3098511070304936e+16)),
                    ok = assert_roundtrip_bin(?MODULE:id(-1.8830736066034812e+16))
            end,
            ok;
        4 ->
            %% 32-bit roundtrip on edge values
            F2 = ?MODULE:id(0.1) + ?MODULE:id(0.2),
            F2 = erlang:binary_to_float(erlang:float_to_binary(?MODULE:id(F2), [short])),
            ok
    end.

assert_roundtrip_bin(F) ->
    F = erlang:binary_to_float(erlang:float_to_binary(?MODULE:id(F), [short])),
    ok.

test_short_combinations() ->
    %% short + compact: compact is a no-op with short
    <<"1.5">> = erlang:float_to_binary(?MODULE:id(1.5), [short, compact]),
    %% Last option wins (matching OTP semantics)
    <<"1.5000">> = erlang:float_to_binary(?MODULE:id(1.5), [short, {decimals, 4}]),
    <<"1.5">> = erlang:float_to_binary(?MODULE:id(1.5), [{decimals, 4}, short]),
    <<"1.50e+00">> = erlang:float_to_binary(?MODULE:id(1.5), [short, {scientific, 2}]),
    <<"1.5">> = erlang:float_to_binary(?MODULE:id(1.5), [{scientific, 2}, short]),
    ok.

test_short_badarg() ->
    ok = expect_badarg(fun() -> erlang:float_to_binary(?MODULE:id({1}), [short]) end),
    ok = expect_badarg(fun() -> erlang:float_to_binary(?MODULE:id(1.0), [short, invalid]) end),
    ok = expect_badarg(fun() -> erlang:float_to_binary(?MODULE:id(1.0), [{invalid, 0}, short]) end),
    ok = expect_badarg(fun() -> erlang:float_to_binary(?MODULE:id(1.0), [{scientific, invalid}]) end),
    ok = expect_badarg(fun() -> erlang:float_to_binary(?MODULE:id(1.0), [short | invalid]) end),
    ok.

expect_badarg(Fun) ->
    try Fun() of
        _ -> fail
    catch
        error:badarg -> ok
    end.

floatsize() ->
    case erlang:system_info(machine) of
        "BEAM" -> 8;
        "ATOM" -> erlang:system_info(avm_floatsize)
    end.

negate(X) -> -1.0 * X.

id(X) -> X.
