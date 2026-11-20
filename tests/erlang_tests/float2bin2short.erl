%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(float2bin2short).

-export([start/0, compare_bin/2, float_to_bin_badarg/2, negate/1]).

start() ->
    case is_float32_fallback() of
        true ->
            Short712 = erlang:float_to_binary(7.12, [short]),
            roundtrip_bin(0.1, [short]) +
                roundtrip_bin(0.3, [short]) * 2 +
                compare_bin(erlang:float_to_binary(1.0, [short]), <<"1.0">>) * 4 +
                compare_bin(erlang:float_to_binary(1.5, [short]), <<"1.5">>) * 8 +
                roundtrip_bin(1.2e20, [short]) * 16 +
                roundtrip_bin(1.2e-20, [short]) * 32 +
                compare_bin(erlang:float_to_binary(negate(0.0), [short]), <<"-0.0">>) * 64 +
                roundtrip_bin(3.14159265, [short]) * 128 +
                compare_bin(erlang:float_to_binary(7.12, [short, compact]), Short712) * 256 +
                compare_bin(erlang:float_to_binary(7.12, [short, {decimals, 3}]), <<"7.120">>) *
                    512 +
                compare_bin(erlang:float_to_binary(7.12, [{decimals, 3}, short]), Short712) *
                    1024 +
                compare_bin(
                    erlang:float_to_binary(7.12, [short, {scientific, 3}]), <<"7.120e+00">>
                ) *
                    2048 +
                compare_bin(erlang:float_to_binary(7.12, [{scientific, 3}, short]), Short712) *
                    4096 +
                float_to_bin_badarg({1}, [short]) * 8192 +
                float_to_bin_badarg(7.12, [short, invalid]) * 16384 +
                float_to_bin_badarg(7.12, [{invalid, 0}, short]) * 32768 +
                float_to_bin_badarg(7.12, [{scientific, invalid}, short]) * 65536 +
                float_to_bin_badarg(7.12, [short | invalid]) * 131072;
        false ->
            PiExpected = expected_pi_bin(),
            compare_bin(erlang:float_to_binary(0.1, [short]), <<"0.1">>) +
                compare_bin(erlang:float_to_binary(0.3, [short]), <<"0.3">>) * 2 +
                compare_bin(erlang:float_to_binary(1.0, [short]), <<"1.0">>) * 4 +
                compare_bin(erlang:float_to_binary(1.5, [short]), <<"1.5">>) * 8 +
                compare_bin(erlang:float_to_binary(1.2e20, [short]), <<"1.2e20">>) * 16 +
                compare_bin(erlang:float_to_binary(1.2e-20, [short]), <<"1.2e-20">>) * 32 +
                compare_bin(erlang:float_to_binary(negate(0.0), [short]), <<"-0.0">>) * 64 +
                compare_bin(erlang:float_to_binary(3.14159265, [short]), PiExpected) * 128 +
                compare_bin(erlang:float_to_binary(7.12, [short, compact]), <<"7.12">>) * 256 +
                compare_bin(erlang:float_to_binary(7.12, [short, {decimals, 3}]), <<"7.120">>) *
                    512 +
                compare_bin(erlang:float_to_binary(7.12, [{decimals, 3}, short]), <<"7.12">>) *
                    1024 +
                compare_bin(
                    erlang:float_to_binary(7.12, [short, {scientific, 3}]), <<"7.120e+00">>
                ) *
                    2048 +
                compare_bin(erlang:float_to_binary(7.12, [{scientific, 3}, short]), <<"7.12">>) *
                    4096 +
                float_to_bin_badarg({1}, [short]) * 8192 +
                float_to_bin_badarg(7.12, [short, invalid]) * 16384 +
                float_to_bin_badarg(7.12, [{invalid, 0}, short]) * 32768 +
                float_to_bin_badarg(7.12, [{scientific, invalid}, short]) * 65536 +
                float_to_bin_badarg(7.12, [short | invalid]) * 131072
    end.

is_float32_fallback() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            erlang:system_info(avm_floatsize) =:= 4 andalso
                erlang:float_to_binary(0.1, [short]) =/= <<"0.1">>;
        _ ->
            false
    end.

roundtrip_bin(F, Opts) ->
    case erlang:binary_to_float(erlang:float_to_binary(F, Opts)) =:= F of
        true -> 1;
        false -> 0
    end.

expected_pi_bin() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            case erlang:system_info(avm_floatsize) of
                4 -> <<"3.1415927">>;
                _ -> <<"3.14159265">>
            end;
        _ ->
            <<"3.14159265">>
    end.

compare_bin(Bin1, Bin2) when byte_size(Bin1) == byte_size(Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1);
compare_bin(_Bin1, _Bin2) ->
    0.

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

float_to_bin_badarg(F, O) ->
    try erlang:float_to_binary(F, O) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> -1
    end.

negate(X) ->
    -1 * X.
