%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_op_bs_create_bin).

-export([start/0]).

start() ->
    HasBSCreateBin =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(otp_release) >= "25";
            "ATOM" ->
                % If code was compiled with OTP < 25, we won't have bs_create_bin asm file
                ?OTP_RELEASE >= 25
        end,
    ok =
        if
            HasBSCreateBin ->
                ok = test_bs_create_bin_utf8(),
                ok = test_bs_create_bin_utf16(),
                ok = test_bs_create_bin_utf32(),
                ok = test_bs_create_bin_integer(),
                ok = test_bs_create_bin_binary(),
                ok = test_bs_create_bin_alloc_list();
            true ->
                ok
        end,
    0.

test_bs_create_bin_utf8() ->
    <<0>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(0, [], undefined),
    <<0>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_fail(0, [], undefined),
    <<1>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(1, [], undefined),
    <<1>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_fail(1, [], undefined),
    <<227, 130, 162>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(12450, [], undefined),
    <<227, 130, 162>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_fail(12450, [], undefined),
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(-1, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_utf8_fail(-1, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(not_int, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_utf8_fail(not_int, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    <<0>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(0, [], all),
    <<0>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(0, [], 8),
    <<0>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(0, [], 16),
    <<0>> = test_op_bs_create_bin_asm:bs_create_bin_utf8_no_fail(0, [], foo),
    ok.

test_bs_create_bin_utf16() ->
    <<0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(0, [], undefined),
    <<0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_fail(0, [], undefined),
    <<0, 1>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(1, [], undefined),
    <<0, 1>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_fail(1, [], undefined),
    <<48, 162>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(12450, [], undefined),
    <<48, 162>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_fail(12450, [], undefined),
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(-1, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_utf16_fail(-1, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(not_int, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_utf16_fail(not_int, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    <<0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(0, [], all),
    <<0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(0, [], 8),
    <<0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(0, [], 16),
    <<0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf16_no_fail(0, [], foo),
    ok.

test_bs_create_bin_utf32() ->
    <<0, 0, 0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(0, [], undefined),
    <<0, 0, 0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_fail(0, [], undefined),
    <<0, 0, 0, 1>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(1, [], undefined),
    <<0, 0, 0, 1>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_fail(1, [], undefined),
    <<0, 0, 48, 162>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(12450, [], undefined),
    <<0, 0, 48, 162>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_fail(12450, [], undefined),
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(-1, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_utf32_fail(-1, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(not_int, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_utf32_fail(not_int, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    <<0, 0, 0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(0, [], all),
    <<0, 0, 0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(0, [], 8),
    <<0, 0, 0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(0, [], 16),
    <<0, 0, 0, 0>> = test_op_bs_create_bin_asm:bs_create_bin_utf32_no_fail(0, [], foo),
    ok.

test_bs_create_bin_integer() ->
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_integer_no_fail(0, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_integer_fail(0, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_integer_no_fail(0, [], all),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_integer_fail(0, [], all),
            ok
        catch
            error:badarg -> unexpected
        end,
    <<42>> = test_op_bs_create_bin_asm:bs_create_bin_integer_no_fail(42, [], 1),
    <<42>> = test_op_bs_create_bin_asm:bs_create_bin_integer_fail(42, [], 1),
    <<0, 42>> = test_op_bs_create_bin_asm:bs_create_bin_integer_no_fail(42, [], 2),
    <<0, 42>> = test_op_bs_create_bin_asm:bs_create_bin_integer_fail(42, [], 2),
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_integer_no_fail(0, [], foo),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_integer_fail(0, [], foo),
            ok
        catch
            error:badarg -> unexpected
        end,
    ok.

test_bs_create_bin_binary() ->
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_binary_no_fail(<<0>>, [], undefined),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_binary_fail(<<0>>, [], undefined),
            ok
        catch
            error:badarg -> unexpected
        end,
    % AtomVM allows all for binaries while OTP28 seems to disallow it.
    % It may only accept them for bs match state (?)
    %   ok = try test_op_bs_create_bin_asm:bs_create_bin_binary_no_fail(<<0>>, [], all), unexpected catch error:badarg -> ok end,
    %   ok = try fail = test_op_bs_create_bin_asm:bs_create_bin_binary_fail(<<0>>, [], all), ok catch error:badarg -> unexpected end,
    <<42>> = test_op_bs_create_bin_asm:bs_create_bin_binary_no_fail(<<42>>, [], 1),
    <<42>> = test_op_bs_create_bin_asm:bs_create_bin_binary_fail(<<42>>, [], 1),
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_binary_no_fail(<<42>>, [], 2),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_binary_fail(<<42>>, [], 2),
            ok
        catch
            error:badarg -> unexpected
        end,
    ok =
        try
            test_op_bs_create_bin_asm:bs_create_bin_binary_no_fail(<<0>>, [], foo),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            fail = test_op_bs_create_bin_asm:bs_create_bin_binary_fail(<<0>>, [], foo),
            ok
        catch
            error:badarg -> unexpected
        end,
    ok.

test_bs_create_bin_alloc_list() ->
    %% Test that bs_create_bin with allocator list works correctly
    %% Expected result: <<0,0,0,42>> (42 as 32-bit big-endian integer)
    Expected = <<0, 0, 0, 42>>,
    Result = test_op_bs_create_bin_asm:bs_create_bin_alloc_list(42),
    case Result of
        Expected ->
            ok;
        _ ->
            error({unexpected_result, Result, expected, Expected})
    end.
