%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_op_bs_test_unit).

-export([start/0]).

start() ->
    HasBSStartMatch3 =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(otp_release) >= "23";
            "ATOM" ->
                % If code was compiled with OTP < 23, we won't have bs_test_unit asm file
                ?OTP_RELEASE >= 23
        end,
    ok =
        if
            HasBSStartMatch3 ->
                ok = test_byte_aligned(),
                ok = test_unit_16();
            true ->
                ok
        end,
    0.

test_byte_aligned() ->
    %% A byte-aligned binary should pass bs_test_unit with unit=8
    <<1, 2, 3, 4>> = test_op_bs_test_unit_asm:get_tail_if_byte_aligned(id(<<1, 2, 3, 4>>)),
    <<>> = test_op_bs_test_unit_asm:get_tail_if_byte_aligned(id(<<>>)),
    <<2, 3>> = test_op_bs_test_unit_asm:get_tail_if_byte_aligned(id(<<2, 3>>)),
    ok.

test_unit_16() ->
    %% After skipping 8 bits, remaining must be 16-bit aligned
    %% 5 bytes = 40 bits, skip 8 => 32 remaining, 32 rem 16 = 0 => ok
    <<2, 3, 4, 5>> = test_op_bs_test_unit_asm:get_tail_unit_16(id(<<1, 2, 3, 4, 5>>)),
    %% 3 bytes = 24 bits, skip 8 => 16 remaining, 16 rem 16 = 0 => ok
    <<2, 3>> = test_op_bs_test_unit_asm:get_tail_unit_16(id(<<1, 2, 3>>)),
    %% 2 bytes = 16 bits, skip 8 => 8 remaining, 8 rem 16 = 8 => fail
    error = test_op_bs_test_unit_asm:get_tail_unit_16(id(<<1, 2>>)),
    %% 4 bytes = 32 bits, skip 8 => 24 remaining, 24 rem 16 = 8 => fail
    error = test_op_bs_test_unit_asm:get_tail_unit_16(id(<<1, 2, 3, 4>>)),
    ok.

id(X) -> X.
