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

-module(jit_armv7m_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(arm_thumb2, Bin, Str), Value)
).

%% B.W (32-bit unconditional branch) tests
b_w_test_() ->
    [
        % b.w .+0 (branch to self+4, offset 0)
        ?_assertAsmEqual(
            <<16#F000:16/little, 16#B800:16/little>>,
            "b.w .+4",
            jit_armv7m_asm:b_w(0)
        ),
        % b.w .+4 (forward offset 4)
        ?_assertAsmEqual(
            <<16#F000:16/little, 16#B802:16/little>>,
            "b.w .+8",
            jit_armv7m_asm:b_w(4)
        ),
        % b.w .-4 (backward offset -4)
        ?_assertAsmEqual(
            <<16#F7FF:16/little, 16#BFFE:16/little>>,
            "b.w .",
            jit_armv7m_asm:b_w(-4)
        ),
        % b.w .+256
        ?_assertAsmEqual(
            <<16#F000:16/little, 16#B880:16/little>>,
            "b.w .+260",
            jit_armv7m_asm:b_w(256)
        ),
        % b.w .-256
        ?_assertAsmEqual(
            <<16#F7FF:16/little, 16#BF80:16/little>>,
            "b.w .-252",
            jit_armv7m_asm:b_w(-256)
        ),
        % b.w .+4096
        ?_assertAsmEqual(
            <<16#F001:16/little, 16#B800:16/little>>,
            "b.w .+4100",
            jit_armv7m_asm:b_w(4096)
        ),
        % b.w .-4096
        ?_assertAsmEqual(
            <<16#F7FF:16/little, 16#B800:16/little>>,
            "b.w .-4092",
            jit_armv7m_asm:b_w(-4096)
        ),
        % b.w maximum positive offset (+16777214)
        ?_assertAsmEqual(
            <<16#F3FF:16/little, 16#97FF:16/little>>,
            "b.w .+16777218",
            jit_armv7m_asm:b_w(16777214)
        ),
        % b.w maximum negative offset (-16777216)
        ?_assertAsmEqual(
            <<16#F400:16/little, 16#9000:16/little>>,
            "b.w .-16777212",
            jit_armv7m_asm:b_w(-16777216)
        ),
        % b.w out of range (positive)
        ?_assertError(
            {unencodable_branch_offset, 16777216},
            jit_armv7m_asm:b_w(16777216)
        ),
        % b.w out of range (negative)
        ?_assertError(
            {unencodable_branch_offset, -16777218},
            jit_armv7m_asm:b_w(-16777218)
        ),
        % b.w odd offset
        ?_assertError(
            {unencodable_branch_offset, 3},
            jit_armv7m_asm:b_w(3)
        )
    ].

%% MOVW (move 16-bit immediate to lower half) tests
movw_test_() ->
    [
        % movw r0, #0
        ?_assertAsmEqual(
            <<16#F240:16/little, 16#0000:16/little>>,
            "movw r0, #0",
            jit_armv7m_asm:movw(r0, 0)
        ),
        % movw r0, #1
        ?_assertAsmEqual(
            <<16#F240:16/little, 16#0001:16/little>>,
            "movw r0, #1",
            jit_armv7m_asm:movw(r0, 1)
        ),
        % movw r0, #255
        ?_assertAsmEqual(
            <<16#F240:16/little, 16#00FF:16/little>>,
            "movw r0, #255",
            jit_armv7m_asm:movw(r0, 255)
        ),
        % movw r0, #256
        ?_assertAsmEqual(
            <<16#F240:16/little, 16#1000:16/little>>,
            "movw r0, #256",
            jit_armv7m_asm:movw(r0, 256)
        ),
        % movw r3, #0x1234
        ?_assertAsmEqual(
            <<16#F241:16/little, 16#2334:16/little>>,
            "movw r3, #0x1234",
            jit_armv7m_asm:movw(r3, 16#1234)
        ),
        % movw r7, #0xFFFF
        ?_assertAsmEqual(
            <<16#F64F:16/little, 16#77FF:16/little>>,
            "movw r7, #0xFFFF",
            jit_armv7m_asm:movw(r7, 16#FFFF)
        ),
        % movw r12, #0xABCD
        ?_assertAsmEqual(
            <<16#F64A:16/little, 16#3CCD:16/little>>,
            "movw r12, #0xABCD",
            jit_armv7m_asm:movw(r12, 16#ABCD)
        ),
        % movw r1, #0x800 (test i bit)
        ?_assertAsmEqual(
            <<16#F640:16/little, 16#0100:16/little>>,
            "movw r1, #2048",
            jit_armv7m_asm:movw(r1, 2048)
        )
    ].

%% MOVT (move 16-bit immediate to upper half) tests
movt_test_() ->
    [
        % movt r0, #0
        ?_assertAsmEqual(
            <<16#F2C0:16/little, 16#0000:16/little>>,
            "movt r0, #0",
            jit_armv7m_asm:movt(r0, 0)
        ),
        % movt r0, #1
        ?_assertAsmEqual(
            <<16#F2C0:16/little, 16#0001:16/little>>,
            "movt r0, #1",
            jit_armv7m_asm:movt(r0, 1)
        ),
        % movt r3, #0x1234
        ?_assertAsmEqual(
            <<16#F2C1:16/little, 16#2334:16/little>>,
            "movt r3, #0x1234",
            jit_armv7m_asm:movt(r3, 16#1234)
        ),
        % movt r7, #0xFFFF
        ?_assertAsmEqual(
            <<16#F6CF:16/little, 16#77FF:16/little>>,
            "movt r7, #0xFFFF",
            jit_armv7m_asm:movt(r7, 16#FFFF)
        ),
        % movt r12, #0xABCD
        ?_assertAsmEqual(
            <<16#F6CA:16/little, 16#3CCD:16/little>>,
            "movt r12, #0xABCD",
            jit_armv7m_asm:movt(r12, 16#ABCD)
        )
    ].
