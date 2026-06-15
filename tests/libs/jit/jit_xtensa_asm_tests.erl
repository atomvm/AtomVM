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

-module(jit_xtensa_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(xtensa, Bin, Str), Value)
).

add_test_() ->
    [
        ?_assertAsmEqual(<<16#803450:24/little>>, "add a3, a4, a5", jit_xtensa_asm:add(a3, a4, a5)),
        ?_assertAsmEqual(<<16#800000:24/little>>, "add a0, a0, a0", jit_xtensa_asm:add(a0, a0, a0)),
        ?_assertAsmEqual(
            <<16#80FFE0:24/little>>, "add a15, a15, a14", jit_xtensa_asm:add(a15, a15, a14)
        )
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(<<16#C03450:24/little>>, "sub a3, a4, a5", jit_xtensa_asm:sub(a3, a4, a5)),
        ?_assertAsmEqual(
            <<16#C0BAC0:24/little>>, "sub a11, a10, a12", jit_xtensa_asm:sub(a11, a10, a12)
        )
    ].

and_test_() ->
    [
        ?_assertAsmEqual(<<16#103450:24/little>>, "and a3, a4, a5", jit_xtensa_asm:and_(a3, a4, a5))
    ].

or_test_() ->
    [
        ?_assertAsmEqual(<<16#203450:24/little>>, "or a3, a4, a5", jit_xtensa_asm:or_(a3, a4, a5))
    ].

xor_test_() ->
    [
        ?_assertAsmEqual(<<16#303450:24/little>>, "xor a3, a4, a5", jit_xtensa_asm:xor_(a3, a4, a5))
    ].

slli_test_() ->
    [
        ?_assertAsmEqual(<<16#113480:24/little>>, "slli a3, a4, 8", jit_xtensa_asm:slli(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#113400:24/little>>, "slli a3, a4, 16", jit_xtensa_asm:slli(a3, a4, 16)
        ),
        ?_assertAsmEqual(
            <<16#013110:24/little>>, "slli a3, a1, 31", jit_xtensa_asm:slli(a3, a1, 31)
        )
    ].

srli_test_() ->
    [
        ?_assertAsmEqual(<<16#413840:24/little>>, "srli a3, a4, 8", jit_xtensa_asm:srli(a3, a4, 8)),
        ?_assertAsmEqual(<<16#410040:24/little>>, "srli a0, a4, 0", jit_xtensa_asm:srli(a0, a4, 0))
    ].

srai_test_() ->
    [
        ?_assertAsmEqual(
            <<16#313440:24/little>>, "srai a3, a4, 20", jit_xtensa_asm:srai(a3, a4, 20)
        ),
        ?_assertAsmEqual(<<16#213840:24/little>>, "srai a3, a4, 8", jit_xtensa_asm:srai(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#313F40:24/little>>, "srai a3, a4, 31", jit_xtensa_asm:srai(a3, a4, 31)
        )
    ].

ssr_test_() ->
    [
        ?_assertAsmEqual(<<16#400300:24/little>>, "ssr a3", jit_xtensa_asm:ssr(a3))
    ].

srl_test_() ->
    [
        ?_assertAsmEqual(<<16#913050:24/little>>, "srl a3, a5", jit_xtensa_asm:srl(a3, a4, a5)),
        ?_assertAsmEqual(<<16#910000:24/little>>, "srl a0, a0", jit_xtensa_asm:srl(a0, a0, a0))
    ].

mull_test_() ->
    [
        ?_assertAsmEqual(
            <<16#823450:24/little>>, "mull a3, a4, a5", jit_xtensa_asm:mull(a3, a4, a5)
        )
    ].

quos_test_() ->
    [
        ?_assertAsmEqual(
            <<16#D23450:24/little>>, "quos a3, a4, a5", jit_xtensa_asm:quos(a3, a4, a5)
        )
    ].

rems_test_() ->
    [
        ?_assertAsmEqual(
            <<16#F23450:24/little>>, "rems a3, a4, a5", jit_xtensa_asm:rems(a3, a4, a5)
        )
    ].

addi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0AC432:24/little>>, "addi a3, a4, 10", jit_xtensa_asm:addi(a3, a4, 10)
        ),
        ?_assertAsmEqual(
            <<16#FFC432:24/little>>, "addi a3, a4, -1", jit_xtensa_asm:addi(a3, a4, -1)
        ),
        ?_assertAsmEqual(
            <<16#80C432:24/little>>, "addi a3, a4, -128", jit_xtensa_asm:addi(a3, a4, -128)
        ),
        ?_assertAsmEqual(
            <<16#7FC432:24/little>>, "addi a3, a4, 127", jit_xtensa_asm:addi(a3, a4, 127)
        ),
        ?_assertAsmEqual(<<16#00C002:24/little>>, "addi a0, a0, 0", jit_xtensa_asm:addi(a0, a0, 0))
    ].

addmi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#01D432:24/little>>, "addmi a3, a4, 256", jit_xtensa_asm:addmi(a3, a4, 256)
        ),
        ?_assertAsmEqual(
            <<16#FFD432:24/little>>, "addmi a3, a4, -256", jit_xtensa_asm:addmi(a3, a4, -256)
        )
    ].

movi_test_() ->
    [
        ?_assertAsmEqual(<<16#00A032:24/little>>, "movi a3, 0", jit_xtensa_asm:movi(a3, 0)),
        ?_assertAsmEqual(<<16#64A032:24/little>>, "movi a3, 100", jit_xtensa_asm:movi(a3, 100)),
        ?_assertAsmEqual(<<16#FFAF32:24/little>>, "movi a3, -1", jit_xtensa_asm:movi(a3, -1)),
        ?_assertAsmEqual(<<16#FFA732:24/little>>, "movi a3, 2047", jit_xtensa_asm:movi(a3, 2047)),
        ?_assertAsmEqual(<<16#00A832:24/little>>, "movi a3, -2048", jit_xtensa_asm:movi(a3, -2048))
    ].

l32i_test_() ->
    [
        ?_assertAsmEqual(<<16#002432:24/little>>, "l32i a3, a4, 0", jit_xtensa_asm:l32i(a3, a4, 0)),
        ?_assertAsmEqual(<<16#022432:24/little>>, "l32i a3, a4, 8", jit_xtensa_asm:l32i(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#FF2432:24/little>>, "l32i a3, a4, 1020", jit_xtensa_asm:l32i(a3, a4, 1020)
        )
    ].

s32i_test_() ->
    [
        ?_assertAsmEqual(<<16#006432:24/little>>, "s32i a3, a4, 0", jit_xtensa_asm:s32i(a3, a4, 0)),
        ?_assertAsmEqual(<<16#026432:24/little>>, "s32i a3, a4, 8", jit_xtensa_asm:s32i(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#FF6432:24/little>>, "s32i a3, a4, 1020", jit_xtensa_asm:s32i(a3, a4, 1020)
        )
    ].

beq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#081347:24/little>>, "beq a3, a4, . + 12", jit_xtensa_asm:beq(a3, a4, 8)
        ),
        ?_assertAsmEqual(
            <<16#001347:24/little>>, "beq a3, a4, . + 4", jit_xtensa_asm:beq(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#FC1347:24/little>>, "beq a3, a4, . + 0", jit_xtensa_asm:beq(a3, a4, -4)
        )
    ].

bne_test_() ->
    [
        ?_assertAsmEqual(
            <<16#009347:24/little>>, "bne a3, a4, . + 4", jit_xtensa_asm:bne(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#089347:24/little>>, "bne a3, a4, . + 12", jit_xtensa_asm:bne(a3, a4, 8)
        )
    ].

blt_test_() ->
    [
        ?_assertAsmEqual(
            <<16#002347:24/little>>, "blt a3, a4, . + 4", jit_xtensa_asm:blt(a3, a4, 0)
        )
    ].

beqi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#001326:24/little>>, "beqi a3, 1, . + 4", jit_xtensa_asm:beqi(a3, 1, 0)
        ),
        ?_assertAsmEqual(
            <<16#000326:24/little>>, "beqi a3, -1, . + 4", jit_xtensa_asm:beqi(a3, -1, 0)
        )
    ].

bnei_test_() ->
    [
        ?_assertAsmEqual(
            <<16#001366:24/little>>, "bnei a3, 1, . + 4", jit_xtensa_asm:bnei(a3, 1, 0)
        )
    ].

blti_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0013A6:24/little>>, "blti a3, 1, . + 4", jit_xtensa_asm:blti(a3, 1, 0)
        )
    ].

beqz_test_() ->
    [
        ?_assertAsmEqual(<<16#000316:24/little>>, "beqz a3, . + 4", jit_xtensa_asm:beqz(a3, 0)),
        ?_assertAsmEqual(<<16#07F316:24/little>>, "beqz a3, . + 131", jit_xtensa_asm:beqz(a3, 127))
    ].

bnez_test_() ->
    [
        ?_assertAsmEqual(<<16#000356:24/little>>, "bnez a3, . + 4", jit_xtensa_asm:bnez(a3, 0)),
        ?_assertAsmEqual(<<16#001356:24/little>>, "bnez a3, . + 5", jit_xtensa_asm:bnez(a3, 1))
    ].

bltz_test_() ->
    [
        ?_assertAsmEqual(<<16#000396:24/little>>, "bltz a3, . + 4", jit_xtensa_asm:bltz(a3, 0))
    ].

j_test_() ->
    [
        ?_assertAsmEqual(<<16#000006:24/little>>, "j . + 4", jit_xtensa_asm:j(0)),
        ?_assertAsmEqual(<<16#001906:24/little>>, "j . + 104", jit_xtensa_asm:j(100))
    ].

jx_test_() ->
    [
        ?_assertAsmEqual(<<16#0003A0:24/little>>, "jx a3", jit_xtensa_asm:jx(a3)),
        ?_assertAsmEqual(<<16#0000A0:24/little>>, "jx a0", jit_xtensa_asm:jx(a0))
    ].

retw_test_() ->
    [
        ?_assertAsmEqual(<<16#000090:24/little>>, "retw", jit_xtensa_asm:retw())
    ].

callx8_test_() ->
    [
        ?_assertAsmEqual(<<16#0008E0:24/little>>, "callx8 a8", jit_xtensa_asm:callx8(a8)),
        ?_assertAsmEqual(<<16#0003E0:24/little>>, "callx8 a3", jit_xtensa_asm:callx8(a3))
    ].

entry_test_() ->
    [
        ?_assertAsmEqual(<<16#004136:24/little>>, "entry a1, 32", jit_xtensa_asm:entry(a1, 32)),
        ?_assertAsmEqual(<<16#006136:24/little>>, "entry a1, 48", jit_xtensa_asm:entry(a1, 48))
    ].

mov_n_test_() ->
    [
        ?_assertAsmEqual(<<16#043D:16/little>>, "mov.n a3, a4", jit_xtensa_asm:mov(a3, a4)),
        ?_assertAsmEqual(<<16#000D:16/little>>, "mov.n a0, a0", jit_xtensa_asm:mov(a0, a0))
    ].

nop_test_() ->
    [
        ?_assertAsmEqual(<<16#0020f0:24/little>>, "nop", jit_xtensa_asm:nop())
    ].

break_test_() ->
    [
        ?_assertAsmEqual(<<16#004000:24/little>>, "break 0, 0", jit_xtensa_asm:break(0, 0)),
        ?_assertAsmEqual(<<16#004120:24/little>>, "break 1, 2", jit_xtensa_asm:break(1, 2))
    ].

neg_test_() ->
    [
        ?_assertAsmEqual(<<16#603050:24/little>>, "neg a3, a5", jit_xtensa_asm:neg(a3, a5)),
        ?_assertAsmEqual(<<16#600000:24/little>>, "neg a0, a0", jit_xtensa_asm:neg(a0, a0))
    ].

l32r_test_() ->
    [
        ?_assertAsmEqual(<<16#000031:24/little>>, "l32r a3, 0", jit_xtensa_asm:l32r(a3, 0)),
        ?_assertAsmEqual(
            <<16#FFFF31:24/little>>, ".byte 0x31, 0xff, 0xff", jit_xtensa_asm:l32r(a3, -1)
        )
    ].
