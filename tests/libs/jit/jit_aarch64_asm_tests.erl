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

-module(jit_aarch64_asm_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    list_to_integer/1,
    list_to_integer/2
]).

list_to_integer(X) -> erlang:list_to_integer(X).
list_to_integer(X, B) -> erlang:list_to_integer(X, B).

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(aarch64, Bin, Str), Value)
).
-define(_assertAsmEqualLargeInt(Bin, Str, Value),
    ?_test(begin
        case erlang:system_info(machine) of
            "BEAM" ->
                ?assertEqual(jit_tests_common:asm(aarch64, Bin, Str), Value);
            "ATOM" ->
                % AtomVM doesn't handle large integers yet.
                % Skip the test
                ok
        end
    end)
).

add_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9100e0e7:32/little>>, "add x7, x7, #56", jit_aarch64_asm:add(r7, r7, 56)
        ),
        ?_assertAsmEqual(
            <<16#91000000:32/little>>, "add x0, x0, #0", jit_aarch64_asm:add(r0, r0, 0)
        ),
        ?_assertAsmEqual(
            <<16#91000421:32/little>>, "add x1, x1, #1", jit_aarch64_asm:add(r1, r1, 1)
        ),
        ?_assertAsmEqual(
            <<16#8b031041:32/little>>,
            "add x1, x2, x3, lsl #4",
            jit_aarch64_asm:add(r1, r2, r3, {lsl, 4})
        ),
        ?_assertAsmEqual(
            <<16#8b030041:32/little>>, "add x1, x2, x3", jit_aarch64_asm:add(r1, r2, r3)
        ),
        %% Test add with invalid immediate
        ?_assertError({unencodable_immediate, 16#FFFF}, jit_aarch64_asm:add(r0, r0, 16#FFFF)),

        %% Test cases for additional registers (r11, r12, r14, r22-r30)
        ?_assertAsmEqual(
            <<16#8b0b000b:32/little>>, "add x11, x0, x11", jit_aarch64_asm:add(r11, r0, r11)
        ),
        ?_assertAsmEqual(
            <<16#8b0c000c:32/little>>, "add x12, x0, x12", jit_aarch64_asm:add(r12, r0, r12)
        ),
        ?_assertAsmEqual(
            <<16#8b0e000e:32/little>>, "add x14, x0, x14", jit_aarch64_asm:add(r14, r0, r14)
        ),
        ?_assertAsmEqual(
            <<16#8b160016:32/little>>, "add x22, x0, x22", jit_aarch64_asm:add(r22, r0, r22)
        ),
        ?_assertAsmEqual(
            <<16#8b170017:32/little>>, "add x23, x0, x23", jit_aarch64_asm:add(r23, r0, r23)
        ),
        ?_assertAsmEqual(
            <<16#8b180018:32/little>>, "add x24, x0, x24", jit_aarch64_asm:add(r24, r0, r24)
        ),
        ?_assertAsmEqual(
            <<16#8b190019:32/little>>, "add x25, x0, x25", jit_aarch64_asm:add(r25, r0, r25)
        ),
        ?_assertAsmEqual(
            <<16#8b1a001a:32/little>>, "add x26, x0, x26", jit_aarch64_asm:add(r26, r0, r26)
        ),
        ?_assertAsmEqual(
            <<16#8b1b001b:32/little>>, "add x27, x0, x27", jit_aarch64_asm:add(r27, r0, r27)
        ),
        ?_assertAsmEqual(
            <<16#8b1c001c:32/little>>, "add x28, x0, x28", jit_aarch64_asm:add(r28, r0, r28)
        ),
        ?_assertAsmEqual(
            <<16#8b1d001d:32/little>>, "add x29, x0, x29", jit_aarch64_asm:add(r29, r0, r29)
        ),
        ?_assertAsmEqual(
            <<16#8b1e001e:32/little>>, "add x30, x0, x30", jit_aarch64_asm:add(r30, r0, r30)
        )
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(
            <<16#d100e0e7:32/little>>, "sub x7, x7, #56", jit_aarch64_asm:sub(r7, r7, 56)
        ),
        ?_assertAsmEqual(
            <<16#d1000000:32/little>>, "sub x0, x0, #0", jit_aarch64_asm:sub(r0, r0, 0)
        ),
        ?_assertAsmEqual(
            <<16#d1000421:32/little>>, "sub x1, x1, #1", jit_aarch64_asm:sub(r1, r1, 1)
        ),
        ?_assertAsmEqual(
            <<16#cb031041:32/little>>,
            "sub x1, x2, x3, lsl #4",
            jit_aarch64_asm:sub(r1, r2, r3, {lsl, 4})
        ),
        ?_assertAsmEqual(
            <<16#cb030041:32/little>>, "sub x1, x2, x3", jit_aarch64_asm:sub(r1, r2, r3)
        )
    ].

madd_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9b037c41:32/little>>, "mul x1, x2, x3", jit_aarch64_asm:mul(r1, r2, r3)
        ),
        ?_assertAsmEqual(
            <<16#9b031041:32/little>>, "madd x1, x2, x3, x4", jit_aarch64_asm:madd(r1, r2, r3, r4)
        )
    ].

b_test_() ->
    [
        ?_assertAsmEqual(<<16#14000000:32/little>>, "b .+0", jit_aarch64_asm:b(0)),
        ?_assertAsmEqual(<<16#14000004:32/little>>, "b .+16", jit_aarch64_asm:b(16)),
        ?_assertAsmEqual(<<16#17fffff0:32/little>>, "b .-64", jit_aarch64_asm:b(-64)),
        ?_assertAsmEqual(<<16#14000001:32/little>>, "b .+4", jit_aarch64_asm:b(4))
    ].

brk_test_() ->
    [
        ?_assertAsmEqual(<<16#D4200000:32/little>>, "brk #0", jit_aarch64_asm:brk(0)),
        ?_assertAsmEqual(<<16#D4201900:32/little>>, "brk #200", jit_aarch64_asm:brk(200))
    ].

blr_test_() ->
    [
        ?_assertAsmEqual(<<16#D63F0000:32/little>>, "blr x0", jit_aarch64_asm:blr(r0)),
        ?_assertAsmEqual(<<16#D63F0020:32/little>>, "blr x1", jit_aarch64_asm:blr(r1)),
        ?_assertAsmEqual(<<16#D63F01A0:32/little>>, "blr x13", jit_aarch64_asm:blr(r13))
    ].

br_test_() ->
    [
        ?_assertAsmEqual(<<16#D61F0000:32/little>>, "br x0", jit_aarch64_asm:br(r0)),
        ?_assertAsmEqual(<<16#D61F0020:32/little>>, "br x1", jit_aarch64_asm:br(r1)),
        ?_assertAsmEqual(<<16#D61F01A0:32/little>>, "br x13", jit_aarch64_asm:br(r13))
    ].

ldr_test_() ->
    [
        ?_assertAsmEqual(
            <<16#F9400421:32/little>>, "ldr x1, [x1, #8]", jit_aarch64_asm:ldr(r1, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#F9403042:32/little>>, "ldr x2, [x2, #96]", jit_aarch64_asm:ldr(r2, {r2, 96})
        ),
        % Load-update (writeback) with SP, negative offset
        ?_assertAsmEqual(
            <<16#F85F0FE7:32/little>>,
            "ldr x7, [sp, #-16]!",
            jit_aarch64_asm:ldr(r7, {sp, -16}, '!')
        ),
        % Load-update (writeback) with SP, positive offset
        ?_assertAsmEqual(
            <<16#F8410FE7:32/little>>, "ldr x7, [sp, #16]!", jit_aarch64_asm:ldr(r7, {sp, 16}, '!')
        ),
        % Load-update (writeback) with SP, zero offset
        ?_assertAsmEqual(
            <<16#F84007E7:32/little>>, "ldr x7, [sp], #0", jit_aarch64_asm:ldr(r7, {sp}, 0)
        ),
        % shift
        ?_assertAsmEqual(
            <<16#f8637841:32/little>>,
            "ldr x1, [x2, x3, lsl #3]",
            jit_aarch64_asm:ldr(r1, {r2, r3, lsl, 3})
        ),
        ?_assertAsmEqual(
            <<16#f8677907:32/little>>,
            "ldr x7, [x8, x7, lsl #3]",
            jit_aarch64_asm:ldr(r7, {r8, r7, lsl, 3})
        ),
        ?_assertAsmEqual(
            <<16#f8636841:32/little>>, "ldr x1, [x2, x3]", jit_aarch64_asm:ldr(r1, {r2, r3})
        )
    ].

ldr_w_test_() ->
    [
        ?_assertAsmEqual(
            <<16#b9400821:32/little>>, "ldr w1, [x1, 8]", jit_aarch64_asm:ldr_w(r1, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#b9406042:32/little>>, "ldr w2, [x2, 96]", jit_aarch64_asm:ldr_w(r2, {r2, 96})
        ),
        ?_assertAsmEqual(
            <<16#b97ffc60:32/little>>, "ldr w0, [x3, 16380]", jit_aarch64_asm:ldr_w(r0, {r3, 16380})
        )
    ].

str_w_test_() ->
    [
        ?_assertAsmEqual(
            <<16#b9000821:32/little>>, "str w1, [x1, 8]", jit_aarch64_asm:str_w(r1, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#b9006042:32/little>>, "str w2, [x2, 96]", jit_aarch64_asm:str_w(r2, {r2, 96})
        ),
        ?_assertAsmEqual(
            <<16#b93ffc60:32/little>>, "str w0, [x3, 16380]", jit_aarch64_asm:str_w(r0, {r3, 16380})
        )
    ].

mov_test_() ->
    [
        % mov immediate - simple cases
        ?_assertAsmEqual(<<16#D2800000:32/little>>, "mov x0, #0", jit_aarch64_asm:mov(r0, 0)),
        ?_assertAsmEqual(<<16#D2801901:32/little>>, "mov x1, #200", jit_aarch64_asm:mov(r1, 200)),
        ?_assertAsmEqual(<<16#d28000b3:32/little>>, "mov x19, #5", jit_aarch64_asm:mov(r19, 5)),
        ?_assertAsmEqual(<<16#92800094:32/little>>, "mov x20, #-5", jit_aarch64_asm:mov(r20, -5)),
        ?_assertAsmEqual(<<16#d2800015:32/little>>, "mov x21, #0", jit_aarch64_asm:mov(r21, 0)),
        ?_assertAsmEqual(
            <<16#d29ffff0:32/little>>, "mov x16, #0xffff", jit_aarch64_asm:mov(r16, 16#FFFF)
        ),
        ?_assertAsmEqual(
            <<16#929fffcf:32/little>>, "mov x15, #-0xffff", jit_aarch64_asm:mov(r15, -16#FFFF)
        ),

        % mov immediate - complex cases requiring multiple instructions
        ?_assertAsmEqual(
            <<16#d2a00052:32/little>>, "mov x18, #0x20000", jit_aarch64_asm:mov(r18, 16#20000)
        ),
        ?_assertAsmEqual(
            <<16#b26fbbf1:32/little>>, "mov x17, #-0x20000", jit_aarch64_asm:mov(r17, -131072)
        ),

        % mov immediate - very large value requiring multiple instructions
        ?_assertAsmEqualLargeInt(
            <<16#D29579A1:32/little, 16#F2B7C041:32/little, 16#F2DFD741:32/little,
                16#F2EFF941:32/little>>,
            "mov x1, #0xabcd\n"
            "movk x1, #0xbe02, lsl #16\n"
            "movk x1, #0xfeba, lsl #32\n"
            "movk x1, #0x7fca, lsl #48",
            jit_aarch64_asm:mov(r1, ?MODULE:list_to_integer("9208452466117618637"))
        ),

        % mov register
        ?_assertAsmEqual(<<16#AA0103E0:32/little>>, "mov x0, x1", jit_aarch64_asm:mov(r0, r1)),
        ?_assertAsmEqual(<<16#AA0703E1:32/little>>, "mov x1, x7", jit_aarch64_asm:mov(r1, r7)),

        %% Test mov with zero immediate (should use movz with 0)
        ?_assertAsmEqual(
            <<16#d2800000:32/little>>, "movz x0, #0", jit_aarch64_asm:mov(r0, 0)
        ),

        %% Test 4-bit pattern encoding
        ?_assertAsmEqual(
            <<16#929fffe0:32/little>>,
            "mov x0, #-65536",
            jit_aarch64_asm:mov(r0, -65536)
        ),
        %% Test complex immediate that will use fallback sequence
        ?_assertAsmEqualLargeInt(
            <<
                16#d29bde00:32/little,
                16#f2b35780:32/little,
                16#f2cacf00:32/little,
                16#f2e24680:32/little
            >>,
            "mov x0, #0xdef0\n"
            "movk x0, #0x9abc, lsl #16\n"
            "movk x0, #0x5678, lsl #32\n"
            "movk x0, #0x1234, lsl #48",
            jit_aarch64_asm:mov(r0, ?MODULE:list_to_integer("123456789ABCDEF0", 16))
        ),

        %% Test negative immediate that uses build_negative_immediate fallback
        ?_assertAsmEqualLargeInt(
            <<
                16#d2842200:32/little,
                16#f2aca860:32/little,
                16#f2d530e0:32/little,
                16#f2fdb960:32/little
            >>,
            "mov	x0, #0x2110\n"
            "movk	x0, #0x6543, lsl #16\n"
            "movk	x0, #0xa987, lsl #32\n"
            "movk	x0, #0xedcb, lsl #48",
            jit_aarch64_asm:mov(r0, ?MODULE:list_to_integer("-123456789ABCDEF0", 16))
        ),

        %% Test bitmask patterns with different sizes
        %% Size 16 pattern: repeats every 16 bits
        ?_assertAsmEqualLargeInt(
            <<16#b20083e0:32/little>>,
            "mov	x0, #0x0001000100010001",
            jit_aarch64_asm:mov(r0, ?MODULE:list_to_integer("0001000100010001", 16))
        ),
        %% Size 4 pattern: repeats every 4 bits
        ?_assertAsmEqualLargeInt(
            <<16#b200e7e0:32/little>>,
            "mov	x0, #0x3333333333333333",
            jit_aarch64_asm:mov(r0, ?MODULE:list_to_integer("3333333333333333", 16))
        ),
        %% Size 2 pattern: repeats every 2 bits
        ?_assertAsmEqualLargeInt(
            <<16#b200f3e0:32/little>>,
            "mov	x0, #0x5555555555555555",
            jit_aarch64_asm:mov(r0, ?MODULE:list_to_integer("5555555555555555", 16))
        )
    ].

orr_test_() ->
    [
        % ORR Rd, XZR, Rm (MOV)
        ?_assertAsmEqual(
            <<16#AA0103E0:32/little>>, "orr x0, xzr, x1", jit_aarch64_asm:orr(r0, xzr, r1)
        ),
        % ORR Rd, Rn, Rm
        ?_assertAsmEqual(
            <<16#AA010020:32/little>>, "orr x0, x1, x1", jit_aarch64_asm:orr(r0, r1, r1)
        ),
        ?_assertAsmEqual(
            <<16#AA020041:32/little>>, "orr x1, x2, x2", jit_aarch64_asm:orr(r1, r2, r2)
        ),

        %% Test orr with valid bitmask immediate
        ?_assertAsmEqual(
            <<16#b24007e0:32/little>>, "orr x0, xzr, #0x3", jit_aarch64_asm:orr(r0, xzr, 16#3)
        ),
        %% Test orr with another bitmask pattern
        ?_assertAsmEqual(
            <<16#b27f1fe0:32/little>>, "orr x0, xzr, #0x1fe", jit_aarch64_asm:orr(r0, xzr, 16#1fe)
        ),

        %% Test orr with unencodable immediate
        ?_assertError({unencodable_immediate, 16#123456}, jit_aarch64_asm:orr(r0, r0, 16#123456)),

        %% Test orr with -1 (all ones) - should be unencodable as bitmask immediate
        ?_assertError({unencodable_immediate, -1}, jit_aarch64_asm:orr(r8, xzr, -1)),

        %% Test orr with 0 (all zeros) - should be unencodable as bitmask immediate
        ?_assertError({unencodable_immediate, 0}, jit_aarch64_asm:orr(r8, xzr, 0))
    ].

str_test_() ->
    [
        ?_assertAsmEqual(
            <<16#F9000421:32/little>>, "str x1, [x1, #8]", jit_aarch64_asm:str(r1, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#F9003042:32/little>>, "str x2, [x2, #96]", jit_aarch64_asm:str(r2, {r2, 96})
        ),
        % str with xzr (zero register) - stores zero to memory
        ?_assertAsmEqual(
            <<16#F900001F:32/little>>, "str xzr, [x0]", jit_aarch64_asm:str(xzr, {r0, 0})
        ),
        ?_assertAsmEqual(
            <<16#F900043F:32/little>>, "str xzr, [x1, #8]", jit_aarch64_asm:str(xzr, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#F900085F:32/little>>, "str xzr, [x2, #16]", jit_aarch64_asm:str(xzr, {r2, 16})
        ),
        % Store-update (writeback) with SP
        ?_assertAsmEqual(
            <<16#F81F0FE7:32/little>>,
            "str x7, [sp, #-16]!",
            jit_aarch64_asm:str(r7, {sp, -16}, '!')
        ),
        % Store-update (writeback) with SP, positive offset
        ?_assertAsmEqual(
            <<16#F8010FE7:32/little>>, "str x7, [sp, #16]!", jit_aarch64_asm:str(r7, {sp, 16}, '!')
        ),
        % Store-update (writeback) with SP, zero offset
        ?_assertAsmEqual(
            <<16#F80007E7:32/little>>, "str x7, [sp], #0", jit_aarch64_asm:str(r7, {sp}, 0)
        ),
        % shift
        ?_assertAsmEqual(
            <<16#f8237841:32/little>>,
            "str x1, [x2, x3, lsl #3]",
            jit_aarch64_asm:str(r1, {r2, r3, lsl, 3})
        )
    ].

cmp_test_() ->
    [
        % cmp reg, reg
        ?_assertAsmEqual(<<16#EB01001F:32/little>>, "cmp x0, x1", jit_aarch64_asm:cmp(r0, r1)),
        % cmp reg, imm
        ?_assertAsmEqual(<<16#F100001F:32/little>>, "cmp x0, #0", jit_aarch64_asm:cmp(r0, 0)),
        ?_assertAsmEqual(<<16#F103001F:32/little>>, "cmp x0, #192", jit_aarch64_asm:cmp(r0, 192)),

        %% Test large immediate compare (uses temporary register)
        ?_assertAsmEqual(
            <<
                16#d28acf10:32/little,
                16#f2a24690:32/little,
                16#eb10001f:32/little
            >>,
            "mov x16, #0x5678\n"
            "movk x16, #0x1234, lsl #16\n"
            "cmp x0, x16",
            jit_aarch64_asm:cmp(r0, 16#12345678)
        ),

        %% Test negative immediate compare (uses MOVN)
        ?_assertAsmEqual(
            <<
                16#92800010:32/little,
                16#eb1000ff:32/little
            >>,
            "movn x16, #0\n"
            "cmp x7, x16",
            jit_aarch64_asm:cmp(r7, -1)
        )
    ].

cmp_w_test_() ->
    [
        % cmp_w reg, imm
        ?_assertAsmEqual(<<16#7100001F:32/little>>, "cmp w0, #0", jit_aarch64_asm:cmp_w(r0, 0)),
        ?_assertAsmEqual(<<16#7103001F:32/little>>, "cmp w0, #192", jit_aarch64_asm:cmp_w(r0, 192)),

        %% Test 32-bit compare with negative immediate
        ?_assertAsmEqual(
            <<16#3100041f:32/little>>, "adds wzr, w0, #1", jit_aarch64_asm:cmp_w(r0, -1)
        ),
        ?_assertAsmEqual(
            <<16#31000c1f:32/little>>, "adds wzr, w0, #3", jit_aarch64_asm:cmp_w(r0, -3)
        )
    ].

and_test_() ->
    [
        % AND reg, reg, reg
        ?_assertAsmEqual(
            <<16#8A010020:32/little>>, "and x0, x1, x1", jit_aarch64_asm:and_(r0, r1, r1)
        ),
        % AND reg, reg, imm
        ?_assertAsmEqual(
            <<16#927A0420:32/little>>, "and x0, x1, #0xc0", jit_aarch64_asm:and_(r0, r1, 192)
        ),
        ?_assertAsmEqual(
            <<16#927ff8e7:32/little>>,
            "and x7, x7, #0xfffffffffffffffe",
            jit_aarch64_asm:and_(r7, r7, -2)
        ),
        ?_assertAsmEqual(
            <<16#9200cc41:32/little>>,
            "and x1, x2, #0xf0f0f0f0f0f0f0f",
            jit_aarch64_asm:and_(r1, r2, 16#f0f0f0f0f0f0f0f)
        ),
        ?_assertAsmEqual(
            <<16#92603c62:32/little>>,
            "and x2, x3, #0xffff00000000",
            jit_aarch64_asm:and_(r2, r3, 16#ffff00000000)
        ),
        ?_assertAsmEqual(
            <<16#92785c83:32/little>>,
            "and x3, x4, #0xffffff00",
            jit_aarch64_asm:and_(r3, r4, 16#ffffff00)
        ),
        %% Test and_ with unencodable immediate
        ?_assertError(
            {unencodable_immediate, 16#123456}, jit_aarch64_asm:and_(r0, r0, 16#123456)
        )
    ].

lsl_test_() ->
    [
        ?_assertAsmEqual(
            <<16#D3607C00:32/little>>, "lsl x0, x0, #32", jit_aarch64_asm:lsl(r0, r0, 32)
        )
    ].

lsr_test_() ->
    [
        ?_assertAsmEqual(
            <<16#D340FC00:32/little>>, "lsr x0, x0, 0", jit_aarch64_asm:lsr(r0, r0, 0)
        ),
        ?_assertAsmEqual(
            <<16#D340FC01:32/little>>, "lsr x1, x0, 0", jit_aarch64_asm:lsr(r1, r0, 0)
        ),
        ?_assertAsmEqual(
            <<16#D360FC00:32/little>>, "lsr x0, x0, #32", jit_aarch64_asm:lsr(r0, r0, 32)
        )
    ].

ret_test_() ->
    [
        ?_assertAsmEqual(<<16#D65F03C0:32/little>>, "ret", jit_aarch64_asm:ret())
    ].

tst_test_() ->
    [
        ?_assertAsmEqual(<<16#EA01001F:32/little>>, "tst x0, x1", jit_aarch64_asm:tst(r0, r1)),
        ?_assertAsmEqual(<<16#f240003f:32/little>>, "tst x1, #1", jit_aarch64_asm:tst(r1, 1)),
        ?_assertAsmEqual(<<16#f27c005f:32/little>>, "tst x2, #16", jit_aarch64_asm:tst(r2, 16)),
        ?_assertAsmEqual(<<16#f2401c7f:32/little>>, "tst x3, #255", jit_aarch64_asm:tst(r3, 255)),
        ?_assertAsmEqual(<<16#f240249f:32/little>>, "tst x4, #1023", jit_aarch64_asm:tst(r4, 1023)),
        ?_assertAsmEqual(<<16#f24014bf:32/little>>, "tst x5, #63", jit_aarch64_asm:tst(r5, 63)),
        ?_assertAsmEqual(<<16#f27b00df:32/little>>, "tst x6, #32", jit_aarch64_asm:tst(r6, 32)),
        ?_assertAsmEqual(<<16#f27a00ff:32/little>>, "tst x7, #64", jit_aarch64_asm:tst(r7, 64)),
        ?_assertAsmEqual(<<16#f27e051f:32/little>>, "tst x8, #0xc", jit_aarch64_asm:tst(r8, 16#c)),

        %% Test tst with unencodable immediate
        ?_assertError({unencodable_immediate, 16#123456}, jit_aarch64_asm:tst(r0, 16#123456))
    ].

tst_w_test_() ->
    [
        ?_assertAsmEqual(<<16#6a01001f:32/little>>, "tst w0, w1", jit_aarch64_asm:tst_w(r0, r1)),
        ?_assertAsmEqual(<<16#7200003f:32/little>>, "tst w1, #1", jit_aarch64_asm:tst_w(r1, 1)),
        ?_assertAsmEqual(<<16#721c005f:32/little>>, "tst w2, #16", jit_aarch64_asm:tst_w(r2, 16)),
        ?_assertAsmEqual(<<16#72001c7f:32/little>>, "tst w3, #255", jit_aarch64_asm:tst_w(r3, 255)),
        ?_assertAsmEqual(
            <<16#7200249f:32/little>>, "tst w4, #1023", jit_aarch64_asm:tst_w(r4, 1023)
        ),
        ?_assertAsmEqual(<<16#720014bf:32/little>>, "tst w5, #63", jit_aarch64_asm:tst_w(r5, 63)),
        ?_assertAsmEqual(<<16#721b00df:32/little>>, "tst w6, #32", jit_aarch64_asm:tst_w(r6, 32)),
        ?_assertAsmEqual(<<16#721a00ff:32/little>>, "tst w7, #64", jit_aarch64_asm:tst_w(r7, 64)),
        ?_assertAsmEqual(
            <<16#721e051f:32/little>>, "tst w8, #0xc", jit_aarch64_asm:tst_w(r8, 16#c)
        ),

        %% Test tst_w with unencodable immediate
        ?_assertError({unencodable_immediate, 16#123456}, jit_aarch64_asm:tst_w(r0, 16#123456))
    ].

bcc_test_() ->
    [
        ?_assertAsmEqual(<<16#54000000:32/little>>, "b.eq .+0", jit_aarch64_asm:bcc(eq, 0)),
        ?_assertAsmEqual(<<16#54000001:32/little>>, "b.ne .+0", jit_aarch64_asm:bcc(ne, 0)),
        ?_assertAsmEqual(<<16#54fffe01:32/little>>, "b.ne .-64", jit_aarch64_asm:bcc(ne, -64)),
        ?_assertAsmEqual(<<16#54000400:32/little>>, "b.eq 128", jit_aarch64_asm:bcc(eq, 128)),
        ?_assertAsmEqual(<<16#54000402:32/little>>, "b.cs 128", jit_aarch64_asm:bcc(cs, 128)),
        ?_assertAsmEqual(<<16#54000403:32/little>>, "b.cc 128", jit_aarch64_asm:bcc(cc, 128)),
        ?_assertAsmEqual(<<16#54000404:32/little>>, "b.mi 128", jit_aarch64_asm:bcc(mi, 128)),
        ?_assertAsmEqual(<<16#54000405:32/little>>, "b.pl 128", jit_aarch64_asm:bcc(pl, 128)),
        ?_assertAsmEqual(<<16#54000406:32/little>>, "b.vs 128", jit_aarch64_asm:bcc(vs, 128)),
        ?_assertAsmEqual(<<16#54000408:32/little>>, "b.hi 128", jit_aarch64_asm:bcc(hi, 128)),
        ?_assertAsmEqual(<<16#54000409:32/little>>, "b.ls 128", jit_aarch64_asm:bcc(ls, 128)),
        ?_assertAsmEqual(<<16#5400040a:32/little>>, "b.ge 128", jit_aarch64_asm:bcc(ge, 128)),
        ?_assertAsmEqual(<<16#5400040b:32/little>>, "b.lt 128", jit_aarch64_asm:bcc(lt, 128)),
        ?_assertAsmEqual(<<16#5400040c:32/little>>, "b.gt 128", jit_aarch64_asm:bcc(gt, 128)),
        ?_assertAsmEqual(<<16#5400040d:32/little>>, "b.le 128", jit_aarch64_asm:bcc(le, 128)),
        ?_assertAsmEqual(<<16#5400040e:32/little>>, "b.al 128", jit_aarch64_asm:bcc(al, 128)),
        ?_assertAsmEqual(<<16#5400040f:32/little>>, "b.nv 128", jit_aarch64_asm:bcc(nv, 128)),
        ?_assertAsmEqual(<<16#54000007:32/little>>, "b.vc .+0", jit_aarch64_asm:bcc(vc, 0))
    ].

cbnz_test_() ->
    [
        ?_assertAsmEqual(<<16#b5000401:32/little>>, "cbnz x1, 128", jit_aarch64_asm:cbnz(r1, 128)),
        ?_assertAsmEqual(
            <<16#35000402:32/little>>, "cbnz w2, 128", jit_aarch64_asm:cbnz_w(r2, 128)
        ),
        ?_assertAsmEqual(<<16#b5fffc03:32/little>>, "cbnz x3, -128", jit_aarch64_asm:cbnz(r3, -128))
    ].

tbz_test_() ->
    [
        ?_assertAsmEqual(
            <<16#b6f80400:32/little>>, "tbz x0, #63, 128", jit_aarch64_asm:tbz(r0, 63, 128)
        ),
        ?_assertAsmEqual(
            <<16#36180400:32/little>>, "tbz x0, #3, 128", jit_aarch64_asm:tbz(r0, 3, 128)
        ),
        ?_assertAsmEqual(
            <<16#363ffc03:32/little>>, "tbz x3, #7, -128", jit_aarch64_asm:tbz(r3, 7, -128)
        )
    ].

tbnz_test_() ->
    [
        ?_assertAsmEqual(
            <<16#37000400:32/little>>, "tbnz x0, #0, 128", jit_aarch64_asm:tbnz(r0, 0, 128)
        ),
        ?_assertAsmEqual(
            <<16#37180400:32/little>>, "tbnz x0, #3, 128", jit_aarch64_asm:tbnz(r0, 3, 128)
        ),
        ?_assertAsmEqual(
            <<16#373ffc03:32/little>>, "tbnz x3, #7, -128", jit_aarch64_asm:tbnz(r3, 7, -128)
        )
    ].

stp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#a8815113:32/little>>,
            "stp x19, x20, [x8], #16",
            jit_aarch64_asm:stp(r19, r20, {r8}, 16)
        ),
        ?_assertAsmEqual(
            <<16#a88153f3:32/little>>,
            "stp x19, x20, [sp], #16",
            jit_aarch64_asm:stp(r19, r20, {sp}, 16)
        ),
        % Store-update (writeback) variants
        ?_assertAsmEqual(
            <<16#a9bf27e8:32/little>>,
            "stp x8, x9, [sp, #-16]!",
            jit_aarch64_asm:stp(r8, r9, {sp, -16}, '!')
        ),
        ?_assertAsmEqual(
            <<16#a98127e8:32/little>>,
            "stp x8, x9, [sp, #16]!",
            jit_aarch64_asm:stp(r8, r9, {sp, 16}, '!')
        ),
        ?_assertAsmEqual(
            <<16#a98027e8:32/little>>,
            "stp x8, x9, [sp, #0]!",
            jit_aarch64_asm:stp(r8, r9, {sp, 0}, '!')
        )
    ].

ldp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#a8c15113:32/little>>,
            "ldp x19, x20, [x8], #16",
            jit_aarch64_asm:ldp(r19, r20, {r8}, 16)
        ),
        ?_assertAsmEqual(
            <<16#a8c153f3:32/little>>,
            "ldp x19, x20, [sp], #16",
            jit_aarch64_asm:ldp(r19, r20, {sp}, 16)
        )
    ].

subs_test_() ->
    [
        % SUBS with immediate
        ?_assertAsmEqual(
            <<16#F1000021:32/little>>, "subs x1, x1, #0", jit_aarch64_asm:subs(r1, r1, 0)
        ),
        ?_assertAsmEqual(
            <<16#F1000421:32/little>>, "subs x1, x1, #1", jit_aarch64_asm:subs(r1, r1, 1)
        ),
        % SUBS with register
        ?_assertAsmEqual(
            <<16#eb000021:32/little>>, "subs x1, x1, x0", jit_aarch64_asm:subs(r1, r1, r0)
        ),
        ?_assertAsmEqual(
            <<16#eb0a0021:32/little>>, "subs x1, x1, x10", jit_aarch64_asm:subs(r1, r1, r10)
        )
    ].

adr_test_() ->
    [
        %% ADR x0, #0
        ?_assertAsmEqual(<<16#10000000:32/little>>, "adr x0, .+0", jit_aarch64_asm:adr(r0, 0)),
        %% ADR x1, #4
        ?_assertAsmEqual(<<16#10000021:32/little>>, "adr x1, .+4", jit_aarch64_asm:adr(r1, 4)),
        %% ADR x2, #-4
        ?_assertAsmEqual(<<16#10ffffe2:32/little>>, "adr x2, .-4", jit_aarch64_asm:adr(r2, -4)),
        %% ADR x3, #1048572 (max positive)
        ?_assertAsmEqual(
            <<16#107fffe3:32/little>>, "adr x3, .+1048572", jit_aarch64_asm:adr(r3, 1048572)
        ),
        %% ADR x4, #-1048576 (max negative)
        ?_assertAsmEqual(
            <<16#10800004:32/little>>, "adr x4, .-1048576", jit_aarch64_asm:adr(r4, -1048576)
        ),
        %% ADR with offset not a multiple of 4 is valid
        ?_assertAsmEqual(<<16#70000000:32/little>>, "adr x0, .+3", jit_aarch64_asm:adr(r0, 3))
    ].

%% Test nop instruction
nop_test_() ->
    [
        ?_assertAsmEqual(
            <<16#d503201f:32/little>>, "nop", jit_aarch64_asm:nop()
        )
    ].
