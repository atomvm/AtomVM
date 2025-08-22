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

add_test_() ->
    [
        ?_assertEqual(
            asm(<<16#9100e0e7:32/little>>, "add x7, x7, #56"), jit_aarch64_asm:add(r7, r7, 56)
        ),
        ?_assertEqual(
            asm(<<16#91000000:32/little>>, "add x0, x0, #0"), jit_aarch64_asm:add(r0, r0, 0)
        ),
        ?_assertEqual(
            asm(<<16#91000421:32/little>>, "add x1, x1, #1"), jit_aarch64_asm:add(r1, r1, 1)
        ),
        ?_assertEqual(
            asm(<<16#8b031041:32/little>>, "add x1, x2, x3, lsl #4"),
            jit_aarch64_asm:add(r1, r2, r3, {lsl, 4})
        ),
        ?_assertEqual(
            asm(<<16#8b030041:32/little>>, "add x1, x2, x3"), jit_aarch64_asm:add(r1, r2, r3)
        )
    ].

sub_test_() ->
    [
        ?_assertEqual(
            asm(<<16#d100e0e7:32/little>>, "sub x7, x7, #56"), jit_aarch64_asm:sub(r7, r7, 56)
        ),
        ?_assertEqual(
            asm(<<16#d1000000:32/little>>, "sub x0, x0, #0"), jit_aarch64_asm:sub(r0, r0, 0)
        ),
        ?_assertEqual(
            asm(<<16#d1000421:32/little>>, "sub x1, x1, #1"), jit_aarch64_asm:sub(r1, r1, 1)
        ),
        ?_assertEqual(
            asm(<<16#cb031041:32/little>>, "sub x1, x2, x3, lsl #4"),
            jit_aarch64_asm:sub(r1, r2, r3, {lsl, 4})
        ),
        ?_assertEqual(
            asm(<<16#cb030041:32/little>>, "sub x1, x2, x3"), jit_aarch64_asm:sub(r1, r2, r3)
        )
    ].

madd_test_() ->
    [
        ?_assertEqual(
            asm(<<16#9b037c41:32/little>>, "mul x1, x2, x3"), jit_aarch64_asm:mul(r1, r2, r3)
        ),
        ?_assertEqual(
            asm(<<16#9b031041:32/little>>, "madd x1, x2, x3, x4"),
            jit_aarch64_asm:madd(r1, r2, r3, r4)
        )
    ].

b_test_() ->
    [
        ?_assertEqual(<<16#14000000:32/little>>, jit_aarch64_asm:b(0)),
        ?_assertEqual(<<16#14000004:32/little>>, jit_aarch64_asm:b(16)),
        ?_assertEqual(<<16#17fffff0:32/little>>, jit_aarch64_asm:b(-64)),
        ?_assertEqual(<<16#14000001:32/little>>, jit_aarch64_asm:b(4))
    ].

brk_test_() ->
    [
        ?_assertEqual(<<16#D4200000:32/little>>, jit_aarch64_asm:brk(0)),
        ?_assertEqual(<<16#D4201900:32/little>>, jit_aarch64_asm:brk(200))
    ].

blr_test_() ->
    [
        ?_assertEqual(<<16#D63F0000:32/little>>, jit_aarch64_asm:blr(r0)),
        ?_assertEqual(<<16#D63F0020:32/little>>, jit_aarch64_asm:blr(r1)),
        ?_assertEqual(<<16#D63F01A0:32/little>>, jit_aarch64_asm:blr(r13))
    ].

br_test_() ->
    [
        ?_assertEqual(<<16#D61F0000:32/little>>, jit_aarch64_asm:br(r0)),
        ?_assertEqual(<<16#D61F0020:32/little>>, jit_aarch64_asm:br(r1)),
        ?_assertEqual(<<16#D61F01A0:32/little>>, jit_aarch64_asm:br(r13))
    ].

ldr_test_() ->
    [
        ?_assertEqual(<<16#F9400421:32/little>>, jit_aarch64_asm:ldr(r1, {r1, 8})),
        ?_assertEqual(<<16#F9403042:32/little>>, jit_aarch64_asm:ldr(r2, {r2, 96})),
        % Load-update (writeback) with SP, negative offset
        ?_assertEqual(
            <<16#F85F0FE7:32/little>>,
            jit_aarch64_asm:ldr(r7, {sp, -16}, '!')
        ),
        % Load-update (writeback) with SP, positive offset
        ?_assertEqual(
            <<16#F8410FE7:32/little>>,
            jit_aarch64_asm:ldr(r7, {sp, 16}, '!')
        ),
        % Load-update (writeback) with SP, zero offset
        ?_assertEqual(
            <<16#F84007E7:32/little>>,
            jit_aarch64_asm:ldr(r7, {sp}, 0)
        ),
        % shift
        ?_assertEqual(
            <<16#f8637841:32/little>>,
            jit_aarch64_asm:ldr(r1, {r2, r3, lsl, 3})
        ),
        ?_assertEqual(
            <<16#f8677907:32/little>>,
            jit_aarch64_asm:ldr(r7, {r8, r7, lsl, 3})
        ),
        ?_assertEqual(
            <<16#f8636841:32/little>>,
            jit_aarch64_asm:ldr(r1, {r2, r3})
        )
    ].

ldr_w_test_() ->
    [
        ?_assertEqual(
            asm(<<16#b9400821:32/little>>, "ldr w1, [x1, 8]"), jit_aarch64_asm:ldr_w(r1, {r1, 8})
        ),
        ?_assertEqual(
            asm(<<16#b9406042:32/little>>, "ldr w2, [x2, 96]"), jit_aarch64_asm:ldr_w(r2, {r2, 96})
        ),
        ?_assertEqual(
            asm(<<16#b97ffc60:32/little>>, "ldr w0, [x3, 16380]"),
            jit_aarch64_asm:ldr_w(r0, {r3, 16380})
        )
    ].

str_w_test_() ->
    [
        ?_assertEqual(
            asm(<<16#b9000821:32/little>>, "str w1, [x1, 8]"), jit_aarch64_asm:str_w(r1, {r1, 8})
        ),
        ?_assertEqual(
            asm(<<16#b9006042:32/little>>, "str w2, [x2, 96]"), jit_aarch64_asm:str_w(r2, {r2, 96})
        ),
        ?_assertEqual(
            asm(<<16#b93ffc60:32/little>>, "str w0, [x3, 16380]"),
            jit_aarch64_asm:str_w(r0, {r3, 16380})
        )
    ].

ldr_d_test_() ->
    [
        ?_assertEqual(
            asm(<<16#fd40001e:32/little>>, "ldr d30, [x0]"), jit_aarch64_asm:ldr_d(v30, {r0, 0})
        ),
        ?_assertEqual(
            asm(<<16#fd400420:32/little>>, "ldr d0, [x1, #8]"), jit_aarch64_asm:ldr_d(v0, {r1, 8})
        )
    ].

mov_test_() ->
    [
        % mov immediate - simple cases
        ?_assertEqual(<<16#D2800000:32/little>>, jit_aarch64_asm:mov(r0, 0)),
        ?_assertEqual(<<16#D2801901:32/little>>, jit_aarch64_asm:mov(r1, 200)),
        ?_assertEqual(<<16#d28000b3:32/little>>, jit_aarch64_asm:mov(r19, 5)),
        ?_assertEqual(<<16#92800094:32/little>>, jit_aarch64_asm:mov(r20, -5)),
        ?_assertEqual(<<16#d2800015:32/little>>, jit_aarch64_asm:mov(r21, 0)),
        ?_assertEqual(<<16#d29ffff0:32/little>>, jit_aarch64_asm:mov(r16, 16#FFFF)),
        ?_assertEqual(<<16#929fffcf:32/little>>, jit_aarch64_asm:mov(r15, -16#FFFF)),

        % mov immediate - complex cases requiring multiple instructions
        ?_assertEqual(<<16#d2a00052:32/little>>, jit_aarch64_asm:mov(r18, 16#20000)),
        ?_assertEqual(<<16#b26fbbf1:32/little>>, jit_aarch64_asm:mov(r17, -131072)),

        % mov immediate - very large value requiring multiple instructions
        ?_assertEqual(
            <<16#D29579A1:32/little, 16#F2B7C041:32/little, 16#F2DFD741:32/little,
                16#F2EFF941:32/little>>,
            jit_aarch64_asm:mov(r1, 9208452466117618637)
        ),

        % mov register
        ?_assertEqual(<<16#AA0103E0:32/little>>, jit_aarch64_asm:mov(r0, r1)),
        ?_assertEqual(<<16#AA0703E1:32/little>>, jit_aarch64_asm:mov(r1, r7))
    ].

orr_test_() ->
    [
        % ORR Rd, XZR, Rm (MOV)
        ?_assertEqual(<<16#AA0103E0:32/little>>, jit_aarch64_asm:orr(r0, xzr, r1)),
        % ORR Rd, Rn, Rm
        ?_assertEqual(<<16#AA010020:32/little>>, jit_aarch64_asm:orr(r0, r1, r1)),
        ?_assertEqual(<<16#AA020041:32/little>>, jit_aarch64_asm:orr(r1, r2, r2))
    ].

str_test_() ->
    [
        ?_assertEqual(<<16#F9000421:32/little>>, jit_aarch64_asm:str(r1, {r1, 8})),
        ?_assertEqual(<<16#F9003042:32/little>>, jit_aarch64_asm:str(r2, {r2, 96})),
        % str with xzr (zero register) - stores zero to memory
        ?_assertEqual(<<16#F900001F:32/little>>, jit_aarch64_asm:str(xzr, {r0, 0})),
        ?_assertEqual(<<16#F900043F:32/little>>, jit_aarch64_asm:str(xzr, {r1, 8})),
        ?_assertEqual(<<16#F900085F:32/little>>, jit_aarch64_asm:str(xzr, {r2, 16})),
        % Store-update (writeback) with SP
        ?_assertEqual(
            <<16#F81F0FE7:32/little>>,
            jit_aarch64_asm:str(r7, {sp, -16}, '!')
        ),
        % Store-update (writeback) with SP, positive offset
        ?_assertEqual(
            <<16#F8010FE7:32/little>>,
            jit_aarch64_asm:str(r7, {sp, 16}, '!')
        ),
        % Store-update (writeback) with SP, zero offset
        ?_assertEqual(
            <<16#F80007E7:32/little>>,
            jit_aarch64_asm:str(r7, {sp}, 0)
        ),
        % shift
        ?_assertEqual(
            <<16#f8237841:32/little>>,
            jit_aarch64_asm:str(r1, {r2, r3, lsl, 3})
        )
    ].

cmp_test_() ->
    [
        % cmp reg, reg
        ?_assertEqual(<<16#EB01001F:32/little>>, jit_aarch64_asm:cmp(r0, r1)),
        % cmp reg, imm
        ?_assertEqual(<<16#F100001F:32/little>>, jit_aarch64_asm:cmp(r0, 0)),
        ?_assertEqual(<<16#F103001F:32/little>>, jit_aarch64_asm:cmp(r0, 192))
    ].

cmp32_test_() ->
    [
        % cmp32 reg, reg
        ?_assertEqual(<<16#6B01001F:32/little>>, jit_aarch64_asm:cmp32(r0, r1)),
        % cmp32 reg, imm
        ?_assertEqual(<<16#7100001F:32/little>>, jit_aarch64_asm:cmp32(r0, 0)),
        ?_assertEqual(<<16#7103001F:32/little>>, jit_aarch64_asm:cmp32(r0, 192))
    ].

and_test_() ->
    [
        % AND reg, reg, reg
        ?_assertEqual(<<16#8A010020:32/little>>, jit_aarch64_asm:and_(r0, r1, r1)),
        % AND reg, reg, imm
        ?_assertEqual(<<16#927A0420:32/little>>, jit_aarch64_asm:and_(r0, r1, 192)),
        ?_assertEqual(<<16#927ff8e7:32/little>>, jit_aarch64_asm:and_(r7, r7, -2)),
        ?_assertEqual(<<16#9200cc41:32/little>>, jit_aarch64_asm:and_(r1, r2, 16#f0f0f0f0f0f0f0f)),
        ?_assertEqual(<<16#92603c62:32/little>>, jit_aarch64_asm:and_(r2, r3, 16#ffff00000000)),
        ?_assertEqual(<<16#92785c83:32/little>>, jit_aarch64_asm:and_(r3, r4, 16#ffffff00))
    ].

lsl_test_() ->
    [
        ?_assertEqual(<<16#D3607C00:32/little>>, jit_aarch64_asm:lsl(r0, r0, 32))
    ].

lsr_test_() ->
    [
        ?_assertEqual(
            asm(<<16#D340FC00:32/little>>, "lsr x0, x0, 0"), jit_aarch64_asm:lsr(r0, r0, 0)
        ),
        ?_assertEqual(
            asm(<<16#D340FC01:32/little>>, "lsr x1, x0, 0"), jit_aarch64_asm:lsr(r1, r0, 0)
        ),
        ?_assertEqual(<<16#D360FC00:32/little>>, jit_aarch64_asm:lsr(r0, r0, 32))
    ].

ret_test_() ->
    [
        ?_assertEqual(asm(<<16#D65F03C0:32/little>>, "ret"), jit_aarch64_asm:ret())
    ].

tst_test_() ->
    [
        ?_assertEqual(asm(<<16#EA01001F:32/little>>, "tst x0, x1"), jit_aarch64_asm:tst(r0, r1)),
        ?_assertEqual(asm(<<16#f240003f:32/little>>, "tst x1, #1"), jit_aarch64_asm:tst(r1, 1)),
        ?_assertEqual(asm(<<16#f27c005f:32/little>>, "tst x2, #16"), jit_aarch64_asm:tst(r2, 16)),
        ?_assertEqual(asm(<<16#f2401c7f:32/little>>, "tst x3, #255"), jit_aarch64_asm:tst(r3, 255)),
        ?_assertEqual(
            asm(<<16#f240249f:32/little>>, "tst x4, #1023"), jit_aarch64_asm:tst(r4, 1023)
        ),
        ?_assertEqual(asm(<<16#f24014bf:32/little>>, "tst x5, #63"), jit_aarch64_asm:tst(r5, 63)),
        ?_assertEqual(asm(<<16#f27b00df:32/little>>, "tst x6, #32"), jit_aarch64_asm:tst(r6, 32)),
        ?_assertEqual(asm(<<16#f27a00ff:32/little>>, "tst x7, #64"), jit_aarch64_asm:tst(r7, 64)),
        ?_assertEqual(asm(<<16#f27e051f:32/little>>, "tst x8, #0xc"), jit_aarch64_asm:tst(r8, 16#c))
    ].

tst_w_test_() ->
    [
        ?_assertEqual(asm(<<16#6a01001f:32/little>>, "tst w0, w1"), jit_aarch64_asm:tst_w(r0, r1)),
        ?_assertEqual(asm(<<16#7200003f:32/little>>, "tst w1, #1"), jit_aarch64_asm:tst_w(r1, 1)),
        ?_assertEqual(asm(<<16#721c005f:32/little>>, "tst w2, #16"), jit_aarch64_asm:tst_w(r2, 16)),
        ?_assertEqual(
            asm(<<16#72001c7f:32/little>>, "tst w3, #255"), jit_aarch64_asm:tst_w(r3, 255)
        ),
        ?_assertEqual(
            asm(<<16#7200249f:32/little>>, "tst w4, #1023"), jit_aarch64_asm:tst_w(r4, 1023)
        ),
        ?_assertEqual(asm(<<16#720014bf:32/little>>, "tst w5, #63"), jit_aarch64_asm:tst_w(r5, 63)),
        ?_assertEqual(asm(<<16#721b00df:32/little>>, "tst w6, #32"), jit_aarch64_asm:tst_w(r6, 32)),
        ?_assertEqual(asm(<<16#721a00ff:32/little>>, "tst w7, #64"), jit_aarch64_asm:tst_w(r7, 64)),
        ?_assertEqual(
            asm(<<16#721e051f:32/little>>, "tst w8, #0xc"), jit_aarch64_asm:tst_w(r8, 16#c)
        )
    ].

bcc_test_() ->
    [
        ?_assertEqual(<<16#54000000:32/little>>, jit_aarch64_asm:bcc(eq, 0)),
        ?_assertEqual(<<16#54000001:32/little>>, jit_aarch64_asm:bcc(ne, 0)),
        ?_assertEqual(<<16#54fffe01:32/little>>, jit_aarch64_asm:bcc(ne, -64)),
        ?_assertEqual(<<16#54000400:32/little>>, jit_aarch64_asm:bcc(eq, 128))
    ].

stp_test_() ->
    [
        ?_assertEqual(
            <<16#a8815113:32/little>>,
            jit_aarch64_asm:stp(r19, r20, {r8}, 16)
        ),
        ?_assertEqual(
            <<16#a88153f3:32/little>>,
            jit_aarch64_asm:stp(r19, r20, {sp}, 16)
        ),
        % Store-update (writeback) variants
        ?_assertEqual(
            <<16#a9bf27e8:32/little>>,
            jit_aarch64_asm:stp(r8, r9, {sp, -16}, '!')
        ),
        ?_assertEqual(
            <<16#a98127e8:32/little>>,
            jit_aarch64_asm:stp(r8, r9, {sp, 16}, '!')
        ),
        ?_assertEqual(
            <<16#a98027e8:32/little>>,
            jit_aarch64_asm:stp(r8, r9, {sp, 0}, '!')
        )
    ].

ldp_test_() ->
    [
        ?_assertEqual(
            <<16#a8c15113:32/little>>,
            jit_aarch64_asm:ldp(r19, r20, {r8}, 16)
        ),
        ?_assertEqual(
            <<16#a8c153f3:32/little>>,
            jit_aarch64_asm:ldp(r19, r20, {sp}, 16)
        )
    ].

subs_test_() ->
    [
        % SUBS with immediate
        ?_assertEqual(<<16#F1000021:32/little>>, jit_aarch64_asm:subs(r1, r1, 0)),
        ?_assertEqual(<<16#F1000421:32/little>>, jit_aarch64_asm:subs(r1, r1, 1)),
        % SUBS with register
        ?_assertEqual(<<16#eb000021:32/little>>, jit_aarch64_asm:subs(r1, r1, r0)),
        ?_assertEqual(<<16#eb0a0021:32/little>>, jit_aarch64_asm:subs(r1, r1, r10))
    ].

adr_test_() ->
    [
        %% ADR x0, #0
        ?_assertEqual(<<16#10000000:32/little>>, jit_aarch64_asm:adr(r0, 0)),
        %% ADR x1, #4
        ?_assertEqual(<<16#10000021:32/little>>, jit_aarch64_asm:adr(r1, 4)),
        %% ADR x2, #-4
        ?_assertEqual(<<16#10ffffe2:32/little>>, jit_aarch64_asm:adr(r2, -4)),
        %% ADR x3, #1048572 (max positive)
        ?_assertEqual(<<16#107fffe3:32/little>>, jit_aarch64_asm:adr(r3, 1048572)),
        %% ADR x4, #-1048576 (max negative)
        ?_assertEqual(<<16#10800004:32/little>>, jit_aarch64_asm:adr(r4, -1048576)),
        %% ADR with offset not a multiple of 4 is valid
        ?_assertEqual(<<16#70000000:32/little>>, jit_aarch64_asm:adr(r0, 3))
    ].

asm(Bin, Str) ->
    case erlang:system_info(machine) of
        "ATOM" ->
            Bin;
        "BEAM" ->
            case os:cmd("which aarch64-elf-as") of
                [] ->
                    Bin;
                _ ->
                    ok = file:write_file("test.S", Str ++ "\n"),
                    Dump = os:cmd(
                        "aarch64-elf-as -c test.S -o test.o && aarch64-elf-objdump -D test.o"
                    ),
                    DumpBin = list_to_binary(Dump),
                    DumpLines = binary:split(DumpBin, <<"\n">>, [global]),
                    AsmBin = asm_lines(DumpLines, <<>>),
                    if
                        AsmBin =:= Bin ->
                            ok;
                        true ->
                            io:format(
                                "-------------------------------------------\n"
                                "~s\n"
                                "-------------------------------------------\n",
                                [Dump]
                            )
                    end,
                    ?assertEqual(AsmBin, Bin),
                    Bin
            end
    end.

asm_lines([<<" ", Tail/binary>> | T], Acc) ->
    [_Offset, HexStr0] = binary:split(Tail, <<":\t">>),
    [HexStr, _] = binary:split(HexStr0, <<"\t">>),
    AssembledBin = hex_to_bin(HexStr, <<>>),
    asm_lines(T, <<Acc/binary, AssembledBin/binary>>);
asm_lines([_OtherLine | T], Acc) ->
    asm_lines(T, Acc);
asm_lines([], Acc) ->
    Acc.

hex_to_bin(<<>>, Acc) ->
    Acc;
hex_to_bin(HexStr, Acc) ->
    [HexChunk, Rest] = binary:split(HexStr, <<" ">>),
    NumBits = byte_size(HexChunk) * 4,
    HexVal = binary_to_integer(HexChunk, 16),
    NewAcc = <<Acc/binary, HexVal:NumBits/little>>,
    hex_to_bin(Rest, NewAcc).
