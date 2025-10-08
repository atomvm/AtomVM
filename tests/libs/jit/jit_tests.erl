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

-module(jit_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").

-define(CODE_CHUNK_0,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 177, 0, 0, 0, 7, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 0,
        1, 32, 64, 50, 3, 19, 1, 48, 153, 0, 2, 18, 66, 0, 1, 64, 64, 18, 3, 78, 16, 0, 1, 80, 153,
        0, 2, 18, 66, 16, 1, 96, 64, 3, 19, 64, 18, 3, 78, 32, 16, 3>>
).

% Code chunk with typed register from test_term_to_int.erl
% Contains bs_get_binary2 opcode with typed register that uses term_to_int optimization
-define(CODE_CHUNK_1,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 182, 0, 0, 0, 4, 0, 0, 0, 1, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 45, 21, 19, 166, 53, 3, 32, 35, 117, 53, 87, 35, 16, 48, 87, 19, 32, 16, 0, 19, 182,
        53, 35, 23, 32, 50, 0, 64, 19, 3, 19, 1, 48, 153, 32, 72, 3, 3>>
).
-define(ATU8_CHUNK_1,
    <<255, 255, 255, 253, 8, 16, 116, 101, 115, 116, 95, 116, 101, 114, 109, 95, 116, 111, 95, 105,
        110, 116, 144, 101, 120, 116, 114, 97, 99, 116, 95, 105, 224, 101, 110, 115, 117, 114, 101,
        95, 101, 120, 97, 99, 116, 108, 121>>
).
-define(TYPE_CHUNK_1,
    <<0, 0, 0, 3, 0, 0, 0, 3, 15, 255, 0, 2, 0, 32>>
).

% Code chunk with typed register from test_call_simple.erl
% Contains call_fun2 opcode with typed register that uses verify_is_function optimization
-define(CODE_CHUNK_2,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 3, 0, 0, 0, 1, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 77, 21, 19, 12, 0, 32, 153, 32, 178, 50, 16, 87, 19, 16, 18, 0, 19, 3>>
).
-define(ATU8_CHUNK_2,
    <<255, 255, 255, 253, 8, 16, 116, 101, 115, 116, 95, 99, 97, 108, 108, 95, 115, 105, 109, 112,
        108, 101, 144, 116, 101, 115, 116, 95, 99, 97, 108, 108, 96, 117, 110, 115, 97, 102, 101>>
).
-define(TYPE_CHUNK_2,
    <<0, 0, 0, 3, 0, 0, 0, 2, 15, 255, 0, 16>>
).

compile_minimal_x86_64_test() ->
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_0,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_0,
        fun(_) -> undefined end,
        fun(_) -> undefined end,
        fun(_) -> any end,
        jit_x86_64,
        Stream2
    ),
    Stream4 = jit_x86_64:stream(Stream3),
    <<16:32, LabelsCount:32, ?JIT_FORMAT_VERSION:16, 1:16, ?JIT_ARCH_X86_64:16, ?JIT_VARIANT_PIC:16,
        0:32, Code/binary>> = Stream4,
    {JumpTable, _} = split_binary(Code, (LabelsCount + 1) * 5),
    ok = check_x86_64_jt(JumpTable),
    <<16#E9, LabelsLinesTable0:32/little, _/binary>> = JumpTable,
    {_, LabelsLinesCode0} = split_binary(Code, LabelsLinesTable0 + 5),
    {LabelsLinesCode, LabelsLinesTable} = split_binary(LabelsLinesCode0, 8),
    % 48 8d 05 01 00 00 00 	lea    0x1(%rip),%rax
    % c3                   	retq
    ?assertEqual(<<16#48, 16#8D, 16#05, 1:32/little, 16#C3>>, LabelsLinesCode),
    {ok, LinesTable} = check_labels_table(LabelsCount, LabelsLinesTable),
    ok = check_lines_table(LinesTable),
    ok.

check_x86_64_jt(<<>>) -> ok;
check_x86_64_jt(<<16#e9, _Offset:32/little, Tail/binary>>) -> check_x86_64_jt(Tail);
check_x86_64_jt(Bin) -> {unexpected, Bin}.

check_labels_table(LabelsCount, <<LabelsCount:16, Labels:(LabelsCount * 6)/binary, Rest/binary>>) ->
    ok = check_labels_table0(1, Labels),
    {ok, Rest}.

check_labels_table0(_, <<>>) -> ok;
check_labels_table0(N, <<N:16, _Offset:32, Rest/binary>>) -> check_labels_table0(N + 1, Rest).

check_lines_table(<<LinesCount:16, _Lines:(LinesCount * 6)/binary>>) -> ok.

term_to_int_verify_is_match_state_typed_optimization_x86_64_test() ->
    % Compile CODE_CHUNK_1 which contains a typed register for term_to_int optimization
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_1,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_1),
    LiteralResolver = fun(_) -> test_literal end,
    TypeResolver = jit_precompile:type_resolver(?TYPE_CHUNK_1),

    % Compile with typed register support
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_1, AtomResolver, LiteralResolver, TypeResolver, jit_x86_64, Stream2
    ),
    CompiledCode = jit_x86_64:stream(Stream3),

    % Check the reading of x[1] is immediatly followed by a shift right.
    % 15c:	4c 8b 5f 38          	mov    0x38(%rdi),%r11
    % 160:	49 c1 eb 04          	shr    $0x4,%r11

    % As opposed to testing its type
    % 15c:	4c 8b 5f 38          	mov    0x38(%rdi),%r11
    % 160:	4d 89 da             	mov    %r11,%r10
    % 163:	41 80 e2 0f          	and    $0xf,%r10b
    % 167:	41 80 fa 0f          	cmp    $0xf,%r10b
    % 16b:	74 05                	je     0x172
    % 16d:	e9 ab 00 00 00       	jmpq   0x21d
    % 172:	49 c1 eb 04          	shr    $0x4,%r11
    ?assertMatch(
        {_, 8},
        binary:match(CompiledCode, <<16#4c, 16#8b, 16#5f, 16#38, 16#49, 16#c1, 16#eb, 16#04>>)
    ),

    % Check call to bs_start_match3 is followed by a skip of verify_is_boxed
    %  100:	48 8b 77 30          	mov    0x30(%rdi),%rsi
    %  104:	48 c7 c2 00 00 00 00 	mov    $0x0,%rdx
    %  10b:	ff d0                	callq  *%rax
    %  10d:	5a                   	pop    %rdx
    %  10e:	5e                   	pop    %rsi
    %  10f:	5f                   	pop    %rdi
    %  110:	48 89 47 40          	mov    %rax,0x40(%rdi)
    %  114:	48 8b 47 40          	mov    0x40(%rdi),%rax
    %  118:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax

    % As opposed to:
    %  100:	48 8b 77 30          	mov    0x30(%rdi),%rsi
    %  104:	48 c7 c2 00 00 00 00 	mov    $0x0,%rdx
    %  10b:	ff d0                	callq  *%rax
    %  10d:	5a                   	pop    %rdx
    %  10e:	5e                   	pop    %rsi
    %  10f:	5f                   	pop    %rdi
    %  110:	48 89 47 40          	mov    %rax,0x40(%rdi)
    %  114:	48 8b 47 40          	mov    0x40(%rdi),%rax
    %  118:	49 89 c3             	mov    %rax,%r11
    %  11b:	41 80 e3 03          	and    $0x3,%r11b
    %  11f:	41 80 fb 02          	cmp    $0x2,%r11b
    %  123:	74 13                	je     0x138
    %  125:	48 8b 02             	mov    (%rdx),%rax
    %  128:	48 c7 c2 28 01 00 00 	mov    $0x128,%rdx
    %  12f:	48 c7 c1 0b 01 00 00 	mov    $0x10b,%rcx
    %  136:	ff e0                	jmpq   *%rax
    %  138:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax
    ?assertMatch(
        {_, 28},
        binary:match(
            CompiledCode,
            <<16#48, 16#8b, 16#77, 16#30, 16#48, 16#c7, 16#c2, 16#00, 16#00, 16#00, 16#00, 16#ff,
                16#d0, 16#5a, 16#5e, 16#5f, 16#48, 16#89, 16#47, 16#40, 16#48, 16#8b, 16#47, 16#40,
                16#48, 16#83, 16#e0, 16#fc>>
        )
    ),

    ok.

verify_is_function_typed_optimization_x86_64_test() ->
    % Compile CODE_CHUNK_1 which contains a typed register for term_to_int optimization
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_2,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_2),
    LiteralResolver = fun(_) -> test_literal end,
    TypeResolver = jit_precompile:type_resolver(?TYPE_CHUNK_2),

    % Compile with typed register support
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_2, AtomResolver, LiteralResolver, TypeResolver, jit_x86_64, Stream2
    ),
    CompiledCode = jit_x86_64:stream(Stream3),

    % Check that call to allocate is directly followed by the building the cp
    % for call
    % b6:	48 8b 42 10          	mov    0x10(%rdx),%rax
    % ba:	ff e0                	jmpq   *%rax
    % bc:	48 8b 47 38          	mov    0x38(%rdi),%rax
    % c0:	4c 8b 1e             	mov    (%rsi),%r11
    % c3:	45 8b 1b             	mov    (%r11),%r11d
    % c6:	49 c1 e3 18          	shl    $0x18,%r11
    % ...

    % As opposed to:
    % b6:	48 8b 42 10          	mov    0x10(%rdx),%rax
    % ba:	ff e0                	jmpq   *%rax
    % bc:	48 8b 47 38          	mov    0x38(%rdi),%rax
    % c0:	49 89 c3             	mov    %rax,%r11
    % c3:	4d 89 da             	mov    %r11,%r10
    % c6:	41 80 e2 03          	and    $0x3,%r10b
    % ca:	41 80 fa 02          	cmp    $0x2,%r10b
    % ce:	74 1a                	je     0xea
    % d0:	48 8b 82 98 00 00 00 	mov    0x98(%rdx),%rax
    % d7:	48 c7 c2 d7 00 00 00 	mov    $0xd7,%rdx
    % de:	48 c7 c1 8b 01 00 00 	mov    $0x18b,%rcx
    % e5:	4d 89 d8             	mov    %r11,%r8
    % e8:	ff e0                	jmpq   *%rax
    % ea:	49 83 e3 fc          	and    $0xfffffffffffffffc,%r11
    % ee:	4d 8b 1b             	mov    (%r11),%r11
    % f1:	4d 89 da             	mov    %r11,%r10
    % f4:	41 80 e2 3f          	and    $0x3f,%r10b
    % f8:	41 80 fa 14          	cmp    $0x14,%r10b
    % fc:	74 1a                	je     0x118
    % fe:	48 8b 82 98 00 00 00 	mov    0x98(%rdx),%rax
    % 105:	48 c7 c2 05 01 00 00 	mov    $0x105,%rdx
    % 10c:	48 c7 c1 8b 01 00 00 	mov    $0x18b,%rcx
    % 113:	4d 89 d8             	mov    %r11,%r8
    % 116:	ff e0                	jmpq   *%rax
    % 118:	4c 8b 1e             	mov    (%rsi),%r11
    % 11b:	45 8b 1b             	mov    (%r11),%r11d
    % 11e:	49 c1 e3 18          	shl    $0x18,%r11
    % ...

    ?assertMatch(
        {_, 20},
        binary:match(
            CompiledCode,
            <<16#48, 16#8b, 16#42, 16#10, 16#ff, 16#e0, 16#48, 16#8b, 16#47, 16#38, 16#4c, 16#8b,
                16#1e, 16#45, 16#8b, 16#1b, 16#49, 16#c1, 16#e3, 16#18>>
        )
    ),
    ok.
