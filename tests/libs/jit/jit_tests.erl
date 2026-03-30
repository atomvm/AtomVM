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

% Code chunk from bool_min2.erl - tests tail-call cache optimization
% This module has multiple return opcodes which trigger the tail-call cache:
% - The first return creates a cached implementation
% - Subsequent returns use jump_to_offset to jump back to the cached code
-define(CODE_CHUNK_3,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B2, 16#00,
        16#00, 16#00, 16#09, 16#00, 16#00, 16#00, 16#03, 16#01, 16#10, 16#99, 16#10, 16#02, 16#12,
        16#22, 16#00, 16#01, 16#20, 16#0C, 16#10, 16#00, 16#AC, 16#17, 16#10, 16#04, 16#40, 16#32,
        16#23, 16#40, 16#32, 16#33, 16#40, 16#32, 16#13, 16#40, 16#42, 16#43, 16#40, 16#32, 16#03,
        16#99, 16#20, 16#04, 16#50, 16#45, 16#04, 16#10, 16#65, 16#40, 16#03, 16#04, 16#40, 16#42,
        16#23, 16#40, 16#42, 16#33, 16#40, 16#32, 16#13, 16#40, 16#42, 16#43, 16#40, 16#42, 16#03,
        16#99, 16#30, 16#04, 16#50, 16#45, 16#04, 16#10, 16#65, 16#99, 16#20, 16#7D, 16#05, 16#10,
        16#00, 16#57, 16#04, 16#10, 16#57, 16#03, 16#10, 16#03, 16#12, 16#10, 16#13, 16#01, 16#30,
        16#99, 16#40, 16#02, 16#12, 16#72, 16#50, 16#01, 16#40, 16#99, 16#50, 16#0B, 16#05, 16#10,
        16#03, 16#13, 16#03, 16#0B, 16#05, 16#10, 16#23, 16#33, 16#13, 16#0B, 16#05, 16#20, 16#57,
        16#03, 16#20, 16#57, 16#13, 16#20, 16#03, 16#0A, 16#05, 16#30, 16#43, 16#13, 16#0B, 16#05,
        16#20, 16#57, 16#03, 16#20, 16#57, 16#13, 16#20, 16#03, 16#13, 16#01, 16#50, 16#99, 16#60,
        16#02, 16#12, 16#B2, 16#10, 16#01, 16#60, 16#3B, 16#03, 16#55, 16#17, 16#40, 16#32, 16#85,
        16#42, 16#75, 16#01, 16#70, 16#40, 16#11, 16#03, 16#13, 16#01, 16#80, 16#40, 16#01, 16#03,
        16#13, 16#03>>
).
-define(ATU8_CHUNK_3,
    <<16#FF, 16#FF, 16#FF, 16#F5, 16#90, 16#62, 16#6F, 16#6F, 16#6C, 16#5F, 16#6D, 16#69, 16#6E,
        16#32, 16#50, 16#73, 16#74, 16#61, 16#72, 16#74, 16#50, 16#66, 16#61, 16#6C, 16#73, 16#65,
        16#40, 16#74, 16#72, 16#75, 16#65, 16#60, 16#65, 16#72, 16#6C, 16#61, 16#6E, 16#67, 16#10,
        16#2B, 16#10, 16#66, 16#30, 16#61, 16#6E, 16#64, 16#20, 16#6F, 16#72, 16#30, 16#6E, 16#6F,
        16#74, 16#B0, 16#6F, 16#6E, 16#65, 16#5F, 16#69, 16#66, 16#5F, 16#74, 16#72, 16#75, 16#65>>
).
-define(TYPE_CHUNK_3,
    <<16#00, 16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#03, 16#0F, 16#FF, 16#30, 16#20, 16#00,
        16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
        16#00, 16#01, 16#00, 16#01>>
).
-define(LINE_CHUNK_3,
    <<16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#07, 16#00,
        16#00, 16#00, 16#06, 16#00, 16#00, 16#00, 16#00, 16#41, 16#51, 16#61, 16#81, 16#91, 16#B1>>
).

-ifdef(JIT_DWARF).
compile_stream_setup(CodeChunk) ->
    compile_stream_setup_for_backend(jit_x86_64, CodeChunk).

compile_stream_setup_for_backend(Backend, CodeChunk) ->
    Arch = backend_to_arch(Backend),
    Stream0 = jit_dwarf:new(Backend, test_module, jit_stream_binary, 0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = CodeChunk,
    Stream1 = jit_dwarf:append(
        Stream0, jit:beam_chunk_header(LabelsCount, Arch, ?JIT_VARIANT_PIC)
    ),
    Stream2 = Backend:new(?JIT_VARIANT_PIC, jit_dwarf, Stream1),
    {LabelsCount, Stream2}.

compile_stream_finalize(Stream3) ->
    compile_stream_finalize_for_backend(jit_x86_64, Stream3).

compile_stream_finalize_for_backend(Backend, Stream3) ->
    DwarfStream = Backend:stream(Stream3),
    jit_dwarf:stream(DwarfStream).
-else.
compile_stream_setup(CodeChunk) ->
    compile_stream_setup_for_backend(jit_x86_64, CodeChunk).

compile_stream_setup_for_backend(Backend, CodeChunk) ->
    Arch = backend_to_arch(Backend),
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = CodeChunk,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, Arch, ?JIT_VARIANT_PIC)
    ),
    Stream2 = Backend:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),
    {LabelsCount, Stream2}.

compile_stream_finalize(Stream3) ->
    compile_stream_finalize_for_backend(jit_x86_64, Stream3).

compile_stream_finalize_for_backend(Backend, Stream3) ->
    Backend:stream(Stream3).
-endif.

compile_minimal_x86_64_test() ->
    {LabelsCount, Stream2} = compile_stream_setup(?CODE_CHUNK_0),
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_0,
        fun(_) -> undefined end,
        fun(_) -> undefined end,
        fun(_) -> any end,
        fun(_) -> undefined end,
        jit_x86_64,
        Stream2
    ),
    Stream4 = compile_stream_finalize(Stream3),
    <<16:32, LabelsCount:32, ?JIT_FORMAT_VERSION:16, 1:16, ?JIT_ARCH_X86_64:16, ?JIT_VARIANT_PIC:16,
        0:32, Code/binary>> = Stream4,
    {JumpTable, _} = split_binary(Code, (LabelsCount + 1) * 5),
    ok = check_x86_64_jt(JumpTable),
    <<16#E9, LabelsLinesTable0:32/little, _/binary>> = JumpTable,
    {_, LabelsLinesCode0} = split_binary(Code, LabelsLinesTable0 + 5),
    {LabelsLinesCode, LabelsLinesTable} = split_binary(LabelsLinesCode0, 8),
    % 48 8d 05 01 00 00 00 	lea    0x1(%rip),%rax
    % c3                   	retq
    <<16#48, 16#8D, 16#05, 1:32/little, 16#C3>> = LabelsLinesCode,
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

backend_to_arch(jit_x86_64) -> ?JIT_ARCH_X86_64;
backend_to_arch(jit_aarch64) -> ?JIT_ARCH_AARCH64;
backend_to_arch(jit_armv6m) -> ?JIT_ARCH_ARMV6M.

compile_stream_for_backend(Backend, CodeChunk, AtomChunk, TypeChunk) ->
    compile_stream_for_backend(Backend, CodeChunk, AtomChunk, TypeChunk, fun(_) ->
        {test, test, 2}
    end).

compile_stream_for_backend(Backend, CodeChunk, AtomChunk, TypeChunk, ImportResolver) ->
    {LabelsCount, Stream2} = compile_stream_setup_for_backend(Backend, CodeChunk),

    AtomResolver = jit_precompile:atom_resolver(AtomChunk),
    LiteralResolver = fun(_) -> test_literal end,
    TypeResolver = jit_precompile:type_resolver(TypeChunk),

    % Compile with typed register support
    {LabelsCount, Stream3} = jit:compile(
        CodeChunk, AtomResolver, LiteralResolver, TypeResolver, ImportResolver, Backend, Stream2
    ),
    compile_stream_finalize_for_backend(Backend, Stream3).

term_to_int_verify_is_match_state_typed_optimization_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?CODE_CHUNK_1, ?ATU8_CHUNK_1, ?TYPE_CHUNK_1
    ),

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
    % The register value cache eliminates the redundant load after the store,
    % since %rax already holds the value.
    %   48 8b 77 30          	mov    0x30(%rdi),%rsi
    %   31 d2                	xor    %edx,%edx
    %   ff d0                	callq  *%rax
    %   5a                   	pop    %rdx
    %   5e                   	pop    %rsi
    %   5f                   	pop    %rdi
    %   48 89 47 40          	mov    %rax,0x40(%rdi)
    %   48 83 e0 fc          	and    $0xfffffffffffffffc,%rax

    % As opposed to (without typed optimization, verify_is_boxed would be emitted):
    %   48 8b 77 30          	mov    0x30(%rdi),%rsi
    %   31 d2                	xor    %edx,%edx
    %   ff d0                	callq  *%rax
    %   5a                   	pop    %rdx
    %   5e                   	pop    %rsi
    %   5f                   	pop    %rdi
    %   48 89 47 40          	mov    %rax,0x40(%rdi)
    %   49 89 c3             	mov    %rax,%r11
    %   41 80 e3 03          	and    $0x3,%r11b
    %   41 80 fb 02          	cmp    $0x2,%r11b
    %   74 0f                	je     <skip>
    %   48 8b 02             	mov    (%rdx),%rax
    %   ba xx xx 00 00       	mov    $0x...,%edx
    %   b9 xx xx 00 00       	mov    $0x...,%ecx
    %   ff e0                	jmpq   *%rax
    %   48 83 e0 fc          	and    $0xfffffffffffffffc,%rax
    ?assertMatch(
        {_, 19},
        binary:match(
            CompiledCode,
            <<16#48, 16#8b, 16#77, 16#30, 16#31, 16#d2, 16#ff, 16#d0, 16#5a, 16#5e, 16#5f, 16#48,
                16#89, 16#47, 16#40, 16#48, 16#83, 16#e0, 16#fc>>
        )
    ),

    ok.

verify_is_function_typed_optimization_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?CODE_CHUNK_2, ?ATU8_CHUNK_2, ?TYPE_CHUNK_2
    ),

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

tail_call_cache_armv6m_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_armv6m, ?CODE_CHUNK_3, ?ATU8_CHUNK_3, ?TYPE_CHUNK_3
    ),

    % PRIM_RETURN (primitive index 1) tail call pattern on armv6m:
    %   ldr  r7, [r2, #4]   ; load PRIM_RETURN function pointer
    %   ldr  r6, [sp, #20]  ; load saved LR
    %   str  r7, [sp, #20]  ; store function ptr as return address
    %   mov  lr, r6         ; restore LR
    %   pop  {r1,r4,r5,r6,r7,pc}  ; tail call via popped PC
    ReturnPattern =
        <<16#6857:16/little, 16#9e05:16/little, 16#9705:16/little, 16#46b6:16/little,
            16#bdf2:16/little>>,

    % The PRIM_RETURN pattern must appear exactly once in the compiled code.
    % The module has multiple OP_RETURN opcodes, so if caching works,
    % subsequent returns jump back to the first instance instead of
    % duplicating the tail call sequence.
    {Pos, Len} = binary:match(CompiledCode, ReturnPattern),
    % Verify there is no second occurrence after the first one
    ?assertEqual(
        nomatch,
        binary:match(CompiledCode, ReturnPattern, [
            {scope, {Pos + Len, byte_size(CompiledCode) - Pos - Len}}
        ])
    ),
    ok.

small_integer_bounds_test_() ->
    [
        ?_assertEqual({-(1 bsl 59), (1 bsl 59) - 1}, jit:small_integer_bounds(jit_x86_64)),
        ?_assertEqual({-(1 bsl 27), (1 bsl 27) - 1}, jit:small_integer_bounds(jit_armv6m))
    ].

is_small_integer_range_test_() ->
    [
        % Both ranges within 32-bit small integer bounds
        ?_assert(jit:is_small_integer_range({0, 100}, {-50, 50}, jit_armv6m)),
        % Both ranges within 64-bit small integer bounds
        ?_assert(jit:is_small_integer_range({0, 100}, {-50, 50}, jit_x86_64)),
        % At the exact boundary for 32-bit
        ?_assert(
            jit:is_small_integer_range(
                {-(1 bsl 27), (1 bsl 27) - 1}, {0, 0}, jit_armv6m
            )
        ),
        % Exceeding boundary for 32-bit
        ?_assertNot(
            jit:is_small_integer_range(
                {-(1 bsl 27) - 1, 0}, {0, 0}, jit_armv6m
            )
        ),
        ?_assertNot(
            jit:is_small_integer_range(
                {0, (1 bsl 27)}, {0, 0}, jit_armv6m
            )
        ),
        % Second range exceeding boundary
        ?_assertNot(
            jit:is_small_integer_range(
                {0, 0}, {0, (1 bsl 27)}, jit_armv6m
            )
        ),
        % Unbounded range (atom bounds like '-inf'/'+inf')
        ?_assertNot(jit:is_small_integer_range({'-inf', 100}, {0, 50}, jit_x86_64)),
        ?_assertNot(jit:is_small_integer_range({0, '+inf'}, {0, 50}, jit_x86_64)),
        % At the exact boundary for 64-bit
        ?_assert(
            jit:is_small_integer_range(
                {-(1 bsl 59), (1 bsl 59) - 1}, {0, 0}, jit_x86_64
            )
        ),
        ?_assertNot(
            jit:is_small_integer_range(
                {-(1 bsl 59) - 1, 0}, {0, 0}, jit_x86_64
            )
        )
    ].

% Code chunk for byte_size inline test.
% Equivalent to: f(X) when is_binary(X) -> byte_size(X).
%
% Bytecodes:
%   label 1
%   line 0
%   func_info atom_1, atom_2, 1
%   label 2
%   is_binary label_1, x[0]
%   gc_bif1 label_0, 1, 0, typed_x[0]:t_bitstring(8), x[0]
%   return
%   int_call_end
-define(CODE_CHUNK_4,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 182, 0, 0, 0, 2, 0, 0, 0, 1, 1, 16, 153, 0, 2, 18, 34, 16,
        1, 32, 53, 21, 3, 124, 5, 16, 0, 16#57, 3, 0, 3, 19, 3>>
).
-define(ATU8_CHUNK_4,
    <<255, 255, 255, 254, 224, 116, 101, 115, 116, 95, 98, 121, 116, 101, 95, 115, 105, 122, 101,
        16, 102>>
).
% Type chunk for byte_size inline test.
% Version 3, 1 entry: t_bitstring with unit=8
-define(TYPE_CHUNK_4,
    <<0, 0, 0, 3, 0, 0, 0, 1, 16#40, 16#02, 7>>
).

byte_size_inline_binary_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_CHUNK_4,
        ?ATU8_CHUNK_4,
        ?TYPE_CHUNK_4,
        fun(_) -> {erlang, byte_size, 1} end
    ),

    % When is_binary guard precedes gc_bif1 byte_size, the JIT inlines
    % the operation. The inline code:
    %   1. Strips the primary tag (and $0xfc)
    %   2. Reads boxed_value[1] (the byte size)
    %   3. Encodes as tagged integer (shl $4, or $0xf)
    %
    % The shl $4 + or $0xf pattern is distinctive to the inline path.
    % In the non-inlined path, a function pointer call would appear instead.
    %
    % Inline sequence on rax:
    %   48 83 e0 fc    and    $0xfffffffffffffffc,%rax
    %   48 8b 40 08    mov    0x8(%rax),%rax
    %   48 c1 e0 04    shl    $0x4,%rax
    %   48 83 c8 0f    or     $0xf,%rax
    ?assertMatch(
        {_, 16},
        binary:match(
            CompiledCode,
            <<16#48, 16#83, 16#e0, 16#fc, 16#48, 16#8b, 16#40, 16#08, 16#48, 16#c1, 16#e0, 16#04,
                16#48, 16#83, 16#c8, 16#0f>>
        )
    ),
    ok.

%%-----------------------------------------------------------------------------
%% Tuple fusion tests
%%
%% Test that is_tuple + test_arity + get_tuple_element sequences are fused
%% into a single operation that loads the register, strips the tag, and loads
%% the header only once.
%%
%% The distinctive fused pattern on x86_64 is:
%%   48 83 e0 fc    and  $-4,%rax           (strip tag)
%%   4c 8b 18       mov  (%rax),%r11        (header into separate register)
%%
%% In the unfused code, move_array_element loads the header into the SAME
%% register as the pointer:
%%   48 83 e0 fc    and  $-4,%rax
%%   48 8b 00       mov  (%rax),%rax        (header overwrites pointer)
%%-----------------------------------------------------------------------------

% is_tuple + test_arity + single get_tuple_element
% f({A, _B}) -> A.
-define(FUSE_CODE_1,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#15, 16#03, 16#20, 16#42, 16#03, 16#00, 16#03,
        16#13, 16#03>>
).
-define(FUSE_ATU8_1,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#31, 16#10, 16#66>>
).

% is_tuple + test_arity + multiple get_tuple_elements
% f({A, B, C}) -> {C, B, A}. (just the destructuring part)
-define(FUSE_CODE_2,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#15, 16#03, 16#30, 16#42, 16#03, 16#00, 16#13,
        16#42, 16#03, 16#10, 16#23, 16#42, 16#03, 16#20, 16#03, 16#13, 16#03>>
).
-define(FUSE_ATU8_2,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#32, 16#10, 16#66>>
).

% is_tuple + test_arity only (no get_tuple_element)
-define(FUSE_CODE_3,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#15, 16#03, 16#20, 16#13, 16#03>>
).
-define(FUSE_ATU8_3,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#33, 16#10, 16#66>>
).

% is_tuple + test_arity with different fail labels + get_tuple_element
-define(FUSE_CODE_4,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#04, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#35, 16#03, 16#20, 16#42, 16#03, 16#00, 16#03,
        16#13, 16#01, 16#30, 16#40, 16#32, 16#03, 16#13, 16#03>>
).
-define(FUSE_ATU8_4,
    <<16#FF, 16#FF, 16#FF, 16#FD, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#34, 16#10, 16#66, 16#50, 16#66, 16#61, 16#6C, 16#73, 16#65>>
).

% is_tuple alone (no test_arity follows) - should NOT fuse
-define(FUSE_CODE_5,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#13, 16#03>>
).
-define(FUSE_ATU8_5,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#35, 16#10, 16#66>>
).

fuse_tuple_single_get_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_1, ?FUSE_ATU8_1, <<>>
    ),
    % Fused: strip tag + load header into separate register
    %   48 83 e0 fc    and  $-4,%rax
    %   4c 8b 18       mov  (%rax),%r11
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    % Fused: element 0 loaded using kept untagged pointer
    %   4c 8b 58 08    mov  0x8(%rax),%r11
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#4c, 16#8b, 16#58, 16#08>>)
    ),
    ok.

fuse_tuple_multi_get_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_2, ?FUSE_ATU8_2, <<>>
    ),
    % Fused: strip + header into separate register
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    % All three elements loaded from the same untagged pointer:
    %   4c 8b 58 08    mov  0x8(%rax),%r11     (element 0 -> x[1])
    %   4c 89 5f 38    mov  %r11,0x38(%rdi)
    %   4c 8b 58 10    mov  0x10(%rax),%r11    (element 1 -> x[2])
    %   4c 89 5f 40    mov  %r11,0x40(%rdi)
    %   4c 8b 58 18    mov  0x18(%rax),%r11    (element 2 -> x[0])
    ?assertMatch(
        {_, _},
        binary:match(
            CompiledCode,
            <<16#4c, 16#8b, 16#58, 16#08, 16#4c, 16#89, 16#5f, 16#38, 16#4c, 16#8b, 16#58, 16#10,
                16#4c, 16#89, 16#5f, 16#40, 16#4c, 16#8b, 16#58, 16#18>>
        )
    ),
    ok.

fuse_tuple_arity_only_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_3, ?FUSE_ATU8_3, <<>>
    ),
    % Fused: strip + header into separate register (even without get_tuple_element)
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    ok.

fuse_tuple_diff_labels_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_4, ?FUSE_ATU8_4, <<>>
    ),
    % Fused: strip + header into separate register
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    ok.

no_fuse_tuple_alone_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_5, ?FUSE_ATU8_5, <<>>
    ),
    % Unfused: header loaded into SAME register (rax) since no need to keep ptr
    %   48 83 e0 fc    and  $-4,%rax
    %   48 8b 00       mov  (%rax),%rax
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#48, 16#8b, 16#00>>)
    ),
    % The fused pattern should NOT appear
    ?assertEqual(
        nomatch,
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    ok.

fuse_tuple_armv6m_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_armv6m, ?FUSE_CODE_1, ?FUSE_ATU8_1, <<>>
    ),
    % Fused: header loaded into r6 while r7 keeps the untagged pointer
    %   43b7    bics  r7, r6       (strip tag, r7 = untagged ptr)
    %   683e    ldr   r6, [r7, #0] (header into r6)
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#b7, 16#43, 16#3e, 16#68>>)
    ),
    % Element loaded using kept r7
    %   687e    ldr   r6, [r7, #4]
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#7e, 16#68>>)
    ),
    ok.
