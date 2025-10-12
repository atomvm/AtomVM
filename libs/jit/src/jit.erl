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

-module(jit).

-export([
    stream/1,
    backend/1,
    beam_chunk_header/3,
    compile/6
]).

% NIFs
-export([
    stream_module/0,
    backend_module/0
]).

-export_type([
    stream/0
]).

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("default_atoms.hrl").
-include("opcodes.hrl").
-include("primitives.hrl").
-include("term.hrl").

-define(COMPACT_LITERAL, 0).
-define(COMPACT_INTEGER, 1).
-define(COMPACT_ATOM, 2).
-define(COMPACT_XREG, 3).
-define(COMPACT_YREG, 4).
-define(COMPACT_LABEL, 5).
-define(COMPACT_EXTENDED, 7).
-define(COMPACT_LARGE_LITERAL, 8).
-define(COMPACT_LARGE_INTEGER, 9).
-define(COMPACT_LARGE_ATOM, 10).
-define(COMPACT_LARGE_XREG, 11).
-define(COMPACT_LARGE_YREG, 12).

% OTP-20+ format
-define(COMPACT_EXTENDED_LIST, 16#17).
-define(COMPACT_EXTENDED_FP_REGISTER, 16#27).
-define(COMPACT_EXTENDED_ALLOCATION_LIST, 16#37).
-define(COMPACT_EXTENDED_LITERAL, 16#47).
% https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_asm.erl#L433
-define(COMPACT_EXTENDED_TYPED_REGISTER, 16#57).

-define(COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_WORDS, 0).
-define(COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FLOATS, 1).
-define(COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FUNS, 2).

-define(COMPACT_LARGE_IMM_MASK, 16#18).
-define(COMPACT_11BITS_VALUE, 16#8).
-define(COMPACT_NBITS_VALUE, 16#18).

-define(COMPACT_LARGE_INTEGER_11BITS, (?COMPACT_LARGE_INTEGER bor ?COMPACT_11BITS_VALUE)).
-define(COMPACT_LARGE_INTEGER_NBITS, (?COMPACT_LARGE_INTEGER bor ?COMPACT_NBITS_VALUE)).

-define(BOXED_FUN_SIZE, 3).
-define(FLOAT_SIZE_64, 2).
-define(FLOAT_SIZE_32, 3).

-define(INT32_MIN, -16#80000000).
-define(INT32_MAX, 16#7FFFFFFF).

-define(INT64_MIN, -16#8000000000000000).
-define(INT64_MAX, 16#7FFFFFFFFFFFFFFF).

-define(WAITING_TIMEOUT_EXPIRED, 2).

-define(BITSTRING_FLAG_LITTLE_ENDIAN, 16#2).
-define(BITSTRING_FLAG_SIGNED, 16#4).
-define(BITSTRING_FLAG_NATIVE_ENDIAN, 16#10).

-record(state, {
    line_offsets :: [{integer(), integer()}],
    labels_count :: pos_integer(),
    atom_resolver :: fun((integer()) -> atom()),
    literal_resolver :: fun((integer()) -> any()),
    type_resolver :: fun((integer()) -> any())
}).

-type stream() :: any().

%%-define(TRACE(Fmt, Args), io:format(Fmt, Args)).
-define(TRACE(Fmt, Args), ok).

%%-define(ASSERT_ALL_NATIVE_FREE(St), MMod:assert_all_native_free(St)).
%%-define(ASSERT(Expr), true = Expr).
-define(ASSERT_ALL_NATIVE_FREE(St), ok).
-define(ASSERT(Expr), ok).

%%-----------------------------------------------------------------------------
%% @param   LabelsCount number of labels
%% @param   Arch code for the architecture
%% @param   Variant code for the JIT variant
%% @returns Beam chunk header
%% @doc     Create the beam chunk header for a single architecture/variant
%% @end
%%-----------------------------------------------------------------------------
beam_chunk_header(LabelsCount, Arch, Variant) ->
    Info = <<LabelsCount:32, ?JIT_FORMAT_VERSION:16, 1:16, Arch:16, Variant:16, 0:32>>,
    <<(byte_size(Info)):32, Info/binary>>.

%% Current variant supposes any entry point (labels or continuation pointer)
%% has the following signature
%% Context *(*ModuleNativeEntryPoint)(Context *ctx, JITState *jit_state, const ModuleNativeInterface *p)
compile(
    <<16:32, 0:32, OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, Opcodes/binary>>,
    AtomResolver,
    LiteralResolver,
    TypeResolver,
    MMod,
    MSt0
) when OpcodeMax =< ?OPCODE_MAX ->
    MSt1 = MMod:jump_table(MSt0, LabelsCount),
    State0 = #state{
        line_offsets = [],
        labels_count = LabelsCount,
        atom_resolver = AtomResolver,
        literal_resolver = LiteralResolver,
        type_resolver = TypeResolver
    },
    {State1, MSt2} = first_pass(Opcodes, MMod, MSt1, State0),
    MSt3 = second_pass(MMod, MSt2, State1),
    {LabelsCount, MSt3};
compile(
    <<16:32, 0:32, OpcodeMax:32, _LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>>,
    _AtomResolver,
    _LiteralResolver,
    _TypeResolver,
    _MMod,
    _MSt
) ->
    error(badarg, [OpcodeMax]);
compile(CodeChunk, _AtomResolver, _LiteralResolver, _TypeResolver, _MMod, _MSt) ->
    error(badarg, [CodeChunk]).

% 1
first_pass(
    <<?OP_LABEL, Rest0/binary>>, MMod, MSt0, State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_LABEL ~p\n", [Label]),
    MSt1 = MMod:add_label(MSt0, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest1, MMod, MSt1, State0);
% 2
first_pass(<<?OP_FUNC_INFO, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_ModuleAtom, Rest1} = decode_atom(Rest0),
    {_FunctionName, Rest2} = decode_atom(Rest1),
    {_Arity, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_FUNC_INFO ~p, ~p, ~p\n", [_ModuleAtom, _FunctionName, _Arity]),
    % Implement function clause at the previous label. (TODO: optimize it out to save space)
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, offset, ?FUNCTION_CLAUSE_ATOM
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest3, MMod, MSt1, State0);
% 3
first_pass(
    <<?OP_INT_CALL_END>>, MMod, MSt0, #state{labels_count = LabelsCount} = State
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_INT_CALL_END\n", []),
    MSt1 = MMod:add_label(MSt0, LabelsCount),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_TERMINATE_CONTEXT, [
        ctx, jit_state
    ]),
    {State, MSt2};
% 4
first_pass(<<?OP_CALL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CALL ~p, ~p\n", [_Arity, Label]),
    MSt1 = MMod:call_or_schedule_next(MSt0, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State0);
% 5
first_pass(<<?OP_CALL_LAST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    {NWords, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_CALL_LAST ~p, ~p, ~p\n", [_Arity, Label, NWords]),
    MSt1 = MMod:move_to_cp(MSt0, {y_reg, NWords}),
    MSt2 = MMod:increment_sp(MSt1, NWords + 1),
    MSt3 = MMod:call_only_or_schedule_next(MSt2, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest3, MMod, MSt3, State0);
% 6
first_pass(<<?OP_CALL_ONLY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CALL_ONLY ~p, ~p\n", [_Arity, Label]),
    MSt1 = MMod:call_only_or_schedule_next(MSt0, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State0);
% 7
first_pass(<<?OP_CALL_EXT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_CALL_EXT ~p, ~p\n", [Arity, Index]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    MSt2 = MMod:call_primitive_with_cp(MSt1, ?PRIM_CALL_EXT, [
        ctx, jit_state, offset, Arity, Index, -1
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 8
first_pass(<<?OP_CALL_EXT_LAST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    {NWords, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_CALL_EXT_LAST ~p, ~p, ~p\n", [Arity, Index, NWords]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_CALL_EXT, [
        ctx, jit_state, offset, Arity, Index, NWords
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest3, MMod, MSt2, State0);
% 9
first_pass(<<?OP_BIF0, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Bif, Rest1} = decode_literal(Rest0),
    {MSt1, FuncPtr} = MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_BIF0 ~p, ~p\n", [Bif, Dest]),
    {MSt3, ResultReg} = MMod:call_func_ptr(MSt2, {free, FuncPtr}, [
        ctx
    ]),
    MSt4 = MMod:move_to_vm_register(MSt3, ResultReg, Dest),
    MSt5 = MMod:free_native_registers(MSt4, [Dest, ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest2, MMod, MSt5, State0);
% 10
first_pass(<<?OP_BIF1, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Bif, Rest2} = decode_literal(Rest1),
    {MSt1, FuncPtr} = MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt2, Arg, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    {MSt3, Dest, Rest4} = decode_dest(Rest3, MMod, MSt2),
    ?TRACE("OP_BIF1 ~p, ~p, ~p, ~p\n", [FailLabel, Bif, Arg, Dest]),
    {MSt4, ResultReg} = MMod:call_func_ptr(MSt3, {free, FuncPtr}, [
        ctx, FailLabel, {free, Arg}
    ]),
    MSt5 = bif_faillabel_test(FailLabel, MMod, MSt4, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 11
first_pass(<<?OP_BIF2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Bif, Rest2} = decode_literal(Rest1),
    {MSt1, FuncPtr} = MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt2, Arg1, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    {MSt3, Arg2, Rest4} = decode_compact_term(Rest3, MMod, MSt2, State0),
    {MSt4, Dest, Rest5} = decode_dest(Rest4, MMod, MSt3),
    ?TRACE("OP_BIF2 ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Bif, Arg1, Arg2, Dest]),
    {MSt5, ResultReg} = MMod:call_func_ptr(MSt4, {free, FuncPtr}, [
        ctx, FailLabel, {free, Arg1}, {free, Arg2}
    ]),
    MSt6 = bif_faillabel_test(FailLabel, MMod, MSt5, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0);
% 12
first_pass(<<?OP_ALLOCATE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {StackNeed, Rest1} = decode_literal(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_ALLOCATE ~p, ~p\n", [StackNeed, Live]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_ALLOCATE, [
        ctx, jit_state, StackNeed, 0, Live
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 13
first_pass(<<?OP_ALLOCATE_HEAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {StackNeed, Rest1} = decode_literal(Rest0),
    {HeapNeed, Rest2} = decode_allocator_list(MMod, Rest1),
    {Live, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_ALLOCATE_HEAP ~p, ~p, ~p\n", [StackNeed, HeapNeed, Live]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_ALLOCATE, [
        ctx, jit_state, StackNeed, HeapNeed, Live
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest3, MMod, MSt2, State0);
% 16
first_pass(<<?OP_TEST_HEAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {HeapNeed, Rest1} = decode_allocator_list(MMod, Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_TEST_HEAP ~p, ~p\n", [HeapNeed, Live]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_TEST_HEAP, [
        ctx, jit_state, HeapNeed, Live
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 18
first_pass(<<?OP_DEALLOCATE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {NWords, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_DEALLOCATE ~p\n", [NWords]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_DEALLOCATE, [
        ctx, jit_state, NWords
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 19
first_pass(<<?OP_RETURN, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_RETURN\n", []),
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RETURN, [
        ctx, jit_state
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest, MMod, MSt1, State0);
% 20
first_pass(<<?OP_SEND, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_SEND\n", []),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_SEND, [
        ctx, jit_state
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest, MMod, MSt2, State0);
% 21
first_pass(<<?OP_REMOVE_MESSAGE, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_REMOVE_MESSAGE\n", []),
    {MSt1, Reg1} = MMod:call_primitive(MSt0, ?PRIM_CANCEL_TIMEOUT, [
        ctx
    ]),
    MSt2 = MMod:free_native_registers(MSt1, [Reg1]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt4 = MMod:return_if_not_equal_to_ctx(MSt3, {free, ResultReg}),
    {MSt5, Reg2} = MMod:call_primitive(MSt4, ?PRIM_MAILBOX_REMOVE_MESSAGE, [
        ctx
    ]),
    MSt6 = MMod:free_native_registers(MSt5, [Reg2]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest, MMod, MSt6, State0);
% 22
first_pass(<<?OP_TIMEOUT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_TIMEOUT\n", []),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_TIMEOUT, [
        ctx
    ]),
    MSt2 = MMod:free_native_registers(MSt1, [ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest0, MMod, MSt2, State0);
% 23
first_pass(<<?OP_LOOP_REC, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt2 = MMod:return_if_not_equal_to_ctx(MSt1, {free, ResultReg}),
    {MSt3, Dest, Rest2} = decode_compact_term(Rest1, MMod, MSt2, State0),
    ?TRACE("OP_LOOP_REC ~p, ~p\n", [Label, Dest]),
    {MSt4, PeekResult} = MMod:call_primitive(MSt3, ?PRIM_MAILBOX_PEEK, [ctx]),
    MSt5 = cond_jump_to_label({PeekResult, '==', 0}, Label, MMod, MSt4),
    MSt6 = MMod:move_to_vm_register(MSt5, PeekResult, Dest),
    MSt7 = MMod:free_native_registers(MSt6, [PeekResult, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest2, MMod, MSt7, State0);
% 24
first_pass(<<?OP_LOOP_REC_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_LOOP_REC_END ~p\n", [Label]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt2 = MMod:return_if_not_equal_to_ctx(MSt1, {free, ResultReg}),
    {MSt3, Reg1} = MMod:call_primitive(MSt2, ?PRIM_MAILBOX_NEXT, [
        ctx
    ]),
    MSt4 = MMod:free_native_registers(MSt3, [Reg1]),
    MSt5 = MMod:jump_to_label(MSt4, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest1, MMod, MSt5, State0);
% 25
first_pass(<<?OP_WAIT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_WAIT ~p\n", [Label]),
    MSt1 = MMod:set_continuation_to_label(MSt0, Label),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 26
first_pass(<<?OP_WAIT_TIMEOUT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, OffsetRef0} = MMod:set_continuation_to_offset(MSt0),
    {MSt2, Timeout, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_WAIT_TIMEOUT ~p, ~p\n", [Label, Timeout]),
    MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_WAIT_TIMEOUT, [
        ctx, jit_state, {free, Timeout}, Label
    ]),
    MSt4 = MMod:add_label(MSt3, OffsetRef0),
    MSt5 = MMod:continuation_entry_point(MSt4),
    {MSt6, ResultReg0} = MMod:call_primitive(MSt5, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt7 = MMod:return_if_not_equal_to_ctx(MSt6, {free, ResultReg0}),
    {MSt8, ResultReg1} = MMod:call_primitive(MSt7, ?PRIM_CONTEXT_GET_FLAGS, [
        ctx, ?WAITING_TIMEOUT_EXPIRED
    ]),
    MSt9 = MMod:if_block(MSt8, {{free, ResultReg1}, '==', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_WAIT_TIMEOUT_TRAP_HANDLER, [
            ctx, jit_state, Label
        ])
    end),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest2, MMod, MSt9, State0);
% 39
first_pass(<<?OP_IS_LT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_LT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
        ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_NO_OPTS
    ]),
    MSt4 = handle_error_if({'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt3),
    MSt5 = cond_jump_to_label(
        {{free, ResultReg}, '&', ?TERM_GREATER_THAN + ?TERM_EQUALS, '!=', 0}, Label, MMod, MSt4
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 40
first_pass(<<?OP_IS_GE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_GE ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
        ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_NO_OPTS
    ]),
    MSt4 = handle_error_if({'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt3),
    MSt5 = cond_jump_to_label(
        {'(int)', {free, ResultReg}, '==', ?TERM_LESS_THAN}, Label, MMod, MSt4
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 41
first_pass(<<?OP_IS_EQUAL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_EQUAL ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
        ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_NO_OPTS
    ]),
    MSt4 = handle_error_if({'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt3),
    MSt5 = cond_jump_to_label(
        {{free, ResultReg}, '&', ?TERM_LESS_THAN + ?TERM_GREATER_THAN, '!=', 0},
        Label,
        MMod,
        MSt4
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 42
first_pass(<<?OP_IS_NOT_EQUAL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_NOT_EQUAL ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
        ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_NO_OPTS
    ]),
    MSt4 = handle_error_if({'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt3),
    MSt5 = cond_jump_to_label({'(int)', {free, ResultReg}, '==', ?TERM_EQUALS}, Label, MMod, MSt4),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 43
first_pass(<<?OP_IS_EQ_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_EQ_EXACT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    % If Arg2 is an immediate, we don't need to call term_compare
    MSt5 =
        if
            is_integer(Arg2) ->
                {MSt3, Arg1Reg} = MMod:move_to_native_register(MSt2, Arg1),
                cond_jump_to_label({{free, Arg1Reg}, '!=', Arg2}, Label, MMod, MSt3);
            true ->
                {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
                    ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_EXACT
                ]),
                MSt4 = handle_error_if(
                    {'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt3
                ),
                cond_jump_to_label(
                    {{free, ResultReg}, '&', ?TERM_LESS_THAN + ?TERM_GREATER_THAN, '!=', 0},
                    Label,
                    MMod,
                    MSt4
                )
        end,
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 44
first_pass(<<?OP_IS_NOT_EQ_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_NOT_EQ_EXACT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    MSt5 =
        if
            is_integer(Arg2) ->
                {MSt3, Arg1Reg} = MMod:move_to_native_register(MSt2, Arg1),
                cond_jump_to_label({{free, Arg1Reg}, '==', Arg2}, Label, MMod, MSt3);
            true ->
                {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
                    ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_EXACT
                ]),
                MSt4 = handle_error_if(
                    {'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt3
                ),
                cond_jump_to_label(
                    {'(int)', {free, ResultReg}, '==', ?TERM_EQUALS}, Label, MMod, MSt4
                )
        end,
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 45
first_pass(<<?OP_IS_INTEGER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_INTEGER ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_immediate_or_boxed(
        {free, Arg1}, ?TERM_INTEGER_TAG, ?TERM_BOXED_POSITIVE_INTEGER, Label, MMod, MSt1
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 46
first_pass(<<?OP_IS_FLOAT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_FLOAT ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_FLOAT, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 47
first_pass(<<?OP_IS_NUMBER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NUMBER ~p, ~p\n", [Label, Arg1]),
    % test term_is_integer
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:if_block(MSt2, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(BSt0) ->
        % test term_is_boxed
        BSt1 = cond_jump_to_label(
            {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, BSt0
        ),
        BSt2 = MMod:and_(BSt1, Reg, ?TERM_PRIMARY_CLEAR_MASK),
        BSt3 = MMod:move_array_element(BSt2, Reg, 0, Reg),
        % Optimization : ((Reg & 0x3F) != 0x8) && ((Reg & 0x3F) != 0x18)
        % is equivalent to (Reg & 0x2F) != 0x8
        cond_jump_to_label(
            {
                {free, Reg},
                '&',
                ?TERM_BOXED_TAG_MASK_POSITIVE_INTEGER_OR_FLOAT,
                '!=',
                ?TERM_BOXED_TAG_POSITIVE_INTEGER_OR_FLOAT
            },
            Label,
            MMod,
            BSt3
        )
    end),
    MSt4 = MMod:free_native_registers(MSt3, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
% 48
first_pass(<<?OP_IS_ATOM, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_ATOM ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {{free, Reg}, '&', ?TERM_IMMED2_TAG_MASK, '!=', ?TERM_IMMED2_ATOM}, Label, MMod, MSt2
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 49
first_pass(<<?OP_IS_PID, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_PID ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_immediate_or_boxed(
        {free, Arg1}, ?TERM_PID_TAG, ?TERM_BOXED_EXTERNAL_PID, Label, MMod, MSt1
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 50
first_pass(<<?OP_IS_REFERENCE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_REFERENCE ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    MSt6 = MMod:and_(MSt5, Reg, ?TERM_BOXED_TAG_MASK),
    MSt7 = cond_jump_to_label(
        {'and', [{Reg, '!=', ?TERM_BOXED_REF}, {Reg, '!=', ?TERM_BOXED_EXTERNAL_REF}]},
        Label,
        MMod,
        MSt6
    ),
    MSt8 = MMod:free_native_registers(MSt7, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest2, MMod, MSt8, State0);
% 51
first_pass(<<?OP_IS_PORT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_PORT ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_immediate_or_boxed(
        {free, Arg1}, ?TERM_PORT_TAG, ?TERM_BOXED_EXTERNAL_PORT, Label, MMod, MSt1
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 52
first_pass(<<?OP_IS_NIL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NIL ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label({Reg, '!=', ?TERM_NIL}, Label, MMod, MSt2),
    MSt4 = MMod:free_native_registers(MSt3, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
% 53
first_pass(<<?OP_IS_BINARY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BINARY ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_binary(Arg1, Label, MMod, MSt1),
    MSt3 = MMod:free_native_registers(MSt2, [Arg1]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 55
first_pass(<<?OP_IS_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_LIST ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {'and', [
            {Reg, '!=', ?TERM_NIL},
            {{free, Reg}, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_LIST}
        ]},
        Label,
        MMod,
        MSt2
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 56
first_pass(<<?OP_IS_NONEMPTY_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NONEMPTY_LIST ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {{free, Reg}, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_LIST}, Label, MMod, MSt2
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 57
first_pass(<<?OP_IS_TUPLE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_TUPLE ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_TUPLE, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 58
first_pass(<<?OP_TEST_ARITY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Arity, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_TEST_ARITY ~p, ~p, ~p\n", [Label, Arg1, Arity]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    MSt5 = MMod:shift_right(MSt4, Reg, 6),
    MSt6 = cond_jump_to_label({Reg, '!=', Arity}, Label, MMod, MSt5),
    MSt7 = MMod:free_native_registers(MSt6, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest3, MMod, MSt7, State0);
% 59
first_pass(<<?OP_SELECT_VAL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {DefaultLabel, Rest2} = decode_label(Rest1),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_SELECT_VAL ~p, ~p", [SrcValue, DefaultLabel]),
    {MSt2, Rest4} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, CmpValue, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            {JmpLabel, AccRest2} = decode_label(AccRest1),
            ?TRACE(", ~p => ~p", [CmpValue, JmpLabel]),
            {AccMSt2, ResultReg} = MMod:call_primitive(AccMSt1, ?PRIM_TERM_COMPARE, [
                ctx, jit_state, {free, CmpValue}, SrcValue, ?TERM_COMPARE_EXACT
            ]),
            AccMSt3 = handle_error_if(
                {'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, AccMSt2
            ),
            AccMSt4 = cond_jump_to_label(
                {'(int)', {free, ResultReg}, '==', ?TERM_EQUALS}, JmpLabel, MMod, AccMSt3
            ),
            {AccMSt4, AccRest2}
        end,
        {MSt1, Rest3},
        lists:seq(0, (ListSize div 2) - 1)
    ),
    ?TRACE("\n", []),
    MSt3 = MMod:jump_to_label(MSt2, DefaultLabel),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 60
first_pass(<<?OP_SELECT_TUPLE_ARITY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {DefaultLabel, Rest2} = decode_label(Rest1),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_SELECT_TUPLE_ARITY ~p, ~p", [SrcValue, DefaultLabel]),
    {MSt2, Reg} = term_get_tuple_arity({free, SrcValue}, MMod, MSt1),
    {MSt3, Rest4} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {CmpValue, AccRest1} = decode_literal(AccRest0),
            {JmpLabel, AccRest2} = decode_label(AccRest1),
            ?TRACE(", ~p => ~p", [CmpValue, JmpLabel]),
            AccMSt1 = cond_jump_to_label({Reg, '==', CmpValue}, JmpLabel, MMod, AccMSt0),
            {AccMSt1, AccRest2}
        end,
        {MSt2, Rest3},
        lists:seq(0, (ListSize div 2) - 1)
    ),
    ?TRACE("\n", []),
    MSt4 = MMod:free_native_registers(MSt3, [Reg]),
    MSt5 = MMod:jump_to_label(MSt4, DefaultLabel),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 61
first_pass(<<?OP_JUMP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_JUMP ~p\n", [Label]),
    MSt1 = MMod:call_only_or_schedule_next(MSt0, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest1, MMod, MSt1, State0);
% 62
% Same implementation as OP_TRY, to confirm.
first_pass(<<?OP_CATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CATCH ~p, ~p\n", [Dest, Label]),
    MSt2 = term_from_catch_label(Dest, Label, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 63
first_pass(<<?OP_CATCH_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_CATCH_END ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    {MSt4, ResultReg} = MMod:call_primitive(MSt3, ?PRIM_CATCH_END, [ctx, jit_state]),
    MSt5 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt4),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest1, MMod, MSt5, State0);
% 64
first_pass(<<?OP_MOVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Source, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_MOVE ~p, ~p\n", [Source, Dest]),
    MSt3 = MMod:move_to_vm_register(MSt2, Source, Dest),
    MSt4 = MMod:free_native_registers(MSt3, [Source, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
% 65
first_pass(<<?OP_GET_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, List, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, HeadDest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    {MSt3, TailDest, Rest3} = decode_dest(Rest2, MMod, MSt2),
    ?TRACE("OP_GET_LIST ~p, ~p, ~p\n", [List, HeadDest, TailDest]),
    {MSt4, Reg} = MMod:move_to_native_register(MSt3, List),
    MSt5 = MMod:and_(MSt4, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt6 = MMod:move_array_element(MSt5, Reg, ?LIST_HEAD_INDEX, HeadDest),
    MSt7 = MMod:free_native_registers(MSt6, [HeadDest]),
    MSt8 = MMod:move_array_element(MSt7, Reg, ?LIST_TAIL_INDEX, TailDest),
    MSt9 = MMod:free_native_registers(MSt8, [Reg]),
    MSt10 = MMod:free_native_registers(MSt9, [TailDest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt10),
    first_pass(Rest3, MMod, MSt10, State0);
% 66
first_pass(<<?OP_GET_TUPLE_ELEMENT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Source, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {Element, Rest2} = decode_literal(Rest1),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    ?TRACE("OP_GET_TUPLE_ELEMENT ~p, ~p, ~p\n", [Source, Element, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, Source),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, Element + 1, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Reg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 67
first_pass(<<?OP_SET_TUPLE_ELEMENT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, NewElement, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Tuple, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    {Position, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_SET_TUPLE_ELEMENT ~p, ~p, ~p\n", [NewElement, Tuple, Position]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, Tuple),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_to_array_element(MSt4, NewElement, Reg, Position + 1),
    MSt6 = MMod:free_native_registers(MSt5, [NewElement, Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 69
first_pass(<<?OP_PUT_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Head, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Tail, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    {MSt3, Dest, Rest3} = decode_dest(Rest2, MMod, MSt2),
    ?TRACE("OP_PUT_LIST ~p, ~p, ~p\n", [Head, Tail, Dest]),
    {MSt4, ResultReg} = MMod:call_primitive(MSt3, ?PRIM_PUT_LIST, [
        ctx, {free, Head}, {free, Tail}
    ]),
    MSt5 = MMod:move_to_vm_register(MSt4, ResultReg, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [ResultReg]),
    MSt7 = MMod:free_native_registers(MSt6, [Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest3, MMod, MSt7, State0);
% 72
first_pass(<<?OP_BADMATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_BADMATCH ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADMATCH_ATOM, {free, Arg1}
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 73
first_pass(<<?OP_IF_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_IF_END\n", []),
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, offset, ?IF_CLAUSE_ATOM
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest0, MMod, MSt1, State0);
% 74
first_pass(<<?OP_CASE_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_CASE_END ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?CASE_CLAUSE_ATOM, {free, Arg1}
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 75
first_pass(<<?OP_CALL_FUN, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {ArgsCount, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_CALL_FUN ~p\n", [ArgsCount]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    {MSt2, FuncReg} = read_any_xreg(ArgsCount, MMod, MSt1),
    {MSt3, Reg} = verify_is_function(FuncReg, MMod, MSt2),
    MSt4 = MMod:call_primitive_with_cp(MSt3, ?PRIM_CALL_FUN, [
        ctx, jit_state, offset, {free, Reg}, ArgsCount
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest1, MMod, MSt4, State0);
% 77
first_pass(<<?OP_IS_FUNCTION, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_FUNCTION ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_FUN, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 78
first_pass(<<?OP_CALL_EXT_ONLY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_CALL_EXT_ONLY ~p, ~p\n", [Arity, Index]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, Arity, Index, -1]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 96
first_pass(<<?OP_FMOVE, ?COMPACT_EXTENDED_FP_REGISTER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FPRegIndex, Rest1} = decode_literal(Rest0),
    {MSt1, Dest, Rest2} = decode_dest(Rest1, MMod, MSt0),
    ?TRACE("OP_FMOVE {fp_reg, ~p}, ~p\n", [FPRegIndex, Dest]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_TERM_FROM_FLOAT, [ctx, FPRegIndex]),
    MSt3 = MMod:move_to_vm_register(MSt2, ResultReg, Dest),
    MSt4 = MMod:free_native_registers(MSt3, [ResultReg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
first_pass(<<?OP_FMOVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {FPReg, Rest2} = decode_fp_register(Rest1),
    ?TRACE("OP_FMOVE ~p, ~p\n", [SrcValue, FPReg]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
    MSt3 = MMod:free_native_registers(MSt2, [ResultReg]),
    {MSt4, Reg} = MMod:move_to_native_register(MSt3, SrcValue),
    MSt5 = MMod:and_(MSt4, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt6 = MMod:move_to_vm_register(MSt5, {free, {ptr, Reg, 1}}, FPReg),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest2, MMod, MSt6, State0);
% 97
first_pass(<<?OP_FCONV, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, SrcValue),
    {MSt3, IsNumber} = MMod:call_primitive(MSt2, ?PRIM_TERM_IS_NUMBER, [Reg]),
    MSt4 = MMod:if_block(MSt3, {'(bool)', {free, IsNumber}, '==', false}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?BADARITH_ATOM
        ])
    end),
    {{fp_reg, FPRegIndex}, Rest2} = decode_fp_register(Rest1),
    {MSt5, ResultReg} = MMod:call_primitive(MSt4, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
    MSt6 = MMod:free_native_registers(MSt5, [ResultReg]),
    ?TRACE("OP_FCONF ~p, ~p\n", [SrcValue, {fp_reg, FPRegIndex}]),
    {MSt7, ConvToFloatResReg} = MMod:call_primitive(MSt6, ?PRIM_TERM_CONV_TO_FLOAT, [
        ctx, {free, Reg}, FPRegIndex
    ]),
    MSt8 = MMod:free_native_registers(MSt7, [ConvToFloatResReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest2, MMod, MSt8, State0);
% 98
first_pass(<<?OP_FADD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FADD, Rest0, MMod, MSt0, State0);
% 99
first_pass(<<?OP_FSUB, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FSUB, Rest0, MMod, MSt0, State0);
% 100
first_pass(<<?OP_FMUL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FMUL, Rest0, MMod, MSt0, State0);
% 101
first_pass(<<?OP_FDIV, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FDIV, Rest0, MMod, MSt0, State0);
% 102
first_pass(<<?OP_FNEGATE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Label, Rest1} = decode_label(Rest0),
    {{fp_reg, FPRegIndex1}, Rest2} = decode_fp_register(Rest1),
    {{fp_reg, FPRegIndex2}, Rest3} = decode_fp_register(Rest2),
    ?TRACE("OP_FNEGATE ~p, ~p, ~p\n", [_Label, {fp_reg, FPRegIndex1}, {fp_reg, FPRegIndex2}]),
    {MSt1, Reg} = MMod:call_primitive(MSt0, ?PRIM_FNEGATE, [
        ctx, FPRegIndex1, FPRegIndex2
    ]),
    MSt2 = MMod:free_native_registers(MSt1, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest3, MMod, MSt2, State0);
% 104
first_pass(<<?OP_TRY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_TRY ~p, ~p\n", [Dest, Label]),
    MSt2 = term_from_catch_label(Dest, Label, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 105
first_pass(<<?OP_TRY_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_TRY_END ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest1, MMod, MSt3, State0);
% 106
first_pass(<<?OP_TRY_CASE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_TRY_CASE ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest1, MMod, MSt3, State0);
% 107
first_pass(<<?OP_TRY_CASE_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_TRY_CASE_END ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?TRY_CLAUSE_ATOM, Arg1
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 108
first_pass(<<?OP_RAISE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Stacktrace, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, ExcValue, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_RAISE ~p, ~p\n", [Stacktrace, ExcValue]),
    MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_RAISE, [
        ctx, jit_state, offset, Stacktrace, ExcValue
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 112
first_pass(<<?OP_APPLY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {MSt1, Module} = read_any_xreg(Arity, MMod, MSt0),
    {MSt2, Function} = read_any_xreg(Arity + 1, MMod, MSt1),
    ?TRACE("OP_APPLY ~p\n", [Arity]),
    MSt3 = verify_is_atom(Module, 0, MMod, MSt2),
    MSt4 = verify_is_atom(Function, 0, MMod, MSt3),
    MSt5 = MMod:decrement_reductions_and_maybe_schedule_next(MSt4),
    MSt6 = MMod:call_primitive_with_cp(MSt5, ?PRIM_APPLY, [
        ctx, jit_state, offset, {free, Module}, {free, Function}, Arity
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest1, MMod, MSt6, State0);
% 113
first_pass(<<?OP_APPLY_LAST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {NWords, Rest2} = decode_literal(Rest1),
    {MSt1, Module} = read_any_xreg(Arity, MMod, MSt0),
    {MSt2, Function} = read_any_xreg(Arity + 1, MMod, MSt1),
    ?TRACE("OP_APPLY_LAST ~p, ~p\n", [Arity, NWords]),
    MSt3 = verify_is_atom(Module, 0, MMod, MSt2),
    MSt4 = verify_is_atom(Function, 0, MMod, MSt3),
    MSt5 = MMod:decrement_reductions_and_maybe_schedule_next(MSt4),
    MSt6 = MMod:move_to_cp(MSt5, {y_reg, NWords}),
    MSt7 = MMod:increment_sp(MSt6, NWords + 1),
    MSt8 = MMod:call_primitive_last(MSt7, ?PRIM_APPLY, [
        ctx, jit_state, offset, {free, Module}, {free, Function}, Arity
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest2, MMod, MSt8, State0);
% 114
first_pass(<<?OP_IS_BOOLEAN, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BOOLEAN ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {'and', [{Reg, '!=', ?TRUE_ATOM}, {Reg, '!=', ?FALSE_ATOM}]}, Label, MMod, MSt2
    ),
    MSt4 = MMod:free_native_registers(MSt3, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
% 115
first_pass(<<?OP_IS_FUNCTION2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, ArityTerm, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_FUNCTION2 ~p,~p,~p\n", [Label, Arg1, ArityTerm]),
    {MSt3, FuncPtr} = term_is_boxed_with_tag_and_get_ptr(Label, Arg1, ?TERM_BOXED_FUN, MMod, MSt2),
    {MSt4, Arity} = term_to_int(ArityTerm, Label, MMod, MSt3),
    {MSt5, ModuleReg} = MMod:get_array_element(MSt4, FuncPtr, 1),
    {MSt6, IndexOrModuleReg} = MMod:get_array_element(MSt5, FuncPtr, 2),
    MSt7 = MMod:if_else_block(
        MSt6,
        {IndexOrModuleReg, '&', ?TERM_IMMED2_TAG_MASK, '!=', ?TERM_IMMED2_ATOM},
        fun(BSt0) ->
            BSt1 = MMod:shift_right(BSt0, IndexOrModuleReg, 4),
            {BSt2, FunArity} = MMod:call_primitive(BSt1, ?PRIM_MODULE_GET_FUN_ARITY, [
                ModuleReg, IndexOrModuleReg
            ]),
            cond_jump_to_label({'(int)', {free, FunArity}, '!=', Arity}, Label, MMod, BSt2)
        end,
        fun(BSt0) ->
            {BSt1, FunArity} = MMod:get_array_element(BSt0, FuncPtr, 3),
            BSt2 = MMod:shift_right(BSt1, FunArity, 4),
            cond_jump_to_label({'(int)', {free, FunArity}, '!=', Arity}, Label, MMod, BSt2)
        end
    ),
    MSt8 = MMod:free_native_registers(MSt7, [FuncPtr, IndexOrModuleReg, ModuleReg, Arity]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest3, MMod, MSt8, State0);
% 117
first_pass(<<?OP_BS_GET_INTEGER2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {_Live, Rest3} = decode_literal(Rest2),
    {MSt2, Size, Rest4} = decode_typed_compact_term(Rest3, MMod, MSt1, State0),
    {Unit, Rest5} = decode_literal(Rest4),
    {FlagsValue, Rest6} = decode_literal(Rest5),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, SizeReg} = term_to_int(Size, Fail, MMod, MSt3),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt4, SizeReg * Unit};
            true ->
                MSt5 = MMod:mul(MSt4, SizeReg, Unit),
                {MSt5, SizeReg}
        end,
    {MSt7, BSBinaryReg} = MMod:get_array_element(MSt6, MatchStateRegPtr, 1),
    {MSt8, BSOffsetReg} = MMod:get_array_element(MSt7, MatchStateRegPtr, 2),
    {MSt9, Result} = MMod:call_primitive(MSt8, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, {free, BSBinaryReg}, BSOffsetReg, NumBits, {free, FlagsValue}
    ]),
    MSt10 = handle_error_if({Result, '==', 0}, MMod, MSt9),
    MSt11 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt10),
    MSt12 = MMod:add(MSt11, BSOffsetReg, NumBits),
    MSt13 = MMod:free_native_registers(MSt12, [NumBits]),
    MSt14 = MMod:move_to_array_element(MSt13, BSOffsetReg, MatchStateRegPtr, 2),
    MSt15 = MMod:free_native_registers(MSt14, [BSOffsetReg, MatchStateRegPtr]),
    {MSt16, Dest, Rest7} = decode_dest(Rest6, MMod, MSt15),
    ?TRACE("OP_BS_GET_INTEGER2 ~p,~p,~p,~p,~p,~p,~p\n", [
        Fail, Src, _Live, Size, Unit, FlagsValue, Dest
    ]),
    MSt17 = MMod:move_to_vm_register(MSt16, Result, Dest),
    MSt18 = MMod:free_native_registers(MSt17, [Result]),
    ?ASSERT_ALL_NATIVE_FREE(MSt18),
    first_pass(Rest7, MMod, MSt18, State0);
% 118
first_pass(<<?OP_BS_GET_FLOAT2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {_Live, Rest3} = decode_literal(Rest2),
    {MSt2, Size, Rest4} = decode_typed_compact_term(Rest3, MMod, MSt1, State0),
    {Unit, Rest5} = decode_literal(Rest4),
    {FlagsValue, Rest6} = decode_literal(Rest5),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, SizeReg} = term_to_int(Size, Fail, MMod, MSt3),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt4, SizeReg * Unit};
            true ->
                MSt5 = MMod:mul(MSt4, SizeReg, Unit),
                {MSt5, SizeReg}
        end,
    {MSt7, BSBinaryReg} = MMod:get_array_element(MSt6, MatchStateRegPtr, 1),
    {MSt8, BSOffsetReg} = MMod:get_array_element(MSt7, MatchStateRegPtr, 2),
    {MSt9, Result} = MMod:call_primitive(MSt8, ?PRIM_BITSTRING_EXTRACT_FLOAT, [
        ctx, {free, BSBinaryReg}, BSOffsetReg, NumBits, {free, FlagsValue}
    ]),
    MSt10 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt9),
    MSt11 = MMod:add(MSt10, BSOffsetReg, NumBits),
    MSt12 = MMod:free_native_registers(MSt11, [NumBits]),
    MSt13 = MMod:move_to_array_element(MSt12, BSOffsetReg, MatchStateRegPtr, 2),
    MSt14 = MMod:free_native_registers(MSt13, [BSOffsetReg, MatchStateRegPtr]),
    {MSt15, Dest, Rest7} = decode_dest(Rest6, MMod, MSt14),
    ?TRACE("OP_BS_GET_FLOAT2 ~p,~p,~p,~p,~p,~p,~p\n", [
        Fail, Src, _Live, Size, Unit, FlagsValue, Dest
    ]),
    MSt16 = MMod:move_to_vm_register(MSt15, Result, Dest),
    MSt17 = MMod:free_native_registers(MSt16, [Result]),
    ?ASSERT_ALL_NATIVE_FREE(MSt17),
    first_pass(Rest7, MMod, MSt17, State0);
% 119
first_pass(<<?OP_BS_GET_BINARY2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {Live, Rest3} = decode_literal(Rest2),
    {MSt2, Size, Rest4} = decode_compact_term(Rest3, MMod, MSt1, State0),
    {Unit, Rest5} = decode_literal(Rest4),
    {FlagsValue, Rest6} = decode_literal(Rest5),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, BSBinaryReg0} = MMod:get_array_element(MSt3, MatchStateRegPtr, 1),
    {MSt5, BSOffsetReg} = MMod:get_array_element(MSt4, MatchStateRegPtr, 2),
    MSt6 =
        if
            Unit =/= 8 ->
                MMod:call_primitive_last(MSt5, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                ]);
            FlagsValue =/= 0 ->
                MMod:call_primitive_last(MSt5, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                ]);
            true ->
                MSt5
        end,
    MSt7 = MMod:if_block(MSt6, {BSOffsetReg, '&', 16#7, '!=', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [ctx, jit_state, offset, ?BADARG_ATOM])
    end),
    MSt8 = MMod:shift_right(MSt7, BSOffsetReg, 3),
    MSt9 = MMod:and_(MSt8, BSBinaryReg0, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt10, SizeReg} = MMod:get_array_element(MSt9, {free, BSBinaryReg0}, 1),
    {MSt13, SizeValue} =
        if
            Size =:= ?ALL_ATOM ->
                MSt11 = MMod:sub(MSt10, SizeReg, BSOffsetReg),
                {MSt11, SizeReg};
            is_integer(Size) ->
                % SizeReg is binary size
                % SizeVal is a constant
                MSt11 = MMod:sub(MSt10, SizeReg, Size bsl 4),
                MSt12 = cond_jump_to_label({{free, SizeReg}, '<', BSOffsetReg}, Fail, MMod, MSt11),
                {MSt12, Size bsl 4};
            true ->
                {MSt11, SizeValReg} = MMod:move_to_native_register(MSt10, Size),
                MSt12 = MMod:if_else_block(
                    MSt11,
                    {SizeValReg, '==', ?ALL_ATOM},
                    fun(BSt0) ->
                        BSt1 = MMod:sub(BSt0, SizeReg, BSOffsetReg),
                        MMod:free_native_registers(BSt1, [SizeValReg])
                    end,
                    fun(BSt0) ->
                        {BSt1, SizeValReg} = term_to_int(SizeValReg, 0, MMod, BSt0),
                        BSt2 = MMod:sub(BSt1, SizeReg, SizeValReg),
                        BSt3 = cond_jump_to_label({SizeReg, '<', BSOffsetReg}, Fail, MMod, BSt2),
                        BSt4 = MMod:move_to_native_register(BSt3, SizeValReg, SizeReg),
                        MMod:free_native_registers(BSt4, [SizeValReg])
                    end
                ),
                {MSt12, SizeReg}
        end,
    {MSt14, NewOffsetReg} = MMod:copy_to_native_register(MSt13, BSOffsetReg),
    MSt15 = MMod:add(MSt14, NewOffsetReg, SizeValue),
    MSt16 = MMod:shift_left(MSt15, NewOffsetReg, 3),
    % Write new offset
    MSt17 = MMod:move_to_array_element(MSt16, NewOffsetReg, MatchStateRegPtr, 2),
    MSt18 = MMod:free_native_registers(MSt17, [NewOffsetReg]),
    {MSt19, TrimResultReg} = MMod:call_primitive(MSt18, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt20 = MMod:free_native_registers(MSt19, [TrimResultReg]),
    {MSt21, BSBinaryReg1} = MMod:get_array_element(MSt20, {free, MatchStateRegPtr}, 1),
    MSt22 = MMod:or_(MSt21, BSBinaryReg1, ?TERM_PRIMARY_BOXED),
    {MSt23, HeapSizeReg} = MMod:call_primitive(MSt22, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        BSBinaryReg1, SizeValue
    ]),
    {MSt24, BSBinaryReg2} = memory_ensure_free_with_extra_root(
        BSBinaryReg1, Live, {free, HeapSizeReg}, MMod, MSt23
    ),
    {MSt25, ResultTerm} = MMod:call_primitive(MSt24, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, {free, BSBinaryReg2}, {free, BSOffsetReg}, {free, SizeValue}
    ]),
    {MSt26, Dest, Rest7} = decode_dest(Rest6, MMod, MSt25),
    ?TRACE("OP_BS_GET_BINARY2 ~p,~p,~p,~p,~p,~p,~p\n", [
        Fail, Src, Live, Size, Unit, FlagsValue, Dest
    ]),
    MSt27 = MMod:move_to_vm_register(MSt26, ResultTerm, Dest),
    MSt28 = MMod:free_native_registers(MSt27, [ResultTerm, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt28),
    first_pass(Rest7, MMod, MSt28, State0);
% 120
first_pass(<<?OP_BS_SKIP_BITS2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Size, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    {Unit, Rest4} = decode_literal(Rest3),
    {_FlagsValue, Rest5} = decode_literal(Rest4),
    ?TRACE("OP_BS_SKIP_BITS2 ~p, ~p, ~p, ~p, ~p\n", [Fail, Src, Size, Unit, _FlagsValue]),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, SizeReg} = term_to_int(Size, Fail, MMod, MSt3),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt4, SizeReg * Unit};
            true ->
                MSt5 = MMod:mul(MSt4, SizeReg, Unit),
                {MSt5, SizeReg}
        end,
    {MSt7, BSBinaryReg} = MMod:get_array_element(MSt6, MatchStateRegPtr, 1),
    {MSt8, BSOffsetReg} = MMod:get_array_element(MSt7, MatchStateRegPtr, 2),
    MSt9 = MMod:add(MSt8, BSOffsetReg, NumBits),
    MSt10 = MMod:free_native_registers(MSt9, [NumBits]),
    {MSt11, BSBinarySize} = term_binary_size({free, BSBinaryReg}, MMod, MSt10),
    MSt12 = MMod:shift_left(MSt11, BSBinarySize, 3),
    MSt13 = cond_jump_to_label({{free, BSBinarySize}, '<', BSOffsetReg}, Fail, MMod, MSt12),
    MSt14 = MMod:move_to_array_element(MSt13, BSOffsetReg, MatchStateRegPtr, 2),
    MSt15 = MMod:free_native_registers(MSt14, [BSOffsetReg, MatchStateRegPtr]),
    ?ASSERT_ALL_NATIVE_FREE(MSt15),
    first_pass(Rest5, MMod, MSt15, State0);
% 121
first_pass(<<?OP_BS_TEST_TAIL2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {Bits, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_BS_TEST_TAIL2 ~p, ~p, ~p\n", [Fail, Src, Bits]),
    {MSt2, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt1, Src),
    {MSt3, BSBinaryReg} = MMod:get_array_element(MSt2, MatchStateRegPtr, 1),
    {MSt4, BSOffsetReg} = MMod:get_array_element(MSt3, MatchStateRegPtr, 2),
    MSt5 = MMod:free_native_registers(MSt4, [MatchStateRegPtr]),
    MSt6 = MMod:add(MSt5, BSOffsetReg, Bits),
    {MSt7, BSBinarySize} = term_binary_size({free, BSBinaryReg}, MMod, MSt6),
    MSt8 = MMod:shift_left(MSt7, BSBinarySize, 3),
    MSt9 = cond_jump_to_label({{free, BSBinarySize}, '!=', BSOffsetReg}, Fail, MMod, MSt8),
    MSt10 = MMod:free_native_registers(MSt9, [BSOffsetReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt10),
    first_pass(Rest3, MMod, MSt10, State0);
% 124
first_pass(<<?OP_GC_BIF1, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_registers(MSt1, [TrimResultReg]),
    CappedLive =
        if
            Live > ?MAX_REG -> ?MAX_REG;
            true -> Live
        end,
    {Bif, Rest3} = decode_literal(Rest2),
    {MSt3, FuncPtr} = MMod:call_primitive(MSt2, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt4, Arg, Rest4} = decode_compact_term(Rest3, MMod, MSt3, State0),
    {MSt5, Dest, Rest5} = decode_dest(Rest4, MMod, MSt4),
    ?TRACE("OP_GC_BIF1 ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Live, Bif, Arg, Dest]),
    {MSt6, ResultReg} = MMod:call_func_ptr(MSt5, {free, FuncPtr}, [
        ctx, FailLabel, CappedLive, {free, Arg}
    ]),
    MSt7 = bif_faillabel_test(FailLabel, MMod, MSt6, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest5, MMod, MSt7, State0);
% 125
first_pass(<<?OP_GC_BIF2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_registers(MSt1, [TrimResultReg]),
    CappedLive =
        if
            Live > ?MAX_REG -> ?MAX_REG;
            true -> Live
        end,
    {Bif, Rest3} = decode_literal(Rest2),
    {MSt3, FuncPtr} = MMod:call_primitive(MSt2, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt4, Arg1, Rest4} = decode_compact_term(Rest3, MMod, MSt3, State0),
    {MSt5, Arg2, Rest5} = decode_compact_term(Rest4, MMod, MSt4, State0),
    {MSt6, Dest, Rest6} = decode_dest(Rest5, MMod, MSt5),
    ?TRACE("OP_GC_BIF2 ~p, ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Live, Bif, Arg1, Arg2, Dest]),
    {MSt7, ResultReg} = MMod:call_func_ptr(MSt6, {free, FuncPtr}, [
        ctx, FailLabel, CappedLive, {free, Arg1}, {free, Arg2}
    ]),
    MSt8 = bif_faillabel_test(FailLabel, MMod, MSt7, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest6, MMod, MSt8, State0);
% 129
first_pass(<<?OP_IS_BITSTR, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BITSTR ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    MSt6 = MMod:and_(MSt5, Reg, ?TERM_BOXED_TAG_MASK),
    MSt7 = cond_jump_to_label(
        {'and', [
            {Reg, '!=', ?TERM_BOXED_REFC_BINARY},
            {Reg, '!=', ?TERM_BOXED_HEAP_BINARY},
            {Reg, '!=', ?TERM_BOXED_SUB_BINARY}
        ]},
        Label,
        MMod,
        MSt6
    ),
    MSt8 = MMod:free_native_registers(MSt7, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest2, MMod, MSt8, State0);
% 132
first_pass(<<?OP_BS_MATCH_STRING, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {Bits, Rest3} = decode_literal(Rest2),
    {Offset, Rest4} = decode_literal(Rest3),
    ?TRACE("OP_BS_MATCH_STRING ~p,~p,~p,~p\n", [Fail, Src, Bits, Offset]),
    {MSt2, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt1, Src),
    {MSt3, BSBinaryReg} = MMod:get_array_element(MSt2, MatchStateRegPtr, 1),
    {MSt4, BSOffsetReg} = MMod:get_array_element(MSt3, MatchStateRegPtr, 2),
    {MSt5, MatchResult} = MMod:call_primitive(MSt4, ?PRIM_BITSTRING_MATCH_MODULE_STR, [
        ctx, jit_state, {free, BSBinaryReg}, BSOffsetReg, Offset, Bits
    ]),
    MSt6 = cond_jump_to_label({'(bool)', {free, MatchResult}, '==', false}, Fail, MMod, MSt5),
    MSt7 = MMod:add(MSt6, BSOffsetReg, Bits),
    MSt8 = MMod:move_to_array_element(MSt7, BSOffsetReg, MatchStateRegPtr, 2),
    MSt9 = MMod:free_native_registers(MSt8, [BSOffsetReg, MatchStateRegPtr]),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest4, MMod, MSt9, State0);
% 133
first_pass(<<?OP_BS_INIT_WRITABLE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_BS_INIT_WRITABLE\n", []),
    HeapSize = term_binary_heap_size(0, MMod),
    {MSt1, MemoryEnsureFreeReg} = MMod:call_primitive(
        MSt0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
            ctx, jit_state, HeapSize, 0, ?MEMORY_CAN_SHRINK
        ]
    ),
    MSt2 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt1),
    {MSt3, CreatedBin} = MMod:call_primitive(MSt2, ?PRIM_TERM_CREATE_EMPTY_BINARY, [ctx, 0]),
    MSt4 = MMod:set_bs(MSt3, CreatedBin),
    MSt5 = MMod:move_to_vm_register(MSt4, CreatedBin, {x_reg, 0}),
    MSt6 = MMod:free_native_registers(MSt5, [CreatedBin]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest0, MMod, MSt6, State0);
% 136
first_pass(<<?OP_TRIM, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {NWords, Rest1} = decode_literal(Rest0),
    {_NRemaining, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_TRIM ~p, ~p\n", [NWords, _NRemaining]),
    MSt1 = MMod:increment_sp(MSt0, NWords),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State0);
% 138
first_pass(<<?OP_BS_GET_UTF8, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    Rest4 = skip_compact_term(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF8, [{free, Src}]),
    MSt3 = cond_jump_to_label({Value, '==', 0}, Fail, MMod, MSt2),
    {MSt4, Dest, Rest5} = decode_dest(Rest4, MMod, MSt3),
    ?TRACE("OP_BS_GET_UTF8 ~p,~p,~p\n", [Fail, Src, Dest]),
    MSt5 = MMod:move_to_vm_register(MSt4, Value, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Value, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0);
% 139
first_pass(<<?OP_BS_SKIP_UTF8, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    Rest4 = skip_compact_term(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF8, [{free, Src}]),
    MSt3 = cond_jump_to_label({{free, Value}, '==', 0}, Fail, MMod, MSt2),
    ?TRACE("OP_BS_SKIP_UTF8 ~p,~p\n", [Fail, Src]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 140
first_pass(<<?OP_BS_GET_UTF16, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF16, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({Value, '==', 0}, Fail, MMod, MSt2),
    {MSt4, Dest, Rest5} = decode_dest(Rest4, MMod, MSt3),
    ?TRACE("OP_BS_GET_UTF16 ~p,~p,~p,~p\n", [Fail, Src, FlagsValue, Dest]),
    MSt5 = MMod:move_to_vm_register(MSt4, Value, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Value, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0);
% 141
first_pass(<<?OP_BS_SKIP_UTF16, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF16, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({{free, Value}, '==', 0}, Fail, MMod, MSt2),
    ?TRACE("OP_BS_SKIP_UTF16 ~p,~p,~p\n", [Fail, Src, FlagsValue]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 142
first_pass(<<?OP_BS_GET_UTF32, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF32, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({Value, '==', 0}, Fail, MMod, MSt2),
    {MSt4, Dest, Rest5} = decode_dest(Rest4, MMod, MSt3),
    ?TRACE("OP_BS_GET_UTF32 ~p,~p,~p,~p\n", [Fail, Src, FlagsValue, Dest]),
    MSt5 = MMod:move_to_vm_register(MSt4, Value, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Value, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0);
% 143
first_pass(<<?OP_BS_SKIP_UTF32, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF32, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({{free, Value}, '==', 0}, Fail, MMod, MSt2),
    ?TRACE("OP_BS_SKIP_UTF32 ~p,~p,~p\n", [Fail, Src, FlagsValue]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 152
first_pass(<<?OP_GC_BIF3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_registers(MSt1, [TrimResultReg]),
    CappedLive =
        if
            Live > ?MAX_REG -> ?MAX_REG;
            true -> Live
        end,
    {Bif, Rest3} = decode_literal(Rest2),
    {MSt3, FuncPtr} = MMod:call_primitive(MSt2, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt4, Arg1, Rest4} = decode_compact_term(Rest3, MMod, MSt3, State0),
    {MSt5, Arg2, Rest5} = decode_compact_term(Rest4, MMod, MSt4, State0),
    {MSt6, Arg3, Rest6} = decode_compact_term(Rest5, MMod, MSt5, State0),
    {MSt7, Dest, Rest7} = decode_dest(Rest6, MMod, MSt6),
    ?TRACE("OP_GC_BIF3 ~p, ~p, ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Live, Bif, Arg1, Arg2, Arg3, Dest]),
    {MSt8, ResultReg} = MMod:call_func_ptr(MSt7, {free, FuncPtr}, [
        ctx, FailLabel, CappedLive, {free, Arg1}, {free, Arg2}, {free, Arg3}
    ]),
    MSt9 = bif_faillabel_test(FailLabel, MMod, MSt8, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest7, MMod, MSt9, State0);
% 153
first_pass(
    <<?OP_LINE, Rest0/binary>>,
    MMod,
    MSt,
    #state{line_offsets = AccLines} = State0
) ->
    {Line, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_LINE ~p\n", [Line]),
    Offset = MMod:offset(MSt),
    first_pass(Rest1, MMod, MSt, State0#state{
        line_offsets = [{Line, Offset} | AccLines]
    });
% 154
first_pass(<<?OP_PUT_MAP_ASSOC, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    {Live, Rest4} = decode_literal(Rest3),
    ?TRACE("OP_PUT_MAP_ASSOC ~p,~p,~p,~p,[", [_Label, Src, Dest, Live]),
    {ListSize, Rest5} = decode_extended_list_header(Rest4),
    {MSt3, NewEntriesReg} = MMod:move_to_native_register(MSt2, 0),
    % First iteration to compute size
    NumElements = ListSize div 2,
    {MSt4, Rest6} = lists:foldl(
        fun(_Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            ARest2 = skip_compact_term(ARest1),
            {ASt2, PosReg} = MMod:call_primitive(ASt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, Src, {free, Key}
            ]),
            ASt3 = MMod:if_block(ASt2, {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, fun(BSt0) ->
                MMod:add(BSt0, NewEntriesReg, 1)
            end),
            ASt4 = MMod:if_block(
                ASt3, {'(int)', {free, PosReg}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            {ASt4, ARest2}
        end,
        {MSt3, Rest5},
        lists:seq(1, NumElements)
    ),
    {MSt5, SrcSizeReg} = term_get_map_size(Src, MMod, MSt4),
    MSt6 = MMod:if_else_block(
        MSt5,
        {NewEntriesReg, '==', 0},
        fun(BSt0) ->
            MMod:add(BSt0, SrcSizeReg, 2)
        end,
        fun(BSt0) ->
            BSt1 = MMod:add(BSt0, SrcSizeReg, NewEntriesReg),
            BSt2 = MMod:shift_left(BSt1, SrcSizeReg, 1),
            MMod:add(BSt2, SrcSizeReg, 3)
        end
    ),
    {MSt7, TrimResultReg} = MMod:call_primitive(MSt6, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt8 = MMod:free_native_registers(MSt7, [TrimResultReg]),
    {MSt9, NewSrc} = memory_ensure_free_with_extra_root(
        Src, Live, {free, SrcSizeReg}, MMod, MSt8
    ),
    % Second iteration to prepare KV pairs
    {MSt10, KVReg} = MMod:call_primitive(MSt9, ?PRIM_MALLOC, [
        ctx, jit_state, ListSize * MMod:word_size()
    ]),
    MSt11 = handle_error_if({KVReg, '==', 0}, MMod, MSt10),
    {MSt12, Rest6} = lists:foldl(
        fun(Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            {ASt2, Value, ARest2} = decode_compact_term(ARest1, MMod, ASt1, State0),
            ?TRACE("(~p,~p),", [Key, Value]),
            ASt3 = MMod:move_to_array_element(ASt2, Key, KVReg, Index * 2),
            ASt4 = MMod:move_to_array_element(ASt3, Value, KVReg, (Index * 2) + 1),
            ASt5 = MMod:free_native_registers(ASt4, [Key, Value]),
            {ASt5, ARest2}
        end,
        {MSt11, Rest5},
        lists:seq(0, NumElements - 1)
    ),
    ?TRACE("]\n", []),
    {MSt13, PutMapAssocReg} = MMod:call_primitive(MSt12, ?PRIM_PUT_MAP_ASSOC, [
        ctx, jit_state, {free, NewSrc}, {free, NewEntriesReg}, NumElements, KVReg
    ]),
    {MSt14, FreeReg} = MMod:call_primitive(MSt13, ?PRIM_FREE, [{free, KVReg}]),
    MSt15 = MMod:free_native_registers(MSt14, [FreeReg]),
    MSt16 = handle_error_if({PutMapAssocReg, '==', 0}, MMod, MSt15),
    MSt17 = MMod:move_to_vm_register(MSt16, PutMapAssocReg, Dest),
    MSt18 = MMod:free_native_registers(MSt17, [PutMapAssocReg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt18),
    first_pass(Rest6, MMod, MSt18, State0);
% 155
first_pass(<<?OP_PUT_MAP_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    {Live, Rest4} = decode_literal(Rest3),
    ?TRACE("OP_PUT_MAP_EXACT ~p,~p,~p,~p,[", [_Label, Src, Dest, Live]),
    {ListSize, Rest5} = decode_extended_list_header(Rest4),
    % Make sure every key from list is in src
    NumElements = ListSize div 2,
    {MSt3, Rest6} = lists:foldl(
        fun(_Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            ARest2 = skip_compact_term(ARest1),
            {ASt2, PosReg} = MMod:call_primitive(ASt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, Src, {free, Key}
            ]),
            ASt3 = MMod:if_block(ASt2, {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, fun(BSt0) ->
                MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?BADARG_ATOM
                ])
            end),
            ASt4 = MMod:if_block(
                ASt3, {'(int)', {free, PosReg}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            {ASt4, ARest2}
        end,
        {MSt2, Rest5},
        lists:seq(1, NumElements)
    ),
    {MSt4, SrcSizeReg} = term_get_map_size(Src, MMod, MSt3),
    % shared
    MSt5 = MMod:add(MSt4, SrcSizeReg, 2),
    {MSt6, TrimResultReg} = MMod:call_primitive(MSt5, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt7 = MMod:free_native_registers(MSt6, [TrimResultReg]),
    {MSt8, NewSrc} = memory_ensure_free_with_extra_root(
        Src, Live, {free, SrcSizeReg}, MMod, MSt7
    ),
    {MSt9, NewMapPtrReg} = MMod:call_primitive(MSt8, ?PRIM_TERM_COPY_MAP, [ctx, NewSrc]),
    MSt10 = MMod:and_(MSt9, NewMapPtrReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt11, Rest6} = lists:foldl(
        fun(_Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            {ASt2, Value, ARest2} = decode_compact_term(ARest1, MMod, ASt1, State0),
            ?TRACE("(~p,~p),", [Key, Value]),
            {ASt3, PosReg} = MMod:call_primitive(ASt2, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, NewSrc, Key
            ]),
            ASt4 = MMod:if_block(ASt3, {'(int)', PosReg, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(
                BSt0
            ) ->
                MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                ])
            end),
            ASt5 = term_set_map_assoc(
                NewMapPtrReg, {free, PosReg}, {free, Key}, {free, Value}, MMod, ASt4
            ),
            {ASt5, ARest2}
        end,
        {MSt10, Rest5},
        lists:seq(1, NumElements)
    ),
    ?TRACE("]\n", []),
    MSt12 = MMod:or_(MSt11, NewMapPtrReg, ?TERM_PRIMARY_BOXED),
    MSt13 = MMod:move_to_vm_register(MSt12, NewMapPtrReg, Dest),
    MSt14 = MMod:free_native_registers(MSt13, [NewMapPtrReg, Dest, NewSrc]),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest6, MMod, MSt14, State0);
% 156
first_pass(<<?OP_IS_MAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_MAP ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_MAP, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 157
first_pass(<<?OP_HAS_MAP_FIELDS, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_HAS_MAP_FIELDS ~p,~p,[", [Label, Src]),
    {MSt2, Key1, Rest4} = decode_compact_term(Rest3, MMod, MSt1, State0),
    ?TRACE("~p", [Key1]),
    {MSt3, PosReg1} = MMod:call_primitive(MSt2, ?PRIM_TERM_FIND_MAP_POS, [ctx, Src, {free, Key1}]),
    MSt4 = cond_jump_to_label({'(int)', PosReg1, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, MSt3),
    MSt5 = MMod:if_block(MSt4, {'(int)', {free, PosReg1}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(
        BSt0
    ) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
        ])
    end),
    {MSt6, Rest5} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Key, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE(",~p", [Key]),
            {AccMSt2, PosReg} = MMod:call_primitive(AccMSt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, Src, Key
            ]),
            AccMSt3 = cond_jump_to_label(
                {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, AccMSt2
            ),
            AccMSt4 = MMod:if_block(
                AccMSt3, {'(int)', {free, PosReg}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    % TODO: previous implementation yielded a slightly smaller code as raise block was shared.
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            AccMSt5 = MMod:free_native_registers(AccMSt4, [Key]),
            {AccMSt5, AccRest1}
        end,
        {MSt5, Rest4},
        lists:seq(2, ListSize)
    ),
    ?TRACE("]\n", []),
    MSt7 = MMod:free_native_registers(MSt6, [Src]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest5, MMod, MSt7, State0);
% 158
first_pass(<<?OP_GET_MAP_ELEMENTS, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_GET_MAP_ELEMENTS ~p,~p,[", [Label, Src]),
    {MSt2, Key1, Rest4} = decode_compact_term(Rest3, MMod, MSt1, State0),
    ?TRACE("~p", [Key1]),
    {MSt3, PosReg1} = MMod:call_primitive(MSt2, ?PRIM_TERM_FIND_MAP_POS, [ctx, Src, {free, Key1}]),
    MSt4 = cond_jump_to_label({'(int)', PosReg1, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, MSt3),
    MSt5 = MMod:if_block(MSt4, {'(int)', PosReg1, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
        ])
    end),
    {MSt6, SrcReg} = MMod:move_to_native_register(MSt5, Src),
    {MSt7, MapReg} = MMod:copy_to_native_register(MSt6, SrcReg),
    MSt8 = MMod:and_(MSt7, MapReg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt9 = MMod:add(MSt8, MapReg, MMod:word_size() * 2),
    {MSt10, Dest1, Rest5} = decode_dest(Rest4, MMod, MSt9),
    ?TRACE(",~p", [Dest1]),
    MSt11 = MMod:move_array_element(MSt10, MapReg, {free, PosReg1}, Dest1),
    MSt12 = MMod:free_native_registers(MSt11, [Dest1]),
    {MSt13, Rest6} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Key, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE(",~p", [Key]),
            {AccMSt2, PosReg} = MMod:call_primitive(AccMSt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, SrcReg, Key
            ]),
            AccMSt3 = cond_jump_to_label(
                {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, AccMSt2
            ),
            AccMSt4 = MMod:if_block(
                AccMSt3, {'(int)', PosReg, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    % TODO: previous implementation yielded a slightly smaller code as raise block was shared.
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            AccMSt5 = MMod:free_native_registers(AccMSt4, [Key]),
            {AccMSt6, Dest, AccRest2} = decode_dest(AccRest1, MMod, AccMSt5),
            ?TRACE(",~p", [Dest]),
            AccMSt7 = MMod:move_array_element(AccMSt6, MapReg, {free, PosReg}, Dest),
            AccMSt8 = MMod:free_native_registers(AccMSt7, [Dest]),
            {AccMSt8, AccRest2}
        end,
        {MSt12, Rest5},
        lists:seq(2, ListSize div 2)
    ),
    ?TRACE("]\n", []),
    MSt14 = MMod:free_native_registers(MSt13, [MapReg, SrcReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest6, MMod, MSt14, State0);
% 159
first_pass(
    <<?OP_IS_TAGGED_TUPLE, Rest0/binary>>, MMod, MSt0, #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Arity, Rest3} = decode_literal(Rest2),
    {AtomIndex, Rest4} = decode_atom(Rest3),
    ?TRACE("OP_IS_TAGGED_TUPLE ~p, ~p, ~p, ~p\n", [Label, Arg1, Arity, AtomIndex]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt5, TagReg} = MMod:get_array_element(MSt4, Reg, 0),
    MSt6 = cond_jump_to_label(
        {TagReg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_TUPLE}, Label, MMod, MSt5
    ),
    MSt7 = MMod:shift_right(MSt6, TagReg, 6),
    MSt8 = cond_jump_to_label({TagReg, '!=', Arity}, Label, MMod, MSt7),
    MSt9 = MMod:free_native_registers(MSt8, [TagReg]),
    MSt10 = MMod:move_array_element(MSt9, Reg, 1, Reg),
    {MSt11, AtomReg} =
        case maps:find(AtomResolver(AtomIndex), ?DEFAULT_ATOMS) of
            error ->
                MMod:call_primitive(
                    MSt10, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, AtomIndex]
                );
            {ok, Val} ->
                {MSt10, Val}
        end,
    MSt12 = cond_jump_to_label({Reg, '!=', AtomReg}, Label, MMod, MSt11),
    MSt13 = MMod:free_native_registers(MSt12, [Reg]),
    MSt14 = MMod:free_native_registers(MSt13, [AtomReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest4, MMod, MSt14, State0);
% 160
first_pass(<<?OP_BUILD_STACKTRACE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_STACKTRACE_BUILD, [ctx]),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, {x_reg, 0}),
    MSt3 = MMod:free_native_registers(MSt2, [ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest0, MMod, MSt3, State0);
% 161
first_pass(<<?OP_RAW_RAISE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, ExClassReg} = MMod:move_to_native_register(MSt0, {x_reg, 0}),
    MSt2 = MMod:if_block(MSt1, {ExClassReg, '==', ?ERROR_ATOM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    MSt3 = MMod:if_block(MSt2, {ExClassReg, '==', ?LOWERCASE_EXIT_ATOM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    MSt4 = MMod:if_block(MSt3, {{free, ExClassReg}, '==', ?THROW_ATOM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    MSt5 = MMod:move_to_vm_register(MSt4, ?BADARG_ATOM, {x_reg, 0}),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest0, MMod, MSt5, State0);
% 162
first_pass(<<?OP_GET_HD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_GET_HD ~p, ~p\n", [SrcValue, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, SrcValue),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, ?LIST_HEAD_INDEX, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Dest, Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 163
first_pass(<<?OP_GET_TL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_GET_TL ~p, ~p\n", [SrcValue, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, SrcValue),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, ?LIST_TAIL_INDEX, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Dest, Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 164
first_pass(<<?OP_PUT_TUPLE2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {ListSize, Rest2} = decode_extended_list_header(Rest1),
    ?TRACE("OP_PUT_TUPLE2 ~p, [", [Dest]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_TERM_ALLOC_TUPLE, [ctx, ListSize]),
    MSt3 = MMod:and_(MSt2, ResultReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, Rest3} = lists:foldl(
        fun(Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Element, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE("~p,", [Element]),
            AccMSt2 = MMod:move_to_array_element(AccMSt1, Element, ResultReg, Index),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [Element]),
            {AccMSt3, AccRest1}
        end,
        {MSt3, Rest2},
        lists:seq(1, ListSize)
    ),
    ?TRACE("]\n", []),
    MSt5 = MMod:or_(MSt4, ResultReg, ?TERM_PRIMARY_BOXED),
    MSt6 = MMod:move_to_vm_register(MSt5, ResultReg, Dest),
    MSt7 = MMod:free_native_registers(MSt6, [Dest, ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest3, MMod, MSt7, State0);
% 165
first_pass(<<?OP_BS_GET_TAIL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Src, Rest1} = decode_typed_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    {Live, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_BS_GET_TAIL ~p, ~p, ~p\n", [Src, Dest, Live]),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, BSBinaryReg} = MMod:get_array_element(MSt3, MatchStateRegPtr, 1),
    {MSt5, BSOffsetReg} = MMod:get_array_element(MSt4, MatchStateRegPtr, 2),
    MSt6 = MMod:free_native_registers(MSt5, [MatchStateRegPtr]),
    MSt7 = MMod:and_(MSt6, BSBinaryReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt8, ResultTerm, NewMatchState} = do_get_tail(
        Src, Live, BSOffsetReg, BSBinaryReg, MMod, MSt7
    ),
    MSt9 = MMod:free_native_registers(MSt8, [BSBinaryReg]),
    {MSt10, MatchStateReg1} = MMod:move_to_native_register(MSt9, NewMatchState),
    MSt11 = MMod:and_(MSt10, MatchStateReg1, ?TERM_PRIMARY_CLEAR_MASK),
    MSt12 = MMod:move_to_array_element(MSt11, BSOffsetReg, MatchStateReg1, 2),
    MSt13 = MMod:move_to_vm_register(MSt12, ResultTerm, Dest),
    MSt14 = MMod:free_native_registers(MSt13, [MatchStateReg1, BSOffsetReg, ResultTerm, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest3, MMod, MSt14, State0);
% 166
first_pass(<<?OP_BS_START_MATCH3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Live, Rest3} = decode_literal(Rest2),
    {MSt2, Dest, Rest4} = decode_dest(Rest3, MMod, MSt1),
    ?TRACE("OP_BS_START_MATCH3 ~p, ~p, ~p, ~p\n", [Fail, Src, Live, Dest]),
    MSt3 = verify_is_binary_or_match_state(Fail, Src, MMod, MSt2),
    {MSt4, NewSrc} = term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt3),
    MSt5 = MMod:free_native_registers(MSt4, [NewSrc, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 167
first_pass(<<?OP_BS_GET_POSITION, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Src, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    {_Live, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_BS_GET_POSITION ~p, ~p, ~p\n", [Src, Dest, _Live]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, Src),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 2, Reg),
    MSt6 = MMod:shift_left(MSt5, Reg, 4),
    MSt7 = MMod:or_(MSt6, Reg, ?TERM_INTEGER_TAG),
    MSt8 = MMod:move_to_vm_register(MSt7, Reg, Dest),
    MSt9 = MMod:free_native_registers(MSt8, [Reg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest3, MMod, MSt9, State0);
% 168
first_pass(<<?OP_BS_SET_POSITION, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Src, Rest1} = decode_typed_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Pos, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_BS_SET_POSITION ~p, ~p\n", [Src, Pos]),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, PosVal} = term_to_int(Pos, 0, MMod, MSt3),
    MSt5 = MMod:move_to_array_element(MSt4, PosVal, MatchStateRegPtr, 2),
    MSt6 = MMod:free_native_registers(MSt5, [PosVal, MatchStateRegPtr]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest2, MMod, MSt6, State0);
% 169
first_pass(<<?OP_SWAP, Rest0/binary>>, MMod, MSt0, State) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, ArgA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {MSt2, ArgB, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_SWAP ~p, ~p\n", [ArgA, ArgB]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, ArgA),
    MSt4 = MMod:move_to_vm_register(MSt3, ArgB, ArgA),
    MSt5 = MMod:move_to_vm_register(MSt4, Reg, ArgB),
    MSt6 = MMod:free_native_registers(MSt5, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest2, MMod, MSt6, State);
% 170
first_pass(<<?OP_BS_START_MATCH4, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_atom_or_label(Rest0, State0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, Src, Rest3} = decode_compact_term(Rest2, MMod, MSt0, State0),
    {MSt2, Dest, Rest4} = decode_dest(Rest3, MMod, MSt1),
    ?TRACE("OP_BS_START_MATCH4 ~p, ~p, ~p, ~p\n", [Fail, Live, Src, Dest]),
    MSt3 =
        if
            is_integer(Fail) ->
                verify_is_binary_or_match_state(Fail, Src, MMod, MSt2);
            Fail =:= no_fail ->
                MSt2;
            Fail =:= resume ->
                MSt2
        end,
    {MSt4, NewSrc} = term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt3),
    MSt5 = MMod:free_native_registers(MSt4, [NewSrc, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 171
first_pass(<<?OP_MAKE_FUN3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FunIndex, Rest1} = decode_literal(Rest0),
    {MSt1, Dest, Rest2} = decode_dest(Rest1, MMod, MSt0),
    {NumFree, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_MAKE_FUN3 ~p, [", [Dest]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_TERM_ALLOC_FUN, [
        ctx, jit_state, FunIndex, NumFree
    ]),
    MSt3 = MMod:and_(MSt2, ResultReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, Rest4} = lists:foldl(
        fun(Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Element, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE("~p,", [Element]),
            AccMSt2 = MMod:move_to_array_element(AccMSt1, Element, ResultReg, Index),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [Element]),
            {AccMSt3, AccRest1}
        end,
        {MSt3, Rest3},
        lists:seq(3, NumFree + 2)
    ),
    ?TRACE("]\n", []),
    MSt5 = MMod:or_(MSt4, ResultReg, ?TERM_PRIMARY_BOXED),
    MSt6 = MMod:move_to_vm_register(MSt5, ResultReg, Dest),
    MSt7 = MMod:free_native_registers(MSt6, [Dest, ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest4, MMod, MSt7, State0);
% 172
first_pass(<<?OP_INIT_YREGS, Rest0/binary>>, MMod, MSt0, State) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {ListSize, Rest1} = decode_extended_list_header(Rest0),
    ?TRACE("OP_INIT_YREGS ~p\n", [ListSize]),
    {MSt1, Rest2} = lists:foldl(
        fun(_, {AccMSt0, AccRest0}) ->
            {AccMSt1, Dest, AccRest1} = decode_dest(AccRest0, MMod, AccMSt0),
            AccMSt2 = MMod:move_to_vm_register(AccMSt1, ?TERM_NIL, Dest),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [Dest]),
            {AccMSt3, AccRest1}
        end,
        {MSt0, Rest1},
        lists:duplicate(ListSize, [])
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State);
% 173
first_pass(<<?OP_RECV_MARKER_BIND, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {MSt2, RegB, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_RECV_MARKER_BIND ~p, ~p\n", [RegA, RegB]),
    MSt3 = MMod:free_native_registers(MSt2, [RegA, RegB]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 174
first_pass(<<?OP_RECV_MARKER_CLEAR, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_CLEAR ~p\n", [RegA]),
    MSt2 = MMod:free_native_registers(MSt1, [RegA]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 175
first_pass(<<?OP_RECV_MARKER_RESERVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_RESERVE ~p\n", [Dest]),
    % Clear register to avoid any issue with GC
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest1, MMod, MSt3, State0);
% 176
first_pass(<<?OP_RECV_MARKER_USE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_USE ~p\n", [RegA]),
    MSt2 = MMod:free_native_registers(MSt1, [RegA]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 177
first_pass(
    <<?OP_BS_CREATE_BIN, Rest0/binary>>, MMod, MSt0, #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {Alloc, Rest2} = decode_allocator_list(MMod, Rest1),
    {Live, Rest3} = decode_literal(Rest2),
    {_Unit, Rest4} = decode_literal(Rest3),
    % TODO: add skip_dest and redecode when we need it
    {MSt1, Dest, Rest5} = decode_dest(Rest4, MMod, MSt0),
    ?TRACE("OP_BS_CREATE_BIN ~p, ~p, ~p, ~p, [", [Fail, Alloc, Live, _Unit]),
    {ListLen, Rest6} = decode_extended_list_header(Rest5),
    % Compute binary size and verify types in first iteration
    NBSegments = ListLen div 6,
    {Rest7, MSt2, BinaryLitSize, BinaryRegSize, State1} = lists:foldl(
        fun(_Index, {AccRest0, AccMSt0, AccLiteralSize0, AccSizeReg0, AccState0}) ->
            {AtomTypeIndex, AccRest1} = decode_atom(AccRest0),
            AtomType = AtomResolver(AtomTypeIndex),
            {_Seg, AccRest2} = decode_literal(AccRest1),
            {SegmentUnit, AccRest3} = decode_literal(AccRest2),
            AccRest4 = skip_compact_term(AccRest3),
            {AccMSt1, Src, AccRest5} = decode_compact_term(AccRest4, MMod, AccMSt0, AccState0),
            {AccMSt2, Size, AccRest6} = decode_compact_term(AccRest5, MMod, AccMSt1, AccState0),
            {AccMSt3, AccLiteralSize1, AccSizeReg1, AccState1} = first_pass_bs_create_bin_compute_size(
                AtomType,
                Src,
                Size,
                SegmentUnit,
                Fail,
                AccLiteralSize0,
                AccSizeReg0,
                MMod,
                AccMSt2,
                AccState0
            ),
            AccMSt4 = MMod:free_native_registers(AccMSt3, [Src, Size]),
            {AccRest6, AccMSt4, AccLiteralSize1, AccSizeReg1, AccState1}
        end,
        {Rest6, MSt1, 0, undefined, State0},
        lists:seq(1, NBSegments)
    ),
    {MSt4, BinaryTotalSize} =
        case {BinaryLitSize, BinaryRegSize} of
            {_, undefined} ->
                {MSt2, BinaryLitSize};
            {0, Reg} ->
                {MSt2, Reg};
            {_, _} ->
                MSt3 = MMod:add(MSt2, BinaryRegSize, BinaryLitSize),
                {MSt3, BinaryRegSize}
        end,
    MSt5 =
        if
            is_integer(BinaryTotalSize) andalso BinaryTotalSize band 16#7 =/= 0 ->
                MMod:call_primitive_last(MSt4, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                ]);
            is_integer(BinaryTotalSize) ->
                MSt4;
            true ->
                MMod:if_block(MSt4, {BinaryTotalSize, '&', 16#7, '!=', 0}, fun(BlockSt) ->
                    MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                    ])
                end)
        end,
    {MSt6, TrimResultReg} = MMod:call_primitive(MSt5, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt7 = MMod:free_native_registers(MSt6, [TrimResultReg]),
    {MSt12, BinaryTotalSizeInBytes, AllocSize} =
        if
            is_integer(BinaryTotalSize) ->
                {MSt7, (BinaryTotalSize div 8),
                    term_binary_heap_size((BinaryTotalSize div 8), MMod) + Alloc};
            true ->
                MSt8 = MMod:shift_right(MSt7, BinaryTotalSize, 3),
                {MSt9, BinaryTotalSize0} = MMod:copy_to_native_register(MSt8, BinaryTotalSize),
                {MSt10, AllocSizeReg} = term_binary_heap_size({free, BinaryTotalSize0}, MMod, MSt9),
                case Alloc of
                    0 ->
                        {MSt10, BinaryTotalSize, AllocSizeReg};
                    _ ->
                        MSt11 = MMod:add(MSt10, AllocSizeReg, Alloc),
                        {MSt11, BinaryTotalSize, AllocSizeReg}
                end
        end,
    {MSt13, MemoryEnsureFreeReg} = MMod:call_primitive(
        MSt12, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
            ctx, jit_state, {free, AllocSize}, Live, ?MEMORY_CAN_SHRINK
        ]
    ),
    MSt14 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt13),
    {MSt15, CreatedBin} = MMod:call_primitive(MSt14, ?PRIM_TERM_CREATE_EMPTY_BINARY, [
        ctx, {free, BinaryTotalSizeInBytes}
    ]),
    % We redo the decoding. Rest7 should still be equal to previous value.
    {Rest7, MSt16, FinalOffset} = lists:foldl(
        fun(_Index, {AccRest0, AccMSt0, AccOffset0}) ->
            {AtomTypeIndex, AccRest1} = decode_atom(AccRest0),
            AtomType = AtomResolver(AtomTypeIndex),
            {_Seg, AccRest2} = decode_literal(AccRest1),
            {SegmentUnit, AccRest3} = decode_literal(AccRest2),
            {AccMSt1, Flags, AccRest4} = decode_compact_term(AccRest3, MMod, AccMSt0, State1),
            {AccMSt2, Src, AccRest5} = decode_compact_term(AccRest4, MMod, AccMSt1, State1),
            {AccMSt3, Size, AccRest6} = decode_compact_term(AccRest5, MMod, AccMSt2, State1),
            ?TRACE("{~p,~p,~p,~p,~p,~p},", [AtomType, _Seg, SegmentUnit, Flags, Src, Size]),
            {AccMSt4, AccOffset1} = first_pass_bs_create_bin_insert_value(
                AtomType,
                Flags,
                Src,
                Size,
                SegmentUnit,
                Fail,
                CreatedBin,
                AccOffset0,
                MMod,
                AccMSt3
            ),
            AccMSt5 = MMod:free_native_registers(AccMSt4, [Flags, Src, Size]),
            {AccRest6, AccMSt5, AccOffset1}
        end,
        {Rest6, MSt15, 0},
        lists:seq(1, NBSegments)
    ),
    ?TRACE("]\n", []),
    MSt17 = MMod:free_native_registers(MSt16, [FinalOffset]),
    MSt18 = MMod:move_to_vm_register(MSt17, CreatedBin, Dest),
    MSt19 = MMod:free_native_registers(MSt18, [CreatedBin, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt19),
    first_pass(Rest7, MMod, MSt19, State1);
% 178
first_pass(<<?OP_CALL_FUN2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Tag, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {ArgsCount, Rest2} = decode_literal(Rest1),
    {MSt2, Fun, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_CALL_FUN2 ~p, ~p, ~p\n", [Tag, ArgsCount, Fun]),
    % We ignore Tag (could be literal 0 or atom unsafe)
    MSt3 = MMod:free_native_registers(MSt2, [Tag]),
    MSt4 = MMod:decrement_reductions_and_maybe_schedule_next(MSt3),
    {MSt5, Reg} = verify_is_function(Fun, MMod, MSt4),
    MSt6 = MMod:call_primitive_with_cp(MSt5, ?PRIM_CALL_FUN, [
        ctx, jit_state, offset, {free, Reg}, ArgsCount
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 180
first_pass(<<?OP_BADRECORD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_BADRECORD ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADRECORD_ATOM, Arg1
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 181
first_pass(
    <<?OP_UPDATE_RECORD, Rest0/binary>>, MMod, MSt0, #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {HintAtomIndex, Rest1} = decode_atom(Rest0),
    Hint = AtomResolver(HintAtomIndex),
    {Size, Rest2} = decode_literal(Rest1),
    {MSt1, Src, Rest3} = decode_compact_term(Rest2, MMod, MSt0, State0),
    {MSt2, SrcReg} = MMod:move_to_native_register(MSt1, Src),
    MSt3 = MMod:and_(MSt2, SrcReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, Dest, Rest4} = decode_dest(Rest3, MMod, MSt3),
    {ListLen, Rest5} = decode_extended_list_header(Rest4),
    ?TRACE("OP_UPDATE_RECORD ~p, ~p, ~p, ~p, [", [Hint, Size, Src, Dest]),
    {MSt5, DestReg} = MMod:call_primitive(MSt4, ?PRIM_TERM_ALLOC_TUPLE, [ctx, Size]),
    MSt6 = MMod:and_(MSt5, DestReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt7, ReuseReg} = MMod:move_to_native_register(
        MSt6,
        if
            Hint =:= reuse -> 1;
            true -> 0
        end
    ),
    MSt8 = lists:foldl(
        fun(Index, AccMSt0) ->
            {AccMSt1, SrcValue} = MMod:get_array_element(AccMSt0, SrcReg, Index),
            AccMSt2 = MMod:move_to_array_element(AccMSt1, SrcValue, DestReg, Index),
            MMod:free_native_registers(AccMSt2, [SrcValue])
        end,
        MSt7,
        lists:seq(1, Size)
    ),
    {MSt9, Rest6} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {UpdateIx, AccRest1} = decode_literal(AccRest0),
            {AccMSt1, UpdateValue, AccRest2} = decode_compact_term(AccRest1, MMod, AccMSt0, State0),
            AccMSt2 = MMod:if_else_block(
                AccMSt1,
                {'(bool)', ReuseReg, '!=', false},
                fun(BSt0) ->
                    {BSt1, OldValueReg} = MMod:get_array_element(BSt0, DestReg, UpdateIx),
                    {BSt2, ResultReg} = MMod:call_primitive(BSt1, ?PRIM_TERM_COMPARE, [
                        ctx, jit_state, {free, OldValueReg}, UpdateValue, ?TERM_COMPARE_EXACT
                    ]),
                    BSt3 = handle_error_if(
                        {'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, BSt2
                    ),
                    MMod:if_block(BSt3, {'(int)', {free, ResultReg}, '!=', ?TERM_EQUALS}, fun(ESt0) ->
                        ESt1 = MMod:move_to_array_element(ESt0, UpdateValue, DestReg, UpdateIx),
                        MMod:move_to_native_register(ESt1, 0, ReuseReg)
                    end)
                end,
                fun(BSt0) ->
                    MMod:move_to_array_element(BSt0, UpdateValue, DestReg, UpdateIx)
                end
            ),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [UpdateValue]),
            {AccMSt3, AccRest2}
        end,
        {MSt8, Rest5},
        lists:seq(1, ListLen div 2)
    ),
    ?TRACE("]\n", []),
    MSt10 = MMod:if_else_block(
        MSt9,
        {'(bool)', {free, ReuseReg}, '!=', false},
        fun(BSt0) ->
            BSt1 = MMod:or_(BSt0, SrcReg, ?TERM_PRIMARY_BOXED),
            MMod:move_to_vm_register(BSt1, SrcReg, Dest)
        end,
        fun(BSt0) ->
            BSt1 = MMod:or_(BSt0, DestReg, ?TERM_PRIMARY_BOXED),
            MMod:move_to_vm_register(BSt1, DestReg, Dest)
        end
    ),
    MSt11 = MMod:free_native_registers(MSt10, [DestReg, SrcReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt11),
    first_pass(Rest6, MMod, MSt11, State0);
% 182
first_pass(<<?OP_BS_MATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, MatchState, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ListLen, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_BS_MATCH ~p, ~p, [", [Fail, MatchState]),
    {MSt2, MatchStateReg0} = MMod:copy_to_native_register(MSt1, MatchState),
    MSt3 = MMod:and_(MSt2, MatchStateReg0, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, BSBinaryReg} = MMod:get_array_element(MSt3, MatchStateReg0, 1),
    {MSt5, BSOffsetReg} = MMod:get_array_element(MSt4, MatchStateReg0, 2),
    MSt6 = MMod:free_native_registers(MSt5, [MatchStateReg0]),
    MSt7 = MMod:and_(MSt6, BSBinaryReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt8, MatchStateReg1} = MMod:move_to_native_register(MSt7, MatchState),
    {MSt9, Rest4, NewMatchState, NewBSOffsetReg} = first_pass_bs_match(
        Fail, MatchStateReg1, BSBinaryReg, BSOffsetReg, ListLen, Rest3, MMod, MSt8, State0
    ),
    ?TRACE("]\n", []),
    MSt10 = MMod:free_native_registers(MSt9, [BSBinaryReg, NewBSOffsetReg, NewMatchState]),
    ?ASSERT_ALL_NATIVE_FREE(MSt10),
    first_pass(Rest4, MMod, MSt10, State0).

first_pass_bs_create_bin_compute_size(
    AtomType, Src, _Size, _SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when AtomType =:= utf8 orelse AtomType =:= utf16 ->
    {MSt1, SrcValue} = term_to_int(Src, Fail, MMod, MSt0),
    {MSt2, ResultReg} =
        case AtomType of
            utf8 ->
                MMod:call_primitive(MSt1, ?PRIM_BITSTRING_UTF8_SIZE, [{free, SrcValue}]);
            utf16 ->
                MMod:call_primitive(MSt1, ?PRIM_BITSTRING_UTF16_SIZE, [{free, SrcValue}])
        end,
    MSt3 = cond_raise_badarg_or_jump_to_fail_label(
        {ResultReg, '==', 0}, Fail, MMod, MSt2
    ),
    MSt4 = MMod:shift_left(MSt3, ResultReg, 3),
    case AccSizeReg0 of
        undefined ->
            {MSt4, AccLiteralSize0, ResultReg, State0};
        _ ->
            MSt5 = MMod:add(MSt4, AccSizeReg0, ResultReg),
            MSt6 = MMod:free_native_registers(MSt5, [ResultReg]),
            {MSt6, AccLiteralSize0, AccSizeReg0, State0}
    end;
first_pass_bs_create_bin_compute_size(
    utf32, Src, _Size, _SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) ->
    MSt1 = verify_is_integer(Src, Fail, MMod, MSt0),
    {MSt1, AccLiteralSize0 + 32, AccSizeReg0, State0};
first_pass_bs_create_bin_compute_size(
    integer, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) ->
    MSt1 = verify_is_any_integer(Src, Fail, MMod, MSt0),
    first_pass_bs_create_bin_compute_size(
        string, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt1, State0
    );
first_pass_bs_create_bin_compute_size(
    string, _Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt1, State0
) ->
    MSt2 = verify_is_integer(Size, Fail, MMod, MSt1),
    {MSt3, SizeValue} = term_to_int(Size, 0, MMod, MSt2),
    MSt5 =
        if
            is_integer(SizeValue) andalso SizeValue > 0 ->
                MSt3;
            is_integer(SizeValue) andalso Fail =:= 0 ->
                MMod:call_primitive_last(MSt3, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?BADARG_ATOM
                ]);
            is_integer(SizeValue) andalso Fail =/= 0 ->
                MMod:jump_to_label(MSt3, Fail);
            true ->
                cond_raise_badarg_or_jump_to_fail_label(
                    {SizeValue, '<', 0}, Fail, MMod, MSt3
                )
        end,
    if
        is_integer(SizeValue) ->
            {MSt5, AccLiteralSize0 + (SizeValue * SegmentUnit), AccSizeReg0, State0};
        true ->
            MSt6 = MMod:mul(MSt5, SizeValue, SegmentUnit),
            case AccSizeReg0 of
                undefined ->
                    {MSt6, AccLiteralSize0, SizeValue, State0};
                _ ->
                    MSt7 = MMod:add(MSt6, AccSizeReg0, SizeValue),
                    MSt8 = MMod:free_native_registers(MSt7, [SizeValue]),
                    {MSt8, AccLiteralSize0, AccSizeReg0, State0}
            end
    end;
first_pass_bs_create_bin_compute_size(
    AtomType, Src, ?ALL_ATOM, _SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when AtomType =:= binary orelse AtomType =:= append orelse AtomType =:= private_append ->
    MSt1 = verify_is_binary(Src, Fail, MMod, MSt0),
    {MSt2, Reg} = MMod:copy_to_native_register(MSt1, Src),
    MSt3 = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 1, Reg),
    MSt5 = MMod:shift_left(MSt4, Reg, 3),
    case AccSizeReg0 of
        undefined ->
            {MSt5, AccLiteralSize0, Reg, State0};
        _ ->
            MSt6 = MMod:add(MSt5, AccSizeReg0, Reg),
            MSt7 = MMod:free_native_registers(MSt6, [Reg]),
            {MSt7, AccLiteralSize0, AccSizeReg0, State0}
    end;
first_pass_bs_create_bin_compute_size(
    AtomType, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when
    (AtomType =:= binary orelse AtomType =:= append orelse
        AtomType =:= private_append) andalso is_integer(Size) andalso Size > 0
->
    MSt1 = verify_is_binary(Src, Fail, MMod, MSt0),
    {MSt2, SizeValue} = term_to_int(Size, 0, MMod, MSt1),
    {MSt2, AccLiteralSize0 + (SizeValue * SegmentUnit), AccSizeReg0, State0};
first_pass_bs_create_bin_compute_size(
    AtomType, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when AtomType =:= binary orelse AtomType =:= append orelse AtomType =:= private_append ->
    MSt1 = verify_is_binary(Src, Fail, MMod, MSt0),
    {MSt2, Reg0} = MMod:copy_to_native_register(MSt1, Size),
    {MSt3, Reg1} = MMod:copy_to_native_register(MSt2, Src),
    MSt4 = MMod:and_(MSt3, Reg1, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg1, 1, Reg1),
    MSt6 = MMod:shift_left(MSt5, Reg1, 3),
    MSt7 = MMod:if_block(MSt6, {{free, Reg0}, '!=', ?ALL_ATOM}, fun(BSt0) ->
        {BSt1, SizeReg} = term_to_int(Size, Fail, MMod, BSt0),
        BSt2 = cond_raise_badarg_or_jump_to_fail_label(
            {SizeReg, '<', 0}, Fail, MMod, BSt1
        ),
        BSt3 = MMod:mul(BSt2, SizeReg, SegmentUnit),
        BSt4 = cond_raise_badarg_or_jump_to_fail_label(
            {Reg1, '<', SizeReg}, Fail, MMod, BSt3
        ),
        BSt5 = MMod:move_to_native_register(BSt4, SizeReg, Reg1),
        MMod:free_native_registers(BSt5, [SizeReg])
    end),
    case AccSizeReg0 of
        undefined ->
            {MSt7, AccLiteralSize0, Reg1, State0};
        _ ->
            MSt8 = MMod:add(MSt7, AccSizeReg0, Reg1),
            MSt9 = MMod:free_native_registers(MSt8, [Reg1]),
            {MSt9, AccLiteralSize0, AccSizeReg0, State0}
    end.

first_pass_bs_create_bin_insert_value(
    utf8, _Flags, Src, _Size, _SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, SrcValue} = term_to_int(Src, Fail, MMod, MSt0),
    {MSt2, Size} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_INSERT_UTF8, [
        CreatedBin, Offset, {free, SrcValue}
    ]),
    {MSt3, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt2, Offset, Size, 8
    ),
    {MSt3, NewOffset};
first_pass_bs_create_bin_insert_value(
    utf16, Flags, Src, _Size, _SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, FlagsValue} = decode_flags_list(Flags, MMod, MSt0),
    {MSt2, SrcValue} = term_to_int(Src, Fail, MMod, MSt1),
    {MSt3, Size} = MMod:call_primitive(MSt2, ?PRIM_BITSTRING_INSERT_UTF16, [
        CreatedBin, Offset, {free, SrcValue}, {free, FlagsValue}
    ]),
    {MSt4, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt3, Offset, Size, 8
    ),
    {MSt4, NewOffset};
first_pass_bs_create_bin_insert_value(
    utf32, Flags, Src, _Size, _SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, FlagsValue} = decode_flags_list(Flags, MMod, MSt0),
    {MSt2, SrcValue} = term_to_int(Src, Fail, MMod, MSt1),
    {MSt3, BoolResult} = MMod:call_primitive(MSt2, ?PRIM_BITSTRING_INSERT_UTF32, [
        CreatedBin, Offset, {free, SrcValue}, {free, FlagsValue}
    ]),
    MSt4 = cond_raise_badarg_or_jump_to_fail_label(
        {'(bool)', {free, BoolResult}, '==', false}, Fail, MMod, MSt3
    ),
    {MSt5, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt4, Offset, 4, 8
    ),
    {MSt5, NewOffset};
first_pass_bs_create_bin_insert_value(
    integer, Flags, Src, Size, SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    % term_to_int can raise a badarg and use a temp register for this, start
    % with it.
    {MSt1, SizeValue} = term_to_int(Size, Fail, MMod, MSt0),
    % Because we're calling a function without ctx as an arg, we need to move
    % the value now to a register
    {MSt2, SrcReg} = MMod:move_to_native_register(MSt1, Src),
    {MSt3, FlagsValue} = decode_flags_list(Flags, MMod, MSt2),
    MSt4 = MMod:mul(MSt3, SizeValue, SegmentUnit),
    {MSt5, BoolResult} = MMod:call_primitive(MSt4, ?PRIM_BITSTRING_INSERT_INTEGER, [
        CreatedBin, Offset, {free, SrcReg}, SizeValue, {free, FlagsValue}
    ]),
    MSt6 = cond_raise_badarg_or_jump_to_fail_label(
        {'(bool)', {free, BoolResult}, '==', false}, Fail, MMod, MSt5
    ),
    {MSt7, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt6, Offset, SizeValue, 1
    ),
    {MSt7, NewOffset};
first_pass_bs_create_bin_insert_value(
    string, _Flags, Src, Size, SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, SrcValue} = term_to_int(Src, Fail, MMod, MSt0),
    {MSt2, SizeValue} = term_to_int(Size, Fail, MMod, MSt1),
    {MSt3, BitSize} =
        if
            is_integer(SizeValue) andalso is_integer(SegmentUnit) ->
                {MSt2, SizeValue * SegmentUnit};
            true ->
                {MMod:mul(MSt2, SizeValue, SegmentUnit), SizeValue}
        end,
    {MSt4, VoidResult} = MMod:call_primitive(MSt3, ?PRIM_BITSTRING_COPY_MODULE_STR, [
        ctx, jit_state, CreatedBin, Offset, {free, SrcValue}, BitSize
    ]),
    MSt5 = MMod:free_native_registers(MSt4, [VoidResult]),
    {MSt6, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt5, Offset, BitSize, 1
    ),
    {MSt6, NewOffset};
first_pass_bs_create_bin_insert_value(
    AtomType, _Flags, Src, Size, _SegmentUnit, _Fail, CreatedBin, Offset, MMod, MSt0
) when AtomType =:= binary orelse AtomType =:= append orelse AtomType =:= private_append ->
    {MSt1, SizeValue} = MMod:call_primitive(MSt0, ?PRIM_BITSTRING_COPY_BINARY, [
        ctx, jit_state, CreatedBin, Offset, Src, Size
    ]),
    MSt2 = MMod:if_block(MSt1, {SizeValue, '<', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_HANDLE_ERROR, [
            ctx, jit_state, offset
        ])
    end),
    {MSt3, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt2, Offset, SizeValue, 1
    ),
    {MSt3, NewOffset};
first_pass_bs_create_bin_insert_value(
    _OtherType, _Flag, _Src, _Size, _SegmentUnit, _Fail, _CreatedBin, Offset, _MMod, MSt0
) ->
    {MSt0, Offset}.

first_pass_bs_create_bin_insert_value_increment_offset(_MMod, MSt0, Offset, Size, Unit) when
    is_integer(Offset) andalso is_integer(Size) andalso is_integer(Unit)
->
    {MSt0, Offset + (Size * Unit)};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, 0, Size, 8) when is_atom(Size) ->
    MSt1 = MMod:shift_left(MSt0, Size, 3),
    {MSt1, Size};
first_pass_bs_create_bin_insert_value_increment_offset(_MMod, MSt0, 0, Size, 1) ->
    {MSt0, Size};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 8) when
    is_integer(Offset) andalso is_atom(Size)
->
    MSt1 = MMod:shift_left(MSt0, Size, 3),
    MSt2 = MMod:add(MSt1, Size, Offset),
    {MSt2, Size};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 1) when
    is_integer(Offset)
->
    MSt1 = MMod:add(MSt0, Size, Offset),
    {MSt1, Size};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, Unit) when
    is_integer(Size) andalso is_integer(Unit)
->
    MSt1 = MMod:add(MSt0, Offset, Size * Unit),
    {MSt1, Offset};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 8) when
    is_atom(Size)
->
    MSt1 = MMod:shift_left(MSt0, Size, 3),
    MSt2 = MMod:add(MSt1, Offset, Size),
    MSt3 = MMod:free_native_registers(MSt2, [Size]),
    {MSt3, Offset};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 1) ->
    MSt1 = MMod:add(MSt0, Offset, Size),
    MSt2 = MMod:free_native_registers(MSt1, [Size]),
    {MSt2, Offset}.

first_pass_bs_match(_Fail, MatchState, _BSBinaryReg, BSOffsetReg, 0, Rest, _MMod, MSt, _State) ->
    {MSt, Rest, MatchState, BSOffsetReg};
first_pass_bs_match(
    Fail,
    MatchState,
    BSBinaryReg,
    BSOffsetReg,
    J0,
    Rest0,
    MMod,
    MSt0,
    #state{atom_resolver = AtomResolver} = State0
) ->
    {CommandAtomIndex, Rest1} = decode_atom(Rest0),
    Command = AtomResolver(CommandAtomIndex),
    J1 = J0 - 1,
    {J2, Rest2, NewMatchState, NewBSOffsetReg, MSt1} =
        case Command of
            ensure_at_least ->
                first_pass_bs_match_ensure_at_least(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            ensure_exactly ->
                first_pass_bs_match_ensure_exactly(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            integer ->
                first_pass_bs_match_integer(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
                );
            binary ->
                first_pass_bs_match_binary(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
                );
            get_tail ->
                first_pass_bs_match_get_tail(
                    MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            '=:=' ->
                first_pass_bs_match_equal_colon_equal(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            skip ->
                first_pass_bs_match_skip(MatchState, BSOffsetReg, J1, Rest1, MMod, MSt0)
        end,
    % offset needs to be updated in the loop
    {MSt2, MatchStateReg1} = MMod:copy_to_native_register(MSt1, NewMatchState),
    MSt3 = MMod:and_(MSt2, MatchStateReg1, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_to_array_element(MSt3, NewBSOffsetReg, MatchStateReg1, 2),
    MSt5 = MMod:free_native_registers(MSt4, [MatchStateReg1]),
    first_pass_bs_match(
        Fail, NewMatchState, BSBinaryReg, NewBSOffsetReg, J2, Rest2, MMod, MSt5, State0
    ).

first_pass_bs_match_ensure_at_least(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0
) ->
    {Stride, Rest1} = decode_literal(Rest0),
    if
        Stride < 0 ->
            MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                ctx, jit_state, offset, ?BADARG_ATOM
            ]),
            {J0, Rest0, MatchState, BSOffsetReg, MSt1};
        true ->
            % TODO: check use of unit here (TODO is the same in opcodeswitch.h)
            {_Unit, Rest2} = decode_literal(Rest1),
            ?TRACE("{ensure_at_least,~p,~p},", [Stride, _Unit]),
            {MSt1, Reg} = MMod:get_array_element(MSt0, BSBinaryReg, 1),
            MSt2 = MMod:shift_left(MSt1, Reg, 3),
            % Reg is bs_bin_size * 8 (use unit instead ??)
            MSt3 = MMod:sub(MSt2, Reg, BSOffsetReg),
            % Reg is (bs_bin_size * 8) - bs_offset
            MSt4 = cond_jump_to_label({Reg, '<', Stride}, Fail, MMod, MSt3),
            MSt5 = MMod:free_native_registers(MSt4, [Reg]),
            {J0 - 2, Rest2, MatchState, BSOffsetReg, MSt5}
    end.

first_pass_bs_match_ensure_exactly(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0
) ->
    {Stride, Rest1} = decode_literal(Rest0),
    if
        Stride < 0 ->
            MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                ctx, jit_state, offset, ?BADARG_ATOM
            ]),
            {J0, Rest0, MatchState, BSOffsetReg, MSt1};
        true ->
            ?TRACE("{ensure_exactly,~p},", [Stride]),
            {MSt1, Reg} = MMod:get_array_element(MSt0, BSBinaryReg, 1),
            MSt2 = MMod:shift_left(MSt1, Reg, 3),
            % Reg is bs_bin_size * 8 (use unit instead ??)
            MSt3 = MMod:sub(MSt2, Reg, BSOffsetReg),
            % Reg is (bs_bin_size * 8) - bs_offset
            MSt4 = cond_jump_to_label({Reg, '!=', Stride}, Fail, MMod, MSt3),
            MSt5 = MMod:free_native_registers(MSt4, [Reg]),
            {J0 - 1, Rest1, MatchState, BSOffsetReg, MSt5}
    end.

first_pass_bs_match_integer(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0, State0
) ->
    {_Live, Rest1} = decode_literal(Rest0),
    {Flags, Rest2} = decode_compile_time_literal(Rest1, State0),
    {MSt1, FlagsValue} = decode_flags_list(Flags, MMod, MSt0),
    {MSt2, Size, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt0, State0),
    {Unit, Rest4} = decode_literal(Rest3),
    ?TRACE("{integer,~p,~p,~p, ", [Flags, Size, Unit]),
    {MSt3, SizeReg} = term_to_int(Size, 0, MMod, MSt1),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt2, SizeReg * Unit};
            true ->
                MSt3 = MMod:mul(SizeReg, Unit),
                {MSt3, SizeReg}
        end,
    {MSt7, Result} = MMod:call_primitive(MSt6, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, BSBinaryReg, BSOffsetReg, NumBits, {free, FlagsValue}
    ]),
    MSt8 = handle_error_if({Result, '==', 0}, MMod, MSt7),
    MSt9 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt8),
    MSt10 =
        case MMod:available_regs(MSt9) of
            [] ->
                MMod:free_native_registers(MSt9, [BSOffsetReg]);
            _ ->
                MSt9
        end,
    {MSt11, Dest, Rest5} = decode_dest(Rest4, MMod, MSt10),
    ?TRACE("~p},", [Dest]),
    MSt12 = MMod:move_to_vm_register(MSt11, Result, Dest),
    MSt13 = MMod:free_native_registers(MSt12, [Result, Dest]),
    case MMod:available_regs(MSt9) of
        [] ->
            MSt14 = MMod:and_(MSt13, MatchState, ?TERM_PRIMARY_CLEAR_MASK),
            {MSt15, NewBSOffsetReg} = MMod:get_array_element(MSt14, MatchState, 2),
            MSt16 = MMod:or_(MSt15, MatchState, ?TERM_PRIMARY_BOXED),
            MSt17 = MMod:add(MSt16, NewBSOffsetReg, NumBits),
            MSt18 = MMod:free_native_registers(MSt17, [NumBits]),
            {J0 - 5, Rest5, MatchState, NewBSOffsetReg, MSt18};
        _ ->
            MSt14 = MMod:add(MSt13, BSOffsetReg, NumBits),
            MSt15 = MMod:free_native_registers(MSt14, [NumBits]),
            {J0 - 5, Rest5, MatchState, BSOffsetReg, MSt15}
    end.

first_pass_bs_match_binary(
    Fail,
    MatchState,
    BSBinaryReg,
    BSOffsetReg,
    J0,
    Rest0,
    MMod,
    MSt0,
    State0
) ->
    {Live, Rest1} = decode_literal(Rest0),
    {_Flags, Rest2} = decode_compile_time_literal(Rest1, State0),
    %   {_FlagsValue, MSt1} = decode_flags_list(Flags, MMod, MSt0),
    {Size, Rest3} = decode_literal(Rest2),
    {Unit, Rest4} = decode_literal(Rest3),
    ?TRACE("{binary,~p,~p,~p,~p", [Live, _Flags, Size, Unit]),
    MatchedBits = Size * Unit,
    MSt1 =
        if
            MatchedBits rem 8 =:= 0 ->
                cond_raise_badarg({BSOffsetReg, '&', 2#111, '!=', 0}, MMod, MSt0);
            true ->
                MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?BADARG_ATOM
                ])
        end,
    MatchedBytes = MatchedBits div 8,
    {MSt2, BSOffseBytesReg} = MMod:copy_to_native_register(MSt1, BSOffsetReg),
    MSt3 = MMod:shift_right(MSt2, BSOffseBytesReg, 3),
    {MSt4, RemainingBytesReg} = MMod:get_array_element(MSt3, BSBinaryReg, 1),
    MSt5 = MMod:sub(MSt4, RemainingBytesReg, BSOffseBytesReg),
    MSt6 = cond_jump_to_label({RemainingBytesReg, '<', MatchedBytes}, Fail, MMod, MSt5),
    MSt7 = MMod:free_native_registers(MSt6, [RemainingBytesReg]),
    {MSt8, HeapSizeReg} = MMod:call_primitive(MSt7, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        BSBinaryReg, MatchedBytes
    ]),
    {MSt9, NewMatchState} = memory_ensure_free_with_extra_root(
        MatchState, Live, {free, HeapSizeReg}, MMod, MSt8
    ),
    % Restore BSBinaryReg as it may have been gc'd as well
    {MSt10, MatchStateReg0} = MMod:copy_to_native_register(MSt9, NewMatchState),
    MSt11 = MMod:and_(MSt10, MatchStateReg0, ?TERM_PRIMARY_CLEAR_MASK),
    MSt12 = MMod:move_array_element(MSt11, MatchStateReg0, 1, BSBinaryReg),
    MSt13 = MMod:free_native_registers(MSt12, [MatchStateReg0]),
    {MSt14, ResultTerm} = MMod:call_primitive(MSt13, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, BSBinaryReg, {free, BSOffseBytesReg}, MatchedBytes
    ]),
    MSt15 = MMod:and_(MSt14, BSBinaryReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt16, Dest, Rest5} = decode_dest(Rest4, MMod, MSt15),
    ?TRACE("~p},", [Dest]),
    MSt17 = MMod:move_to_vm_register(MSt16, ResultTerm, Dest),
    MSt18 = MMod:free_native_registers(MSt17, [ResultTerm]),
    MSt19 = MMod:add(MSt18, BSOffsetReg, MatchedBits),
    {J0 - 5, Rest5, NewMatchState, BSOffsetReg, MSt19}.

first_pass_bs_match_get_tail(MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0) ->
    {Live, Rest1} = decode_literal(Rest0),
    {_Unit, Rest2} = decode_literal(Rest1),
    ?TRACE("{get_tail,~p,~p,", [Live, _Unit]),
    {MSt1, ResultTerm, NewMatchState} = do_get_tail(
        MatchState, Live, BSOffsetReg, BSBinaryReg, MMod, MSt0
    ),
    % This is get_tail, we don't need to fix BSBinaryReg by doing an and with ?TERM_PRIMARY_CLEAR_MASK
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    ?TRACE("~p},", [Dest]),
    MSt3 = MMod:move_to_vm_register(MSt2, ResultTerm, Dest),
    MSt4 = MMod:free_native_registers(MSt3, [ResultTerm, Dest]),
    {J0 - 3, Rest3, NewMatchState, BSOffsetReg, MSt4}.

do_get_tail(
    MatchState, Live, BSOffsetReg, BSBinaryReg, MMod, MSt0
) ->
    MSt1 = cond_raise_badarg({BSOffsetReg, '&', 2#111, '!=', 0}, MMod, MSt0),
    {MSt2, BSOffseBytesReg} = MMod:copy_to_native_register(MSt1, BSOffsetReg),
    MSt3 = MMod:shift_right(MSt2, BSOffseBytesReg, 3),
    {MSt4, TailBytesReg0} = MMod:get_array_element(MSt3, BSBinaryReg, 1),
    MSt5 = MMod:sub(MSt4, TailBytesReg0, BSOffseBytesReg),
    {MSt6, HeapSizeReg} = MMod:call_primitive(MSt5, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        BSBinaryReg, {free, TailBytesReg0}
    ]),
    {MSt7, NewMatchState} = memory_ensure_free_with_extra_root(
        MatchState, Live, {free, HeapSizeReg}, MMod, MSt6
    ),
    % Restore BSBinaryReg as it may have been gc'd as well
    {MSt8, MatchStateReg0} = MMod:copy_to_native_register(MSt7, NewMatchState),
    MSt9 = MMod:and_(MSt8, MatchStateReg0, ?TERM_PRIMARY_CLEAR_MASK),
    MSt10 = MMod:move_array_element(MSt9, MatchStateReg0, 1, BSBinaryReg),
    MSt11 = MMod:free_native_registers(MSt10, [MatchStateReg0]),
    MSt12 = MMod:and_(MSt11, BSBinaryReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt13, TailBytesReg1} = MMod:get_array_element(MSt12, BSBinaryReg, 1),
    MSt14 = MMod:sub(MSt13, TailBytesReg0, BSOffseBytesReg),
    MSt15 = MMod:add(MSt14, BSBinaryReg, ?TERM_PRIMARY_BOXED),
    {MSt16, ResultTerm} = MMod:call_primitive(MSt15, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, BSBinaryReg, {free, BSOffseBytesReg}, TailBytesReg1
    ]),
    MSt17 = MMod:shift_left(MSt16, TailBytesReg1, 3),
    MSt18 = MMod:add(MSt17, BSOffsetReg, TailBytesReg1),
    MSt19 = MMod:free_native_registers(MSt18, [TailBytesReg1]),
    {MSt19, ResultTerm, NewMatchState}.

first_pass_bs_match_equal_colon_equal(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0
) ->
    % genot.tab says Live, but compiler always put nil
    Rest1 = decode_nil(Rest0),
    {Size, Rest2} = decode_literal(Rest1),
    {PatternValue, Rest3} = decode_literal(Rest2),
    ?TRACE("{'=:=',[],~p,~p},", [Size, PatternValue]),
    {MSt1, Result} = MMod:call_primitive(MSt0, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, BSBinaryReg, BSOffsetReg, Size, 0
    ]),
    MSt2 = handle_error_if({Result, '==', 0}, MMod, MSt1),
    MSt3 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt2),
    MSt6 =
        case MMod:word_size() of
            4 when PatternValue bsr 28 > 0 ->
                % PatternValue doesn't match on immediate integer, so unbox Result for comparison
                MMod:if_block(
                    MSt3, {Result, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
                        MMod:jump_to_label(BSt0, Fail)
                    end
                ),
                MSt4 = MMod:and_(MSt3, Result, ?TERM_PRIMARY_CLEAR_MASK),
                {MSt5, IntValue} = MMod:get_array_element(MSt4, {free, Result}, 1),
                cond_jump_to_label({{free, IntValue}, '!=', PatternValue}, Fail, MMod, MSt5);
            _ ->
                MSt4 = MMod:shift_right(MSt3, Result, 4),
                MSt5 = cond_jump_to_label({Result, '!=', PatternValue}, Fail, MMod, MSt4),
                MMod:free_native_registers(MSt5, [Result])
        end,
    MSt7 = MMod:add(MSt6, BSOffsetReg, Size),
    {J0 - 3, Rest3, MatchState, BSOffsetReg, MSt7}.

first_pass_bs_match_skip(MatchState, BSOffsetReg, J0, Rest0, MMod, MSt0) ->
    {Stride, Rest1} = decode_literal(Rest0),
    MSt1 = MMod:add(MSt0, BSOffsetReg, Stride),
    ?TRACE("{skip,~p},", [Stride]),
    {J0 - 1, Rest1, MatchState, BSOffsetReg, MSt1}.

term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt0) ->
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_registers(MSt1, [TrimResultReg]),
    % Write Src to x_reg to have it as a gc root
    {MSt3, NewSrc} = memory_ensure_free_with_extra_root(
        Src, Live, ?TERM_BOXED_BIN_MATCH_STATE_SIZE, MMod, MSt2
    ),
    {MSt4, AllocMatchStateReg} = MMod:call_primitive(MSt3, ?PRIM_TERM_ALLOC_BIN_MATCH_STATE, [
        ctx, NewSrc, 0
    ]),
    MSt5 = MMod:move_to_vm_register(MSt4, AllocMatchStateReg, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [AllocMatchStateReg]),
    {MSt6, NewSrc}.

term_from_catch_label(Dest, Label, MMod, MSt1) ->
    {MSt2, Reg} = MMod:get_module_index(MSt1),
    MSt3 = MMod:shift_left(MSt2, Reg, 24),
    MSt4 = MMod:or_(MSt3, Reg, (Label bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_CATCH),
    MSt5 = MMod:move_to_vm_register(MSt4, Reg, Dest),
    MMod:free_native_registers(MSt5, [Reg, Dest]).

term_is_boxed_with_tag_and_get_ptr(Label, Arg1, BoxedTag, MMod, MSt1) ->
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt5, BoxTagReg} = MMod:get_array_element(MSt4, Reg, 0),
    MSt6 = cond_jump_to_label(
        {{free, BoxTagReg}, '&', ?TERM_BOXED_TAG_MASK, '!=', BoxedTag}, Label, MMod, MSt5
    ),
    {MSt6, Reg}.

%%-----------------------------------------------------------------------------
%% @doc Raise tuple {badfun, Arg} if Arg is not a function
%% @param Arg element to test
%% @param MMod backend module
%% @param MSt0 backend state
%% @return new backend state
%%-----------------------------------------------------------------------------
verify_is_function({typed, Func, t_fun}, MMod, MSt0) ->
    MMod:move_to_native_register(MSt0, Func);
verify_is_function({typed, Func, any}, MMod, MSt0) ->
    verify_is_function(Func, MMod, MSt0);
verify_is_function({typed, Func, _Other}, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Func),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADFUN_ATOM, Reg
    ]),
    {MSt2, Reg};
verify_is_function(Func, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Func),
    MSt2 = MMod:if_block(MSt1, {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
            ctx, jit_state, offset, ?BADFUN_ATOM, Reg
        ])
    end),
    MSt3 = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    MSt5 = MMod:if_block(MSt4, {Reg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FUN}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
            ctx, jit_state, offset, ?BADFUN_ATOM, Reg
        ])
    end),
    MSt6 = MMod:free_native_registers(MSt5, [Reg]),
    MMod:move_to_native_register(MSt6, Func).

verify_is_binary_or_match_state(Label, Src, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Src),
    MSt2 = verify_is_boxed(MMod, MSt1, Reg, Label),
    MSt3 = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    MSt5 = MMod:and_(MSt4, Reg, ?TERM_BOXED_TAG_MASK),
    MSt6 = cond_raise_badarg_or_jump_to_fail_label(
        {'and', [
            {Reg, '!=', ?TERM_BOXED_REFC_BINARY},
            {Reg, '!=', ?TERM_BOXED_HEAP_BINARY},
            {Reg, '!=', ?TERM_BOXED_SUB_BINARY},
            {Reg, '!=', ?TERM_BOXED_BIN_MATCH_STATE}
        ]},
        Label,
        MMod,
        MSt5
    ),
    MMod:free_native_registers(MSt6, [Reg]).

verify_is_boxed_with_tag(Label, {free, Reg}, BoxedTag, MMod, MSt0) when is_atom(Reg) ->
    MSt1 = verify_is_boxed(MMod, MSt0, Reg, Label),
    MSt2 = MMod:and_(MSt1, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_array_element(MSt2, Reg, 0, Reg),
    cond_raise_badarg_or_jump_to_fail_label(
        {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', BoxedTag}, Label, MMod, MSt3
    );
verify_is_boxed_with_tag(Label, Arg1, BoxedTag, MMod, MSt1) ->
    {MSt2, Reg} = MMod:copy_to_native_register(MSt1, Arg1),
    MSt3 = verify_is_boxed(MMod, MSt2, Reg, Label),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    cond_raise_badarg_or_jump_to_fail_label(
        {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', BoxedTag}, Label, MMod, MSt5
    ).

verify_is_boxed(MMod, MSt0, Reg) ->
    verify_is_boxed(MMod, MSt0, Reg, 0).

verify_is_boxed(MMod, MSt0, Reg, FailLabel) ->
    cond_raise_badarg_or_jump_to_fail_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, FailLabel, MMod, MSt0
    ).

%% @doc verify_match_state and return the term_ptr for Reg.
%% Actually, this means Reg isn't restored with OR ?TERM_PRIMARY_BOXED
verify_is_match_state_and_get_ptr(MMod, MSt0, {typed, Src, {t_bs_matchable, _Unit}}) ->
    %% If Src is of type t_bs_matchable, it means it's boxed but we need to check
    %% if it is a bin_match_state (OTP27 type had a bs_context type but it's
    %% gone with OTP28)
    {MSt1, SrcReg} = MMod:move_to_native_register(MSt0, Src),
    verify_is_match_state_and_get_ptr0(MMod, MSt1, SrcReg);
verify_is_match_state_and_get_ptr(MMod, MSt0, {typed, Src, _}) ->
    verify_is_match_state_and_get_ptr(MMod, MSt0, Src);
verify_is_match_state_and_get_ptr(MMod, MSt0, Src) ->
    % Default case is to check it's boxed
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Src),
    MSt2 = verify_is_boxed(MMod, MSt1, Reg),
    verify_is_match_state_and_get_ptr0(MMod, MSt2, Reg).

verify_is_match_state_and_get_ptr0(MMod, MSt0, Reg) ->
    MSt1 = MMod:and_(MSt0, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt2, BoxTag} = MMod:get_array_element(MSt1, Reg, 0),
    MSt3 = cond_raise_badarg(
        {{free, BoxTag}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_BIN_MATCH_STATE}, MMod, MSt2
    ),
    {MSt3, Reg}.

verify_is_immediate(Arg1, ImmediateTag, FailLabel, MMod, MSt0) ->
    verify_is_immediate(Arg1, ?TERM_IMMED_TAG_MASK, ImmediateTag, FailLabel, MMod, MSt0).

verify_is_immediate(Arg1, ImmediateMask, ImmediateTag, _FailLabel, _MMod, MSt0) when
    is_integer(Arg1) andalso Arg1 band ImmediateMask =:= ImmediateTag
->
    MSt0;
verify_is_immediate(Arg1, ImmediateMask, ImmediateTag, 0, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Arg1),
    cond_raise_badarg(
        {{free, Reg}, '&', ImmediateMask, '!=', ImmediateTag}, MMod, MSt1
    );
verify_is_immediate(Arg1, ImmediateMask, ImmediateTag, FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Arg1),
    cond_jump_to_label(
        {{free, Reg}, '&', ImmediateMask, '!=', ImmediateTag}, FailLabel, MMod, MSt1
    ).

verify_is_integer(Arg1, Fail, MMod, MSt0) ->
    verify_is_immediate(Arg1, ?TERM_INTEGER_TAG, Fail, MMod, MSt0).

verify_is_atom(Arg1, Fail, MMod, MSt0) ->
    verify_is_immediate(Arg1, ?TERM_IMMED2_TAG_MASK, ?TERM_IMMED2_ATOM, Fail, MMod, MSt0).

verify_is_immediate_or_boxed(Arg1, ImmediateTag, _BoxedTag, _FailLabel, _MMod, MSt0) when
    is_integer(Arg1) andalso Arg1 band ?TERM_IMMED_TAG_MASK =:= ImmediateTag
->
    MSt0;
verify_is_immediate_or_boxed({free, Arg1}, ImmediateTag, _BoxedTag, _FailLabel, _MMod, MSt0) when
    is_integer(Arg1) andalso Arg1 band ?TERM_IMMED_TAG_MASK =:= ImmediateTag
->
    MSt0;
verify_is_immediate_or_boxed(
    ArgOrTuple, ImmediateTag, BoxedTag, Label, MMod, MSt0
) ->
    {MSt1, Reg} =
        case ArgOrTuple of
            {free, Arg} -> MMod:move_to_native_register(MSt0, Arg);
            _ -> MMod:copy_to_native_register(MSt0, ArgOrTuple)
        end,
    MSt2 = MMod:if_block(MSt1, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ImmediateTag}, fun(BSt0) ->
        verify_is_boxed_with_tag(Label, {free, Reg}, BoxedTag, MMod, BSt0)
    end),
    MMod:free_native_registers(MSt2, [Reg]).

verify_is_any_integer(Arg1, Fail, MMod, MSt0) ->
    verify_is_immediate_or_boxed(
        Arg1, ?TERM_INTEGER_TAG, ?TERM_BOXED_POSITIVE_INTEGER, Fail, MMod, MSt0
    ).

%%-----------------------------------------------------------------------------
%% @doc Test if Arg1 is a binary, jump to FailLabel if it isn't or raise
%% badarg if FailLabel is 0
%% @param Arg1 element to test
%% @param FailLabel label to jump to if Arg1 is not a binary or 0 to raise
%% @param MMod backend module
%% @param MSt0 backend state
%% @return new backend state
%%-----------------------------------------------------------------------------
verify_is_binary(Arg1, FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Arg1),
    MSt2 = verify_is_boxed(MMod, MSt1, Reg, FailLabel),
    MSt3 = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    MSt5 = MMod:and_(MSt4, Reg, ?TERM_BOXED_TAG_MASK),
    MSt6 = cond_raise_badarg_or_jump_to_fail_label(
        {'and', [
            {Reg, '!=', ?TERM_BOXED_REFC_BINARY},
            {Reg, '!=', ?TERM_BOXED_HEAP_BINARY},
            {Reg, '!=', ?TERM_BOXED_SUB_BINARY}
        ]},
        FailLabel,
        MMod,
        MSt5
    ),
    MMod:free_native_registers(MSt6, [Reg]).

cond_raise_badarg(Cond, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?BADARG_ATOM
        ])
    end).

cond_raise_badarg_or_jump_to_fail_label(Cond, 0, MMod, MSt0) ->
    cond_raise_badarg(Cond, MMod, MSt0);
cond_raise_badarg_or_jump_to_fail_label(Cond, FailLabel, MMod, MSt0) when FailLabel > 0 ->
    cond_jump_to_label(Cond, FailLabel, MMod, MSt0).

term_to_int(Term, _FailLabel, _MMod, MSt0) when is_integer(Term) ->
    {MSt0, Term bsr 4};
term_to_int({literal, Val}, _FailLabel, _MMod, MSt0) when is_integer(Val) ->
    {MSt0, Val};
% Optimized case: when we have type information showing this is an integer, skip the type check
term_to_int({typed, Term, {t_integer, _Range}}, _FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    MSt2 = MMod:shift_right(MSt1, Reg, 4),
    {MSt2, Reg};
term_to_int({typed, Term, _NonIntegerType}, FailLabel, MMod, MSt0) ->
    % Type information shows it's not an integer, fall back to generic path
    term_to_int(Term, FailLabel, MMod, MSt0);
term_to_int(Term, FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    MSt2 = cond_raise_badarg_or_jump_to_fail_label(
        {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, FailLabel, MMod, MSt1
    ),
    MSt3 = MMod:shift_right(MSt2, Reg, 4),
    {MSt3, Reg}.

first_pass_float3(Primitive, Rest0, MMod, MSt0, State0) ->
    {Label, Rest1} = decode_label(Rest0),
    {{fp_reg, FPRegIndex1}, Rest2} = decode_fp_register(Rest1),
    {{fp_reg, FPRegIndex2}, Rest3} = decode_fp_register(Rest2),
    {{fp_reg, FPRegIndex3}, Rest4} = decode_fp_register(Rest3),
    ?TRACE("OP_F3*~p ~p, ~p, ~p, ~p\n", [
        Primitive, Label, {fp_reg, FPRegIndex1}, {fp_reg, FPRegIndex2}, {fp_reg, FPRegIndex3}
    ]),
    {MSt1, Reg} = MMod:call_primitive(MSt0, Primitive, [
        ctx, FPRegIndex1, FPRegIndex2, FPRegIndex3
    ]),
    if
        Label > 0 ->
            MSt2 = cond_jump_to_label({'(bool)', Reg, '==', false}, Label, MMod, MSt1),
            MSt3 = MMod:free_native_registers(MSt2, [Reg]),
            ?ASSERT_ALL_NATIVE_FREE(MSt3),
            first_pass(Rest4, MMod, MSt3, State0);
        true ->
            MSt2 = MMod:if_block(MSt1, {'(bool)', {free, Reg}, '==', false}, fun(BlockSt) ->
                MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?BADARITH_ATOM
                ])
            end),
            ?ASSERT_ALL_NATIVE_FREE(MSt2),
            first_pass(Rest4, MMod, MSt2, State0)
    end.

bif_faillabel_test(FailLabel, MMod, MSt0, {free, ResultReg}, {free, Dest}) when FailLabel > 0 ->
    MSt1 = cond_jump_to_label({ResultReg, '==', 0}, FailLabel, MMod, MSt0),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, Dest),
    MMod:free_native_registers(MSt2, [ResultReg, Dest]);
bif_faillabel_test(0, MMod, MSt0, {free, ResultReg}, {free, Dest}) ->
    MSt1 = handle_error_if({ResultReg, '==', 0}, MMod, MSt0),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, Dest),
    MMod:free_native_registers(MSt2, [ResultReg, Dest]).

memory_ensure_free_with_extra_root({x_reg, N} = ExtraRoot, Live, Size, MMod, MSt0) when N < Live ->
    {MSt1, MemoryEnsureFreeReg} = MMod:call_primitive(MSt0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, Size, Live, ?MEMORY_CAN_SHRINK
    ]),
    MSt2 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt1),
    {MSt2, ExtraRoot};
memory_ensure_free_with_extra_root(ExtraRoot, Live, Size, MMod, MSt0) when is_atom(ExtraRoot) ->
    ExtraRootXReg =
        if
            Live < ?MAX_REG ->
                {x_reg, Live};
            true ->
                {x_reg, extra}
        end,
    MSt1 = MMod:move_to_vm_register(MSt0, ExtraRoot, ExtraRootXReg),
    MSt2 = MMod:free_native_registers(MSt1, [ExtraRoot]),
    {MSt3, MemoryEnsureFreeReg} = MMod:call_primitive(MSt2, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, Size, Live + 1, ?MEMORY_CAN_SHRINK
    ]),
    MSt4 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt3),
    MMod:move_to_native_register(MSt4, ExtraRootXReg);
memory_ensure_free_with_extra_root(ExtraRoot, Live, Size, MMod, MSt0) when is_tuple(ExtraRoot) ->
    ExtraRootXReg =
        if
            Live < ?MAX_REG ->
                {x_reg, Live};
            true ->
                {x_reg, extra}
        end,
    MSt1 = MMod:move_to_vm_register(MSt0, ExtraRoot, ExtraRootXReg),
    {MSt2, MemoryEnsureFreeReg} = MMod:call_primitive(MSt1, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, Size, Live + 1, ?MEMORY_CAN_SHRINK
    ]),
    MSt3 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt2),
    MSt4 = MMod:move_to_vm_register(MSt3, ExtraRootXReg, ExtraRoot),
    {MSt4, ExtraRoot}.

second_pass(MMod, MSt0, #state{line_offsets = Lines}) ->
    ?TRACE("SECOND PASS -- ~B lines\n", [length(Lines)]),
    % Add extra function that returns labels and line information
    MSt1 = MMod:add_label(MSt0, 0),
    SortedLines = lists:keysort(2, Lines),
    MSt2 = MMod:return_labels_and_lines(MSt1, SortedLines),
    MMod:update_branches(MSt2).

decode_literal(<<_Value:5, ?COMPACT_LITERAL:3, _Rest/binary>> = Binary) ->
    decode_value64(Binary);
decode_literal(<<_:4, ?COMPACT_LARGE_LITERAL:4, _Rest/binary>> = Bin) ->
    decode_value64(Bin).

decode_label(<<_Value:5, ?COMPACT_LABEL:3, _Rest/binary>> = Binary) ->
    decode_value64(Binary).

decode_atom(<<_Value:5, ?COMPACT_ATOM:3, _Rest/binary>> = Binary) ->
    decode_value64(Binary).

decode_atom_or_label(<<_Value:5, ?COMPACT_ATOM:3, _Rest/binary>> = Binary, #state{
    atom_resolver = AtomResolver
}) ->
    {AtomIndex, Rest1} = decode_value64(Binary),
    {AtomResolver(AtomIndex), Rest1};
decode_atom_or_label(Binary, _State0) ->
    decode_label(Binary).

decode_nil(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>) ->
    Rest.

decode_value64(<<Val:4, 0:1, _:3, Rest/binary>>) -> {Val, Rest};
decode_value64(<<Val:3, 1:2, _:3, NextByte, Rest/binary>>) -> {(Val bsl 8) bor NextByte, Rest};
decode_value64(<<Size0:3, 3:2, _:3, Value:(8 * (Size0 + 2)), Rest/binary>>) -> {Value, Rest}.

% @doc Decode a compact term to an integer or to a register tuple, building
% code if it needs to be decoded at runtime.
-spec decode_compact_term(binary(), module(), any(), #state{}) ->
    {
        any(),
        integer()
        | {x_reg, non_neg_integer()}
        | {y_reg, non_neg_integer()}
        | {atom, integer(), atom()}
        | {ptr, any()},
        binary()
    }.
decode_compact_term(<<_:4, ?COMPACT_INTEGER:4, _Rest/binary>> = Bin, _MMod, MSt, _State) ->
    {Value, Rest} = decode_value64(Bin),
    {MSt, term_from_int(Value), Rest};
decode_compact_term(
    <<Val:3, ?COMPACT_LARGE_INTEGER_11BITS:5, NextByte, Rest/binary>>, _MMod, MSt, _State
) ->
    {MSt, term_from_int((Val bsl 8) bor NextByte), Rest};
decode_compact_term(
    <<Size0:3, ?COMPACT_LARGE_INTEGER_NBITS:5, Value:(8 * (Size0 + 2))/signed, Rest/binary>>,
    MMod,
    MSt,
    _State
) ->
    decode_compact_term_integer(Value, MMod, MSt, Rest);
decode_compact_term(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>, _MMod, MSt, _State) ->
    {MSt, ?TERM_NIL, Rest};
decode_compact_term(<<_:4, ?COMPACT_ATOM:4, _Rest/binary>> = Bin, MMod, MSt, State) ->
    {Value, Rest} = decode_value64(Bin),
    decode_compact_term_atom(Value, MMod, MSt, Rest, State);
decode_compact_term(<<_:4, ?COMPACT_LARGE_ATOM:4, _Rest/binary>> = Bin, MMod, MSt, State) ->
    {Value, Rest} = decode_value64(Bin),
    decode_compact_term_atom(Value, MMod, MSt, Rest, State);
decode_compact_term(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>, MMod, MSt, _State) ->
    {Value, Rest1} = decode_literal(Rest0),
    decode_compact_term_module_literal(Value, MMod, MSt, Rest1);
decode_compact_term(<<?COMPACT_EXTENDED_TYPED_REGISTER, Rest0/binary>>, MMod, MSt0, _State) ->
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {_Type, Rest2} = decode_literal(Rest1),
    {MSt1, Dest, Rest2};
decode_compact_term(<<_Value:5, ?COMPACT_LABEL:3, _Rest/binary>> = Binary, _MMod, MSt0, _State) ->
    {Value, Rest} = decode_label(Binary),
    {MSt0, {label, Value}, Rest};
decode_compact_term(<<_Value:5, ?COMPACT_LITERAL:3, _Rest/binary>> = Binary, _MMod, MSt0, _State) ->
    {Value, Rest} = decode_value64(Binary),
    {MSt0, {literal, Value}, Rest};
decode_compact_term(Other, MMod, MSt, _State) ->
    decode_dest(Other, MMod, MSt).

% Decode compact term with type information awareness
decode_typed_compact_term(<<?COMPACT_EXTENDED_TYPED_REGISTER, Rest0/binary>>, MMod, MSt0, #state{
    type_resolver = TypeResover
}) ->
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {TypeIx, Rest2} = decode_literal(Rest1),
    Type = TypeResover(TypeIx),
    {MSt1, {typed, Dest, Type}, Rest2};
decode_typed_compact_term(Other, MMod, MSt, State) ->
    decode_compact_term(Other, MMod, MSt, State).

skip_compact_term(<<_:4, ?COMPACT_INTEGER:4, _Rest/binary>> = Bin) ->
    {_Value, Rest} = decode_value64(Bin),
    Rest;
skip_compact_term(<<_Val:3, ?COMPACT_LARGE_INTEGER_11BITS:5, _NextByte, Rest/binary>>) ->
    Rest;
skip_compact_term(
    <<Size0:3, ?COMPACT_LARGE_INTEGER_NBITS:5, _Value:(8 * (Size0 + 2))/signed, Rest/binary>>
) ->
    Rest;
skip_compact_term(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_:4, ?COMPACT_ATOM:4, _Rest/binary>> = Bin) ->
    {_Value, Rest} = decode_value64(Bin),
    Rest;
skip_compact_term(<<_:4, ?COMPACT_LARGE_ATOM:4, _Rest/binary>> = Bin) ->
    {_Value, Rest} = decode_value64(Bin),
    Rest;
skip_compact_term(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>) ->
    {_Value, Rest1} = decode_literal(Rest0),
    Rest1;
skip_compact_term(<<?COMPACT_EXTENDED_TYPED_REGISTER, Rest0/binary>>) ->
    Rest1 = skip_compact_term(Rest0),
    Rest2 = decode_literal(Rest1),
    Rest2;
skip_compact_term(<<_ValueH:5, ?COMPACT_LABEL:3, _Rest/binary>> = Binary) ->
    {_Value, Rest} = decode_label(Binary),
    Rest;
skip_compact_term(<<_ValueH:5, ?COMPACT_LITERAL:3, _Rest/binary>> = Binary) ->
    {_Value, Rest} = decode_value64(Binary),
    Rest;
skip_compact_term(<<_RegIndex:4, ?COMPACT_XREG:4, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_RegIndex:4, ?COMPACT_YREG:4, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_RegIndexH:3, 0:1, ?COMPACT_LARGE_XREG:4, _RegIndexL, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_RegIndexH:3, 0:1, ?COMPACT_LARGE_YREG:4, _RegIndexL, Rest/binary>>) ->
    Rest.

decode_compile_time_literal(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>, _State) ->
    {[], Rest};
decode_compile_time_literal(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>, #state{
    literal_resolver = Resolver
}) ->
    {LiteralIndex, Rest1} = decode_literal(Rest0),
    LiteralTerm = Resolver(LiteralIndex),
    {LiteralTerm, Rest1}.

decode_flags_list(L, _MMod, MSt) when is_list(L) ->
    % compile time decoding
    Value = decode_flags_list0(L, 0),
    {MSt, Value};
decode_flags_list(L, MMod, MSt0) ->
    % run-time decoding
    {MSt1, FlagsValue} = MMod:call_primitive(MSt0, ?PRIM_DECODE_FLAGS_LIST, [ctx, jit_state, L]),
    MSt2 = MMod:if_block(MSt1, {FlagsValue, '<', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_HANDLE_ERROR, [
            ctx, jit_state, offset
        ])
    end),
    {MSt2, FlagsValue}.

decode_flags_list0([], Val) ->
    Val;
decode_flags_list0([little | T], Val) ->
    decode_flags_list0(T, Val bor ?BITSTRING_FLAG_LITTLE_ENDIAN);
decode_flags_list0([signed | T], Val) ->
    decode_flags_list0(T, Val bor ?BITSTRING_FLAG_SIGNED);
decode_flags_list0([native | T], Val) ->
    decode_flags_list0(T, Val bor ?BITSTRING_FLAG_NATIVE_ENDIAN).

decode_compact_term_atom(AtomIndex, MMod, MSt0, Rest, #state{atom_resolver = Resolver}) ->
    Atom = Resolver(AtomIndex),
    case maps:find(Atom, ?DEFAULT_ATOMS) of
        error ->
            {MSt1, Reg} = MMod:call_primitive(
                MSt0, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, AtomIndex]
            ),
            ?TRACE("(get_atom_term_by_id(~p) => ~p)", [AtomIndex, Reg]),
            {MSt1, Reg, Rest};
        {ok, DefaultAtomIndex} ->
            {MSt0, DefaultAtomIndex, Rest}
    end.

decode_compact_term_module_literal(LiteralIndex, MMod, MSt0, Rest) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_MODULE_LOAD_LITERAL, [ctx, jit_state, LiteralIndex]
    ),
    ?TRACE("(module_load_literal(~p) => ~p)", [LiteralIndex, Reg]),
    {MSt1, Reg, Rest}.

decode_compact_term_integer(Value, _MMod, MSt, Rest) when
    Value >= (?INT32_MIN bsr 4) andalso Value =< (?INT32_MAX bsr 4)
->
    {MSt, term_from_int(Value), Rest};
decode_compact_term_integer(Value, MMod, MSt0, Rest) when
    Value >= (?INT64_MIN bsr 4) andalso Value =< (?INT64_MAX bsr 4)
->
    case MMod:word_size() of
        4 ->
            {MSt1, Reg} = MMod:call_primitive(
                MSt0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [ctx, {avm_int64_t, Value}]
            ),
            ?TRACE("(alloc_boxed_integer_fragment(~p) => ~p)", [Value, Reg]),
            {MSt1, Reg, Rest};
        8 ->
            {MSt0, term_from_int(Value), Rest}
    end;
decode_compact_term_integer(Value, MMod, MSt0, Rest) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [ctx, {avm_int64_t, Value}]
    ),
    ?TRACE("(alloc_boxed_integer_fragment(~p) => ~p)", [Value, Reg]),
    {MSt1, Reg, Rest}.

decode_dest(<<RegIndex:4, ?COMPACT_XREG:4, Rest/binary>>, _MMod, MSt) ->
    {MSt, {x_reg, RegIndex}, Rest};
decode_dest(<<RegIndex:4, ?COMPACT_YREG:4, Rest/binary>>, _MMod, MSt) ->
    {MSt, {y_reg, RegIndex}, Rest};
decode_dest(<<RegIndexH:3, 0:1, ?COMPACT_LARGE_XREG:4, RegIndexL, Rest/binary>>, MMod, MSt0) ->
    RegIndex = (RegIndexH bsl 8) bor RegIndexL,
    if
        RegIndex < ?MAX_REG ->
            {MSt0, {x_reg, RegIndex}, Rest};
        true ->
            {MSt1, Reg} = MMod:call_primitive(
                MSt0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, RegIndex]
            ),
            ?TRACE("(extended_register_ptr(~p) => ~p)", [RegIndex, Reg]),
            {MSt1, {ptr, Reg}, Rest}
    end;
decode_dest(<<RegIndexH:3, 0:1, ?COMPACT_LARGE_YREG:4, RegIndexL, Rest/binary>>, _MMod, MSt) ->
    {MSt, {y_reg, (RegIndexH bsl 8) bor RegIndexL}, Rest}.

decode_fp_register(<<?COMPACT_EXTENDED_FP_REGISTER, Rest0/binary>>) ->
    {FPRegIndex, Rest1} = decode_literal(Rest0),
    {{fp_reg, FPRegIndex}, Rest1}.

read_any_xreg(RegIndex, _MMod, MSt0) when RegIndex < ?MAX_REG ->
    {MSt0, {x_reg, RegIndex}};
read_any_xreg(RegIndex, MMod, MSt0) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, RegIndex]
    ),
    ?TRACE("extended_register_ptr(~p) => ~p\n", [RegIndex, Reg]),
    {MSt1, {ptr, Reg}}.

decode_extended_list_header(<<?COMPACT_EXTENDED_LIST, Rest0/binary>>) ->
    decode_literal(Rest0).

decode_allocator_list(MMod, <<?COMPACT_EXTENDED_ALLOCATION_LIST, Rest0/binary>>) ->
    {ListSize, Rest1} = decode_literal(Rest0),
    decode_allocator_list0(MMod, 0, ListSize, Rest1);
decode_allocator_list(_MMod, Bin) ->
    decode_literal(Bin).

decode_allocator_list0(_MMod, Need, 0, Rest) ->
    {Need, Rest};
decode_allocator_list0(MMod, AccNeed, Remaining, Rest0) ->
    {AllocatorTag, Rest1} = decode_literal(Rest0),
    {AllocatorSize, Rest2} = decode_literal(Rest1),
    NeedIncrement =
        case AllocatorTag of
            ?COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FLOATS ->
                case MMod:word_size() of
                    4 ->
                        AllocatorSize * ?FLOAT_SIZE_32;
                    8 ->
                        AllocatorSize * ?FLOAT_SIZE_64
                end;
            ?COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FUNS ->
                AllocatorSize * ?BOXED_FUN_SIZE;
            _ ->
                AllocatorSize
        end,
    decode_allocator_list0(MMod, AccNeed + NeedIncrement, Remaining - 1, Rest2).

term_from_int(Int) when is_integer(Int) ->
    (Int bsl 4) bor ?TERM_INTEGER_TAG.

term_get_tuple_arity(Tuple, MMod, MSt0) ->
    {MSt1, Reg} =
        case Tuple of
            {free, TupleReg} -> MMod:move_to_native_register(MSt0, TupleReg);
            _ -> MMod:copy_to_native_register(MSt0, Tuple)
        end,
    MSt2 = MMod:and_(MSt1, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_array_element(MSt2, Reg, 0, Reg),
    MSt4 = MMod:shift_right(MSt3, Reg, 6),
    {MSt4, Reg}.

term_get_map_size(Map, MMod, MSt0) ->
    {MSt1, MapKeys} = term_get_map_keys(Map, MMod, MSt0),
    term_get_tuple_arity({free, MapKeys}, MMod, MSt1).

term_get_map_keys(Map, MMod, MSt0) ->
    {MSt1, Reg} =
        case Map of
            {free, MapReg} -> MMod:move_to_native_register(MSt0, MapReg);
            _ -> MMod:copy_to_native_register(MSt0, Map)
        end,
    MSt2 = MMod:and_(MSt1, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_array_element(MSt2, Reg, 1, Reg),
    {MSt3, Reg}.

handle_error_if(Cond, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end).

cond_jump_to_label(Cond, Label, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:jump_to_label(BSt0, Label)
    end).

term_binary_heap_size(Size, MMod) when is_integer(Size) ->
    case MMod:word_size() of
        4 when Size < ?REFC_BINARY_MIN_32 ->
            ((Size + 3) bsr 2) + 1 + ?BINARY_HEADER_SIZE;
        8 when Size < ?REFC_BINARY_MIN_64 ->
            ((Size + 7) bsr 3) + 1 + ?BINARY_HEADER_SIZE;
        _ ->
            ?TERM_BOXED_REFC_BINARY_SIZE
    end.

term_binary_heap_size({free, Immediate}, MMod, MSt0) when is_integer(Immediate) ->
    {MSt0, term_binary_heap_size(Immediate, MMod)};
term_binary_heap_size({free, Reg}, MMod, MSt0) ->
    MSt1 =
        case MMod:word_size() of
            4 ->
                MMod:if_else_block(
                    MSt0,
                    {Reg, '<', ?REFC_BINARY_MIN_32},
                    fun(BSt0) ->
                        BSt1 = MMod:add(BSt0, Reg, 3),
                        BSt2 = MMod:shift_right(BSt1, Reg, 2),
                        MMod:add(BSt2, Reg, 1 + ?BINARY_HEADER_SIZE)
                    end,
                    fun(BSt0) ->
                        % pretty sure  + ?BINARY_HEADER_SIZE is too much, same issue in opcodeswitch
                        MMod:move_to_native_register(
                            BSt0, ?TERM_BOXED_REFC_BINARY_SIZE + ?BINARY_HEADER_SIZE, Reg
                        )
                    end
                );
            8 ->
                MMod:if_else_block(
                    MSt0,
                    {Reg, '<', ?REFC_BINARY_MIN_64},
                    fun(BSt0) ->
                        BSt1 = MMod:add(BSt0, Reg, 7),
                        BSt2 = MMod:shift_right(BSt1, Reg, 3),
                        MMod:add(BSt2, Reg, 1 + ?BINARY_HEADER_SIZE)
                    end,
                    fun(BSt0) ->
                        MMod:move_to_native_register(
                            BSt0, ?TERM_BOXED_REFC_BINARY_SIZE + ?BINARY_HEADER_SIZE, Reg
                        )
                    end
                )
        end,
    {MSt1, Reg}.

term_binary_size({free, BinReg}, MMod, MSt0) ->
    MSt1 = MMod:and_(MSt0, BinReg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt2 = MMod:move_array_element(MSt1, BinReg, 1, BinReg),
    {MSt2, BinReg}.

term_set_map_assoc(MapPtrReg, {free, PosReg}, {free, Key}, {free, Value}, MMod, MSt0) ->
    {MSt1, MapKeysReg} = MMod:get_array_element(MSt0, MapPtrReg, 1),
    MSt2 = term_put_tuple_element({free, MapKeysReg}, PosReg, {free, Key}, MMod, MSt1),
    MSt3 = MMod:move_to_array_element(MSt2, Value, MapPtrReg, PosReg, 2),
    MMod:free_native_registers(MSt3, [PosReg, Value]).

term_put_tuple_element({free, TupleReg}, PosReg, {free, Value}, MMod, MSt0) ->
    MSt1 = MMod:and_(MSt0, TupleReg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt2 = MMod:move_to_array_element(MSt1, Value, TupleReg, PosReg, 1),
    MMod:free_native_registers(MSt2, [TupleReg, Value]).

%% @doc Get the stream module
%% @return The stream module for jit on this platform
-spec stream_module() -> module().
stream_module() ->
    erlang:nif_error(undefined).

%% @doc Return a new stream for this platform
%% @param MaxSize estimation of the maximum size of the stream
%% @return A tuple with the stream module and the stream resource for this platform
-spec stream(MaxSize :: pos_integer()) -> {module(), stream()}.
stream(MaxSize) ->
    StreamModule = ?MODULE:stream_module(),
    {StreamModule, StreamModule:new(MaxSize)}.

%% @doc Get the backend module
%% @return The backend module for jit on this platform
-spec backend_module() -> module().
backend_module() ->
    erlang:nif_error(undefined).

%% @doc Instantiate backend for this platform
%% @return A tuple with the backend module and the backend state for this platform
backend({StreamModule, Stream}) ->
    BackendModule = ?MODULE:backend_module(),
    BackendState = BackendModule:new(?JIT_VARIANT_PIC, StreamModule, Stream),
    {BackendModule, BackendState}.
