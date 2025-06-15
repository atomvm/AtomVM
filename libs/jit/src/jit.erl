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
    beam_chunk_header/2,
    compile/5
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
    labels :: [{integer(), integer()}],
    atom_resolver :: fun((integer()) -> atom()),
    literal_resolver :: fun((integer()) -> any())
}).

-define(TRACE(Fmt, Args), io:format(Fmt, Args)).
%% -define(TRACE(Fmt, Args), ok).

-define(ASSERT_ALL_NATIVE_FREE(St), MMod:assert_all_native_free(St)).

%%-----------------------------------------------------------------------------
%% @param   Architecture
%% @param   Variant
%% @returns Beam chunk header
%% @doc     Create the beam chunk header for a single architecture/variant
%% @end
%%-----------------------------------------------------------------------------
beam_chunk_header(Arch, Variant) ->
    Info = <<?JIT_FORMAT_VERSION:16, 1:16, Arch:16, Variant:16, 0:32>>,
    <<(byte_size(Info)):32, Info/binary>>.

%% Current variant supposes any entry point (labels or continuation pointer)
%% has the following signature
%% Context *(*ModuleNativeEntryPoint)(Context *ctx, JITState *jit_state, const ModuleNativeInterface *p)
compile(
    <<16:32, 0:32, OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, Opcodes/binary>>,
    AtomResolver,
    LiteralResolver,
    MMod,
    MSt0
) when OpcodeMax =< ?OPCODE_MAX ->
    MSt1 = MMod:jump_table(MSt0, LabelsCount),
    State0 = #state{
        line_offsets = [],
        labels = [],
        atom_resolver = AtomResolver,
        literal_resolver = LiteralResolver
    },
    {State1, MSt2} = first_pass(Opcodes, MMod, MSt1, State0),
    MSt3 = second_pass(MMod, MSt2, State1),
    {State1#state.line_offsets, MSt3};
compile(
    <<16:32, 0:32, OpcodeMax:32, _LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>>,
    _AtomResolver,
    _LiteralResolver,
    _MMod,
    _MSt
) ->
    error(badarg, [OpcodeMax]);
compile(CodeChunk, _AtomResolver, _LiteralResolver, _MMod, _MSt) ->
    error(badarg, [CodeChunk]).

% 1
first_pass(
    <<?OP_LABEL, Rest0/binary>>, MMod, MSt, #state{labels = AccLabels} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt),
    {Label, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_LABEL ~p\n", [Label]),
    Offset = MMod:offset(MSt),
    first_pass(Rest1, MMod, MSt, State0#state{
        labels = [{Label, Offset} | AccLabels]
    });
% 2
first_pass(<<?OP_FUNC_INFO, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_ModuleAtom, Rest1} = decode_atom(Rest0),
    {_FunctionName, Rest2} = decode_atom(Rest1),
    {_Arity, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_FUNC_INFO ~p, ~p, ~p\n", [_ModuleAtom, _FunctionName, _Arity]),
    % Implement function clause at the previous label. (TODO: optimize it out to save space)
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, ?FUNCTION_CLAUSE_ATOM
    ]),
    first_pass(Rest3, MMod, MSt1, State0);
% 3
first_pass(<<?OP_INT_CALL_END>>, MMod, MSt0, #state{labels = AccLabels} = State) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_INT_CALL_END\n", []),
    Offset = MMod:offset(MSt0),
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_TERMINATE_CONTEXT, [
        ctx, jit_state
    ]),
    {State#state{labels = [{0, Offset} | AccLabels]}, MSt1};
% 4
first_pass(<<?OP_CALL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CALL ~p, ~p\n", [_Arity, Label]),
    MSt1 = MMod:call_or_schedule_next(MSt0, Label),
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
    first_pass(Rest3, MMod, MSt3, State0);
% 6
first_pass(<<?OP_CALL_ONLY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CALL_ONLY ~p, ~p\n", [_Arity, Label]),
    MSt1 = MMod:call_only_or_schedule_next(MSt0, Label),
    first_pass(Rest2, MMod, MSt1, State0);
% 7
first_pass(<<?OP_CALL_EXT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_CALL_EXT ~p, ~p\n", [Arity, Index]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    MSt2 = MMod:call_primitive_with_cp(MSt1, ?PRIM_CALL_EXT, [ctx, jit_state, Arity, Index, -1]),
    first_pass(Rest2, MMod, MSt2, State0);
% 8
first_pass(<<?OP_CALL_EXT_LAST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    {NWords, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_CALL_EXT_LAST ~p, ~p, ~p\n", [Arity, Index, NWords]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_CALL_EXT, [ctx, jit_state, Arity, Index, NWords]),
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
    MSt5 = MMod:free_native_register(MSt4, Dest),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest2, MMod, MSt6, State0);
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
    MSt2 = MMod:handle_error_if_false(MSt1, ResultReg),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    first_pass(Rest2, MMod, MSt3, State0);
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
    MSt2 = MMod:handle_error_if_false(MSt1, ResultReg),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    first_pass(Rest3, MMod, MSt3, State0);
% 16
first_pass(<<?OP_TEST_HEAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {HeapNeed, Rest1} = decode_allocator_list(MMod, Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_TEST_HEAP ~p, ~p\n", [HeapNeed, Live]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_TEST_HEAP, [
        ctx, jit_state, HeapNeed, Live
    ]),
    MSt2 = MMod:handle_error_if_false(MSt1, ResultReg),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    first_pass(Rest2, MMod, MSt3, State0);
% 18
first_pass(<<?OP_DEALLOCATE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {NWords, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_DEALLOCATE ~p\n", [NWords]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_DEALLOCATE, [
        ctx, jit_state, NWords
    ]),
    MSt2 = MMod:handle_error_if_false(MSt1, ResultReg),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    first_pass(Rest1, MMod, MSt3, State0);
% 19
first_pass(<<?OP_RETURN, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_RETURN\n", []),
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RETURN, [
        ctx, jit_state
    ]),
    first_pass(Rest, MMod, MSt1, State0);
% 20
first_pass(<<?OP_SEND, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_SEND\n", []),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_SEND, [
        ctx, jit_state
    ]),
    MSt2 = MMod:handle_error_if_false(MSt1, ResultReg),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    first_pass(Rest, MMod, MSt3, State0);
% 21
first_pass(<<?OP_REMOVE_MESSAGE, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_REMOVE_MESSAGE\n", []),
    {MSt1, Reg1} = MMod:call_primitive(MSt0, ?PRIM_CANCEL_TIMEOUT, [
        ctx
    ]),
    MSt2 = MMod:free_native_register(MSt1, Reg1),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt4 = MMod:return_if_not_equal(MSt3, ctx, {free, ResultReg}),
    {MSt5, Reg2} = MMod:call_primitive(MSt4, ?PRIM_MAILBOX_REMOVE_MESSAGE, [
        ctx
    ]),
    MSt6 = MMod:free_native_register(MSt5, Reg2),
    first_pass(Rest, MMod, MSt6, State0);
% 22
first_pass(<<?OP_TIMEOUT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_TIMEOUT\n", []),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_TIMEOUT, [
        ctx
    ]),
    MSt2 = MMod:free_native_register(MSt1, ResultReg),
    first_pass(Rest0, MMod, MSt2, State0);
% 23
first_pass(<<?OP_LOOP_REC, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt2 = MMod:return_if_not_equal(MSt1, ctx, {free, ResultReg}),
    {MSt3, Dest, Rest2} = decode_compact_term(Rest1, MMod, MSt2, State0),
    ?TRACE("OP_LOOP_REC ~p, ~p\n", [Label, Dest]),
    {MSt4, PointerReg} = MMod:get_pointer_to_vm_register(MSt3, Dest),
    {MSt5, PeekResultReg} = MMod:call_primitive(MSt4, ?PRIM_MAILBOX_PEEK, [
        ctx, {free, PointerReg}
    ]),
    MSt6 = MMod:jump_to_label_if_zero(MSt5, PeekResultReg, Label),
    MSt7 = MMod:free_native_register(MSt6, PeekResultReg),
    first_pass(Rest2, MMod, MSt7, State0);
% 24
first_pass(<<?OP_LOOP_REC_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_LOOP_REC_END ~p\n", [Label]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt2 = MMod:return_if_not_equal(MSt1, ctx, {free, ResultReg}),
    {MSt3, Reg1} = MMod:call_primitive(MSt2, ?PRIM_MAILBOX_NEXT, [
        ctx
    ]),
    MSt4 = MMod:free_native_register(MSt3, Reg1),
    MSt5 = MMod:jump_to_label(MSt4, Label),
    first_pass(Rest1, MMod, MSt5, State0);
% 25
first_pass(<<?OP_WAIT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_WAIT ~p\n", [Label]),
    MSt1 = MMod:set_continuation_to_label(MSt0, Label),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),
    first_pass(Rest1, MMod, MSt2, State0);
% 26
first_pass(<<?OP_WAIT_TIMEOUT, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, OffsetRef0} = MMod:set_continuation_to_offset(MSt0),
    {MSt2, Timeout, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_WAIT_TIMEOUT ~p, ~p\n", [Label, Timeout]),
    MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_WAIT_TIMEOUT, [
        ctx, jit_state, {free, Timeout}, Label
    ]),
    Offset0 = MMod:offset(MSt3),
    {MSt4, ResultReg0} = MMod:call_primitive(MSt3, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt5 = MMod:return_if_not_equal(MSt4, ctx, {free, ResultReg0}),
    {MSt6, ResultReg1} = MMod:call_primitive(MSt5, ?PRIM_CONTEXT_GET_FLAGS, [
        ctx, ?WAITING_TIMEOUT_EXPIRED
    ]),
    {MSt7, OffsetRef1, JumpToken1} = MMod:jump_to_offset_if_not_zero(MSt6, ResultReg1),
    MSt8 = MMod:call_primitive_last(MSt7, ?PRIM_WAIT_TIMEOUT_TRAP_HANDLER, [
        ctx, jit_state, Label
    ]),
    {MSt9, Offset1} = MMod:offset(MSt8, [JumpToken1]),
    MSt10 = MMod:free_native_register(MSt9, ResultReg1),
    Labels1 = [{OffsetRef0, Offset0}, {OffsetRef1, Offset1} | Labels0],
    first_pass(Rest2, MMod, MSt10, State0#state{labels = Labels1});
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
    MSt4 = MMod:handle_error_if_zero(MSt3, ResultReg),
    MSt5 = MMod:jump_to_label_if_and_non_zero_b(
        MSt4, ResultReg, ?TERM_GREATER_THAN + ?TERM_EQUALS, Label
    ),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest3, MMod, MSt6, State0);
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
    MSt4 = MMod:handle_error_if_zero(MSt3, ResultReg),
    MSt5 = MMod:jump_to_label_if_equal(
        MSt4, ResultReg, ?TERM_LESS_THAN, Label
    ),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest3, MMod, MSt6, State0);
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
    MSt4 = MMod:handle_error_if_zero(MSt3, ResultReg),
    MSt5 = MMod:jump_to_label_if_and_non_zero_b(
        MSt4, ResultReg, ?TERM_LESS_THAN + ?TERM_GREATER_THAN, Label
    ),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest3, MMod, MSt6, State0);
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
    MSt4 = MMod:handle_error_if_zero(MSt3, ResultReg),
    MSt5 = MMod:jump_to_label_if_equal(
        MSt4, ResultReg, ?TERM_EQUALS, Label
    ),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest3, MMod, MSt6, State0);
% 43
first_pass(<<?OP_IS_EQ_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_EQ_EXACT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
        ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_EXACT
    ]),
    MSt4 = MMod:handle_error_if_zero(MSt3, ResultReg),
    MSt5 = MMod:jump_to_label_if_and_non_zero_b(
        MSt4, ResultReg, ?TERM_LESS_THAN + ?TERM_GREATER_THAN, Label
    ),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest3, MMod, MSt6, State0);
% 44
first_pass(<<?OP_IS_NOT_EQ_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_NOT_EQ_EXACT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_COMPARE, [
        ctx, jit_state, {free, Arg1}, {free, Arg2}, ?TERM_COMPARE_EXACT
    ]),
    MSt4 = MMod:handle_error_if_zero(MSt3, ResultReg),
    MSt5 = MMod:jump_to_label_if_equal(
        MSt4, ResultReg, ?TERM_EQUALS, Label
    ),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest3, MMod, MSt6, State0);
% 45
first_pass(<<?OP_IS_INTEGER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_INTEGER ~p, ~p\n", [Label, Arg1]),
    {MSt2, State1} = term_is_immediate_or_boxed(
        Label, Arg1, ?TERM_INTEGER_TAG, ?TERM_BOXED_POSITIVE_INTEGER, MMod, MSt1, State0
    ),
    first_pass(Rest2, MMod, MSt2, State1);
% 46
first_pass(<<?OP_IS_FLOAT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_FLOAT ~p, ~p\n", [Label, Arg1]),
    MSt2 = term_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_FLOAT, MMod, MSt1),
    first_pass(Rest2, MMod, MSt2, State0);
% 47
first_pass(<<?OP_IS_NUMBER, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NUMBER ~p, ~p\n", [Label, Arg1]),
    % test term_is_integer
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    {MSt3, OffsetRef0, JumpToken0} = MMod:jump_to_offset_if_and_equal(
        MSt2, Reg, ?TERM_IMMED_TAG_MASK, ?TERM_INTEGER_TAG
    ),
    % test term_is_boxed
    MSt4 = MMod:jump_to_label_if_and_not_equal(
        MSt3, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED, Label
    ),
    MSt5 = MMod:and_(MSt4, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt6 = MMod:move_array_element(MSt5, Reg, 0, Reg),
    % test tag
    {MSt7, OffsetRef1, JumpToken1} = MMod:jump_to_offset_if_and_equal(
        MSt6, Reg, ?TERM_BOXED_TAG_MASK, ?TERM_BOXED_POSITIVE_INTEGER
    ),
    MSt8 = MMod:jump_to_label_if_and_not_equal(
        MSt7, {free, Reg}, ?TERM_BOXED_TAG_MASK, ?TERM_BOXED_FLOAT, Label
    ),
    {MSt9, Offset} = MMod:offset(MSt8, [JumpToken0, JumpToken1]),
    MSt10 = MMod:free_native_register(MSt9, Reg),
    Labels1 = [{OffsetRef0, Offset}, {OffsetRef1, Offset} | Labels0],
    first_pass(Rest2, MMod, MSt10, State0#state{labels = Labels1});
% 48
first_pass(<<?OP_IS_ATOM, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_ATOM ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:jump_to_label_if_and_not_equal(
        MSt2, {free, Reg}, ?TERM_IMMED2_TAG_MASK, ?TERM_IMMED2_ATOM, Label
    ),
    first_pass(Rest2, MMod, MSt3, State0);
% 49
first_pass(<<?OP_IS_PID, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_PID ~p, ~p\n", [Label, Arg1]),
    {MSt2, State1} = term_is_immediate_or_boxed(
        Label, Arg1, ?TERM_PID_TAG, ?TERM_BOXED_EXTERNAL_PID, MMod, MSt1, State0
    ),
    first_pass(Rest2, MMod, MSt2, State1);
% 50
first_pass(<<?OP_IS_REFERENCE, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_REFERENCE ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:jump_to_label_if_and_not_equal(
        MSt2, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED, Label
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    MSt6 = MMod:and_(MSt5, Reg, ?TERM_BOXED_TAG_MASK),
    {MSt7, OffsetRef, JumpToken} = MMod:jump_to_offset_if_equal(
        MSt6, Reg, ?TERM_BOXED_REF
    ),
    MSt8 = MMod:jump_to_label_if_not_equal(
        MSt7, Reg, ?TERM_BOXED_EXTERNAL_REF, Label
    ),
    {MSt9, Offset} = MMod:offset(MSt8, [JumpToken]),
    MSt10 = MMod:free_native_register(MSt9, Reg),
    Labels1 = [{OffsetRef, Offset} | Labels0],
    first_pass(Rest2, MMod, MSt10, State0#state{labels = Labels1});
% 51
first_pass(<<?OP_IS_PORT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_PORT ~p, ~p\n", [Label, Arg1]),
    {MSt2, State1} = term_is_immediate_or_boxed(
        Label, Arg1, ?TERM_PORT_TAG, ?TERM_BOXED_EXTERNAL_PORT, MMod, MSt1, State0
    ),
    first_pass(Rest2, MMod, MSt2, State1);
% 52
first_pass(<<?OP_IS_NIL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NIL ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:jump_to_label_if_not_equal(MSt2, Reg, ?TERM_NIL, Label),
    MSt4 = MMod:free_native_register(MSt3, Reg),
    first_pass(Rest2, MMod, MSt4, State0);
% 53
first_pass(<<?OP_IS_BINARY, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BINARY ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:jump_to_label_if_and_not_equal(
        MSt2, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED, Label
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    MSt6 = MMod:and_(MSt5, Reg, ?TERM_BOXED_TAG_MASK),
    {MSt7, OffsetRef1, JumpToken1} = MMod:jump_to_offset_if_equal(
        MSt6, Reg, ?TERM_BOXED_REFC_BINARY
    ),
    {MSt8, OffsetRef2, JumpToken2} = MMod:jump_to_offset_if_equal(
        MSt7, Reg, ?TERM_BOXED_HEAP_BINARY
    ),
    MSt9 = MMod:jump_to_label_if_not_equal(
        MSt8, Reg, ?TERM_BOXED_SUB_BINARY, Label
    ),
    {MSt10, Offset} = MMod:offset(MSt9, [JumpToken1, JumpToken2]),
    MSt11 = MMod:free_native_register(MSt10, Reg),
    Labels1 = [{OffsetRef1, Offset}, {OffsetRef2, Offset} | Labels0],
    first_pass(Rest2, MMod, MSt11, State0#state{labels = Labels1});
% 55
first_pass(<<?OP_IS_LIST, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_LIST ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    {MSt3, OffsetRef, JumpToken} = MMod:jump_to_offset_if_equal(MSt2, Reg, ?TERM_NIL),
    MSt4 = MMod:jump_to_label_if_and_not_equal(
        MSt3, {free, Reg}, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_LIST, Label
    ),
    {MSt5, Offset} = MMod:offset(MSt4, [JumpToken]),
    MSt6 = MMod:free_native_register(MSt5, Reg),
    Labels1 = [{OffsetRef, Offset} | Labels0],
    first_pass(Rest2, MMod, MSt6, State0#state{labels = Labels1});
% 56
first_pass(<<?OP_IS_NONEMPTY_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NONEMPTY_LIST ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:jump_to_label_if_and_not_equal(
        MSt2, {free, Reg}, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_LIST, Label
    ),
    first_pass(Rest2, MMod, MSt3, State0);
% 57
first_pass(<<?OP_IS_TUPLE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_TUPLE ~p, ~p\n", [Label, Arg1]),
    MSt2 = term_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_TUPLE, MMod, MSt1),
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
    MSt6 = MMod:jump_to_label_if_not_equal(MSt5, Reg, Arity, Label),
    MSt7 = MMod:free_native_register(MSt6, Reg),
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
            AccMSt3 = MMod:handle_error_if_zero(AccMSt2, ResultReg),
            AccMSt4 = MMod:jump_to_label_if_equal(
                AccMSt3, ResultReg, ?TERM_EQUALS, JmpLabel
            ),
            AccMSt5 = MMod:free_native_register(AccMSt4, ResultReg),
            {AccMSt5, AccRest2}
        end,
        {MSt1, Rest3},
        lists:seq(0, (ListSize div 2) - 1)
    ),
    ?TRACE("\n", []),
    MSt3 = MMod:jump_to_label(MSt2, DefaultLabel),
    first_pass(Rest4, MMod, MSt3, State0);
% 60
first_pass(<<?OP_SELECT_TUPLE_ARITY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {DefaultLabel, Rest2} = decode_label(Rest1),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_SELECT_TUPLE_ARITY ~p, ~p", [SrcValue, DefaultLabel]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, SrcValue),
    MSt3 = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    MSt5 = MMod:shift_right(MSt4, Reg, 6),
    {MSt6, Rest4} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {CmpValue, AccRest1} = decode_literal(AccRest0),
            {JmpLabel, AccRest2} = decode_label(AccRest1),
            ?TRACE(", ~p => ~p", [CmpValue, JmpLabel]),
            AccMSt1 = MMod:jump_to_label_if_equal(
                AccMSt0, Reg, CmpValue, JmpLabel
            ),
            {AccMSt1, AccRest2}
        end,
        {MSt5, Rest3},
        lists:seq(0, (ListSize div 2) - 1)
    ),
    ?TRACE("\n", []),
    MSt7 = MMod:free_native_register(MSt6, Reg),
    MSt8 = MMod:jump_to_label(MSt7, DefaultLabel),
    first_pass(Rest4, MMod, MSt8, State0);
% 61
first_pass(<<?OP_JUMP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_JUMP ~p\n", [Label]),
    MSt1 = MMod:call_only_or_schedule_next(MSt0, Label),
    first_pass(Rest1, MMod, MSt1, State0);
% 62
% Same implementation as OP_TRY, to confirm.
first_pass(<<?OP_CATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CATCH ~p, ~p\n", [Dest, Label]),
    MSt2 = term_from_catch_label(Dest, Label, MMod, MSt1),
    first_pass(Rest2, MMod, MSt2, State0);
% 63
first_pass(<<?OP_CATCH_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_CATCH_END ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_register(MSt2, Dest),
    {MSt4, ResultReg} = MMod:call_primitive(MSt3, ?PRIM_CATCH_END, [ctx, jit_state]),
    MSt5 = MMod:handle_error_if_false(MSt4, ResultReg),
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    first_pass(Rest1, MMod, MSt6, State0);
% 64
first_pass(<<?OP_MOVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Source, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_MOVE ~p, ~p\n", [Source, Dest]),
    MSt3 = MMod:move_to_vm_register(MSt2, Source, Dest),
    MSt4 = MMod:free_native_register(MSt3, Source),
    MSt5 = MMod:free_native_register(MSt4, Dest),
    first_pass(Rest2, MMod, MSt5, State0);
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
    MSt7 = MMod:free_native_register(MSt6, HeadDest),
    MSt8 = MMod:move_array_element(MSt7, Reg, ?LIST_TAIL_INDEX, TailDest),
    MSt9 = MMod:free_native_register(MSt8, Reg),
    MSt10 = MMod:free_native_register(MSt9, TailDest),
    first_pass(Rest3, MMod, MSt10, State0);
% 66
first_pass(<<?OP_GET_TUPLE_ELEMENT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    %   MSt0 = MMod:debugger(MStR),
    {MSt1, Source, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {Element, Rest2} = decode_literal(Rest1),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    ?TRACE("OP_GET_TUPLE_ELEMENT ~p, ~p, ~p\n", [Source, Element, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, Source),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, Element + 1, Dest),
    MSt6 = MMod:free_native_register(MSt5, Reg),
    MSt7 = MMod:free_native_register(MSt6, Dest),
    first_pass(Rest3, MMod, MSt7, State0);
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
    MSt6 = MMod:free_native_register(MSt5, ResultReg),
    MSt7 = MMod:free_native_register(MSt6, Dest),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest3, MMod, MSt7, State0);
% 72
first_pass(<<?OP_BADMATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_BADMATCH ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, ?BADMATCH_ATOM, {free, Arg1}
    ]),
    first_pass(Rest1, MMod, MSt2, State0);
% 73
first_pass(<<?OP_IF_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_IF_END\n", []),
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, ?IF_CLAUSE_ATOM
    ]),
    first_pass(Rest0, MMod, MSt1, State0);
% 74
first_pass(<<?OP_CASE_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_CASE_END ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, ?CASE_CLAUSE_ATOM, {free, Arg1}
    ]),
    first_pass(Rest1, MMod, MSt2, State0);
% 75
first_pass(<<?OP_CALL_FUN, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    % MSt0 = MMod:debugger(MStR),
    {ArgsCount, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_CALL_FUN ~p\n", [ArgsCount]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    {MSt2, FuncReg} = read_any_xreg(ArgsCount, MMod, MSt1),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, FuncReg),
    {MSt4, State1} = validate_is_function(MMod, MSt3, Reg, State0),
    MSt5 = MMod:call_primitive_with_cp(MSt4, ?PRIM_CALL_FUN, [ctx, jit_state, Reg, ArgsCount]),
    first_pass(Rest1, MMod, MSt5, State1);
% 77
first_pass(<<?OP_IS_FUNCTION, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_FUNCTION ~p, ~p\n", [Label, Arg1]),
    MSt2 = term_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_FUN, MMod, MSt1),
    first_pass(Rest2, MMod, MSt2, State0);
% 78
first_pass(<<?OP_CALL_EXT_ONLY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_CALL_EXT_ONLY ~p, ~p\n", [Arity, Index]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_CALL_EXT, [ctx, jit_state, Arity, Index, -1]),
    first_pass(Rest2, MMod, MSt2, State0);
% 96
first_pass(<<?OP_FMOVE, ?COMPACT_EXTENDED_FP_REGISTER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    % MSt0 = MMod:debugger(MStR),
    {FPRegIndex, Rest1} = decode_literal(Rest0),
    FPReg = {fp_reg, FPRegIndex},
    {MSt1, Dest, Rest2} = decode_dest(Rest1, MMod, MSt0),
    ?TRACE("OP_FMOVE ~p, ~p\n", [FPReg, Dest]),
    {MSt2, FPUReg} = MMod:move_to_native_register(MSt1, FPReg),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_FROM_FLOAT, [ctx, {free, FPUReg}]),
    MSt4 = MMod:move_to_vm_register(MSt3, ResultReg, Dest),
    MSt5 = MMod:free_native_register(MSt4, ResultReg),
    MSt6 = MMod:free_native_register(MSt5, Dest),
    first_pass(Rest2, MMod, MSt6, State0);
first_pass(<<?OP_FMOVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    % MSt0 = MMod:debugger(MStR),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {FPReg, Rest2} = decode_fp_register(Rest1),
    ?TRACE("OP_FMOVE ~p, ~p\n", [SrcValue, FPReg]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    {MSt4, Reg} = MMod:move_to_native_register(MSt3, SrcValue),
    MSt5 = MMod:and_(MSt4, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt6 = MMod:move_array_element(MSt5, Reg, 1, Reg),
    MSt7 = MMod:move_to_vm_register(MSt6, Reg, FPReg),
    MSt8 = MMod:free_native_register(MSt7, Reg),
    first_pass(Rest2, MMod, MSt8, State0);
% 97
first_pass(<<?OP_FCONV, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, SrcValue),
    {MSt3, IsNumber} = MMod:call_primitive(MSt2, ?PRIM_TERM_IS_NUMBER, [Reg]),
    {MSt4, OffsetRef0, JumpToken0} = MMod:jump_to_offset_if_true(MSt3, IsNumber),
    MSt5 = MMod:call_primitive_last(MSt4, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, ?BADARITH_ATOM
    ]),
    {MSt6, Offset0} = MMod:offset(MSt5, [JumpToken0]),
    MSt7 = MMod:free_native_register(MSt6, IsNumber),
    {{fp_reg, FPRegIndex}, Rest2} = decode_fp_register(Rest1),
    {MSt8, ResultReg} = MMod:call_primitive(MSt7, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
    MSt9 = MMod:free_native_register(MSt8, ResultReg),
    ?TRACE("OP_FCONF ~p, ~p\n", [SrcValue, {fp_reg, FPRegIndex}]),
    {MSt10, ConvToFloatResReg} = MMod:call_primitive(MSt9, ?PRIM_TERM_CONV_TO_FLOAT, [
        ctx, {free, Reg}, FPRegIndex
    ]),
    MSt11 = MMod:free_native_register(MSt10, ConvToFloatResReg),
    Labels1 = [{OffsetRef0, Offset0} | Labels0],
    first_pass(Rest2, MMod, MSt11, State0#state{labels = Labels1});
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
    {Label, Rest1} = decode_label(Rest0),
    {{fp_reg, FPRegIndex1}, Rest2} = decode_fp_register(Rest1),
    {{fp_reg, FPRegIndex2}, Rest3} = decode_fp_register(Rest2),
    ?TRACE("OP_FNEGATE ~p, ~p, ~p\n", [Label, {fp_reg, FPRegIndex1}, {fp_reg, FPRegIndex2}]),
    {MSt1, Reg} = MMod:call_primitive(MSt0, ?PRIM_FNEGATE, [
        ctx, FPRegIndex1, FPRegIndex2
    ]),
    MSt2 = MMod:free_native_register(MSt1, Reg),
    first_pass(Rest3, MMod, MSt2, State0);
% 104
first_pass(<<?OP_TRY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_TRY ~p, ~p\n", [Dest, Label]),
    MSt2 = term_from_catch_label(Dest, Label, MMod, MSt1),
    first_pass(Rest2, MMod, MSt2, State0);
% 105
first_pass(<<?OP_TRY_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_TRY_END ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_register(MSt2, Dest),
    first_pass(Rest1, MMod, MSt3, State0);
% 106
first_pass(<<?OP_TRY_CASE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_TRY_CASE ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_register(MSt2, Dest),
    first_pass(Rest1, MMod, MSt3, State0);
% 107
first_pass(<<?OP_TRY_CASE_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_TRY_CASE_END ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, ?TRY_CLAUSE_ATOM, Arg1
    ]),
    first_pass(Rest1, MMod, MSt2, State0);
% 108
first_pass(<<?OP_RAISE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Stacktrace, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, ExcValue, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_RAISE ~p, ~p\n", [Stacktrace, ExcValue]),
    MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_RAISE, [
        ctx, jit_state, Stacktrace, ExcValue
    ]),
    first_pass(Rest2, MMod, MSt3, State0);
% 112
% first_pass(<<?OP_APPLY, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 113
% first_pass(<<?OP_APPLY_LAST, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 114
first_pass(<<?OP_IS_BOOLEAN, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BOOLEAN ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    {MSt3, OffsetRef, JumpToken} = MMod:jump_to_offset_if_equal(
        MSt2, Reg, ?TRUE_ATOM
    ),
    MSt4 = MMod:jump_to_label_if_not_equal(
        MSt3, Reg, ?FALSE_ATOM, Label
    ),
    {MSt5, Offset} = MMod:offset(MSt4, [JumpToken]),
    MSt6 = MMod:free_native_register(MSt5, Reg),
    Labels1 = [{OffsetRef, Offset} | Labels0],
    first_pass(Rest2, MMod, MSt6, State0#state{labels = Labels1});
% 115
% first_pass(<<?OP_IS_FUNCTION2, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 119
% first_pass(<<?OP_BS_GET_BINARY2, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 124
first_pass(<<?OP_GC_BIF1, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_register(MSt1, TrimResultReg),
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
    first_pass(Rest5, MMod, MSt7, State0);
% 125
first_pass(<<?OP_GC_BIF2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_register(MSt1, TrimResultReg),
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
    first_pass(Rest6, MMod, MSt8, State0);
% 129
first_pass(<<?OP_IS_BITSTR, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BITSTR ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:jump_to_label_if_and_not_equal(
        MSt2, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED, Label
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    MSt6 = MMod:and_(MSt5, Reg, ?TERM_BOXED_TAG_MASK),
    {MSt7, OffsetRef1, JumpToken1} = MMod:jump_to_offset_if_equal(
        MSt6, Reg, ?TERM_BOXED_REFC_BINARY
    ),
    {MSt8, OffsetRef2, JumpToken2} = MMod:jump_to_offset_if_equal(
        MSt7, Reg, ?TERM_BOXED_HEAP_BINARY
    ),
    MSt9 = MMod:jump_to_label_if_not_equal(
        MSt8, Reg, ?TERM_BOXED_SUB_BINARY, Label
    ),
    {MSt10, Offset} = MMod:offset(MSt9, [JumpToken1, JumpToken2]),
    MSt11 = MMod:free_native_register(MSt10, Reg),
    Labels1 = [{OffsetRef1, Offset}, {OffsetRef2, Offset} | Labels0],
    first_pass(Rest2, MMod, MSt11, State0#state{labels = Labels1});
% 132
% first_pass(<<?OP_BS_MATCH_STRING, Rest0/binary>>, MMod, MSt0, State0) ->
% 136
first_pass(<<?OP_TRIM, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {NWords, Rest1} = decode_literal(Rest0),
    {_NRemaining, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_TRIM ~p, ~p\n", [NWords, _NRemaining]),
    MSt1 = MMod:increment_sp(MSt0, NWords),
    first_pass(Rest2, MMod, MSt1, State0);
% 152
first_pass(<<?OP_GC_BIF3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_register(MSt1, TrimResultReg),
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
% first_pass(<<?OP_PUT_MAP_ASSOC, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 156
first_pass(<<?OP_IS_MAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_MAP ~p, ~p\n", [Label, Arg1]),
    MSt2 = term_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_MAP, MMod, MSt1),
    first_pass(Rest2, MMod, MSt2, State0);
% 157
% first_pass(<<?OP_HAS_MAP_FIELDS, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 158
% first_pass(<<?OP_GET_MAP_ELEMENTS, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
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
    MSt3 = MMod:jump_to_label_if_and_not_equal(
        MSt2, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED, Label
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt5, TagReg} = MMod:get_array_element(MSt4, Reg, 0),
    MSt6 = MMod:jump_to_label_if_and_not_equal(
        MSt5, TagReg, ?TERM_BOXED_TAG_MASK, ?TERM_BOXED_TUPLE, Label
    ),
    MSt7 = MMod:shift_right(MSt6, TagReg, 6),
    MSt8 = MMod:jump_to_label_if_not_equal(MSt7, TagReg, Arity, Label),
    MSt9 = MMod:free_native_register(MSt8, TagReg),
    MSt10 = MMod:move_array_element(MSt9, Reg, 1, Reg),
    {MSt11, AtomReg} =
        case maps:find(AtomResolver(AtomIndex), ?DEFAULT_ATOMS) of
            error ->
                MMod:call_primitive(
                    MSt10, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, AtomIndex]
                );
            {ok, Val} ->
                {MSt0, Val}
        end,
    MSt12 = MMod:jump_to_label_if_not_equal(MSt11, AtomReg, Reg, Label),
    MSt13 = MMod:free_native_register(MSt12, Reg),
    MSt14 = MMod:free_native_register(MSt13, AtomReg),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest4, MMod, MSt14, State0);
% 160
% first_pass(<<?OP_BUILD_STACKTRACE, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 162
first_pass(<<?OP_GET_HD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_GET_HD ~p, ~p\n", [SrcValue, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, SrcValue),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, ?LIST_HEAD_INDEX, Dest),
    MSt6 = MMod:free_native_register(MSt5, Dest),
    MSt7 = MMod:free_native_register(MSt6, Reg),
    first_pass(Rest3, MMod, MSt7, State0);
% 163
first_pass(<<?OP_GET_TL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_GET_TL ~p, ~p\n", [SrcValue, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, SrcValue),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, ?LIST_TAIL_INDEX, Dest),
    MSt6 = MMod:free_native_register(MSt5, Dest),
    MSt7 = MMod:free_native_register(MSt6, Reg),
    first_pass(Rest3, MMod, MSt7, State0);
% 164
first_pass(<<?OP_PUT_TUPLE2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    %   MSt0 = MMod:debugger(MStR),
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
            AccMSt3 = MMod:free_native_register(AccMSt2, Element),
            {AccMSt3, AccRest1}
        end,
        {MSt3, Rest2},
        lists:seq(1, ListSize)
    ),
    ?TRACE("]\n", []),
    MSt5 = MMod:or_(MSt4, ResultReg, ?TERM_PRIMARY_BOXED),
    MSt6 = MMod:move_to_vm_register(MSt5, ResultReg, Dest),
    MSt7 = MMod:free_native_register(MSt6, Dest),
    MSt8 = MMod:free_native_register(MSt7, ResultReg),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest3, MMod, MSt8, State0);
% 166
first_pass(<<?OP_BS_START_MATCH3, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Live, Rest3} = decode_literal(Rest2),
    {MSt2, Dest, Rest4} = decode_dest(Rest3, MMod, MSt1),
    ?TRACE("OP_BS_START_MATCH3 ~p, ~p, ~p, ~p\n", [Fail, Src, Live, Dest]),
    {MSt3, Reg, BoxTagReg, OffsetRefsFail, TokensFail, OffsetRefsCont, TokensCont} = term_is_binary_or_match_state(
        Src, MMod, MSt2
    ),
    {MSt4, FailOffset} = MMod:offset(MSt3, TokensFail),
    MSt5 = MMod:free_native_register(MSt4, BoxTagReg),
    MSt6 = MMod:move_to_vm_register(MSt5, Src, Dest),
    MSt7 = MMod:free_native_register(MSt6, Reg),
    MSt8 = MMod:jump_to_label(MSt7, Fail),
    {MSt9, ContinueOffset} = MMod:offset(MSt8, TokensCont),
    MSt10 = MMod:free_native_register(MSt9, BoxTagReg),
    MSt11 = MMod:free_native_register(MSt10, Reg),
    FailLabels = [{OffsetRef, FailOffset} || OffsetRef <- OffsetRefsFail],
    ContLabels = [{OffsetRef, ContinueOffset} || OffsetRef <- OffsetRefsCont],
    Labels1 = FailLabels ++ ContLabels ++ Labels0,
    {MSt12, _NewSrc} = term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt11),
    MSt13 = MMod:free_native_register(MSt12, Src),
    MSt14 = MMod:free_native_register(MSt13, Dest),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest4, MMod, MSt14, State0#state{labels = Labels1});
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
    MSt9 = MMod:free_native_register(MSt8, Reg),
    first_pass(Rest3, MMod, MSt9, State0);
% 169
first_pass(<<?OP_SWAP, Rest0/binary>>, MMod, MSt0, State) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, ArgA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {MSt2, ArgB, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_SWAP ~p, ~p\n", [ArgA, ArgB]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, ArgA),
    MSt4 = MMod:move_to_vm_register(MSt3, ArgB, ArgA),
    MSt5 = MMod:move_to_vm_register(MSt4, Reg, ArgB),
    MSt6 = MMod:free_native_register(MSt5, Reg),
    first_pass(Rest2, MMod, MSt6, State);
% 170
first_pass(<<?OP_BS_START_MATCH4, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    % fail since OTP 23 might be either 'no_fail', 'resume' or a fail label
    % TODO: figure out what could fail
    {Fail, Rest1} = decode_atom_or_label(Rest0, State0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, Src, Rest3} = decode_compact_term(Rest2, MMod, MSt0, State0),
    {MSt2, Dest, Rest4} = decode_dest(Rest3, MMod, MSt1),
    ?TRACE("OP_BS_START_MATCH4 ~p, ~p, ~p, ~p\n", [Fail, Live, Src, Dest]),
    {MSt3, Reg, BoxTagReg, OffsetRefsFail, TokensFail, OffsetRefsCont, TokensCont} = term_is_binary_or_match_state(
        Src, MMod, MSt2
    ),
    {MSt4, FailOffset} = MMod:offset(MSt3, TokensFail),
    MSt5 = MMod:free_native_register(MSt4, BoxTagReg),
    MSt6 = MMod:move_to_vm_register(MSt5, Src, Dest),
    MSt7 = MMod:free_native_register(MSt6, Reg),
    MSt8 =
        if
            is_integer(Fail) ->
                MMod:jump_to_label(MSt7, Fail);
            true ->
                % fail since OTP 23 might be either 'no_fail', 'resume' or a fail label
                MMod:call_primitive_last(MSt7, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, ?BADARG_ATOM
                ])
        end,
    {MSt9, ContinueOffset} = MMod:offset(MSt8, TokensCont),
    MSt10 = MMod:free_native_register(MSt9, BoxTagReg),
    MSt11 = MMod:free_native_register(MSt10, Reg),
    FailLabels = [{OffsetRef, FailOffset} || OffsetRef <- OffsetRefsFail],
    ContLabels = [{OffsetRef, ContinueOffset} || OffsetRef <- OffsetRefsCont],
    Labels1 = FailLabels ++ ContLabels ++ Labels0,
    {MSt12, _NewSrc} = term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt11),
    MSt13 = MMod:free_native_register(MSt12, Src),
    MSt14 = MMod:free_native_register(MSt13, Dest),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest4, MMod, MSt14, State0#state{labels = Labels1});
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
            AccMSt3 = MMod:free_native_register(AccMSt2, Element),
            {AccMSt3, AccRest1}
        end,
        {MSt3, Rest3},
        lists:seq(3, NumFree + 2)
    ),
    ?TRACE("]\n", []),
    MSt5 = MMod:or_(MSt4, ResultReg, ?TERM_PRIMARY_BOXED),
    MSt6 = MMod:move_to_vm_register(MSt5, ResultReg, Dest),
    MSt7 = MMod:free_native_register(MSt6, Dest),
    MSt8 = MMod:free_native_register(MSt7, ResultReg),
    first_pass(Rest4, MMod, MSt8, State0);
% 172
first_pass(<<?OP_INIT_YREGS, Rest0/binary>>, MMod, MSt0, State) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {ListSize, Rest1} = decode_extended_list_header(Rest0),
    ?TRACE("OP_INIT_YREGS ~p\n", [ListSize]),
    {MSt1, Rest2} = lists:foldl(
        fun(_, {AccMSt0, AccRest0}) ->
            {AccMSt1, Dest, AccRest1} = decode_dest(AccRest0, MMod, AccMSt0),
            AccMSt2 = MMod:move_to_vm_register(AccMSt1, ?TERM_NIL, Dest),
            AccMSt3 = MMod:free_native_register(AccMSt2, Dest),
            {AccMSt3, AccRest1}
        end,
        {MSt0, Rest1},
        lists:duplicate(ListSize, [])
    ),
    first_pass(Rest2, MMod, MSt1, State);
% 173
first_pass(<<?OP_RECV_MARKER_BIND, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {MSt2, RegB, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_RECV_MARKER_BIND ~p, ~p\n", [RegA, RegB]),
    MSt3 = MMod:free_native_register(MSt2, RegA),
    MSt4 = MMod:free_native_register(MSt3, RegB),
    first_pass(Rest2, MMod, MSt4, State0);
% 174
first_pass(<<?OP_RECV_MARKER_CLEAR, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_CLEAR ~p\n", [RegA]),
    MSt2 = MMod:free_native_register(MSt1, RegA),
    first_pass(Rest1, MMod, MSt2, State0);
% 175
first_pass(<<?OP_RECV_MARKER_RESERVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_RESERVE ~p\n", [Dest]),
    % Clear register to avoid any issue with GC
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_register(MSt2, Dest),
    first_pass(Rest1, MMod, MSt3, State0);
% 176
first_pass(<<?OP_RECV_MARKER_USE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_USE ~p\n", [RegA]),
    MSt2 = MMod:free_native_register(MSt1, RegA),
    first_pass(Rest1, MMod, MSt2, State0);
% 177
% first_pass(<<?OP_BS_CREATE_BIN, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
% 178
first_pass(<<?OP_CALL_FUN2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Tag, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {ArgsCount, Rest2} = decode_literal(Rest1),
    {MSt2, Fun, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_CALL_FUN2 ~p, ~p, ~p\n", [Tag, ArgsCount, Fun]),
    % We ignore Tag (could be literal 0 or atom unsafe)
    MSt3 = MSt3 = MMod:free_native_register(MSt2, Tag),
    MSt4 = MMod:decrement_reductions_and_maybe_schedule_next(MSt3),
    {MSt5, Reg} = MMod:move_to_native_register(MSt4, Fun),
    {MSt6, State1} = validate_is_function(MMod, MSt5, Reg, State0),
    MSt7 = MMod:call_primitive_with_cp(MSt6, ?PRIM_CALL_FUN, [ctx, jit_state, Reg, ArgsCount]),
    first_pass(Rest3, MMod, MSt7, State1);
% 181
% first_pass(<<?OP_UPDATE_RECORD, Rest0/binary>>, MMod, MSt0, #state{labels = Labels0} = State0) ->
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
    MSt6 = MMod:free_native_register(MSt5, MatchStateReg0),
    MSt7 = MMod:and_(MSt6, BSBinaryReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt8, Rest4} = first_pass_bs_match(
        Fail, MatchState, BSBinaryReg, BSOffsetReg, ListLen, Rest3, MMod, MSt7, State0
    ),
    ?TRACE("]\n", []),
    MSt9 = MMod:free_native_register(MSt8, BSBinaryReg),
    {MSt10, MatchStateReg1} = MMod:copy_to_native_register(MSt9, MatchState),
    MSt11 = MMod:and_(MSt10, MatchStateReg1, ?TERM_PRIMARY_CLEAR_MASK),
    MSt12 = MMod:move_to_array_element(MSt11, MatchStateReg1, BSOffsetReg, 2),
    MSt13 = MMod:free_native_register(MSt12, MatchStateReg1),
    MSt14 = MMod:free_native_register(MSt13, BSOffsetReg),
    MSt15 = MMod:free_native_register(MSt14, MatchState),
    ?ASSERT_ALL_NATIVE_FREE(MSt15),
    first_pass(Rest4, MMod, MSt15, State0).

first_pass_bs_match(_Fail, _MatchState, _BSBinaryReg, _BSOffsetReg, 0, Rest, _MMod, MSt, _State) ->
    {MSt, Rest};
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
    {J2, Rest2, NewMatchState, MSt1, State1} =
        case Command of
            ensure_at_least ->
                first_pass_bs_match_ensure_at_least(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
                );
            ensure_exactly ->
                first_pass_bs_match_ensure_exactly(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
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
                    MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
                );
            '=:=' ->
                first_pass_bs_match_equal_colon_equal(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
                );
            skip ->
                first_pass_bs_match_skip(MatchState, BSOffsetReg, J1, Rest1, MMod, MSt0, State0)
        end,
    {MSt2, MatchStateReg} = MMod:copy_to_native_register(MSt1, NewMatchState),
    MSt3 = MMod:and_(MSt2, MatchStateReg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_to_array_element(MSt3, MatchStateReg, BSOffsetReg, 2),
    MSt5 = MMod:free_native_register(MSt4, MatchStateReg),
    first_pass_bs_match(
        Fail, NewMatchState, BSBinaryReg, BSOffsetReg, J2, Rest2, MMod, MSt5, State1
    ).

first_pass_bs_match_ensure_at_least(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0, State0
) ->
    {Stride, Rest1} = decode_literal(Rest0),
    if
        Stride < 0 ->
            MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                ctx, jit_state, ?BADARG_ATOM
            ]),
            {J0, Rest0, MatchState, MSt1, State0};
        true ->
            % TODO: check use of unit here (TODO is the same in opcodeswitch.h)
            {_Unit, Rest2} = decode_literal(Rest1),
            ?TRACE("{ensure_at_least,~p,~p},", [Stride, _Unit]),
            {MSt1, Reg} = MMod:get_array_element(MSt0, BSBinaryReg, 1),
            MSt2 = MMod:shift_left(MSt1, Reg, 3),
            % Reg is bs_bin_size * 8 (use unit instead ??)
            MSt3 = MMod:sub(MSt2, Reg, BSOffsetReg),
            % Reg is (bs_bin_size * 8) - bs_offset
            MSt4 = MMod:jump_to_label_if_lt(MSt3, Reg, Stride, Fail),
            MSt5 = MMod:free_native_register(MSt4, Reg),
            {J0 - 2, Rest2, MatchState, MSt5, State0}
    end.

first_pass_bs_match_ensure_exactly(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0, State0
) ->
    {Stride, Rest1} = decode_literal(Rest0),
    if
        Stride < 0 ->
            MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                ctx, jit_state, ?BADARG_ATOM
            ]),
            {J0, Rest0, MatchState, MSt1, State0};
        true ->
            ?TRACE("{ensure_exactly,~p},", [Stride]),
            {MSt1, Reg} = MMod:get_array_element(MSt0, BSBinaryReg, 1),
            MSt2 = MMod:shift_left(MSt1, Reg, 3),
            % Reg is bs_bin_size * 8 (use unit instead ??)
            MSt3 = MMod:sub(MSt2, Reg, BSOffsetReg),
            % Reg is (bs_bin_size * 8) - bs_offset
            MSt4 = MMod:jump_to_label_if_not_equal(MSt3, Reg, Stride, Fail),
            MSt5 = MMod:free_native_register(MSt4, Reg),
            {J0 - 1, Rest1, MatchState, MSt5, State0}
    end.

first_pass_bs_match_integer(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0, State0
) ->
    {_Live, Rest1} = decode_literal(Rest0),
    {Flags, Rest2} = decode_compile_time_literal(Rest1, State0),
    FlagsValue = decode_flags_list(Flags),
    {MSt1, Size, Rest3} = decode_compact_term(Rest2, MMod, MSt0, State0),
    {Unit, Rest4} = decode_literal(Rest3),
    ?TRACE("{integer,~p,~p,~p, ", [Flags, Size, Unit]),
    {MSt2, SizeReg, State1} = term_to_int(Size, MMod, MSt1, State0),
    {MSt5, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt2, SizeReg * Unit};
            Unit =:= 8 ->
                MSt3 = MMod:shift_left(SizeReg, 3),
                {MSt3, SizeReg};
            Unit =:= 1 ->
                {MSt2, SizeReg};
            true ->
                MSt3 = MMod:mul(SizeReg, Unit),
                {MSt3, SizeReg}
        end,
    {MSt6, Result} = MMod:call_primitive(MSt5, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, BSBinaryReg, BSOffsetReg, NumBits, FlagsValue
    ]),
    MSt7 = MMod:handle_error_if_zero(MSt6, Result),
    MSt8 = MMod:jump_to_label_if_equal(
        MSt7, Result, ?FALSE_ATOM, Fail
    ),
    {MSt9, Dest, Rest5} = decode_dest(Rest4, MMod, MSt8),
    ?TRACE("~p},", [Dest]),
    MSt10 = MMod:move_to_vm_register(MSt9, Result, Dest),
    MSt11 = MMod:free_native_register(MSt10, Result),
    MSt12 = MMod:add(MSt11, BSOffsetReg, NumBits),
    MSt13 = MMod:free_native_register(MSt12, NumBits),
    {J0 - 5, Rest5, MatchState, MSt13, State1}.

first_pass_bs_match_binary(
    Fail,
    MatchState,
    BSBinaryReg,
    BSOffsetReg,
    J0,
    Rest0,
    MMod,
    MSt0,
    #state{labels = Labels0} = State0
) ->
    {Live, Rest1} = decode_literal(Rest0),
    {Flags, Rest2} = decode_compile_time_literal(Rest1, State0),
    _FlagsValue = decode_flags_list(Flags),
    {Size, Rest3} = decode_literal(Rest2),
    {Unit, Rest4} = decode_literal(Rest3),
    ?TRACE("{binary,~p,~p,~p,~p", [Live, Flags, Size, Unit]),
    MatchedBits = Size * Unit,
    {MSt4, Labels2} =
        if
            MatchedBits rem 8 =:= 0 ->
                {MSt1, OffsetRef, JumpToken} = MMod:jump_to_offset_if_and_equal(
                    MSt0, BSOffsetReg, 2#111, 0
                ),
                MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, ?BADARG_ATOM
                ]),
                {MSt3, ContinueOffset} = MMod:offset(MSt2, [JumpToken]),
                Labels1 = [{OffsetRef, ContinueOffset} | Labels0],
                {MSt3, Labels1};
            true ->
                MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, ?BADARG_ATOM
                ]),
                {MSt1, Labels0}
        end,
    MatchedBytes = MatchedBits div 8,
    {MSt5, BSOffseBytesReg} = MMod:copy_to_native_register(MSt4, BSOffsetReg),
    MSt6 = MMod:shift_right(MSt5, BSOffseBytesReg, 3),
    {MSt7, RemainingBytesReg} = MMod:get_array_element(MSt6, BSBinaryReg, 1),
    MSt8 = MMod:sub(MSt7, RemainingBytesReg, BSOffseBytesReg),
    MSt9 = MMod:jump_to_label_if_lt(MSt8, RemainingBytesReg, MatchedBytes, Fail),
    MSt10 = MMod:free_native_register(MSt9, RemainingBytesReg),
    {MSt11, HeapSizeReg} = MMod:call_primitive(MSt10, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        ctx, jit_state, BSBinaryReg, MatchedBytes
    ]),
    {MSt12, NewMatchState} = memory_ensure_free_with_extra_root(
        MatchState, Live, {free, HeapSizeReg}, MMod, MSt11
    ),
    % Restore BSBinaryReg as it may have been gc'd as well
    {MSt13, MatchStateReg0} = MMod:copy_to_native_register(MSt12, NewMatchState),
    MSt14 = MMod:and_(MSt13, MatchStateReg0, ?TERM_PRIMARY_CLEAR_MASK),
    MSt15 = MMod:move_array_element(MSt14, MatchStateReg0, 1, BSBinaryReg),
    MSt16 = MMod:free_native_register(MSt15, MatchStateReg0),
    {MSt17, ResultTerm} = MMod:call_primitive(MSt16, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, jit_state, BSBinaryReg, {free, BSOffseBytesReg}, MatchedBytes
    ]),
    MSt18 = MMod:and_(MSt17, BSBinaryReg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt19, Dest, Rest5} = decode_dest(Rest4, MMod, MSt18),
    ?TRACE("~p},", [Dest]),
    MSt20 = MMod:move_to_vm_register(MSt19, ResultTerm, Dest),
    MSt21 = MMod:free_native_register(MSt20, ResultTerm),
    MSt22 = MMod:add(MSt21, BSOffsetReg, MatchedBits),
    State1 = State0#state{labels = Labels2},
    {J0 - 5, Rest5, NewMatchState, MSt22, State1}.

first_pass_bs_match_get_tail(
    MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0, #state{labels = Labels0} = State0
) ->
    {Live, Rest1} = decode_literal(Rest0),
    {Unit, Rest2} = decode_literal(Rest1),
    ?TRACE("{get_tail,~p,~p,", [Live, Unit]),
    {MSt1, OffsetRef, JumpToken} = MMod:jump_to_offset_if_and_equal(
        MSt0, BSOffsetReg, 2#111, 0
    ),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, ?BADARG_ATOM
    ]),
    {MSt3, ContinueOffset} = MMod:offset(MSt2, [JumpToken]),
    {MSt4, BSOffseBytesReg} = MMod:copy_to_native_register(MSt3, BSOffsetReg),
    MSt5 = MMod:shift_right(MSt4, BSOffseBytesReg, 3),
    {MSt6, TailBytesReg} = MMod:get_array_element(MSt5, BSBinaryReg, 1),
    MSt7 = MMod:sub(MSt6, TailBytesReg, BSOffseBytesReg),
    {MSt8, HeapSizeReg} = MMod:call_primitive(MSt7, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        ctx, jit_state, BSBinaryReg, TailBytesReg
    ]),
    {MSt9, NewMatchState} = memory_ensure_free_with_extra_root(
        MatchState, Live, {free, HeapSizeReg}, MMod, MSt8
    ),
    % Restore BSBinaryReg as it may have been gc'd as well
    {MSt10, MatchStateReg0} = MMod:copy_to_native_register(MSt9, NewMatchState),
    MSt11 = MMod:and_(MSt10, MatchStateReg0, ?TERM_PRIMARY_CLEAR_MASK),
    MSt12 = MMod:move_array_element(MSt11, MatchStateReg0, 1, BSBinaryReg),
    MSt13 = MMod:free_native_register(MSt12, MatchStateReg0),
    {MSt14, ResultTerm} = MMod:call_primitive(MSt13, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, jit_state, BSBinaryReg, {free, BSOffseBytesReg}, TailBytesReg
    ]),
    % This is get_tail, we don't need to fix BSBinaryReg by doing an and with ?TERM_PRIMARY_CLEAR_MASK
    {MSt15, Dest, Rest3} = decode_dest(Rest2, MMod, MSt14),
    ?TRACE("~p},", [Dest]),
    MSt16 = MMod:move_to_vm_register(MSt15, ResultTerm, Dest),
    MSt17 = MMod:free_native_register(MSt16, ResultTerm),
    MSt18 = MMod:shift_left(MSt17, TailBytesReg, 3),
    MSt19 = MMod:add(MSt18, BSOffsetReg, TailBytesReg),
    MSt20 = MMod:free_native_register(MSt19, TailBytesReg),
    Labels1 = [{OffsetRef, ContinueOffset} | Labels0],
    State1 = State0#state{labels = Labels1},
    {J0 - 3, Rest3, NewMatchState, MSt20, State1}.

first_pass_bs_match_equal_colon_equal(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0, State0
) ->
    % genot.tab says Live, but compiler always put nil
    Rest1 = decode_nil(Rest0),
    {Size, Rest2} = decode_literal(Rest1),
    {PatternValue, Rest3} = decode_literal(Rest2),
    ?TRACE("{'=:=',[],~p,~p},", [Size, PatternValue]),
    {MSt1, Result} = MMod:call_primitive(MSt0, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, BSBinaryReg, BSOffsetReg, Size, 0
    ]),
    MSt2 = MMod:handle_error_if_zero(MSt1, Result),
    MSt3 = MMod:jump_to_label_if_equal(
        MSt2, Result, ?FALSE_ATOM, Fail
    ),
    MSt4 = MMod:shift_right(MSt3, Result, 4),
    MSt5 = MMod:jump_to_label_if_not_equal(
        MSt4, Result, PatternValue, Fail
    ),
    MSt6 = MMod:add(MSt5, BSOffsetReg, Size),
    MSt7 = MMod:free_native_register(MSt6, Result),
    {J0 - 3, Rest3, MatchState, MSt7, State0}.

first_pass_bs_match_skip(MatchState, BSOffsetReg, J0, Rest0, MMod, MSt0, State0) ->
    {Stride, Rest1} = decode_literal(Rest0),
    MSt1 = MMod:add(MSt0, BSOffsetReg, Stride),
    {J0 - 1, Rest1, MatchState, MSt1, State0}.

term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt0) ->
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_register(MSt1, TrimResultReg),
    % Write Src to x_reg to have it as a gc root
    {MSt3, NewSrc} = memory_ensure_free_with_extra_root(
        Src, Live, ?TERM_BOXED_BIN_MATCH_STATE_SIZE, MMod, MSt2
    ),
    {MSt4, AllocMatchStateReg} = MMod:call_primitive(MSt3, ?PRIM_TERM_ALLOC_BIN_MATCH_STATE, [
        ctx, NewSrc, 0
    ]),
    MSt5 = MMod:move_to_vm_register(MSt4, AllocMatchStateReg, Dest),
    MSt6 = MMod:free_native_register(MSt5, AllocMatchStateReg),
    {MSt6, NewSrc}.

term_is_binary_or_match_state(Src, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Src),
    {MSt2, OffsetRef0, JumpToken0} = MMod:jump_to_offset_if_and_not_equal(
        MSt1, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED
    ),
    MSt3 = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, BoxTagReg} = MMod:get_array_element(MSt3, Reg, 0),
    MSt5 = MMod:and_(MSt4, BoxTagReg, ?TERM_BOXED_TAG_MASK),
    {MSt6, OffsetRef1, JumpToken1} = MMod:jump_to_offset_if_equal(
        MSt5, BoxTagReg, ?TERM_BOXED_REFC_BINARY
    ),
    {MSt7, OffsetRef2, JumpToken2} = MMod:jump_to_offset_if_equal(
        MSt6, BoxTagReg, ?TERM_BOXED_HEAP_BINARY
    ),
    {MSt8, OffsetRef3, JumpToken3} = MMod:jump_to_offset_if_equal(
        MSt7, BoxTagReg, ?TERM_BOXED_SUB_BINARY
    ),
    {MSt9, OffsetRef4, JumpToken4} = MMod:jump_to_offset_if_not_equal(
        MSt8, BoxTagReg, ?TERM_BOXED_BIN_MATCH_STATE
    ),
    {MSt9, Reg, BoxTagReg, [OffsetRef0], [JumpToken0],
        [OffsetRef1, OffsetRef2, OffsetRef3, OffsetRef4], [
            JumpToken1, JumpToken2, JumpToken3, JumpToken4
        ]}.

term_from_catch_label(Dest, Label, MMod, MSt1) ->
    {MSt2, Reg} = MMod:get_module_index(MSt1),
    MSt3 = MMod:shift_left(MSt2, Reg, 24),
    MSt4 = MMod:or_(MSt3, Reg, (Label bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_CATCH),
    MSt5 = MMod:move_to_vm_register(MSt4, Reg, Dest),
    MSt6 = MMod:free_native_register(MSt5, Reg),
    MMod:free_native_register(MSt6, Dest).

term_is_boxed_with_tag(Label, Arg1, BoxedTag, MMod, MSt1) ->
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = MMod:jump_to_label_if_and_not_equal(
        MSt2, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED, Label
    ),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    MSt6 = MMod:jump_to_label_if_and_not_equal(
        MSt5, {free, Reg}, ?TERM_BOXED_TAG_MASK, BoxedTag, Label
    ),
    MSt6.

term_is_immediate_or_boxed(
    Label, Arg1, ImmediateTag, BoxedTag, MMod, MSt1, #state{labels = Labels0} = State0
) ->
    % test term_is_local_port/term_is_integer, etc.
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    {MSt3, OffsetRef, JumpToken} = MMod:jump_to_offset_if_and_equal(
        MSt2, Reg, ?TERM_IMMED_TAG_MASK, ImmediateTag
    ),
    MSt4 = term_is_boxed_with_tag(Label, Arg1, BoxedTag, MMod, MSt3),
    {MSt5, Offset} = MMod:offset(MSt4, [JumpToken]),
    MSt6 = MMod:free_native_register(MSt5, Reg),
    Labels1 = [{OffsetRef, Offset} | Labels0],
    {MSt6, State0#state{labels = Labels1}}.

validate_is_function(MMod, MSt0, Reg, #state{labels = Labels0} = State0) ->
    {MSt1, OffsetRef0, JumpToken0} = MMod:jump_to_offset_if_and_equal(
        MSt0, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED
    ),
    ErrorOffset = MMod:offset(MSt1),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, ?BADFUN_ATOM, Reg
    ]),
    {MSt3, ContinueOffset} = MMod:offset(MSt2, [JumpToken0]),
    MSt4 = MMod:and_(MSt3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt5, BoxTag} = MMod:get_array_element(MSt4, Reg, 0),
    MSt6 = MMod:or_(MSt5, Reg, ?TERM_PRIMARY_BOXED),
    {MSt7, OffsetRef1, _JumpToken1} = MMod:jump_to_offset_if_and_not_equal(
        MSt6, {free, BoxTag}, ?TERM_BOXED_TAG_MASK, ?TERM_BOXED_FUN
    ),
    Labels1 = [{OffsetRef0, ContinueOffset}, {OffsetRef1, ErrorOffset} | Labels0],
    {MSt7, State0#state{labels = Labels1}}.

term_to_int(Term, _MMod, MSt0, State0) when is_integer(Term) ->
    {MSt0, Term bsl 4, State0};
term_to_int({literal, Val}, _MMod, MSt0, State0) when is_integer(Val) ->
    {MSt0, Val, State0};
term_to_int(Term, MMod, MSt0, #state{labels = Labels0} = State0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    {MSt2, OffsetRef0, JumpToken0} = MMod:jump_to_offset_if_and_equal(
        MSt1, Reg, ?TERM_IMMED_TAG_MASK, ?TERM_INTEGER_TAG
    ),
    MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, ?BADARG_ATOM
    ]),
    {MSt4, ContinueOffset} = MMod:offset(MSt3, [JumpToken0]),
    MSt5 = MMod:shift_right(MSt4, Reg, 4),
    Labels1 = [{OffsetRef0, ContinueOffset} | Labels0],
    {MSt5, Reg, State0#state{labels = Labels1}}.

first_pass_float3(Primitive, Rest0, MMod, MSt0, #state{labels = Labels0} = State0) ->
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
            MSt2 = MMod:jump_to_label_if_false(MSt1, Reg, Label),
            MSt3 = MMod:free_native_register(MSt2, Reg),
            first_pass(Rest4, MMod, MSt3, State0);
        true ->
            {MSt2, OffsetRef0, JumpToken0} = MMod:jump_to_offset_if_true(MSt1, Reg),
            MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_RAISE_ERROR, [
                ctx, jit_state, ?BADARITH_ATOM
            ]),
            {MSt4, Offset0} = MMod:offset(MSt3, [JumpToken0]),
            MSt5 = MMod:free_native_register(MSt4, Reg),
            Labels1 = [{OffsetRef0, Offset0} | Labels0],
            first_pass(Rest4, MMod, MSt5, State0#state{labels = Labels1})
    end.

bif_faillabel_test(FailLabel, MMod, MSt0, {free, ResultReg}, {free, Dest}) when FailLabel > 0 ->
    MSt1 = MMod:jump_to_label_if_zero(MSt0, ResultReg, FailLabel),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, Dest),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    MMod:free_native_register(MSt3, Dest);
bif_faillabel_test(0, MMod, MSt0, {free, ResultReg}, {free, Dest}) ->
    MSt1 = MMod:handle_error_if_zero(MSt0, ResultReg),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, Dest),
    MSt3 = MMod:free_native_register(MSt2, ResultReg),
    MMod:free_native_register(MSt3, Dest).

memory_ensure_free_with_extra_root(ExtraRoot, Live, Size, MMod, MSt0) ->
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
    MSt3 = MMod:handle_error_if_false(MSt2, MemoryEnsureFreeReg),
    MSt4 = MMod:free_native_register(MSt3, MemoryEnsureFreeReg),
    {MSt4, ExtraRootXReg}.

second_pass(MMod, MSt, #state{labels = Labels}) ->
    MMod:update_branches(MSt, Labels).

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

decode_compile_time_literal(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>, _State) ->
    {[], Rest};
decode_compile_time_literal(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>, #state{
    literal_resolver = Resolver
}) ->
    {LiteralIndex, Rest1} = decode_literal(Rest0),
    LiteralTerm = Resolver(LiteralIndex),
    {LiteralTerm, Rest1}.

decode_flags_list(L) ->
    decode_flags_list0(L, 0).

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
            ?TRACE("get_atom_term_by_id(~p) => ~p\n", [AtomIndex, Reg]),
            {MSt1, Reg, Rest};
        {ok, DefaultAtomIndex} ->
            {MSt0, DefaultAtomIndex, Rest}
    end.

decode_compact_term_module_literal(LiteralIndex, MMod, MSt0, Rest) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_MODULE_LOAD_LITERAL, [ctx, jit_state, LiteralIndex]
    ),
    ?TRACE("module_load_literal(~p) => ~p\n", [LiteralIndex, Reg]),
    {MSt1, Reg, Rest}.

decode_compact_term_integer(Value, _MMod, MSt, Rest) when
    Value >= ?INT32_MIN andalso Value =< ?INT32_MAX
->
    {MSt, term_from_int(Value), Rest};
decode_compact_term_integer(Value, MMod, MSt0, Rest) when
    Value >= (?INT64_MIN bsr 4) andalso Value =< (?INT64_MAX bsr 4)
->
    case MMod:word_size() of
        4 ->
            {MSt1, Reg} = MMod:call_primitive(
                MSt0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [ctx, Value]
            ),
            ?TRACE("alloc_boxed_integer_fragment(~p) => ~p\n", [Value, Reg]),
            {MSt1, Reg, Rest};
        8 ->
            {MSt0, term_from_int(Value), Rest}
    end;
decode_compact_term_integer(Value, MMod, MSt0, Rest) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [ctx, Value]
    ),
    ?TRACE("alloc_boxed_integer_fragment(~p) => ~p\n", [Value, Reg]),
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
            ?TRACE("extended_register_ptr(~p) => ~p\n", [RegIndex, Reg]),
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
