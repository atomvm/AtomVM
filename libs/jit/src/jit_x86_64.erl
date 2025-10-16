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

-module(jit_x86_64).

-export([
    word_size/0,
    new/3,
    stream/1,
    offset/1,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/1,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    return_if_not_equal_to_ctx/2,
    jump_to_label/2,
    if_block/3,
    if_else_block/4,
    shift_right/3,
    shift_left/3,
    move_to_vm_register/3,
    move_to_native_register/2,
    move_to_native_register/3,
    move_to_cp/2,
    move_array_element/4,
    move_to_array_element/4,
    move_to_array_element/5,
    set_bs/2,
    copy_to_native_register/2,
    get_array_element/3,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    continuation_entry_point/1,
    get_module_index/1,
    and_/3,
    or_/3,
    add/3,
    sub/3,
    mul/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3
]).

-include_lib("jit.hrl").

-include("primitives.hrl").

-define(ASSERT(Expr), true = Expr).

%% System V X86_64 calling conventions which we apply here.
%% (Integer) parameters : rdi, rsi, rdx, rcx, r8, r9
%% (Integer) result : rax
%%
%% Function is called as (Context *, JITState *, ModuleNativeInterface *) so:
%% Context * is rdi
%% JITState * is rsi
%% ModuleNativeInterface * is rdx
%%
%% rax, r11, r10, r9, r8 and rcx can be used as scratch registers.
%% rdi / rsi / rdx are pushed to stack before calling a primitive and popped back.
%% when returning (some push call pop push call pop sequences could be optimized)

-type x86_64_register() ::
    rax
    | rcx
    | rdx
    | rsi
    | rdi
    | r8
    | r9
    | r10
    | r11.

-define(IS_GPR(Reg),
    (Reg =:= rax orelse Reg =:= rcx orelse Reg =:= rdx orelse Reg =:= rsi orelse Reg =:= r8 orelse
        Reg =:= r9 orelse Reg =:= r10 orelse Reg =:= r11)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    available_regs :: [x86_64_register()],
    used_regs :: [x86_64_register()],
    labels :: [{integer() | reference(), integer()}]
}).

-type state() :: #state{}.
-type vm_register() ::
    {x_reg, non_neg_integer()}
    | {x_reg, extra}
    | {y_reg, non_neg_integer()}
    | {ptr, x86_64_register()}
    | {fp_reg, non_neg_integer()}.
-type value() :: integer() | vm_register() | x86_64_register() | {ptr, x86_64_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_x86_64_register() :: x86_64_register() | {free, x86_64_register()}.

-type condition() ::
    {x86_64_register(), '<', integer()}
    | {maybe_free_x86_64_register(), '<', x86_64_register()}
    | {maybe_free_x86_64_register(), '==', integer()}
    | {maybe_free_x86_64_register(), '!=', x86_64_register() | integer()}
    | {'(int)', maybe_free_x86_64_register(), '==', integer()}
    | {'(int)', maybe_free_x86_64_register(), '!=', x86_64_register() | integer()}
    | {'(bool)', maybe_free_x86_64_register(), '==', false}
    | {'(bool)', maybe_free_x86_64_register(), '!=', false}
    | {maybe_free_x86_64_register(), '&', non_neg_integer(), '!=', integer()}.

-define(WORD_SIZE, 8).

% Following offsets are verified with static asserts in jit.c
% ctx->e is 0x28
% ctx->x is 0x30
% ctx->cp is 0xB8
% ctx->fr is 0xC0
% ctx->bs is 0xC8
% ctx->bs_offset is 0xD0
-define(CTX_REG, rdi).
-define(JITSTATE_REG, rsi).
-define(NATIVE_INTERFACE_REG, rdx).
-define(Y_REGS, {16#28, ?CTX_REG}).
-define(X_REG(N), {16#30 + (N * ?WORD_SIZE), ?CTX_REG}).
-define(CP, {16#B8, ?CTX_REG}).
-define(FP_REGS, {16#C0, ?CTX_REG}).
-define(BS, {16#C8, ?CTX_REG}).
-define(BS_OFFSET, {16#D0, ?CTX_REG}).
-define(JITSTATE_MODULE, {0, ?JITSTATE_REG}).
-define(JITSTATE_CONTINUATION, {16#8, ?JITSTATE_REG}).
-define(JITSTATE_REMAINING_REDUCTIONS, {16#10, ?JITSTATE_REG}).
-define(PRIMITIVE(N), {N * ?WORD_SIZE, ?NATIVE_INTERFACE_REG}).
-define(MODULE_INDEX(ModuleReg), {0, ModuleReg}).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-define(AVAILABLE_REGS, [rax, r11, r10, r9, r8, rcx]).
-define(PARAMETER_REGS, [rdi, rsi, rdx, rcx, r8, r9]).
-define(SCRATCH_REGS, [rdi, rsi, rdx, rcx, r8, r9, r10, r11]).

%%-----------------------------------------------------------------------------
%% @doc Return the word size in bytes, i.e. the sizeof(term) i.e.
%% sizeof(uintptr_t)
%%
%% C code equivalent is:
%% #if UINTPTR_MAX == UINT32_MAX
%%    #define TERM_BYTES 4
%% #elif UINTPTR_MAX == UINT64_MAX
%%    #define TERM_BYTES 8
%% #else
%%    #error "Term size must be either 32 bit or 64 bit."
%% #endif
%%
%% @end
%% @return Word size in bytes
%%-----------------------------------------------------------------------------
-spec word_size() -> 4 | 8.
word_size() -> ?WORD_SIZE.

%%-----------------------------------------------------------------------------
%% @doc Create a new backend state for provided variant, module and stream.
%% @end
%% @param Variant JIT variant to use (currently ?JIT_VARIANT_PIC)
%% @param StreamModule module to stream instructions
%% @param Stream stream state
%% @return New backend state
%%-----------------------------------------------------------------------------
-spec new(any(), module(), stream()) -> state().
new(_Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS,
        used_regs = [],
        labels = []
    }.

%%-----------------------------------------------------------------------------
%% @doc Access the stream object.
%% @end
%% @param State current backend state
%% @return The stream object
%%-----------------------------------------------------------------------------
-spec stream(state()) -> stream().
stream(#state{stream = Stream}) ->
    Stream.

%%-----------------------------------------------------------------------------
%% @doc Get the current offset in the stream
%% @end
%% @param State current backend state
%% @return The current offset
%%-----------------------------------------------------------------------------
-spec offset(state()) -> non_neg_integer().
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

%%-----------------------------------------------------------------------------
%% @doc Emit a debugger of breakpoint instruction. This is used for debugging
%% and not in production.
%% @end
%% @param State current backend state
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec debugger(state()) -> state().
debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:append(Stream0, <<16#CC>>),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [x86_64_register()].
used_regs(#state{used_regs = Used}) -> Used.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [x86_64_register()].
available_regs(#state{available_regs = Available}) -> Available.

%%-----------------------------------------------------------------------------
%% @doc Free native registers. The passed list of registers can contain
%% registers, pointer to registers or other values that are ignored.
%% @end
%% @param State current backend state
%% @param Regs list of registers or other values
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Reg | Rest]) ->
    State1 = free_native_register(State, Reg),
    free_native_registers(State1, Rest).

-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_regs = Available0, used_regs = Used0} = State,
    Reg
) when
    is_atom(Reg)
->
    {Available1, Used1} = free_reg(Available0, Used0, Reg),
    State#state{available_regs = Available1, used_regs = Used1};
free_native_register(State, {ptr, Reg}) ->
    free_native_register(State, Reg);
free_native_register(State, _Other) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Assert that all native scratch registers are available. This is used
%% for debugging and not in production.
%% @end
%% @param State current backend state
%% @return ok
%%-----------------------------------------------------------------------------
-spec assert_all_native_free(state()) -> ok.
assert_all_native_free(State) ->
    [] = State#state.used_regs,
    ?AVAILABLE_REGS = State#state.available_regs,
    ok.

%%-----------------------------------------------------------------------------
%% @doc Emit the jump table at the beginning of the module. Branches will be
%% updated afterwards with update_branches/2. Emit branches for labels from
%% 0 (special entry for lines and labels information) to LabelsCount included
%% (special entry for OP_INT_CALL_END).
%% @end
%% @param State current backend state
%% @param LabelsCount number of labels in the module.
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec jump_table(state(), pos_integer()) -> state().
jump_table(State, LabelsCount) ->
    jump_table0(State, 0, LabelsCount).

jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State,
    N,
    LabelsCount
) ->
    Offset = StreamModule:offset(Stream0),
    {RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(1),
    Reloc = {N, Offset + RelocOffset, 32},
    Stream1 = StreamModule:append(Stream0, I1),
    jump_table0(State#state{stream = Stream1, branches = [Reloc | Branches]}, N + 1, LabelsCount).

%%-----------------------------------------------------------------------------
%% @doc Rewrite stream to update all branches for labels.
%% @end
%% @param State current backend state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec update_branches(state()) -> state().
update_branches(#state{branches = []} = State) ->
    State;
update_branches(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = [{Label, Offset, Size} | BranchesT],
        labels = Labels
    } = State
) ->
    {Label, LabelOffset} = lists:keyfind(Label, 1, Labels),
    Stream1 = StreamModule:map(Stream0, Offset, Size div 8, fun(<<Delta:Size/signed-little>>) ->
        <<(Delta + LabelOffset - Offset):Size/little>>
    end),
    update_branches(State#state{stream = Stream1, branches = BranchesT}).

%%-----------------------------------------------------------------------------
%% @doc Emit a call (call with return) to a primitive with arguments. This
%% function converts arguments and pass them following the backend ABI
%% convention. It also saves scratch registers we need to preserve.
%% @end
%% @param State current backend state
%% @param Primitive index to the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), x86_64_register()}.
call_primitive(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs
    } = State,
    Primitive,
    Args
) ->
    % We need a register for the function pointer that should not be used as a parameter
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    case AvailableRegs0 -- ParamRegs of
        [Temp | _] ->
            AvailableRegs1 = AvailableRegs0 -- [Temp],
            PrepCall =
                case Primitive of
                    0 ->
                        jit_x86_64_asm:movq({0, ?NATIVE_INTERFACE_REG}, Temp);
                    N ->
                        jit_x86_64_asm:movq(?PRIMITIVE(N), Temp)
                end,
            Stream1 = StreamModule:append(Stream0, PrepCall),
            call_func_ptr(
                State#state{
                    stream = Stream1, available_regs = AvailableRegs1, used_regs = [Temp | UsedRegs]
                },
                {free, Temp},
                Args
            );
        [] ->
            % No register left, we'll use the stack to save NATIVE_INTERFACE_REG
            % and rax when calling function.
            call_func_ptr(State, {primitive, Primitive}, Args)
    end.

%%-----------------------------------------------------------------------------
%% @doc Emit a jump (call without return) to a primitive with arguments. This
%% function converts arguments and pass them following the backend ABI
%% convention.
%% @end
%% @param State current backend state
%% @param Primitive index to the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
call_primitive_last(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    Primitive,
    Args
) ->
    % We need a register for the function pointer that should not be used as a parameter
    % Since we're not returning, we can use all scratch registers except
    % registers used for parameters
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    ArgsRegs = args_regs(Args),
    ScratchRegs = ?AVAILABLE_REGS -- ArgsRegs -- ParamRegs,
    [Temp | AvailableRegs1] = ScratchRegs,
    UsedRegs = ?AVAILABLE_REGS -- AvailableRegs1,
    PrepCall =
        case Primitive of
            0 ->
                jit_x86_64_asm:movq({0, ?NATIVE_INTERFACE_REG}, Temp);
            N ->
                jit_x86_64_asm:movq(?PRIMITIVE(N), Temp)
        end,
    Stream1 = StreamModule:append(Stream0, PrepCall),
    State1 = set_args(
        State0#state{
            stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs
        },
        Args
    ),
    #state{stream = Stream2} = State1,
    Call = jit_x86_64_asm:jmpq({Temp}),
    Stream3 = StreamModule:append(Stream2, Call),
    State1#state{stream = Stream3, available_regs = ?AVAILABLE_REGS, used_regs = []}.

%%-----------------------------------------------------------------------------
%% @doc Emit a return of a value if it's not equal to ctx.
%% This logic is used to break out to the scheduler, typically after signal
%% messages have been processed.
%% @end
%% @param State current backend state
%% @param Reg register to compare to (should be {free, Reg} as it's always freed)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
return_if_not_equal_to_ctx(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    {free, Reg}
) ->
    I1 = jit_x86_64_asm:cmpq(?CTX_REG, Reg),
    I3 =
        case Reg of
            rax -> <<>>;
            _ -> jit_x86_64_asm:movq(Reg, rax)
        end,
    I4 = jit_x86_64_asm:retq(),
    I2 = jit_x86_64_asm:jz(byte_size(I3) + byte_size(I4) + 2),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, Reg),
    State#state{
        stream = Stream1,
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a jump to a label. The offset of the relocation is saved and will
%% be updated with `update_branches/2`.
%% @end
%% @param State current backend state
%% @param Label to jump to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches, labels = Labels} =
        State,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    case lists:keyfind(Label, 1, Labels) of
        {Label, LabelOffset} ->
            % Label is already known, emit direct branch without relocation

            % Calculate relative offset (assembler will adjust for instruction size)
            RelOffset = LabelOffset - Offset,
            I1 = jit_x86_64_asm:jmp(RelOffset),
            Stream1 = StreamModule:append(Stream0, I1),
            State#state{stream = Stream1};
        false ->
            % Label not yet known, emit placeholder and add relocation
            {RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(1),
            Reloc = {Label, Offset + RelocOffset, 32},
            Stream1 = StreamModule:append(Stream0, I1),
            State#state{stream = Stream1, branches = [Reloc | AccBranches]}
    end.

%%-----------------------------------------------------------------------------
%% @doc Emit an if block, i.e. emit a test of a condition and conditionnally
%% execute a block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockFn function to emit the block that may be executed
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec if_block(state(), condition() | {'and', [condition()]}, fun((state()) -> state())) -> state().
if_block(
    #state{stream_module = StreamModule} = State0,
    {'and', CondList},
    BlockFn
) ->
    {Replacements, State1} = lists:foldl(
        fun(Cond, {AccReplacements, AccState}) ->
            Offset = StreamModule:offset(AccState#state.stream),
            {NewAccState, ReplaceDelta} = if_block_cond(AccState, Cond),
            OffsetAfterCond = StreamModule:offset(NewAccState#state.stream),
            {[{Offset + ReplaceDelta, OffsetAfterCond} | AccReplacements], NewAccState}
        end,
        {[], State0},
        CondList
    ),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({ReplacementOffset, OffsetAfterCond}, AccStream) ->
            ?ASSERT(OffsetAfter - OffsetAfterCond < 16#80),
            StreamModule:replace(AccStream, ReplacementOffset, <<
                (OffsetAfter - OffsetAfterCond)
            >>)
        end,
        Stream2,
        Replacements
    ),
    merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs);
if_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, ReplaceDelta} = if_block_cond(State0, Cond),
    OffsetAfterCond = StreamModule:offset(State1#state.stream),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    ?ASSERT(OffsetAfter - OffsetAfterCond < 16#80),
    Stream3 = StreamModule:replace(Stream2, Offset + ReplaceDelta, <<
        (OffsetAfter - OffsetAfterCond)
    >>),
    merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs).

%%-----------------------------------------------------------------------------
%% @doc Emit an if else block, i.e. emit a test of a condition and
%% conditionnally execute a block or another block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockTrueFn function to emit the block that is executed if condition is true
%% @param BlockFalseFn function to emit the block that is executed if condition is false
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec if_else_block(state(), condition(), fun((state()) -> state()), fun((state()) -> state())) ->
    state().
if_else_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockTrueFn,
    BlockFalseFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, ReplaceDelta} = if_block_cond(State0, Cond),
    OffsetAfterCond = StreamModule:offset(State1#state.stream),
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    ElseJumpOffset = StreamModule:offset(Stream2),
    {RelocJMPOffset, I} = jit_x86_64_asm:jmp_rel8(1),
    Stream3 = StreamModule:append(Stream2, I),
    OffsetAfter = StreamModule:offset(Stream3),
    ?ASSERT(OffsetAfter - OffsetAfterCond < 16#80),
    Stream4 = StreamModule:replace(Stream3, Offset + ReplaceDelta, <<
        (OffsetAfter - OffsetAfterCond)
    >>),
    StateElse = State2#state{
        stream = Stream4,
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset + RelocJMPOffset, <<
        (OffsetFinal - OffsetAfter)
    >>),
    merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs).

-spec if_block_cond(state(), condition()) -> {state(), non_neg_integer()}.
if_block_cond(#state{stream_module = StreamModule} = State0, Cond) ->
    {State1, Code, ReplaceDelta} = if_block_cond0(State0, Cond),
    Stream1 = StreamModule:append(State1#state.stream, Code),
    State2 = State1#state{stream = Stream1},
    {State2, ReplaceDelta}.

-spec if_block_cond0(state(), condition()) -> {state(), binary(), non_neg_integer()}.
if_block_cond0(State0, {Reg, '<', 0}) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocJGEOffset, I2} = jit_x86_64_asm:jge_rel8(1),
    {State0, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJGEOffset};
if_block_cond0(State0, {RegOrTuple, '<', Value}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Value, Reg),
    {RelocJGEOffset, I2} = jit_x86_64_asm:jge_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJGEOffset};
if_block_cond0(State0, {RegOrTuple, '==', 0}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJNZOffset};
if_block_cond0(State0, {'(int)', RegOrTuple, '==', 0}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testl(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJNZOffset};
if_block_cond0(
    State0,
    {RegOrTuple, '!=', Val}
) when ?IS_SINT32_T(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    #state{available_regs = [Temp | _]} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:movabsq(Val, Temp),
    I2 = jit_x86_64_asm:cmpq(Temp, Reg),
    {RelocJZOffset, I3} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJZOffset};
if_block_cond0(
    State0,
    {'(int)', RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpl(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    State0,
    {RegOrTuple, '==', Val}
) when ?IS_SINT32_T(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    #state{available_regs = [Temp | _]} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:movabsq(Val, Temp),
    I2 = jit_x86_64_asm:cmpq(Temp, Reg),
    {RelocJZOffset, I3} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJZOffset};
if_block_cond0(
    State0,
    {'(int)', RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpl(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJNZOffset};
if_block_cond0(
    State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Reg, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(State0, {RegOrTuple, '&', Mask, '!=', 0}) when ?IS_UINT8_T(Mask) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Mask, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(State0, {{free, Reg} = RegTuple, '&', Mask, '!=', Val}) when ?IS_UINT8_T(Mask) ->
    I1 = jit_x86_64_asm:andb(Mask, Reg),
    I2 = jit_x86_64_asm:cmpb(Val, Reg),
    {RelocJZOffset, I3} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegTuple, State0),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJZOffset};
if_block_cond0(State0, {Reg, '&', Mask, '!=', Val}) when ?IS_UINT8_T(Mask) ->
    Temp = hd(State0#state.available_regs),
    I1 = jit_x86_64_asm:movq(Reg, Temp),
    I2 = jit_x86_64_asm:andb(Mask, Temp),
    I3 = jit_x86_64_asm:cmpb(Val, Temp),
    {RelocJZOffset, I4} = jit_x86_64_asm:jz_rel8(1),
    {State0, <<I1/binary, I2/binary, I3/binary, I4/binary>>,
        byte_size(I1) + byte_size(I2) + byte_size(I3) + RelocJZOffset}.

-spec if_block_free_reg(x86_64_register() | {free, x86_64_register()}, state()) -> state().
if_block_free_reg({free, Reg}, State0) ->
    #state{available_regs = AvR0, used_regs = UR0} = State0,
    {AvR1, UR1} = free_reg(AvR0, UR0, Reg),
    State0#state{
        available_regs = AvR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

-spec merge_used_regs(state(), [x86_64_register()]) -> state().
merge_used_regs(#state{used_regs = UR0, available_regs = AvR0} = State, [
    Reg | T
]) ->
    case lists:member(Reg, UR0) of
        true ->
            merge_used_regs(State, T);
        false ->
            AvR1 = lists:delete(Reg, AvR0),
            UR1 = [Reg | UR0],
            merge_used_regs(
                State#state{used_regs = UR1, available_regs = AvR1}, T
            )
    end;
merge_used_regs(State, []) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register right by a fixed number of bits, effectively
%% dividing it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
shift_right(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_x86_64_asm:shrq(Shift, Reg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register left by a fixed number of bits, effectively
%% multiplying it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
shift_left(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    is_atom(Reg)
->
    I = jit_x86_64_asm:shlq(Shift, Reg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a function pointer with arguments. This function converts
%% arguments and passes them following the backend ABI convention.
%% @end
%% @param State current backend state
%% @param FuncPtrTuple either {free, Reg} or {primitive, PrimitiveIndex}
%% @param Args arguments to pass to the function
%% @return Updated backend state and return register
%%-----------------------------------------------------------------------------
-spec call_func_ptr(state(), {free, x86_64_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), x86_64_register()}.
call_func_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State0,
    FuncPtrTuple,
    Args
) ->
    FreeRegs = lists:flatmap(
        fun
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [FuncPtrTuple | Args]
    ),
    UsedRegs1 = UsedRegs0 -- FreeRegs,
    SavedRegs = [?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | UsedRegs1],
    Stream1 = lists:foldl(
        fun(Reg, AccStream) ->
            StreamModule:append(AccStream, jit_x86_64_asm:pushq(Reg))
        end,
        Stream0,
        SavedRegs
    ),
    PushOdds = length(SavedRegs) rem 2,
    Stream3 =
        case FuncPtrTuple of
            {free, _} ->
                Stream1;
            {primitive, Primitive} ->
                PrepCall0 =
                    case Primitive of
                        0 ->
                            jit_x86_64_asm:movq({0, ?NATIVE_INTERFACE_REG}, ?NATIVE_INTERFACE_REG);
                        N ->
                            jit_x86_64_asm:movq(?PRIMITIVE(N), ?NATIVE_INTERFACE_REG)
                    end,
                PrepCall1 = jit_x86_64_asm:pushq(?NATIVE_INTERFACE_REG),
                StreamModule:append(Stream1, <<PrepCall0/binary, PrepCall1/binary>>)
        end,
    % x86 64 stack should be aligned to 16 bytes when running callq instruction
    % It is therefore unaligned and we need to always push an odd number of
    % registers.
    % Align stack by pushing ?NATIVE_INTERFACE_REG
    % ?NATIVE_INTERFACE_REG may have been pushed as the function pointer
    Stream4 =
        case PushOdds of
            1 ->
                Stream3;
            0 ->
                PrepCall2 = jit_x86_64_asm:pushq(?NATIVE_INTERFACE_REG),
                StreamModule:append(Stream3, PrepCall2)
        end,
    State1 = set_args(State0#state{stream = Stream4}, Args),
    #state{stream = Stream5} = State1,
    Call =
        case FuncPtrTuple of
            {free, FuncPtrReg} ->
                jit_x86_64_asm:callq({FuncPtrReg});
            {primitive, _} ->
                Call0 = jit_x86_64_asm:popq(rax),
                Call1 = jit_x86_64_asm:callq({rax}),
                <<Call0/binary, Call1/binary>>
        end,
    Stream6 = StreamModule:append(Stream5, Call),
    % Unalign stack
    Stream7 =
        case PushOdds of
            1 ->
                Stream6;
            0 ->
                PostCall1 = jit_x86_64_asm:popq(?NATIVE_INTERFACE_REG),
                StreamModule:append(Stream6, PostCall1)
        end,
    % If rax is in used regs, save it to another temporary register
    AvailableRegs1 = FreeRegs ++ AvailableRegs0,
    {Stream8, ResultReg} =
        case lists:member(rax, SavedRegs) of
            true ->
                [Temp | _] = AvailableRegs1,
                {StreamModule:append(Stream7, jit_x86_64_asm:movq(rax, Temp)), Temp};
            false ->
                {Stream7, rax}
        end,
    Stream9 = lists:foldl(
        fun(Reg, AccStream) ->
            StreamModule:append(AccStream, jit_x86_64_asm:popq(Reg))
        end,
        Stream8,
        lists:reverse(SavedRegs)
    ),
    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    UsedRegs2 = [ResultReg | UsedRegs1],
    {
        State1#state{
            stream = Stream9,
            available_regs = AvailableRegs3,
            used_regs = UsedRegs2
        },
        ResultReg
    }.

-spec set_args(state(), [arg()]) -> state().
set_args(
    #state{stream = Stream0, stream_module = StreamModule, used_regs = UsedRegs} = State0, Args
) ->
    ParamRegs = parameter_regs(Args),
    ArgsRegs = args_regs(Args),
    AvailableScratchGP =
        ?SCRATCH_REGS -- ParamRegs -- ArgsRegs -- UsedRegs,
    Offset = StreamModule:offset(Stream0),
    Args1 = [
        case Arg of
            offset -> Offset;
            _ -> Arg
        end
     || Arg <- Args
    ],
    SetArgsCode = set_args0(Args1, ArgsRegs, ParamRegs, AvailableScratchGP, []),
    Stream1 = StreamModule:append(Stream0, SetArgsCode),
    NewUsedRegs = lists:foldl(
        fun
            ({free, {ptr, Reg}}, AccUsed) -> lists:delete(Reg, AccUsed);
            ({free, Reg}, AccUsed) -> lists:delete(Reg, AccUsed);
            (_, AccUsed) -> AccUsed
        end,
        UsedRegs,
        Args
    ),
    State0#state{
        stream = Stream1,
        available_regs = ?AVAILABLE_REGS -- ParamRegs -- NewUsedRegs,
        used_regs = ParamRegs ++ (NewUsedRegs -- ParamRegs)
    }.

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

parameter_regs0([], _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([Special | T], [GPReg | GPRegsT], Acc) when
    Special =:= ctx orelse Special =:= jit_state orelse Special =:= offset
->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{free, Free} | T], GPRegs, Acc) ->
    parameter_regs0([Free | T], GPRegs, Acc);
parameter_regs0([{ptr, Reg} | T], [GPReg | GPRegsT], Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([Reg | T], [GPReg | GPRegsT], Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{x_reg, _} | T], [GPReg | GPRegsT], Acc) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{y_reg, _} | T], [GPReg | GPRegsT], Acc) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([Int | T], [GPReg | GPRegsT], Acc) when is_integer(Int) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [GPReg | GPRegsT], Acc) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]).

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

% Exchange registers in both Args and ArgsRegs lists
exchange_reg(Args, ArgsRegs, Reg1, Reg2) ->
    NewArgs = replace_reg(Args, Reg1, Reg2),
    NewArgsRegs = lists:map(
        fun
            (R) when R =:= Reg1 -> Reg2;
            (R) -> R
        end,
        ArgsRegs
    ),
    {NewArgs, NewArgsRegs}.

set_args0([], [], [], _AvailGP, Acc) ->
    list_to_binary(lists:reverse(Acc));
set_args0([{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, Acc) ->
    set_args0([FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, Acc);
set_args0([ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, Acc) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, Acc);
set_args0(
    [jit_state | ArgsT],
    [?JITSTATE_REG | ArgsRegs],
    [?JITSTATE_REG | ParamRegs],
    AvailGP,
    Acc
) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, Acc);
set_args0(
    [jit_state | ArgsT], [?JITSTATE_REG | ArgsRegs], [ParamReg | ParamRegs], AvailGP, Acc
) ->
    false = lists:member(ParamReg, ArgsRegs),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, [
        jit_x86_64_asm:movq(?JITSTATE_REG, ParamReg) | Acc
    ]);
% ctx is special as we need it to access x_reg/y_reg/fp_reg
set_args0([Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, Acc) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    J = set_args1(Arg, ?CTX_REG),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, [J | Acc]);
set_args0(
    [Arg | ArgsT],
    [ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    AvailGP,
    Acc
) ->
    case lists:member(ParamReg, ArgsRegs) of
        false ->
            % Normal case: ParamReg is free, just move Arg to ParamReg
            J = set_args1(Arg, ParamReg),
            set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, [J | Acc]);
        true ->
            % ParamReg is occupied by another argument that will go elsewhere
            % Use xchg to swap ArgReg and ParamReg
            % After xchg, the value from Arg (which was in ArgReg) is now in ParamReg
            I = jit_x86_64_asm:xchgq(ArgReg, ParamReg),
            {NewArgsT, NewArgsRegs} = exchange_reg(ArgsT, ArgsRegs, ParamReg, ArgReg),
            set_args0(NewArgsT, NewArgsRegs, ParamRegs, AvailGP, [I | Acc])
    end.

set_args1(Reg, Reg) ->
    [];
set_args1({x_reg, extra}, Reg) ->
    jit_x86_64_asm:movq(?X_REG(?MAX_REG), Reg);
set_args1({x_reg, X}, Reg) ->
    jit_x86_64_asm:movq(?X_REG(X), Reg);
set_args1({ptr, Source}, Reg) ->
    jit_x86_64_asm:movq({0, Source}, Reg);
set_args1({y_reg, X}, Reg) ->
    [
        jit_x86_64_asm:movq(?Y_REGS, Reg),
        jit_x86_64_asm:movq({X * 8, Reg}, Reg)
    ];
set_args1(ArgReg, Reg) when ?IS_GPR(ArgReg) ->
    jit_x86_64_asm:movq(ArgReg, Reg);
set_args1(Arg, Reg) when is_integer(Arg) andalso Arg >= -16#80000000 andalso Arg < 16#80000000 ->
    jit_x86_64_asm:movq(Arg, Reg);
set_args1(Arg, Reg) when is_integer(Arg) ->
    jit_x86_64_asm:movabsq(Arg, Reg);
set_args1({avm_int64_t, Value}, Reg) when is_integer(Value) ->
    jit_x86_64_asm:movabsq(Value, Reg).

%%-----------------------------------------------------------------------------
%% @doc Emit a move to a vm register (x_reg, y_reg, fpreg or a pointer on x_reg)
%% from an immediate, a native register or another vm register.
%% @end
%% @param State current backend state
%% @param Src value to move to vm register
%% @param Dest vm register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_vm_register
    (state(), Src :: value() | vm_register(), Dest :: vm_register()) -> state();
    (state(), Src :: {free, {ptr, x86_64_register(), 1}}, Dest :: {fp_reg, non_neg_integer()}) ->
        state().
% Src = 0, we can andq as an optimization
move_to_vm_register(State, 0, {x_reg, X}) when X < ?MAX_REG ->
    I1 = jit_x86_64_asm:andq(0, ?X_REG(X)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register(State, 0, {x_reg, extra}) ->
    I1 = jit_x86_64_asm:andq(0, ?X_REG(?MAX_REG)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register(State, 0, {ptr, Reg}) ->
    I1 = jit_x86_64_asm:andq(0, {0, Reg}),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State, 0, {y_reg, Y}) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:andq(0, {Y * 8, Temp}),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
% ?IS_SINT32_T(Src), we can use movq to set the value
move_to_vm_register(State, N, {x_reg, X}) when X < ?MAX_REG andalso ?IS_SINT32_T(N) ->
    Stream1 = (State#state.stream_module):append(
        State#state.stream, jit_x86_64_asm:movq(N, ?X_REG(X))
    ),
    State#state{stream = Stream1};
move_to_vm_register(State, N, {x_reg, extra}) when ?IS_SINT32_T(N) ->
    Stream1 = (State#state.stream_module):append(
        State#state.stream, jit_x86_64_asm:movq(N, ?X_REG(?MAX_REG))
    ),
    State#state{stream = Stream1};
move_to_vm_register(State, N, {ptr, Reg}) when ?IS_SINT32_T(N) ->
    Stream1 = (State#state.stream_module):append(
        State#state.stream, jit_x86_64_asm:movq(N, {0, Reg})
    ),
    State#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State, N, {y_reg, Y}) when
    ?IS_SINT32_T(N)
->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq(N, {Y * 8, Temp}),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
% ?is_integer(Src), we need to use movabsq
move_to_vm_register(#state{available_regs = [Temp | _]} = State, N, {x_reg, X}) when
    X < ?MAX_REG andalso is_integer(N)
->
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State, N, {x_reg, extra}) when
    is_integer(N)
->
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(?MAX_REG)),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State, N, {ptr, Reg}) when
    is_integer(N)
->
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {0, Reg}),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp1, Temp2 | _]} = State, N, {y_reg, Y}) when
    is_integer(N)
->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movabsq(N, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Y * 8, Temp1}),
    Stream1 = (State#state.stream_module):append(
        State#state.stream, <<I1/binary, I2/binary, I3/binary>>
    ),
    State#state{stream = Stream1};
% is_atom(Src) (native register)
move_to_vm_register(State, Reg, {x_reg, X}) when is_atom(Reg) andalso X < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq(Reg, ?X_REG(X)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register(State, Reg, {x_reg, extra}) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(Reg, ?X_REG(?MAX_REG)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register(State, Reg, {ptr, Dest}) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(Reg, {0, Dest}),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State, Reg, {y_reg, Y}) when
    is_atom(Reg)
->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq(Reg, {Y * 8, Temp}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = (State#state.stream_module):append(State#state.stream, Code),
    State#state{stream = Stream1};
% Src is x_reg, store in temporary register and call move_to_vm_register for the four cases
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, X}, Dest) when
    X < ?MAX_REG
->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, extra}, Dest) ->
    I1 = jit_x86_64_asm:movq(?X_REG(?MAX_REG), Temp),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {ptr, Reg}, Dest) ->
    I1 = jit_x86_64_asm:movq({0, Reg}, Temp),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {y_reg, Y}, Dest) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
% term_to_float
move_to_vm_register(
    #state{stream_module = StreamModule, available_regs = [Temp | _], stream = Stream0} = State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq({8, Reg}, Reg),
    I2 = jit_x86_64_asm:movq(?FP_REGS, Temp),
    I3 = jit_x86_64_asm:movq(Reg, {F * 8, Temp}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = free_native_register(State0, Reg),
    State1#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of an array element (reg[x]) to a vm or a native register.
%% @end
%% @param State current backend state
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @param Dest vm or native register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_array_element(
    State :: state(),
    Reg :: x86_64_register(),
    Index :: non_neg_integer() | {free, x86_64_register()},
    Dest :: vm_register() | x86_64_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {ptr, Dest}
) when is_integer(Index) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {0, Dest}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | _]} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Y * 8, Temp1}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Dest),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    I1 = jit_x86_64_asm:shlq(3, IndexReg),
    I2 = jit_x86_64_asm:addq(Reg, IndexReg),
    I3 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I4 = jit_x86_64_asm:movq(IndexReg, ?X_REG(X)),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {ptr, PtrReg}
) when is_atom(IndexReg) ->
    I1 = jit_x86_64_asm:shlq(3, IndexReg),
    I2 = jit_x86_64_asm:addq(Reg, IndexReg),
    I3 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I4 = jit_x86_64_asm:movq(IndexReg, {0, PtrReg}),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _] = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when ?IS_GPR(IndexReg) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:shlq(3, IndexReg),
    I3 = jit_x86_64_asm:addq(Reg, IndexReg),
    I4 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I5 = jit_x86_64_asm:movq(IndexReg, {Y * 8, Temp}),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, IndexReg),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>
    ),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of an array element (reg[x]) to a new native register.
%% @end
%% @param State current backend state
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec get_array_element(
    State :: state(),
    Reg :: x86_64_register() | {free, x86_64_register()},
    Index :: non_neg_integer()
) ->
    {state(), x86_64_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    {State#state{stream = Stream1}, Reg};
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [ElemReg | AvailableT],
        used_regs = UsedRegs0
    } = State,
    Reg,
    Index
) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, ElemReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    {
        State#state{
            stream = Stream1, available_regs = AvailableT, used_regs = [ElemReg | UsedRegs0]
        },
        ElemReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of a value (integer, vm register or native register) to an
%% array element (reg[x])
%% @end
%% @param State current backend state
%% @param Value value to move
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_array_element(
    State :: state(),
    Value :: integer() | vm_register() | x86_64_register(),
    Reg :: x86_64_register(),
    Index :: non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    Reg,
    Index
) when X < ?MAX_REG andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {ptr, Source},
    Reg,
    Index
) ->
    I1 = jit_x86_64_asm:movq({0, Source}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} =
        State,
    {y_reg, Y},
    Reg,
    Index
) when ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_GPR(Source) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_SINT32_T(Source) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Source,
    Reg,
    Index
) when is_integer(Source) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movabsq(Source, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of a value (integer, vm register or native register) to an
%% array element (reg[x+offset])
%% @end
%% @param State current backend state
%% @param Value value to move
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @param Offset additional offset
%% @return Updated backend state
%%-----------------------------------------------------------------------------
move_to_array_element(
    State,
    Source,
    BaseReg,
    Index,
    Offset
) when is_integer(Index) andalso is_integer(Offset) ->
    move_to_array_element(State, Source, BaseReg, Index + Offset);
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    BaseReg,
    IndexReg,
    Offset
) when X < ?MAX_REG andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {y_reg, Y},
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when
    ?IS_GPR(Source) andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset)
->
    I1 = jit_x86_64_asm:movq(Source, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when
    ?IS_SINT32_T(Source) andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso
        is_integer(Offset)
->
    I1 = jit_x86_64_asm:movq(Source, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

-spec move_to_native_register(state(), value()) -> {state(), x86_64_register()}.
move_to_native_register(State, Reg) when is_atom(Reg) ->
    {State, Reg};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq({0, Reg}, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    Imm
) when
    is_integer(Imm)
->
    I1 = jit_x86_64_asm:movq(Imm, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, used_regs = [Reg | Used], available_regs = AvailT}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    {x_reg, extra}
) ->
    I1 = jit_x86_64_asm:movq(?X_REG(?MAX_REG), Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, used_regs = [Reg | Used], available_regs = AvailT}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    {x_reg, X}
) when
    X < ?MAX_REG
->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, used_regs = [Reg | Used], available_regs = AvailT}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    {y_reg, Y}
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Reg),
    I2 = jit_x86_64_asm:movq({Y * 8, Reg}, Reg),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [Reg | Used]}, Reg}.

-spec move_to_native_register(state(), integer() | x86_64_register(), x86_64_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, RegSrc, RegDst
) when is_atom(RegSrc) orelse is_integer(RegSrc) ->
    I = jit_x86_64_asm:movq(RegSrc, RegDst),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

-spec copy_to_native_register(state(), value()) -> {state(), x86_64_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [SaveReg | AvailT],
        used_regs = Used
    } = State,
    Reg
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(Reg, SaveReg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [SaveReg | AvailT],
        used_regs = Used
    } = State,
    {ptr, Reg}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq({0, Reg}, SaveReg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    {y_reg, Y}
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Reg),
    I2 = jit_x86_64_asm:movq({Y * 8, Reg}, Reg),
    I3 = jit_x86_64_asm:movq(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    Offset
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Reg),
    I2 = jit_x86_64_asm:addq(Offset * 8, Reg),
    I3 = jit_x86_64_asm:movq(Reg, ?Y_REGS),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

set_continuation_to_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _],
        branches = Branches
    } = State,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    {RewriteLEAOffset, I1} = jit_x86_64_asm:leaq_rel32({-4, rip}, Temp),
    Reloc = {Label, Offset + RewriteLEAOffset, 32},
    I2 = jit_x86_64_asm:movq(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | Branches]}.

set_continuation_to_offset(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _],
        branches = Branches
    } = State
) ->
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    {RewriteLEAOffset, I1} = jit_x86_64_asm:leaq_rel32({-4, rip}, Temp),
    Reloc = {OffsetRef, Offset + RewriteLEAOffset, 32},
    I2 = jit_x86_64_asm:movq(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | Branches]}, OffsetRef}.

%% @doc Implement a continuation entry point. On x86-64 this is a nop
%% as we don't need to save any register.
-spec continuation_entry_point(#state{}) -> #state{}.
continuation_entry_point(State) ->
    State.

get_module_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailableT],
        used_regs = UsedRegs0
    } = State
) ->
    I1 = jit_x86_64_asm:movq(?JITSTATE_MODULE, Reg),
    I2 = jit_x86_64_asm:movl(?MODULE_INDEX(Reg), Reg),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_regs = AvailableT, used_regs = [Reg | UsedRegs0]},
        Reg
    }.

and_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    % 32 bits instructions on x86-64 zero the high 32 bits
    I1 =
        if
            Val >= 0, Val =< 16#FFFFFFFF -> jit_x86_64_asm:andl(Val, Reg);
            true -> jit_x86_64_asm:andq(Val, Reg)
        end,
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

or_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:orq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

add(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:addq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

sub(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:subq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:imulq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:decl(?JITSTATE_REMAINING_REDUCTIONS),
    {RewriteJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(0),
    {RewriteLEAOffset, I3} = jit_x86_64_asm:leaq_rel32({0, rip}, Temp),
    I4 = jit_x86_64_asm:movq(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite jumps
    #state{stream = Stream2} = State2,
    NewOffset = StreamModule:offset(Stream2),
    Stream3 = StreamModule:replace(Stream2, Offset + byte_size(I1) + RewriteJNZOffset, <<
        (NewOffset - Offset - byte_size(I1) - byte_size(I2))
    >>),
    Stream4 = StreamModule:replace(
        Stream3, Offset + byte_size(I1) + byte_size(I2) + RewriteLEAOffset, <<
            (NewOffset - Offset - byte_size(I1) - byte_size(I2) - byte_size(I3)):32/little
        >>
    ),
    merge_used_regs(State2#state{stream = Stream4}, State1#state.used_regs).

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset).

call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches
    } = State0,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:decl(?JITSTATE_REMAINING_REDUCTIONS),
    {RewriteJMPOffset, I3} = jit_x86_64_asm:jmp_rel32(1),
    I2 = jit_x86_64_asm:jz(byte_size(I3) + 2),
    Sz = byte_size(I1) + byte_size(I2),
    Reloc1 = {Label, Offset + Sz + RewriteJMPOffset, 32},
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1, branches = [Reloc1 | Branches]},
    State2 = set_continuation_to_label(State1, Label),
    call_primitive_last(State2, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset).

-spec set_cp(state()) -> {state(), non_neg_integer()}.
set_cp(State0) ->
    % get module index (dynamically)
    {#state{stream_module = StreamModule, stream = Stream0} = State1, Reg} = get_module_index(
        State0
    ),
    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_x86_64_asm:shlq(24, Reg),
    % next part of cp is instruction offset, after the call.
    {RewriteOffset, I2} = jit_x86_64_asm:orq_rel32(0, Reg),
    AddrOffset = Offset + byte_size(I1) + RewriteOffset,
    I3 = jit_x86_64_asm:movq(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, AddrOffset}.

-spec rewrite_cp_offset(state(), non_neg_integer()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    % Encode ReturnAddrOffset << 2
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, <<(NewOffset bsl 2):32/little>>),
    State0#state{stream = Stream1}.

set_bs(#state{stream_module = StreamModule, stream = Stream0} = State0, TermReg) ->
    I1 = jit_x86_64_asm:movq(TermReg, ?BS),
    I2 = jit_x86_64_asm:movq(0, ?BS_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param State current state
%% @param SortedLines line information, sorted by offset
%% @doc Build labels and line tables and encode a function that returns it.
%% In this case, the function returns the effective address of what immediately
%% follows.
%% @end
%% @return New state
%%-----------------------------------------------------------------------------
return_labels_and_lines(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        labels = Labels
    } = State,
    SortedLines
) ->
    SortedLabels = lists:keysort(2, [
        {Label, LabelOffset}
     || {Label, LabelOffset} <- Labels, is_integer(Label), Label /= 0
    ]),

    I2 = jit_x86_64_asm:retq(),
    {_RewriteLEAOffset, I1} = jit_x86_64_asm:leaq_rel32({byte_size(I2), rip}, rax),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<I1/binary, I2/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

free_reg(AvailableRegs0, UsedRegs0, Reg) when ?IS_GPR(Reg) ->
    AvailableRegs1 = free_reg0(?AVAILABLE_REGS, AvailableRegs0, Reg, []),
    true = lists:member(Reg, UsedRegs0),
    UsedRegs1 = lists:delete(Reg, UsedRegs0),
    {AvailableRegs1, UsedRegs1}.

free_reg0([Reg | _SortedT], PrevRegs0, Reg, Acc) ->
    lists:reverse(Acc, [Reg | PrevRegs0]);
free_reg0([PrevReg | SortedT], [PrevReg | PrevT], Reg, Acc) ->
    free_reg0(SortedT, PrevT, Reg, [PrevReg | Acc]);
free_reg0([_Other | SortedT], PrevRegs, Reg, Acc) ->
    free_reg0(SortedT, PrevRegs, Reg, Acc).

args_regs(Args) ->
    lists:map(
        fun
            ({free, {ptr, Reg}}) -> Reg;
            ({free, Reg}) when is_atom(Reg) -> Reg;
            ({free, Imm}) when is_integer(Imm) -> imm;
            (offset) -> imm;
            (ctx) -> ?CTX_REG;
            (jit_state) -> ?JITSTATE_REG;
            (Reg) when is_atom(Reg) -> Reg;
            (Imm) when is_integer(Imm) -> imm;
            ({ptr, Reg}) -> Reg;
            ({x_reg, _}) -> ?CTX_REG;
            ({y_reg, _}) -> ?CTX_REG;
            ({fp_reg, _}) -> ?CTX_REG;
            ({free, {x_reg, _}}) -> ?CTX_REG;
            ({free, {y_reg, _}}) -> ?CTX_REG;
            ({free, {fp_reg, _}}) -> ?CTX_REG;
            ({avm_int64_t, _}) -> imm
        end,
        Args
    ).

%%-----------------------------------------------------------------------------
%% @doc Add a label at the current offset
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference()) -> state().
add_label(#state{stream_module = StreamModule, stream = Stream} = State, Label) ->
    Offset = StreamModule:offset(Stream),
    add_label(State, Label, Offset).

-spec add_label(state(), integer() | reference(), integer()) -> state().
add_label(#state{labels = Labels} = State, Label, Offset) ->
    State#state{labels = [{Label, Offset} | Labels]}.
