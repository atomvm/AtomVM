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

-module(jit_aarch64).

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

%-define(ASSERT(Expr), true = Expr).
-define(ASSERT(_Expr), ok).

%% AArch64 ABI: r0-r7 are used for argument passing and return value.
%% r8 is the indirect result location register (platform-specific),
%% r9-r15 are caller-saved scratch registers (used by JIT),
%% r16-r17 are intra-procedure-call scratch registers,
%% r18 is platform register (reserved),
%% r19-r28 are callee-saved,
%% r29 is frame pointer, r30 is link register, r31 is stack pointer/zero.
%% d0-d7 are used for FP argument passing and return value.
%% d8-d15 are callee-saved FP registers.
%%
%% https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst
%%
%% Registers used by the JIT backend:
%%   - Scratch GPRs: r9-r15
%%   - Argument/return: r0-r7, d0-d7
%%   - Stack pointer: r31 (sp)
%%   - Frame pointer: r29
%%   - Link register: r30
%%   - Indirect result: r8
%%
%% Note: r18 is reserved for platform use and must not be used.

-type aarch64_register() ::
    r0
    | r1
    | r2
    | r3
    | r4
    | r5
    | r6
    | r7
    | r8
    | r9
    | r10
    | r11
    | r12
    | r13
    | r14
    | r15.

-define(IS_GPR(Reg),
    (Reg =:= r0 orelse Reg =:= r1 orelse Reg =:= r2 orelse Reg =:= r3 orelse Reg =:= r4 orelse
        Reg =:= r5 orelse Reg =:= r6 orelse Reg =:= r7 orelse Reg =:= r8 orelse Reg =:= r9 orelse
        Reg =:= r10 orelse Reg =:= r11 orelse Reg =:= r12 orelse Reg =:= r13 orelse Reg =:= r14 orelse
        Reg =:= r15)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    available_regs :: [aarch64_register()],
    used_regs :: [aarch64_register()],
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer()
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, aarch64_register()}.
-type value() :: immediate() | vm_register() | aarch64_register() | {ptr, aarch64_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_aarch64_register() ::
    {free, aarch64_register()} | aarch64_register().

-type condition() ::
    {aarch64_register(), '<', integer()}
    | {maybe_free_aarch64_register(), '<', aarch64_register()}
    | {maybe_free_aarch64_register(), '==', integer()}
    | {maybe_free_aarch64_register(), '!=', aarch64_register() | integer()}
    | {'(int)', maybe_free_aarch64_register(), '==', integer()}
    | {'(int)', maybe_free_aarch64_register(), '!=', aarch64_register() | integer()}
    | {'(bool)', maybe_free_aarch64_register(), '==', false}
    | {'(bool)', maybe_free_aarch64_register(), '!=', false}
    | {maybe_free_aarch64_register(), '&', non_neg_integer(), '!=', integer()}.

% ctx->e is 0x28
% ctx->x is 0x30
-define(WORD_SIZE, 8).
-define(CTX_REG, r0).
-define(JITSTATE_REG, r1).
-define(NATIVE_INTERFACE_REG, r2).
-define(Y_REGS, {?CTX_REG, 16#28}).
-define(X_REG(N), {?CTX_REG, 16#30 + (N * ?WORD_SIZE)}).
-define(CP, {?CTX_REG, 16#B8}).
-define(FP_REGS, {?CTX_REG, 16#C0}).
-define(FP_REG_OFFSET(State, F),
    (F *
        case (State)#state.variant band ?JIT_VARIANT_FLOAT32 of
            0 -> 8;
            _ -> 4
        end)
).
-define(BS, {?CTX_REG, 16#C8}).
-define(BS_OFFSET, {?CTX_REG, 16#D0}).
-define(JITSTATE_MODULE, {?JITSTATE_REG, 0}).
-define(JITSTATE_CONTINUATION, {?JITSTATE_REG, 16#8}).
-define(JITSTATE_REDUCTIONCOUNT, {?JITSTATE_REG, 16#10}).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).

% aarch64 ABI specific
-define(LR_REG, r30).
-define(IP0_REG, r16).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-define(AVAILABLE_REGS, [r7, r8, r9, r10, r11, r12, r13, r14, r15, r3, r4, r5, r6]).
-define(PARAMETER_REGS, [r0, r1, r2, r3, r4, r5]).
-define(SCRATCH_REGS, [r7, r8, r9, r10, r11, r12, r13, r14, r15, r3, r4, r5, r6, r17]).

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
new(Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS,
        used_regs = [],
        labels = [],
        variant = Variant
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
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:brk(0)),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [aarch64_register()].
used_regs(#state{used_regs = Used}) -> Used.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [aarch64_register()].
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
assert_all_native_free(#state{
    available_regs = ?AVAILABLE_REGS, used_regs = []
}) ->
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

-spec jump_table0(state(), non_neg_integer(), pos_integer()) -> state().
jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State,
    N,
    LabelsCount
) ->
    Offset = StreamModule:offset(Stream0),
    BranchInstr = jit_aarch64_asm:b(0),
    Reloc = {N, Offset, b},
    Stream1 = StreamModule:append(Stream0, BranchInstr),
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
        branches = [{Label, Offset, Type} | BranchesT],
        labels = Labels
    } = State
) ->
    {Label, LabelOffset} = lists:keyfind(Label, 1, Labels),
    Rel = LabelOffset - Offset,
    NewInstr =
        case Type of
            {bcc, CC} -> jit_aarch64_asm:bcc(CC, Rel);
            {adr, Reg} -> jit_aarch64_asm:adr(Reg, Rel);
            b -> jit_aarch64_asm:b(Rel)
        end,
    Stream1 = StreamModule:replace(Stream0, Offset, NewInstr),
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
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), aarch64_register()}.
call_primitive(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    Primitive,
    Args
) ->
    PrepCall =
        case Primitive of
            0 ->
                jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, 0});
            N ->
                jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
        end,
    Stream1 = StreamModule:append(Stream0, PrepCall),
    StateCall = State#state{stream = Stream1},
    call_func_ptr(StateCall, {free, ?IP0_REG}, Args).

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
-spec call_primitive_last(state(), non_neg_integer(), [arg()]) -> state().
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
                jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, 0});
            N ->
                jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
        end,
    Stream1 = StreamModule:append(Stream0, PrepCall),
    State1 = set_args(
        State0#state{
            stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs
        },
        Args
    ),
    #state{stream = Stream2} = State1,
    Call = jit_aarch64_asm:br(Temp),
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
-spec return_if_not_equal_to_ctx(state(), {free, aarch64_register()}) -> state().
return_if_not_equal_to_ctx(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    {free, Reg}
) ->
    I1 = jit_aarch64_asm:cmp(Reg, ?CTX_REG),
    I3 =
        case Reg of
            % Return value is already in r0
            r0 -> <<>>;
            % Move to r0 (return register)
            _ -> jit_aarch64_asm:orr(r0, xzr, Reg)
        end,
    I4 = jit_aarch64_asm:ret(),
    I2 = jit_aarch64_asm:bcc(eq, 4 + byte_size(I3) + byte_size(I4)),
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
-spec jump_to_label(state(), integer() | reference()) -> state().
jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches, labels = Labels} =
        State,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    case lists:keyfind(Label, 1, Labels) of
        {Label, LabelOffset} ->
            % Label is already known, emit direct branch without relocation
            Rel = LabelOffset - Offset,
            I1 = jit_aarch64_asm:b(Rel),
            Stream1 = StreamModule:append(Stream0, I1),
            State#state{stream = Stream1};
        false ->
            % Label not yet known, emit placeholder and add relocation
            I1 = jit_aarch64_asm:b(0),
            Reloc = {Label, Offset, b},
            Stream1 = StreamModule:append(Stream0, I1),
            State#state{stream = Stream1, branches = [Reloc | AccBranches]}
    end.

%% @private
-spec rewrite_branch_instruction(
    jit_aarch64_asm:cc() | {tbz | tbnz, atom(), 0..63} | {cbz, atom()}, integer()
) -> binary().
rewrite_branch_instruction({cbnz, Reg}, Offset) ->
    jit_aarch64_asm:cbnz(Reg, Offset);
rewrite_branch_instruction({cbnz_w, Reg}, Offset) ->
    jit_aarch64_asm:cbnz_w(Reg, Offset);
rewrite_branch_instruction({tbz, Reg, Bit}, Offset) ->
    jit_aarch64_asm:tbz(Reg, Bit, Offset);
rewrite_branch_instruction({tbnz, Reg, Bit}, Offset) ->
    jit_aarch64_asm:tbnz(Reg, Bit, Offset);
rewrite_branch_instruction(CC, Offset) when is_atom(CC) ->
    jit_aarch64_asm:bcc(CC, Offset).

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
            {NewAccState, CC, ReplaceDelta} = if_block_cond(AccState, Cond),
            {[{Offset + ReplaceDelta, CC} | AccReplacements], NewAccState}
        end,
        {[], State0},
        CondList
    ),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({ReplacementOffset, CC}, AccStream) ->
            BranchOffset = OffsetAfter - ReplacementOffset,
            NewBranchInstr = jit_aarch64_asm:bcc(CC, BranchOffset),
            StreamModule:replace(AccStream, ReplacementOffset, NewBranchInstr)
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
    {State1, CC, BranchInstrOffset} = if_block_cond(State0, Cond),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    %% Patch the conditional branch instruction to jump to the end of the block
    BranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = rewrite_branch_instruction(CC, BranchOffset),
    Stream3 = StreamModule:replace(Stream2, Offset + BranchInstrOffset, NewBranchInstr),
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
    {State1, CC, BranchInstrOffset} = if_block_cond(State0, Cond),
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    %% Emit unconditional branch to skip the else block (will be replaced)
    ElseJumpOffset = StreamModule:offset(Stream2),
    ElseJumpInstr = jit_aarch64_asm:b(0),
    Stream3 = StreamModule:append(Stream2, ElseJumpInstr),
    %% Else block starts here.
    OffsetAfter = StreamModule:offset(Stream3),
    %% Patch the conditional branch to jump to the else block
    ElseBranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = rewrite_branch_instruction(CC, ElseBranchOffset),
    Stream4 = StreamModule:replace(Stream3, Offset + BranchInstrOffset, NewBranchInstr),
    %% Build the else block
    StateElse = State2#state{
        stream = Stream4,
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    %% Patch the unconditional branch to jump to the end
    FinalJumpOffset = OffsetFinal - ElseJumpOffset,
    NewElseJumpInstr = jit_aarch64_asm:b(FinalJumpOffset),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs).

%% @private
-spec if_block_cond(state(), condition()) ->
    {
        state(),
        jit_aarch64_asm:cc() | {tbz | tbnz, atom(), 0..63} | {cbz, atom()},
        non_neg_integer()
    }.
if_block_cond(#state{stream_module = StreamModule, stream = Stream0} = State0, {Reg, '<', 0}) ->
    I = jit_aarch64_asm:tbz(Reg, 63, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = State0#state{stream = Stream1},
    {State1, {tbz, Reg, 63}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {Reg, '<', Val}
) when is_atom(Reg), is_integer(Val) ->
    I1 = jit_aarch64_asm:cmp(Reg, Val),
    % ge = greater than or equal
    I2 = jit_aarch64_asm:bcc(ge, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, ge, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, RegB),
    % ge = greater than or equal
    I2 = jit_aarch64_asm:bcc(ge, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ge, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:cbnz(Reg, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {cbnz, Reg}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {'(int)', RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:cbnz_w(Reg, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {cbnz_w, Reg}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '==', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp_w(Reg, Val),
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '!=', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp_w(Reg, Val),
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test lowest bit
    I = jit_aarch64_asm:tbnz(Reg, 0, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {tbnz, Reg, 0}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test lowest bit
    I = jit_aarch64_asm:tbz(Reg, 0, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {tbz, Reg, 0}, 0};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {RegOrTuple, '&', Val, '!=', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test bits
    TestCode =
        try
            jit_aarch64_asm:tst(Reg, Val)
        catch
            error:{unencodable_immediate, Val} ->
                TestCode0 = jit_aarch64_asm:mov(Temp, Val),
                TestCode1 = jit_aarch64_asm:tst(Reg, Temp),
                <<TestCode0/binary, TestCode1/binary>>
        end,
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        TestCode/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(TestCode)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    % AND with mask
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = op_imm(State0, and_, Temp, Reg, Mask),
    Stream1 = State1#state.stream,
    % Compare with value
    I2 = jit_aarch64_asm:cmp(Temp, Val),
    Stream2 = StreamModule:append(Stream1, I2),
    OffsetAfter = StreamModule:offset(Stream2),
    I3 = jit_aarch64_asm:bcc(eq, 0),
    Stream3 = StreamModule:append(Stream2, I3),
    State2 = State1#state{stream = Stream3},
    {State2, eq, OffsetAfter - OffsetBefore};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    % AND with mask
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = and_(State0, Reg, Mask),
    Stream1 = State1#state.stream,
    % Compare with value
    I2 = jit_aarch64_asm:cmp(Reg, Val),
    Stream2 = StreamModule:append(Stream1, I2),
    OffsetAfter = StreamModule:offset(Stream2),
    I3 = jit_aarch64_asm:bcc(eq, 0),
    Stream3 = StreamModule:append(Stream2, I3),
    State3 = State1#state{stream = Stream3},
    State4 = if_block_free_reg(RegTuple, State3),
    {State4, eq, OffsetAfter - OffsetBefore}.

%% @private
-spec if_block_free_reg(aarch64_register() | {free, aarch64_register()}, state()) -> state().
if_block_free_reg({free, Reg}, State0) ->
    #state{available_regs = AvR0, used_regs = UR0} = State0,
    {AvR1, UR1} = free_reg(AvR0, UR0, Reg),
    State0#state{
        available_regs = AvR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

%% @private
-spec merge_used_regs(state(), [aarch64_register()]) -> state().
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
-spec shift_right(state(), aarch64_register(), non_neg_integer()) -> state().
shift_right(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_aarch64_asm:lsr(Reg, Reg, Shift),
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
-spec shift_left(state(), aarch64_register(), non_neg_integer()) -> state().
shift_left(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    is_atom(Reg)
->
    I = jit_aarch64_asm:lsl(Reg, Reg, Shift),
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
-spec call_func_ptr(state(), {free, aarch64_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), aarch64_register()}.
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
            ({free, ?IP0_REG}) -> [];
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [FuncPtrTuple | Args]
    ),
    UsedRegs1 = UsedRegs0 -- FreeRegs,
    SavedRegs = [?LR_REG, ?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | UsedRegs1],
    {SavedRegsOdd, Stream1} = push_registers(SavedRegs, StreamModule, Stream0),

    % Set up arguments following AArch64 calling convention
    State1 = set_args(State0#state{stream = Stream1}, Args),
    #state{stream = Stream2} = State1,

    {FuncPtrReg, Stream3} =
        case FuncPtrTuple of
            {free, Reg} ->
                {Reg, Stream2};
            {primitive, Primitive} ->
                % We use r16 for the address.
                PrepCall =
                    case Primitive of
                        0 ->
                            jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, 0});
                        N ->
                            jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
                    end,
                {?IP0_REG, StreamModule:append(Stream2, PrepCall)}
        end,

    % Call the function pointer (using BLR for call with return)
    Call = jit_aarch64_asm:blr(FuncPtrReg),
    Stream4 = StreamModule:append(Stream3, Call),

    % If r0 is in used regs, save it to another temporary register
    FreeGPRegs = FreeRegs -- (FreeRegs -- ?AVAILABLE_REGS),
    AvailableRegs1 = FreeGPRegs ++ AvailableRegs0,
    {Stream5, ResultReg} =
        case lists:member(r0, SavedRegs) of
            true ->
                [Temp | _] = AvailableRegs1,
                {StreamModule:append(Stream4, jit_aarch64_asm:mov(Temp, r0)), Temp};
            false ->
                {Stream4, r0}
        end,

    Stream6 = pop_registers(SavedRegsOdd, lists:reverse(SavedRegs), StreamModule, Stream5),

    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    UsedRegs2 = [ResultReg | UsedRegs1],
    {
        State1#state{
            stream = Stream6,
            available_regs = AvailableRegs3,
            used_regs = UsedRegs2
        },
        ResultReg
    }.

%% @private
-spec push_registers([aarch64_register()], module(), stream()) -> {boolean(), stream()}.
push_registers([RegA, RegB | Tail], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:stp(RegA, RegB, {sp, -16}, '!')),
    push_registers(Tail, StreamModule, Stream1);
push_registers([], _StreamModule, Stream0) ->
    {false, Stream0};
push_registers([RegA], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:str(RegA, {sp, -16}, '!')),
    {true, Stream1}.

%% @private
-spec pop_registers(boolean(), [aarch64_register()], module(), stream()) -> stream().
pop_registers(true, [Reg | Tail], StreamModule, Stream0) ->
    % Odd number of registers, pop the last one first
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:ldr(Reg, {sp}, 16)),
    pop_registers(false, Tail, StreamModule, Stream1);
pop_registers(false, [], _StreamModule, Stream0) ->
    Stream0;
pop_registers(false, [RegB, RegA | Tail], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:ldp(RegA, RegB, {sp}, 16)),
    pop_registers(false, Tail, StreamModule, Stream1).

%% @private
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

%% @private
-spec parameter_regs([arg()]) -> [aarch64_register()].
parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

%% @private
-spec parameter_regs0([arg()], [aarch64_register()], [aarch64_register()]) -> [aarch64_register()].
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

%% @private
-spec replace_reg([arg()], aarch64_register(), aarch64_register()) -> [arg()].
replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

%% @private
-spec replace_reg0([arg()], aarch64_register(), aarch64_register(), [arg()]) -> [arg()].
replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

%% @private
-spec set_args0([arg()], [aarch64_register() | imm], [aarch64_register()], [aarch64_register()], [
    binary()
]) -> binary().
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
        jit_aarch64_asm:mov(ParamReg, ?JITSTATE_REG) | Acc
    ]);
% ctx is special as we need it to access x_reg/y_reg/fp_reg
set_args0([Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, Acc) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    J = set_args1(Arg, ?CTX_REG),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, [J | Acc]);
set_args0(
    [Arg | ArgsT],
    [_ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    [Avail | AvailGPT] = AvailGP,
    Acc
) ->
    J = set_args1(Arg, ParamReg),
    case lists:member(ParamReg, ArgsRegs) of
        false ->
            set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, [J | Acc]);
        true ->
            I = jit_aarch64_asm:mov(Avail, ParamReg),
            NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
            set_args0(NewArgsT, ArgsRegs, ParamRegs, AvailGPT, [J, I | Acc])
    end.

%% @private
-spec set_args1(arg(), aarch64_register()) -> binary() | [binary()].
set_args1(Reg, Reg) ->
    [];
set_args1({x_reg, extra}, Reg) ->
    jit_aarch64_asm:ldr(Reg, ?X_REG(?MAX_REG));
set_args1({x_reg, X}, Reg) ->
    jit_aarch64_asm:ldr(Reg, ?X_REG(X));
set_args1({ptr, Source}, Reg) ->
    jit_aarch64_asm:ldr(Reg, {Source, 0});
set_args1({y_reg, X}, Reg) ->
    [
        jit_aarch64_asm:ldr(Reg, ?Y_REGS),
        jit_aarch64_asm:ldr(Reg, {Reg, X * ?WORD_SIZE})
    ];
set_args1(ArgReg, Reg) when ?IS_GPR(ArgReg) ->
    jit_aarch64_asm:mov(Reg, ArgReg);
set_args1(Arg, Reg) when is_integer(Arg) ->
    jit_aarch64_asm:mov(Reg, Arg);
set_args1({avm_int64_t, Value}, Reg) when is_integer(Value) ->
    jit_aarch64_asm:mov(Reg, Value).

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
    (state(), Src :: {free, {ptr, aarch64_register(), 1}}, Dest :: {fp_reg, non_neg_integer()}) ->
        state().
% Native register to VM register
move_to_vm_register(State0, Src, {x_reg, extra}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {x_reg, X}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State0, Src, {y_reg, Y}) when
    is_atom(Src)
->
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:str(Src, {Temp, Y * ?WORD_SIZE}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1};
% Source is an integer
move_to_vm_register(State, 0, Dest) ->
    move_to_vm_register(State, xzr, Dest);
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, N, Dest) when
    is_integer(N)
->
    I1 = jit_aarch64_asm:mov(Temp, N),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
% Source is a VM register
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, extra}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, X}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {ptr, Reg}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {y_reg, Y}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Temp, {Temp, Y * ?WORD_SIZE}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
% term_to_float
move_to_vm_register(
    #state{stream_module = StreamModule, available_regs = [Temp | _], stream = Stream0} = State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) ->
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, ?WORD_SIZE}),
    I2 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I3 = jit_aarch64_asm:str(Reg, {Temp, ?FP_REG_OFFSET(State0, F)}),
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
    state(),
    aarch64_register(),
    non_neg_integer() | aarch64_register(),
    vm_register() | aarch64_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, Index * ?WORD_SIZE}),
    I2 = jit_aarch64_asm:str(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, Index * ?WORD_SIZE}),
    I2 = jit_aarch64_asm:str(Temp, {Dest, 0}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | _]} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Temp1, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Temp2, {Reg, Index * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Temp2, {Temp1, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Index * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Reg, {Temp, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Dest, {Reg, Index * ?WORD_SIZE}),
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
    I1 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I2 = jit_aarch64_asm:str(IndexReg, ?X_REG(X)),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
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
    I1 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I2 = jit_aarch64_asm:str(IndexReg, {PtrReg, 0}),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
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
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I3 = jit_aarch64_asm:str(IndexReg, {Temp, Y * ?WORD_SIZE}),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, IndexReg),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary>>
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
    state(), aarch64_register() | {free, aarch64_register()}, non_neg_integer()
) ->
    {state(), aarch64_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, Index * ?WORD_SIZE}),
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
    I1 = jit_aarch64_asm:ldr(ElemReg, {Reg, Index * ?WORD_SIZE}),
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
    state(), integer() | vm_register() | aarch64_register(), aarch64_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:str(ValueReg, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    IndexReg
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_aarch64_asm:str(ValueReg, {Reg, IndexReg, lsl, 3}),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    State0,
    Value,
    Reg,
    Index
) ->
    {State1, Temp} = copy_to_native_register(State0, Value),
    State2 = move_to_array_element(State1, Temp, Reg, Index),
    free_native_register(State2, Temp).

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
-spec move_to_array_element(
    state(), value(), aarch64_register(), aarch64_register() | non_neg_integer(), integer()
) -> state().
move_to_array_element(
    State,
    Value,
    BaseReg,
    IndexVal,
    Offset
) when is_integer(IndexVal) andalso is_integer(Offset) ->
    move_to_array_element(State, Value, BaseReg, IndexVal + Offset);
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    ValueReg,
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_aarch64_asm:add(Temp, IndexReg, Offset),
    I2 = jit_aarch64_asm:str(ValueReg, {BaseReg, Temp, lsl, 3}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    State0,
    Value,
    BaseReg,
    IndexReg,
    Offset
) ->
    {State1, ValueReg} = copy_to_native_register(State0, Value),
    [Temp | _] = State1#state.available_regs,
    I1 = jit_aarch64_asm:add(Temp, IndexReg, Offset),
    I2 = jit_aarch64_asm:str(ValueReg, {BaseReg, Temp, lsl, 3}),
    Stream1 = (State1#state.stream_module):append(State1#state.stream, <<I1/binary, I2/binary>>),
    State2 = State1#state{stream = Stream1},
    free_native_register(State2, ValueReg).

%%-----------------------------------------------------------------------------
%% @doc Move a value (integer, vm register, pointer or native register) to a
%% native register. This allocates a new native register from the available
%% pool if needed.
%% @end
%% @param State current backend state
%% @param Value value to move (can be an immediate, vm register, pointer, or native register)
%% @return Tuple of {Updated backend state, Native register containing the value}
%%-----------------------------------------------------------------------------
-spec move_to_native_register(state(), value()) -> {state(), aarch64_register()}.
move_to_native_register(State, Reg) when is_atom(Reg) ->
    {State, Reg};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}
) when is_atom(Reg) ->
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, 0}),
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
    I1 = jit_aarch64_asm:mov(Reg, Imm),
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
    I1 = jit_aarch64_asm:ldr(Reg, ?X_REG(?MAX_REG)),
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
    I1 = jit_aarch64_asm:ldr(Reg, ?X_REG(X)),
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
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [Reg | Used]}, Reg}.

%%-----------------------------------------------------------------------------
%% @doc Move a value (integer, vm register, pointer or native register) to a
%% specific native register.
%% @end
%% @param State current backend state
%% @param Value value to move (can be an immediate, vm register, pointer, or native register)
%% @param TargetReg the specific native register to move the value to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_native_register(state(), value(), aarch64_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, RegSrc, RegDst
) when is_atom(RegSrc) orelse is_integer(RegSrc) ->
    I = jit_aarch64_asm:mov(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}, RegDst
) when ?IS_GPR(Reg) ->
    I1 = jit_aarch64_asm:ldr(RegDst, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, extra}, RegDst
) ->
    I1 = jit_aarch64_asm:ldr(RegDst, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    I1 = jit_aarch64_asm:ldr(RegDst, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {y_reg, Y}, RegDst
) ->
    I1 = jit_aarch64_asm:ldr(RegDst, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(RegDst, {RegDst, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Copy a value to a native register, allocating a new register from the
%% available pool. Unlike move_to_native_register, this always allocates a new
%% register and copies the value (preserving the source if it's a register).
%% @end
%% @param State current backend state
%% @param Value value to copy (can be an immediate, vm register, pointer, or native register)
%% @return Tuple of {Updated backend state, Native register containing the copied value}
%%-----------------------------------------------------------------------------
-spec copy_to_native_register(state(), value()) -> {state(), aarch64_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [SaveReg | AvailT],
        used_regs = Used
    } = State,
    Reg
) when is_atom(Reg) ->
    I1 = jit_aarch64_asm:mov(SaveReg, Reg),
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
    I1 = jit_aarch64_asm:ldr(SaveReg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

%%-----------------------------------------------------------------------------
%% @doc Move a VM register value to the continuation pointer (CP).
%% @end
%% @param State current backend state
%% @param VMReg VM register to move to CP
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_cp(state(), vm_register()) -> state().
move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    {y_reg, Y}
) ->
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Y * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Increment the stack pointer (SP) by a given offset.
%% @end
%% @param State current backend state
%% @param Offset offset to add to SP (in words, will be multiplied by 8)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec increment_sp(state(), integer()) -> state().
increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    Offset
) ->
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:add(Reg, Reg, Offset * ?WORD_SIZE),
    I3 = jit_aarch64_asm:str(Reg, ?Y_REGS),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Set the continuation address to point to a specific label. The actual
%% address will be resolved during branch update.
%% @end
%% @param State current backend state
%% @param Label label to set as continuation target
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec set_continuation_to_label(state(), integer() | reference()) -> state().
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
    I1 = jit_aarch64_asm:adr(Temp, 0),
    Reloc = {Label, Offset, {adr, Temp}},
    I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | Branches]}.

%%-----------------------------------------------------------------------------
%% @doc Set the continuation address to the current offset, creating a
%% reference for later resolution. Returns a reference that can be used
%% to add the label at the target location.
%% @end
%% @param State current backend state
%% @return Tuple of {Updated backend state, Reference for the continuation offset}
%%-----------------------------------------------------------------------------
-spec set_continuation_to_offset(state()) -> {state(), reference()}.
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
    I1 = jit_aarch64_asm:adr(Temp, 0),
    Reloc = {OffsetRef, Offset, {adr, Temp}},
    I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | Branches]}, OffsetRef}.

%%-----------------------------------------------------------------------------
%% @doc Implement a continuation entry point. On AArch64 this is a nop
%% as we don't need to save any register.
%% @end
%% @param State current backend state
%% @return Updated backend state (unchanged on AArch64)
%%-----------------------------------------------------------------------------
-spec continuation_entry_point(#state{}) -> #state{}.
continuation_entry_point(State) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Get the module index from the JIT state and load it into a native
%% register.
%% @end
%% @param State current backend state
%% @return Tuple of {Updated backend state, Native register containing module index}
%%-----------------------------------------------------------------------------
-spec get_module_index(state()) -> {state(), aarch64_register()}.
get_module_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailableT],
        used_regs = UsedRegs0
    } = State
) ->
    I1 = jit_aarch64_asm:ldr(Reg, ?JITSTATE_MODULE),
    I2 = jit_aarch64_asm:ldr_w(Reg, ?MODULE_INDEX(Reg)),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_regs = AvailableT, used_regs = [Reg | UsedRegs0]},
        Reg
    }.

%% @private
-spec op_imm(state(), atom(), aarch64_register(), aarch64_register(), integer()) -> state().
op_imm(#state{stream_module = StreamModule, stream = Stream0} = State, Op, Reg, Reg, Val) ->
    Stream1 =
        try
            I = jit_aarch64_asm:Op(Reg, Reg, Val),
            StreamModule:append(Stream0, I)
        catch
            error:{unencodable_immediate, Val} ->
                [Temp | _] = State#state.available_regs,
                I1 = jit_aarch64_asm:mov(Temp, Val),
                I2 = jit_aarch64_asm:Op(Reg, Reg, Temp),
                StreamModule:append(Stream0, <<I1/binary, I2/binary>>)
        end,
    State#state{stream = Stream1};
op_imm(#state{stream_module = StreamModule, stream = Stream0} = State, Op, RegA, RegB, Val) ->
    Stream1 =
        try
            I = jit_aarch64_asm:Op(RegA, RegB, Val),
            StreamModule:append(Stream0, I)
        catch
            error:{unencodable_immediate, Val} ->
                MoveI = jit_aarch64_asm:mov(RegA, Val),
                AndI = jit_aarch64_asm:Op(RegA, RegB, RegA),
                StreamModule:append(Stream0, <<MoveI/binary, AndI/binary>>)
        end,
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Perform bitwise AND of a register with an immediate value.
%% @end
%% @param State current backend state
%% @param Reg register to AND with value
%% @param Val immediate value to AND
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec and_(state(), aarch64_register(), integer()) -> state().
and_(State, Reg, Val) ->
    op_imm(State, and_, Reg, Reg, Val).

%%-----------------------------------------------------------------------------
%% @doc Perform bitwise OR of a register with an immediate value.
%% @end
%% @param State current backend state
%% @param Reg register to OR with value
%% @param Val immediate value to OR
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec or_(state(), aarch64_register(), integer()) -> state().
or_(State, Reg, Val) ->
    op_imm(State, orr, Reg, Reg, Val).

%%-----------------------------------------------------------------------------
%% @doc Add an immediate value to a register.
%% @end
%% @param State current backend state
%% @param Reg register to add to
%% @param Val immediate value to add
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add(state(), aarch64_register(), integer()) -> state().
add(State, Reg, Val) ->
    op_imm(State, add, Reg, Reg, Val).

%%-----------------------------------------------------------------------------
%% @doc Subtract an immediate value from a register.
%% @end
%% @param State current backend state
%% @param Reg register to subtract from
%% @param Val immediate value to subtract
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec sub(state(), aarch64_register(), integer()) -> state().
sub(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_aarch64_asm:sub(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Multiply a register by a constant value. Uses optimized instruction
%% sequences for common multipliers (powers of 2, small values).
%% @end
%% @param State current backend state
%% @param Reg register to multiply
%% @param Val constant multiplier (non-negative integer)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec mul(state(), aarch64_register(), non_neg_integer()) -> state().
mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{available_regs = [Temp | _]} = State, Reg, 3) ->
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 1),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 5) ->
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 2),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 7) ->
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 3),
    I2 = jit_aarch64_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{available_regs = [Temp | _]} = State, Reg, 9) ->
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 3),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 15) ->
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 4),
    I2 = jit_aarch64_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Val
) ->
    % multiply by decomposing by power of 2
    I1 = jit_aarch64_asm:mov(Temp, Val),
    I2 = jit_aarch64_asm:mul(Reg, Reg, Temp),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Decrement the reduction count and schedule the next process if it
%% reaches zero. If reductions remain, execution continues; otherwise, the
%% continuation is set and the scheduler is invoked.
%% @end
%% @param State current backend state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0
) ->
    % Load reduction count
    I1 = jit_aarch64_asm:ldr_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch if reduction count is not zero
    I4 = jit_aarch64_asm:bcc(ne, 0),
    % Set continuation to the next instruction
    ADROffset = BNEOffset + byte_size(I4),
    I5 = jit_aarch64_asm:adr(Temp, 0),
    I6 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
    % Append the instructions to the stream
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary, I6/binary>>),
    State1 = State0#state{stream = Stream2},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite the branch and adr instructions
    #state{stream = Stream3} = State2,
    NewOffset = StreamModule:offset(Stream3),
    NewI4 = jit_aarch64_asm:bcc(ne, NewOffset - BNEOffset),
    NewI5 = jit_aarch64_asm:adr(Temp, NewOffset - ADROffset),
    Stream4 = StreamModule:replace(
        Stream3, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    merge_used_regs(State2#state{stream = Stream4}, State1#state.used_regs).

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a label with automatic scheduling. Decrements reductions
%% and calls the label if reductions remain, otherwise schedules the next
%% process. Sets the continuation pointer before the call.
%% @end
%% @param State current backend state
%% @param Label label to call
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset, RewriteSize} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset, RewriteSize).

%%-----------------------------------------------------------------------------
%% @doc Emit a tail call to a label with automatic scheduling. Decrements
%% reductions and jumps to the label if reductions remain, otherwise schedules
%% the next process. Does not set a new continuation pointer (tail call).
%% @end
%% @param State current backend state
%% @param Label label to jump to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_only_or_schedule_next(state(), non_neg_integer()) -> state().
call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        available_regs = [Temp | _]
    } = State0,
    Label
) ->
    % Load reduction count
    I1 = jit_aarch64_asm:ldr_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch to label if reduction count is not zero
    I4 = jit_aarch64_asm:bcc(ne, 0),
    Reloc1 = {Label, BNEOffset, {bcc, ne}},
    Stream2 = StreamModule:append(Stream1, I4),
    State1 = State0#state{stream = Stream2, branches = [Reloc1 | Branches]},
    State2 = set_continuation_to_label(State1, Label),
    call_primitive_last(State2, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a primitive with continuation pointer setup. This is
%% used for primitives that may not return directly (e.g., those that can
%% trap or reschedule). Sets CP before calling the primitive.
%% @end
%% @param State current backend state
%% @param Primitive index of the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_primitive_with_cp(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset, RewriteSize} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset, RewriteSize).

%% @private
-spec set_cp(state()) -> {state(), non_neg_integer(), 4 | 8}.
set_cp(State0) ->
    % get module index (dynamically)
    {#state{stream_module = StreamModule, stream = Stream0} = State1, Reg} = get_module_index(
        State0
    ),
    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_aarch64_asm:lsl(Reg, Reg, 24),
    if
        Offset >= 16250 ->
            I2 = jit_aarch64_asm:nop(),
            I3 = jit_aarch64_asm:nop(),
            RewriteSize = 8;
        true ->
            I2 = jit_aarch64_asm:nop(),
            I3 = <<>>,
            RewriteSize = 4
    end,
    MOVOffset = Offset + byte_size(I1),
    I4 = jit_aarch64_asm:orr(Reg, Reg, ?IP0_REG),
    I5 = jit_aarch64_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, MOVOffset, RewriteSize}.

%% @private
-spec rewrite_cp_offset(state(), non_neg_integer(), 4 | 8) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset,
    _RewriteSize
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    NewMoveInstr = jit_aarch64_asm:mov(?IP0_REG, NewOffset bsl 2),
    ?ASSERT(byte_size(NewMoveInstr) =< _RewriteSize),
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, NewMoveInstr),
    State0#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Set the binary state (BS) register to point to a term and reset the
%% BS offset to zero. Used for binary matching operations.
%% @end
%% @param State current backend state
%% @param TermReg register containing the term to set as binary state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec set_bs(state(), aarch64_register()) -> state().
set_bs(#state{stream_module = StreamModule, stream = Stream0} = State0, TermReg) ->
    I1 = jit_aarch64_asm:str(TermReg, ?BS),
    I2 = jit_aarch64_asm:str(xzr, ?BS_OFFSET),
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
-spec return_labels_and_lines(state(), [{non_neg_integer(), non_neg_integer()}]) -> state().
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
     || {Label, LabelOffset} <- Labels, is_integer(Label)
    ]),

    I1 = jit_aarch64_asm:adr(r0, 8),
    I2 = jit_aarch64_asm:ret(),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<I1/binary, I2/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

%% @private
-spec free_reg([aarch64_register()], [aarch64_register()], aarch64_register()) ->
    {[aarch64_register()], [aarch64_register()]}.
free_reg(AvailableRegs0, UsedRegs0, Reg) when ?IS_GPR(Reg) ->
    AvailableRegs1 = free_reg0(?AVAILABLE_REGS, AvailableRegs0, Reg, []),
    true = lists:member(Reg, UsedRegs0),
    UsedRegs1 = lists:delete(Reg, UsedRegs0),
    {AvailableRegs1, UsedRegs1}.

%% @private
-spec free_reg0([aarch64_register()], [aarch64_register()], aarch64_register(), [aarch64_register()]) ->
    [aarch64_register()].
free_reg0([Reg | _SortedT], PrevRegs0, Reg, Acc) ->
    lists:reverse(Acc, [Reg | PrevRegs0]);
free_reg0([PrevReg | SortedT], [PrevReg | PrevT], Reg, Acc) ->
    free_reg0(SortedT, PrevT, Reg, [PrevReg | Acc]);
free_reg0([_Other | SortedT], PrevRegs, Reg, Acc) ->
    free_reg0(SortedT, PrevRegs, Reg, Acc).

%% @private
-spec args_regs([arg()]) -> [aarch64_register() | imm].
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

%%-----------------------------------------------------------------------------
%% @doc Add a label at a specific offset
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @param Offset the explicit offset for this label
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference(), integer()) -> state().
add_label(#state{labels = Labels} = State, Label, Offset) ->
    State#state{labels = [{Label, Offset} | Labels]}.
