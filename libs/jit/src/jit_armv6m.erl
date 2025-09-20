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

-module(jit_armv6m).

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

%% ARMv6-M AAPCS32 ABI: r0-r3 are used for argument passing and return value.
%% r0-r1 form a double-word for 64-bit returns, additional args passed on stack.
%% r4-r11 are callee-saved registers (must be preserved across calls),
%% r12 (IP) is intra-procedure-call scratch register,
%% r13 (SP) is stack pointer,
%% r14 (LR) is link register,
%% r15 (PC) is program counter.
%% ARMv6-M has no floating-point unit, so no FP registers available.
%%
%% See: Arm® Architecture Procedure Call Standard (AAPCS32)
%% https://developer.arm.com/documentation/ihi0042/latest/
%%
%% Registers used by the JIT backend (ARMv6-M Thumb):
%%   - Argument/return: r0-r3
%%   - Callee-saved: r4-r11 (must preserve)
%%   - Scratch: r12 (IP) - intra-procedure call
%%   - Stack pointer: r13 (SP)
%%   - Link register: r14 (LR)
%%   - Program counter: r15 (PC)
%%   - Available for JIT scratch: r12 only (very limited!)
%%
%% Note: ARMv6-M Thumb instructions are mostly 16-bit with limited
%% register access (many instructions only work with r0-r7).
%%
%% For more details, refer to the AAPCS32 Procedure Call Standard.

-type armv6m_register() ::
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
    available_regs :: [armv6m_register()],
    used_regs :: [armv6m_register()],
    labels :: [{integer() | reference(), integer()}]
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, armv6m_register()}.
-type value() :: immediate() | vm_register() | armv6m_register() | {ptr, armv6m_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()}.

-type maybe_free_armv6m_register() ::
    {free, armv6m_register()} | armv6m_register().

-type condition() ::
    {armv6m_register(), '<', integer()}
    | {maybe_free_armv6m_register(), '<', armv6m_register()}
    | {maybe_free_armv6m_register(), '==', integer()}
    | {maybe_free_armv6m_register(), '!=', armv6m_register() | integer()}
    | {'(int)', maybe_free_armv6m_register(), '==', integer()}
    | {'(int)', maybe_free_armv6m_register(), '!=', armv6m_register() | integer()}
    | {'(bool)', maybe_free_armv6m_register(), '==', false}
    | {'(bool)', maybe_free_armv6m_register(), '!=', false}
    | {maybe_free_armv6m_register(), '&', non_neg_integer(), '!=', integer()}.

% ctx->e is 0x28
% ctx->x is 0x30
-define(CTX_REG, r0).
-define(NATIVE_INTERFACE_REG, r2).
-define(Y_REGS, {?CTX_REG, 16#14}).
-define(X_REG(N), {?CTX_REG, 16#18 + (N * 4)}).
-define(CP, {?CTX_REG, 16#5C}).
-define(FP_REGS, {?CTX_REG, 16#60}).
-define(BS, {?CTX_REG, 16#64}).
-define(BS_OFFSET, {?CTX_REG, 16#68}).
% JITSTATE is on stack, accessed via stack offset
% These macros now expect a register that contains the jit_state pointer
-define(JITSTATE_MODULE(Reg), {Reg, 0}).
-define(JITSTATE_CONTINUATION(Reg), {Reg, 16#4}).
-define(JITSTATE_REDUCTIONCOUNT(Reg), {Reg, 16#8}).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * 4}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).

% aarch64 ABI specific
%% ARMv6-M register mappings

%% Link register
-define(LR_REG, r14).

%% Stack offset for function prolog: push {r1,r4,r5,r6,r7,lr}
%% r1 (JITSTATE_REG) is at SP+0 after push
-define(STACK_OFFSET_JITSTATE, 0).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

%% ARMv6-M register allocation:
%% - r0: context pointer (reserved)
%% - r1, r3: available (r1 saved/restored, r3 can be parameter)
%% - r2: parameter register (not available for scratch)
%% - r4-r7: callee-saved (saved/restored on entry/exit)
%% - r8-r11: high registers, limited Thumb access
%% - r12: intra-procedure call scratch
%% - r13 (SP), r14 (LR), r15 (PC): special purpose
%% Reorder to match AArch64 test expectations (r7 first)
-define(AVAILABLE_REGS, [r7, r6, r5, r4, r3, r1]).
-define(PARAMETER_REGS, [r0, r1, r2, r3]).
-define(SCRATCH_REGS, [r7, r6, r5, r4, r3, r2, r1, r0, r12]).

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
word_size() -> 4.

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
    Stream1 = StreamModule:append(Stream0, jit_armv6m_asm:bkpt(0)),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [armv6m_register()].
used_regs(#state{used_regs = Used}) -> Used.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [armv6m_register()].
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
%%
%% On this platform, the jump table is composed of
%% ```
%% ldr r3, offset_to_label_0
%% b common
%% ldr r3, offset_to_label_1
%% b common
%% ...
%% offset_to_label_0: dword (32 bits with offset)
%% offset_to_label_1: dword (32 bits with offset)
%% ...
%% common:
%%   push {r1, r4, r5, r6, r7, lr}
%%   add pc, pc, r3
%% ```
%% so each entry can be anywhere (we're not limited by b's range)
%%
%% @end
%% @param State current backend state
%% @param LabelsCount number of labels in the module.
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec jump_table(state(), pos_integer()) -> state().
jump_table(State, LabelsCount) ->
    jump_table0(State, 0, LabelsCount).

jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    % After all jump table entries, emit the common handler and offset data
    emit_jump_table_common_and_data(State, LabelsCount);
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State,
    N,
    LabelsCount
) ->
    Offset = StreamModule:offset(Stream0),
    % Calculate offsets at emit time:
    % Layout: [entries] [common_handler] [data]

    % 4 bytes per entry
    EntriesSize = (LabelsCount + 1) * 4,
    % push (2 bytes) + add pc, pc, r3 (2 bytes)
    CommonHandlerSize = 4,

    % Offset to common handler from current branch instruction (branch is at entry+2)
    CommonHandlerOffset = EntriesSize - (N * 4) - 2,

    % Offset to data from current ldr instruction

    % PC when ldr executes
    CurrentPC = Offset + 4,
    DataOffset = Offset + EntriesSize + CommonHandlerSize + (N * 4) - CurrentPC,

    % Create jump table entry with calculated offsets
    LdrInstr = jit_armv6m_asm:ldr(r3, {pc, DataOffset}),
    % branch offset in bytes
    BranchInstr = jit_armv6m_asm:b(CommonHandlerOffset),
    JumpEntry = <<LdrInstr/binary, BranchInstr/binary>>,
    Stream1 = StreamModule:append(Stream0, JumpEntry),

    % No relocations needed since we calculated everything at emit time
    jump_table0(State#state{stream = Stream1, branches = Branches}, N + 1, LabelsCount).

%%-----------------------------------------------------------------------------
%% @doc Emit the common handler and offset data for the jump table.
%% @end
%%-----------------------------------------------------------------------------
emit_jump_table_common_and_data(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State,
    LabelsCount
) ->
    % Emit common handler: push {r1, r4, r5, r6, r7, lr} + add pc, pc, r3
    CommonHandlerOffset = StreamModule:offset(Stream0),
    PushInstr = jit_armv6m_asm:push([r1, r4, r5, r6, r7, lr]),
    AddInstrOffset = CommonHandlerOffset + byte_size(PushInstr),
    % indirect jump using loaded offset
    AddInstr = jit_armv6m_asm:add(pc, r3),
    CommonHandler = <<PushInstr/binary, AddInstr/binary>>,
    Stream1 = StreamModule:append(Stream0, CommonHandler),

    % Emit offset data (32-bit offsets for each label, will be updated by update_branches/2)
    {Stream2, NewBranches} = lists:foldl(
        fun(N, {StreamAcc, BranchesAcc}) ->
            Offset = StreamModule:offset(StreamAcc),
            % Each data entry is a 32-bit offset that will be patched by update_branches/2

            % placeholder, will be updated
            DataEntry = <<0:32/little>>,
            StreamNext = StreamModule:append(StreamAcc, DataEntry),
            % Add relocation for this data entry, including the add instruction offset
            DataReloc = {N, Offset, {jump_table_data, AddInstrOffset}},
            {StreamNext, [DataReloc | BranchesAcc]}
        end,
        {Stream1, Branches},
        lists:seq(0, LabelsCount)
    ),
    State#state{stream = Stream2, branches = NewBranches}.

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
            {adr, Reg} when Rel rem 4 =:= 0 -> jit_armv6m_asm:adr(Reg, Rel);
            {adr, Reg} when Rel rem 4 =:= 2 -> jit_armv6m_asm:adr(Reg, Rel + 2);
            b ->
                jit_armv6m_asm:b(Rel);
            {jump_table_data, AddInstrOffset} ->
                % Calculate offset from 'add pc, pc, r3' instruction + 4 to target label

                % PC when add instruction executes
                AddPC = AddInstrOffset + 4,
                RelativeOffset = LabelOffset - AddPC,
                <<RelativeOffset:32/little>>
        end,
    Stream1 = StreamModule:replace(Stream0, Offset, NewInstr),
    update_branches(State#state{stream = Stream1, branches = BranchesT}).

%%-----------------------------------------------------------------------------
%% @doc Generate code to load a primitive function pointer into a register
%% @param Primitive index to the primitive to call
%% @param TargetReg register to load the function pointer into
%% @return Binary instruction sequence
%%-----------------------------------------------------------------------------
-spec load_primitive_ptr(non_neg_integer(), armv6m_register()) -> binary().
load_primitive_ptr(Primitive, TargetReg) ->
    case Primitive of
        0 ->
            jit_armv6m_asm:ldr(TargetReg, {?NATIVE_INTERFACE_REG, 0});
        N when N * 4 =< 124 ->
            jit_armv6m_asm:ldr(TargetReg, {?NATIVE_INTERFACE_REG, N * 4});
        N when N * 4 < 256 ->
            % Can encode N * 4 directly in movs instruction (8-bit immediate limit)
            I1 = jit_armv6m_asm:movs(TargetReg, N * 4),
            I2 = jit_armv6m_asm:ldr(TargetReg, {?NATIVE_INTERFACE_REG, TargetReg}),
            <<I1/binary, I2/binary>>;
        N ->
            % For very large primitive numbers, load N and shift left by 2 (multiply by 4)
            I1 = jit_armv6m_asm:movs(TargetReg, N),
            I2 = jit_armv6m_asm:lsls(TargetReg, TargetReg, 2),
            I3 = jit_armv6m_asm:ldr(TargetReg, {?NATIVE_INTERFACE_REG, TargetReg}),
            <<I1/binary, I2/binary, I3/binary>>
    end.

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
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), armv6m_register()}.
call_primitive(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [TempReg | RestRegs],
        used_regs = UsedRegs
    } = State,
    Primitive,
    Args
) ->
    % Use a low register for LDR since ARM Thumb LDR only works with low registers
    PrepCall = load_primitive_ptr(Primitive, TempReg),
    Stream1 = StreamModule:append(Stream0, PrepCall),
    StateCall = State#state{
        stream = Stream1,
        available_regs = RestRegs,
        used_regs = [TempReg | UsedRegs]
    },
    call_func_ptr(StateCall, {free, TempReg}, Args).

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
    PrepCall = load_primitive_ptr(Primitive, Temp),
    Stream1 = StreamModule:append(Stream0, PrepCall),
    % Assert that jit_state is the second argument for tail_call_with_jit_state
    [FirstArg, jit_state | ArgsT] = Args,
    ArgsForTailCall = [FirstArg, jit_state_tail_call | ArgsT],

    % Handle arguments differently for tail calls with 5+ arguments
    case length(Args) of
        NumArgs when NumArgs >= 5 ->
            % For tail calls with 5+ args, set first 4 args in registers without stack allocation
            State1 = set_args_registers_only(
                State0#state{
                    stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs
                },
                lists:sublist(ArgsForTailCall, 4)
            ),
            % 5th argument needs to be moved to r5
            FifthArg = lists:nth(5, ArgsForTailCall),
            State2 =
                case FifthArg of
                    % Already in r5
                    {free, r5} ->
                        State1;
                    % Already in r5
                    r5 ->
                        State1;
                    % Handle {free, Reg} - extract the register and move to r5
                    {free, Reg} ->
                        move_to_native_register(State1, Reg, r5);
                    _ ->
                        % Move 5th argument to r5
                        move_to_native_register(State1, FifthArg, r5)
                end,
            % Move function pointer to r1 if it's not already in r1
            #state{stream = Stream2} = State2,
            {FinalFuncPtrReg, Stream3} =
                case Temp of
                    % Already in r1, no move needed
                    r1 ->
                        {r1, Stream2};
                    _ ->
                        % Move from Temp register to r1
                        MoveToR1 = jit_armv6m_asm:mov(r1, Temp),
                        {r1, StreamModule:append(Stream2, MoveToR1)}
                end,
            State3 = tail_call_with_jit_state_stack(
                State2#state{stream = Stream3}, FinalFuncPtrReg, NumArgs
            );
        _ ->
            % For 4 or fewer args, use standard argument setup
            State1 = set_args(
                State0#state{
                    stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs
                },
                ArgsForTailCall
            ),
            State3 = tail_call_with_jit_state_registers_only(State1, Temp)
    end,
    State3#state{available_regs = ?AVAILABLE_REGS, used_regs = []}.

%%-----------------------------------------------------------------------------
%% @doc Tail call to address in register, restoring prolog registers including
%% jit_state in r1. Only use when target function expects jit_state as second parameter.
%% Function prolog saves: push {r1,r4,r5,r6,r7,lr}
%% @end
%% @param State current backend state
%% @param Reg register containing the target address
%% @return Updated backend state
%%-----------------------------------------------------------------------------
tail_call_with_jit_state_registers_only(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    Reg
) ->
    % Standard tail call for 4 or fewer arguments
    % First restore LR from stack (so target function can return properly)
    % Choose temp register to avoid conflict with Reg
    TempReg =
        case Reg of
            r7 -> r6;
            _ -> r7
        end,
    % Load saved LR to temp
    RestoreLRToTemp = jit_armv6m_asm:ldr(TempReg, {sp, 20}),
    % Store function pointer (pipeline friendly)
    OverwriteLR = jit_armv6m_asm:str(Reg, {sp, 20}),
    % Move saved LR to LR register
    RestoreLR = jit_armv6m_asm:mov(lr, TempReg),
    % Pop prolog registers: {r1,r4,r5,r6,r7,lr} where lr is now target address
    % This restores jit_state in r1 and branches to target via pc
    PopCode = jit_armv6m_asm:pop([r1, r4, r5, r6, r7, pc]),

    Code = <<RestoreLRToTemp/binary, OverwriteLR/binary, RestoreLR/binary, PopCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

tail_call_with_jit_state_stack(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    FuncPtrReg,
    NumArgs
) when NumArgs >= 5 ->
    % Tail call with 5 or 6 arguments - need to handle 5th (and 6th) stack parameters
    % 5th argument is in r5, 6th argument (if present) is in r6, function pointer in FuncPtrReg
    % Restore lr first (using r7 as temp since r6 might contain 6th arg), then r7, then r6

    % Load lr value to r7 (temp)
    LoadLRtoR7 = jit_armv6m_asm:ldr(r7, {sp, 20}),
    % Move to lr
    MoveLR = jit_armv6m_asm:mov(lr, r7),
    % Restore r7 from stack
    RestoreR7 = jit_armv6m_asm:ldr(r7, {sp, 16}),
    % Store 5th arg where r7 was
    Store5thArg = jit_armv6m_asm:str(r5, {sp, 16}),
    % Store function ptr where lr was
    StoreFuncPtr = jit_armv6m_asm:str(FuncPtrReg, {sp, 20}),

    % Handle 6th argument if present (NumArgs == 6)
    {Store6thArg, RestoreR6, PopAndJump} =
        case NumArgs of
            5 ->
                % For 5 args: restore r6 from stack, pop r1,r4,r5,pc
                RestoreR6_5 = jit_armv6m_asm:ldr(r6, {sp, 12}),
                PopAndJump_5 = jit_armv6m_asm:pop([r1, r4, r5, pc]),
                {<<>>, RestoreR6_5, PopAndJump_5};
            6 ->
                % For 6 args: store r6 (6th arg) where r6 was saved, pop r1,r4,r5,r6,pc
                Store6thArg_6 = jit_armv6m_asm:str(r6, {sp, 12}),
                PopAndJump_6 = jit_armv6m_asm:pop([r1, r4, r5, r6, pc]),
                {Store6thArg_6, <<>>, PopAndJump_6}
        end,

    Code =
        <<LoadLRtoR7/binary, MoveLR/binary, RestoreR7/binary, Store5thArg/binary,
            StoreFuncPtr/binary, Store6thArg/binary, RestoreR6/binary, PopAndJump/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

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
    I1 = jit_armv6m_asm:cmp(Reg, ?CTX_REG),
    I3 =
        case Reg of
            % Return value is already in r0
            r0 -> <<>>;
            % Move to r0 (return register)
            _ -> jit_armv6m_asm:mov(r0, Reg)
        end,
    I4 = jit_armv6m_asm:pop([r1, r4, r5, r6, r7, pc]),
    I2 = jit_armv6m_asm:bcc(eq, 2 + byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    {AvailableRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, UsedRegs0, Reg
    ),
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
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State, Label
) ->
    Offset = StreamModule:offset(Stream0),
    % Placeholder offset, will be patched
    I1 = jit_armv6m_asm:b(0),
    Reloc = {Label, Offset, b},
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

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
            NewBranchInstr = jit_armv6m_asm:bcc(CC, BranchOffset),
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
    NewBranchInstr = jit_armv6m_asm:bcc(CC, BranchOffset),
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
    ElseJumpInstr = jit_armv6m_asm:b(0),
    Stream3 = StreamModule:append(Stream2, ElseJumpInstr),
    %% Else block starts here.
    OffsetAfter = StreamModule:offset(Stream3),
    %% Patch the conditional branch to jump to the else block
    ElseBranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = jit_armv6m_asm:bcc(CC, ElseBranchOffset),
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
    NewElseJumpInstr = jit_armv6m_asm:b(FinalJumpOffset),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs).

-spec if_block_cond(state(), condition()) ->
    {
        state(),
        jit_armv6m_asm:cc() | {tbz | tbnz, atom(), 0..63} | {cbz, atom()},
        non_neg_integer()
    }.
if_block_cond(#state{stream_module = StreamModule, stream = Stream0} = State0, {Reg, '<', 0}) ->
    %% Compare register with 0
    I1 = jit_armv6m_asm:cmp(Reg, 0),
    %% Branch if positive (N flag clear)
    I2 = jit_armv6m_asm:bcc(pl, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State1 = State0#state{stream = Stream1},
    {State1, pl, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {Reg, '<', Val}
) when is_atom(Reg), is_integer(Val) ->
    I1 = jit_armv6m_asm:cmp(Reg, Val),
    % ge = greater than or equal
    I2 = jit_armv6m_asm:bcc(ge, 0),
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
    I1 = jit_armv6m_asm:cmp(Reg, RegB),
    % ge = greater than or equal
    I2 = jit_armv6m_asm:bcc(ge, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, ge, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Compare register with 0
    I1 = jit_armv6m_asm:cmp(Reg, 0),
    %% Branch if not equal
    I2 = jit_armv6m_asm:bcc(ne, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
%% Delegate (int) forms to regular forms since we only have 32-bit words
if_block_cond(State, {'(int)', RegOrTuple, '==', 0}) ->
    if_block_cond(State, {RegOrTuple, '==', 0});
if_block_cond(State, {'(int)', RegOrTuple, '==', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '==', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_armv6m_asm:cmp(Reg, Val),
    I2 = jit_armv6m_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)};
if_block_cond(State, {'(int)', RegOrTuple, '!=', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '!=', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_armv6m_asm:cmp(Reg, Val),
    I2 = jit_armv6m_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test bit 0: shift bit 0 to MSB and branch if positive (bit was 0/false)
    I1 = jit_armv6m_asm:lsls(Temp, Reg, 31),
    % branch if negative (bit was 1/true)
    I2 = jit_armv6m_asm:bcc(mi, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, mi, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test bit 0: shift bit 0 to MSB and branch if negative (bit was 1/true)
    I1 = jit_armv6m_asm:lsls(Temp, Reg, 31),
    % branch if positive (bit was 0/false)
    I2 = jit_armv6m_asm:bcc(pl, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, pl, byte_size(I1)};
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
    % Test bits - optimize for low bits masks that can use lsls
    {TestCode, BranchCond} =
        case bit_test_optimization(Val) of
            {low_bits_mask, BitCount} ->
                % Low bits mask: use lsls to shift high bits away
                ShiftAmount = 32 - BitCount,
                TestCode0 = jit_armv6m_asm:lsls(Temp, Reg, ShiftAmount),
                % branch if not zero (any low bit was set)
                {TestCode0, ne};
            no_optimization ->
                % General case: use mov+tst
                TestCode0 = jit_armv6m_asm:movs(Temp, Val),
                TestCode1 = jit_armv6m_asm:tst(Reg, Temp),
                {<<TestCode0/binary, TestCode1/binary>>, eq}
        end,
    I2 = jit_armv6m_asm:bcc(BranchCond, 0),
    Code = <<TestCode/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, BranchCond, byte_size(TestCode)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {Reg, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    % Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = jit_armv6m_asm:mvns(Temp, Reg),
    % 32 - 4
    I2 = jit_armv6m_asm:lsls(Temp, Temp, 28),
    I3 = jit_armv6m_asm:bcc(eq, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State1 = State0#state{stream = Stream1},
    {State1, eq, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    % Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = jit_armv6m_asm:mvns(Reg, Reg),
    % 32 - 4
    I2 = jit_armv6m_asm:lsls(Reg, Reg, 28),
    I3 = jit_armv6m_asm:bcc(eq, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State1 = State0#state{stream = Stream1},
    State2 = if_block_free_reg(RegTuple, State1),
    {State2, eq, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | AT]
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    % AND with mask
    OffsetBefore = StreamModule:offset(Stream0),
    I1 = jit_armv6m_asm:mov(Temp, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State1 = State0#state{stream = Stream1},
    State2 = and_(State1#state{available_regs = AT}, Temp, Mask),
    Stream2 = State2#state.stream,
    % Compare with value
    I2 = jit_armv6m_asm:cmp(Temp, Val),
    Stream3 = StreamModule:append(Stream2, I2),
    OffsetAfter = StreamModule:offset(Stream3),
    I3 = jit_armv6m_asm:bcc(eq, 0),
    Stream4 = StreamModule:append(Stream3, I3),
    State3 = State2#state{stream = Stream4, available_regs = [Temp | State2#state.available_regs]},
    {State3, eq, OffsetAfter - OffsetBefore};
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
    I2 = jit_armv6m_asm:cmp(Reg, Val),
    Stream2 = StreamModule:append(Stream1, I2),
    OffsetAfter = StreamModule:offset(Stream2),
    I3 = jit_armv6m_asm:bcc(eq, 0),
    Stream3 = StreamModule:append(Stream2, I3),
    State3 = State1#state{stream = Stream3},
    State4 = if_block_free_reg(RegTuple, State3),
    {State4, eq, OffsetAfter - OffsetBefore}.

-spec if_block_free_reg(armv6m_register() | {free, armv6m_register()}, state()) -> state().
if_block_free_reg({free, Reg}, State0) ->
    #state{available_regs = AvR0, used_regs = UR0} = State0,
    {AvR1, UR1} = free_reg(AvR0, UR0, Reg),
    State0#state{
        available_regs = AvR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

%% Helper function to determine if a bit test can be optimized using lsls
-spec bit_test_optimization(non_neg_integer()) ->
    {low_bits_mask, non_neg_integer()} | no_optimization.
% ?TERM_PRIMARY_MASK
bit_test_optimization(16#3) -> {low_bits_mask, 2};
%
bit_test_optimization(16#7) -> {low_bits_mask, 3};
% ?TERM_IMMED_TAG_MASK
bit_test_optimization(16#F) -> {low_bits_mask, 4};
% ?TERM_BOXED_TAG_MASK or ?TERM_IMMED2_TAG_MASK
bit_test_optimization(16#3F) -> {low_bits_mask, 6};
bit_test_optimization(_) -> no_optimization.

-spec merge_used_regs(state(), [armv6m_register()]) -> state().
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
    I = jit_armv6m_asm:lsrs(Reg, Reg, Shift),
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
    I = jit_armv6m_asm:lsls(Reg, Reg, Shift),
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
-spec call_func_ptr(state(), {free, armv6m_register()}, [arg()]) ->
    {state(), armv6m_register()}.
call_func_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State0,
    {free, FuncPtrReg},
    Args
) ->
    FreeRegs = lists:flatmap(
        fun
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [{free, FuncPtrReg} | Args]
    ),
    UsedRegs1 = UsedRegs0 -- FreeRegs,
    SavedRegs = [?CTX_REG, ?NATIVE_INTERFACE_REG | UsedRegs1],
    {_SavedRegsOdd, Stream1} = push_registers(SavedRegs, StreamModule, Stream0),

    % Set up arguments following AArch64 calling convention
    State1 = set_args(State0#state{stream = Stream1}, Args),
    #state{stream = Stream2} = State1,

    % Call the function pointer (using BLX for call with return)
    Call = jit_armv6m_asm:blx(FuncPtrReg),
    Stream4 = StreamModule:append(Stream2, Call),

    % If r0 is in used regs, save it to another temporary register
    FreeGPRegs = FreeRegs -- (FreeRegs -- ?AVAILABLE_REGS),
    AvailableRegs1 = FreeGPRegs ++ AvailableRegs0,
    {Stream5, ResultReg} =
        case lists:member(r0, SavedRegs) of
            true ->
                [Temp | _] = AvailableRegs1,
                {StreamModule:append(Stream4, jit_armv6m_asm:mov(Temp, r0)), Temp};
            false ->
                {Stream4, r0}
        end,

    Stream6 = pop_registers(lists:reverse(SavedRegs), StreamModule, Stream5),

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

push_registers(SavedRegs, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    IsOdd = (length(SavedRegs) rem 2) =:= 1,
    Stream1 = StreamModule:append(Stream0, jit_armv6m_asm:push(SavedRegs)),
    {IsOdd, Stream1};
push_registers([], _StreamModule, Stream0) ->
    {false, Stream0}.

pop_registers(SavedRegs, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    Stream1 = StreamModule:append(Stream0, jit_armv6m_asm:pop(SavedRegs)),
    Stream1;
pop_registers([], _StreamModule, Stream0) ->
    Stream0.

-spec set_args(state(), [arg()]) -> state().
% Handle 5 parameters: handle 5th on stack first, then first 4 in registers r0-r3
set_args(State, [Arg1, Arg2, Arg3, Arg4, Arg5]) ->
    % Handle 5th argument on stack first (with alignment) - this may free registers
    State1 = set_args_push_stack(State, Arg5, undefined),
    % Then set up first 4 arguments in registers using existing logic
    set_args_registers_only(State1, [Arg1, Arg2, Arg3, Arg4]);
% Handle 6 parameters: handle 5th and 6th on stack first, then first 4 in registers r0-r3
set_args(State, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6]) ->
    % Handle 5th and 6th arguments on stack first (no alignment needed) - this may free registers
    State1 = set_args_push_stack(State, Arg5, Arg6),
    % Then set up first 4 arguments in registers using existing logic
    set_args_registers_only(State1, [Arg1, Arg2, Arg3, Arg4]);
% Handle up to 4 parameters: all in registers r0-r3
set_args(State, Args) when length(Args) =< 4 ->
    set_args_registers_only(State, Args).

%% @doc Handle 5th and optionally 6th arguments on stack.
%% For 5 args: push 5th arg at sp+0 with 4-byte padding at sp+4 for 8-byte alignment
%% For 6 args: push 5th arg at sp+0, 6th arg at sp+4 (2×4 bytes = 8-byte aligned, no padding)
set_args_push_stack(
    #state{stream_module = StreamModule, stream = Stream0} = State0, Arg5, Arg6
) ->
    % Decrement stack pointer by 8 bytes once
    I1 = jit_armv6m_asm:sub(sp, sp, 8),
    Stream1 = StreamModule:append(Stream0, I1),

    % Handle Arg6 if present (goes at sp+4)
    State1 =
        case Arg6 of
            undefined ->
                % 5 arguments: no 6th arg to handle
                State0#state{stream = Stream1};
            {free, Reg6} when is_atom(Reg6) ->
                % 6 arguments: Arg6 is already in native register, store directly and free
                I2 = jit_armv6m_asm:str(Reg6, {sp, 4}),
                StreamB = StreamModule:append(Stream1, I2),
                free_native_register(State0#state{stream = StreamB}, Reg6);
            _ ->
                % 6 arguments: store Arg6 at sp+4
                % Handle {free, NonNativeReg} by unwrapping
                ActualArg6 =
                    case Arg6 of
                        {free, InnerArg6} -> InnerArg6;
                        Other6 -> Other6
                    end,
                {StateA, Reg6} = move_to_native_register(
                    State0#state{stream = Stream1}, ActualArg6
                ),
                StreamA = StateA#state.stream,
                I2 = jit_armv6m_asm:str(Reg6, {sp, 4}),
                StreamB = StreamModule:append(StreamA, I2),
                free_native_register(StateA#state{stream = StreamB}, Reg6)
        end,

    % Handle Arg5 (always present, always goes at sp+0)
    State2 =
        case Arg5 of
            {free, Reg5} when is_atom(Reg5) ->
                % Arg5 is already in native register, store directly and free
                I3 = jit_armv6m_asm:str(Reg5, {sp, 0}),
                Stream3 = StreamModule:append(State1#state.stream, I3),
                free_native_register(State1#state{stream = Stream3}, Reg5);
            _ ->
                % Move Arg5 to register, store, and free
                % Handle {free, NonNativeReg} by unwrapping
                ActualArg5 =
                    case Arg5 of
                        {free, InnerArg5} -> InnerArg5;
                        Other5 -> Other5
                    end,
                {StateTemp, Reg5} = move_to_native_register(State1, ActualArg5),
                StreamTemp = StateTemp#state.stream,
                I3 = jit_armv6m_asm:str(Reg5, {sp, 0}),
                Stream3 = StreamModule:append(StreamTemp, I3),
                free_native_register(StateTemp#state{stream = Stream3}, Reg5)
        end,
    State2.

set_args_registers_only(
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
    State1 = set_args0(State0, Args1, ArgsRegs, ParamRegs, AvailableScratchGP),
    Stream1 = State1#state.stream,
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
    Special =:= ctx orelse Special =:= jit_state orelse Special =:= jit_state_tail_call orelse
        Special =:= offset
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
parameter_regs0([{fp_reg, _} | T], [GPRegA, GPRegB | GPRegsT], Acc) ->
    parameter_regs0(T, GPRegsT, [GPRegB, GPRegA | Acc]);
parameter_regs0([Int | T], [GPReg | GPRegsT], Acc) when is_integer(Int) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
% Handle stack parameters when we run out of registers
parameter_regs0([_Arg | T], [], Acc) ->
    parameter_regs0(T, [], [stack | Acc]).

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

set_args0(State, [], [], [], _AvailGP) ->
    State;
set_args0(State, [{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP) ->
    set_args0(State, [FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP);
set_args0(State, [ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP) ->
    set_args0(State, ArgsT, ArgsRegs, ParamRegs, AvailGP);
set_args0(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    [jit_state | ArgsT],
    [jit_state | ArgsRegs],
    [ParamReg | ParamRegs],
    AvailGP
) ->
    false = lists:member(ParamReg, ArgsRegs),
    % jit_state is saved on stack, load from stack offset
    I = jit_armv6m_asm:ldr(ParamReg, {sp, ?STACK_OFFSET_JITSTATE}),
    Stream1 = StreamModule:append(Stream0, I),
    set_args0(State#state{stream = Stream1}, ArgsT, ArgsRegs, ParamRegs, AvailGP);
set_args0(
    State,
    [jit_state_tail_call | ArgsT],
    [jit_state | ArgsRegs],
    [ParamReg | ParamRegs],
    AvailGP
) ->
    false = lists:member(ParamReg, ArgsRegs),
    % For tail calls, jit_state will be restored by pop - skip generating load instruction
    set_args0(State, ArgsT, ArgsRegs, ParamRegs, AvailGP);
% Handle stack parameters - load argument into temp register and push to stack
set_args0(
    #state{stream_module = StreamModule} = State,
    [Arg | ArgsT],
    [stack | ArgsRegs],
    [stack | ParamRegs],
    [TempReg | _] = AvailGP
) ->
    % Generate code to set up argument in temp register
    State1 = set_args1(State, Arg, TempReg),
    % Decrement stack pointer by 4 bytes and store argument
    DecSP = jit_armv6m_asm:sub(sp, sp, 4),
    StoreInstr = jit_armv6m_asm:str(TempReg, {sp, 0}),
    Stream1 = StreamModule:append(State1#state.stream, <<DecSP/binary, StoreInstr/binary>>),
    set_args0(State1#state{stream = Stream1}, ArgsT, ArgsRegs, ParamRegs, AvailGP);
% ctx is special as we need it to access x_reg/y_reg/fp_reg
set_args0(State, [Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    State1 = set_args1(State, Arg, ?CTX_REG),
    set_args0(State1, ArgsT, ArgsRegs, ParamRegs, AvailGP);
set_args0(
    #state{stream_module = StreamModule} = State,
    [Arg | ArgsT],
    [_ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    [Avail | AvailGPT] = AvailGP
) ->
    State1 = set_args1(State, Arg, ParamReg),
    case lists:member(ParamReg, ArgsRegs) of
        false ->
            set_args0(State1, ArgsT, ArgsRegs, ParamRegs, AvailGP);
        true ->
            I = jit_armv6m_asm:mov(Avail, ParamReg),
            Stream1 = StreamModule:append(State1#state.stream, I),
            NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
            set_args0(State1#state{stream = Stream1}, NewArgsT, ArgsRegs, ParamRegs, AvailGPT)
    end.

set_args1(State, Reg, Reg) ->
    State;
set_args1(#state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, extra}, Reg) ->
    I = jit_armv6m_asm:ldr(Reg, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_args1(#state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, Reg) ->
    I = jit_armv6m_asm:ldr(Reg, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_args1(#state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Source}, Reg) ->
    I = jit_armv6m_asm:ldr(Reg, {Source, 0}),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_args1(#state{stream_module = StreamModule, stream = Stream0} = State, {y_reg, X}, Reg) ->
    I1 = jit_armv6m_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_armv6m_asm:ldr(Reg, {Reg, X * 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
set_args1(#state{stream_module = StreamModule, stream = Stream0} = State, ArgReg, Reg) when
    ?IS_GPR(ArgReg)
->
    I = jit_armv6m_asm:mov(Reg, ArgReg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_args1(State, Arg, Reg) when is_integer(Arg) ->
    mov_immediate(State, Reg, Arg).

%%-----------------------------------------------------------------------------
%% @doc Emit a move to a vm register (x_reg, y_reg, fpreg or a pointer on x_reg)
%% from an immediate, a native register or another vm register.
%% @end
%% @param State current backend state
%% @param Src value to move to vm register
%% @param Dest vm register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_vm_register(state(), Src :: value() | vm_register(), Dest :: vm_register()) ->
    state().
% Native register to VM register
move_to_vm_register(State0, Src, {x_reg, extra}) when is_atom(Src) ->
    I1 = jit_armv6m_asm:str(Src, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {x_reg, X}) when is_atom(Src) ->
    I1 = jit_armv6m_asm:str(Src, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_armv6m_asm:str(Src, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State0, Src, {y_reg, Y}) when
    is_atom(Src)
->
    I1 = jit_armv6m_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_armv6m_asm:str(Src, {Temp, Y * 4}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1};
% Source is an integer to y_reg (optimized: ldr first, then movs)
move_to_vm_register(#state{available_regs = [Temp1, Temp2 | _]} = State0, N, {y_reg, Y}) when
    is_integer(N), N >= 0, N =< 255
->
    I1 = jit_armv6m_asm:ldr(Temp1, ?Y_REGS),
    I2 = jit_armv6m_asm:movs(Temp2, N),
    I3 = jit_armv6m_asm:str(Temp2, {Temp1, Y * 4}),
    Stream1 = (State0#state.stream_module):append(
        State0#state.stream, <<I1/binary, I2/binary, I3/binary>>
    ),
    State0#state{stream = Stream1};
% Source is an integer (0-255 for movs, negative values need different handling)
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, N, Dest) when
    is_integer(N), N >= 0, N =< 255
->
    I1 = jit_armv6m_asm:movs(Temp, N),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
%% Handle large values using simple literal pool (branch-over pattern)
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, N, Dest) when
    is_integer(N)
->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, N),
    State2 = move_to_vm_register(State1, Temp, Dest),
    State2#state{available_regs = AR0};
% Source is a VM register
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, extra}, Dest) ->
    I1 = jit_armv6m_asm:ldr(Temp, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, X}, Dest) ->
    I1 = jit_armv6m_asm:ldr(Temp, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {ptr, Reg}, Dest) ->
    I1 = jit_armv6m_asm:ldr(Temp, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {y_reg, Y}, Dest) ->
    I1 = jit_armv6m_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_armv6m_asm:ldr(Temp, {Temp, Y * 4}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(
    #state{stream_module = StreamModule, available_regs = [Temp | _], stream = Stream0} = State,
    {fp, RegA, RegB},
    {fp_reg, F}
) ->
    I1 = jit_armv6m_asm:ldr(Temp, ?FP_REGS),
    I2 = jit_armv6m_asm:str(RegA, {Temp, F * 8}),
    I2 = jit_armv6m_asm:str(RegB, {Temp, F * 8 + 4}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

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
    armv6m_register(),
    non_neg_integer() | armv6m_register(),
    vm_register() | armv6m_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_armv6m_asm:ldr(Temp, {Reg, Index * 4}),
    I2 = jit_armv6m_asm:str(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_armv6m_asm:ldr(Temp, {Reg, Index * 4}),
    I2 = jit_armv6m_asm:str(Temp, {Dest, 0}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | _]} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_armv6m_asm:ldr(Temp1, ?Y_REGS),
    I2 = jit_armv6m_asm:ldr(Temp2, {Reg, Index * 4}),
    I3 = jit_armv6m_asm:str(Temp2, {Temp1, Y * 4}),
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
    I1 = jit_armv6m_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_armv6m_asm:ldr(Reg, {Reg, Index * 4}),
    I3 = jit_armv6m_asm:str(Reg, {Temp, Y * 4}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_armv6m_asm:ldr(Dest, {Reg, Index * 4}),
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
    I1 = jit_armv6m_asm:lsls(IndexReg, IndexReg, 2),
    I2 = jit_armv6m_asm:ldr(IndexReg, {Reg, IndexReg}),
    I3 = jit_armv6m_asm:str(IndexReg, ?X_REG(X)),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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
    I1 = jit_armv6m_asm:lsls(IndexReg, IndexReg, 2),
    I2 = jit_armv6m_asm:ldr(IndexReg, {Reg, IndexReg}),
    I3 = jit_armv6m_asm:str(IndexReg, {PtrReg, 0}),
    {AvailableRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, UsedRegs0, IndexReg
    ),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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
) when is_atom(IndexReg) ->
    I1 = jit_armv6m_asm:lsls(IndexReg, IndexReg, 2),
    I2 = jit_armv6m_asm:ldr(Temp, ?Y_REGS),
    I3 = jit_armv6m_asm:ldr(IndexReg, {Reg, IndexReg}),
    I4 = jit_armv6m_asm:str(IndexReg, {Temp, Y * 4}),
    {AvailableRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, UsedRegs0, IndexReg
    ),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>
    ),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    }.

%% @doc move reg[x] to a vm or native register
-spec get_array_element(state(), armv6m_register(), non_neg_integer()) ->
    {state(), armv6m_register()}.
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
    I1 = jit_armv6m_asm:ldr(ElemReg, {Reg, Index * 4}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    {
        State#state{
            stream = Stream1, available_regs = AvailableT, used_regs = [ElemReg | UsedRegs0]
        },
        ElemReg
    }.

%% @doc move an integer, a vm or native register to reg[x]
-spec move_to_array_element(
    state(), integer() | vm_register() | armv6m_register(), armv6m_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_armv6m_asm:str(ValueReg, {Reg, Index * 4}),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    ValueReg,
    Reg,
    IndexReg
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_armv6m_asm:mov(Temp, IndexReg),
    I2 = jit_armv6m_asm:lsls(Temp, Temp, 2),
    I3 = jit_armv6m_asm:str(ValueReg, {Reg, Temp}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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

move_to_array_element(
    State,
    Value,
    BaseReg,
    IndexReg,
    Offset
) when is_integer(IndexReg) andalso is_integer(Offset) andalso Offset div 8 =:= 0 ->
    move_to_array_element(State, Value, BaseReg, IndexReg + (Offset div 8));
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    ValueReg,
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_armv6m_asm:adds(Temp, IndexReg, Offset),
    I2 = jit_armv6m_asm:lsls(Temp, Temp, 2),
    I3 = jit_armv6m_asm:str(ValueReg, {BaseReg, Temp}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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
    I1 = jit_armv6m_asm:adds(Temp, IndexReg, Offset),
    I2 = jit_armv6m_asm:lsls(Temp, Temp, 2),
    I3 = jit_armv6m_asm:str(ValueReg, {BaseReg, Temp}),
    Stream1 = (State1#state.stream_module):append(
        State1#state.stream, <<I1/binary, I2/binary, I3/binary>>
    ),
    State2 = State1#state{stream = Stream1},
    free_native_register(State2, ValueReg).

-spec move_to_native_register(state(), value()) -> {state(), armv6m_register()}.
move_to_native_register(State, Reg) when is_atom(Reg) ->
    {State, Reg};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}
) when is_atom(Reg) ->
    I1 = jit_armv6m_asm:ldr(Reg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1}, Reg};
move_to_native_register(
    #state{
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State0,
    Imm
) when
    is_integer(Imm)
->
    State1 = State0#state{used_regs = [Reg | Used], available_regs = AvailT},
    {move_to_native_register(State1, Imm, Reg), Reg};
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
    I1 = jit_armv6m_asm:ldr(Reg, ?X_REG(X)),
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
    I1 = jit_armv6m_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_armv6m_asm:ldr(Reg, {Reg, Y * 4}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [Reg | Used]}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [RegA, RegB | AvailT],
        used_regs = Used
    } = State,
    {fp_reg, F}
) ->
    I1 = jit_armv6m_asm:ldr(RegB, ?FP_REGS),
    I2 = jit_armv6m_asm:ldr(RegA, {RegB, F * 8}),
    I3 = jit_armv6m_asm:ldr(RegB, {RegB, F * 8 + 4}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_regs = AvailT, used_regs = [RegB, RegA | Used]},
        {fp, RegA, RegB}
    }.

-spec move_to_native_register(state(), value(), armv6m_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, RegSrc, RegDst
) when is_atom(RegSrc) ->
    I = jit_armv6m_asm:mov(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
move_to_native_register(State, ValSrc, RegDst) when is_integer(ValSrc) ->
    mov_immediate(State, RegDst, ValSrc);
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}, RegDst
) when ?IS_GPR(Reg) ->
    I1 = jit_armv6m_asm:ldr(RegDst, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    I1 = jit_armv6m_asm:ldr(RegDst, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {y_reg, Y}, RegDst
) ->
    I1 = jit_armv6m_asm:ldr(RegDst, ?Y_REGS),
    I2 = jit_armv6m_asm:ldr(RegDst, {RegDst, Y * 4}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {fp_reg, F},
    {fp, RegA, RegB}
) ->
    I1 = jit_armv6m_asm:ldr(RegB, ?FP_REGS),
    I2 = jit_armv6m_asm:ldr(RegA, {RegB, F * 8}),
    I3 = jit_armv6m_asm:ldr(RegB, {RegB, F * 8 + 4}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec copy_to_native_register(state(), value()) -> {state(), armv6m_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [SaveReg | AvailT],
        used_regs = Used
    } = State,
    Reg
) when is_atom(Reg) ->
    I1 = jit_armv6m_asm:mov(SaveReg, Reg),
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
    I1 = jit_armv6m_asm:ldr(SaveReg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    {y_reg, Y}
) ->
    I1 = jit_armv6m_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_armv6m_asm:ldr(Reg, {Reg, Y * 4}),
    I3 = jit_armv6m_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    Offset
) ->
    I1 = jit_armv6m_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_armv6m_asm:adds(Reg, Offset * 4),
    I3 = jit_armv6m_asm:str(Reg, ?Y_REGS),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

set_continuation_to_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp1, Temp2 | _]
    } = State,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    % Calculate jump table entry offset
    JumpTableEntryOffset = Label * 4,

    % Assume mov_immediate will be at most 10 bytes
    MaxMovImmediateSize = 10,
    EstimatedAdrOffset = Offset + MaxMovImmediateSize,
    % +4 for adr base, +4 for minimum adr offset
    EstimatedAdrPC = (EstimatedAdrOffset band (bnot 3)) + 4 + 4,
    RelativeOffset = JumpTableEntryOffset - EstimatedAdrPC,

    % Generate mov_immediate with the relative offset
    State1 = mov_immediate(State, Temp2, RelativeOffset),
    Stream1 = State1#state.stream,
    ActualMovImmediateSize = StreamModule:offset(Stream1) - Offset,

    % Calculate where adr instruction will actually be
    ActualAdrOffset = Offset + ActualMovImmediateSize,
    ActualAdrPC = (ActualAdrOffset band (bnot 3)) + 4,

    % Calculate the correct adr offset: ActualAdrPC + (AdrOffset - 4) + RelativeOffset = JumpTableEntryOffset
    % So: AdrOffset = JumpTableEntryOffset - ActualAdrPC - RelativeOffset + 4
    AdrOffset = JumpTableEntryOffset - ActualAdrPC - RelativeOffset + 4,
    % Ensure adr offset is multiple of 4 and within range
    AdrOffset = ((AdrOffset + 3) div 4) * 4,

    % Get PC address using adr
    I1 = jit_armv6m_asm:adr(Temp1, AdrOffset),

    % Add PC + offset, load jit_state, and store continuation
    I2 = jit_armv6m_asm:adds(Temp2, Temp2, Temp1),
    I3 = jit_armv6m_asm:ldr(Temp1, {sp, ?STACK_OFFSET_JITSTATE}),
    I4 = jit_armv6m_asm:str(Temp2, ?JITSTATE_CONTINUATION(Temp1)),

    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream2 = StreamModule:append(Stream1, Code),
    State1#state{stream = Stream2}.

%% @doc Set the contination to a given offset
%% Return a reference so the offset will be updated with update_branches
%% This is only used with OP_WAIT_TIMEOUT and the offset is after the current
%% code and not too far, so on Thumb we can use adr instruction.
set_continuation_to_offset(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp, TempJitState | _],
        branches = Branches
    } = State
) ->
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    I1 = jit_armv6m_asm:adr(Temp, 4),
    Reloc = {OffsetRef, Offset, {adr, Temp}},
    % Load jit_state pointer from stack, then store continuation
    I2a = jit_armv6m_asm:ldr(TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    I2b = jit_armv6m_asm:str(Temp, ?JITSTATE_CONTINUATION(TempJitState)),
    Code = <<I1/binary, I2a/binary, I2b/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | Branches]}, OffsetRef}.

%% @doc Implement a continuation entry point.
%% TODO: push r4-r7 and lr
-spec continuation_entry_point(#state{}) -> #state{}.
continuation_entry_point(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State
) ->
    % Align if required.
    Offset = StreamModule:offset(Stream0),
    Stream1 =
        case Offset rem 4 of
            0 -> Stream0;
            2 -> StreamModule:append(Stream0, <<0:16>>)
        end,
    Prolog = jit_armv6m_asm:push([r1, r4, r5, r6, r7, lr]),
    Stream2 = StreamModule:append(Stream1, Prolog),
    State#state{stream = Stream2}.

get_module_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg, TempJitState | AvailableT],
        used_regs = UsedRegs0
    } = State
) ->
    % Load jit_state pointer from stack, then load module
    I1a = jit_armv6m_asm:ldr(TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    I1b = jit_armv6m_asm:ldr(Reg, ?JITSTATE_MODULE(TempJitState)),
    I2 = jit_armv6m_asm:ldr(Reg, ?MODULE_INDEX(Reg)),
    Code = <<I1a/binary, I1b/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_regs = AvailableT, used_regs = [Reg | UsedRegs0]},
        Reg
    }.

%% @doc Perform an AND of a register with an immediate.
%% JIT currentl calls this with two values: ?TERM_PRIMARY_CLEAR_MASK (-4) to
%% clear bits and ?TERM_BOXED_TAG_MASK (0x3F). We can avoid any literal pool
%% by using BICS for -4.
and_(
    #state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0,
    Reg,
    Val
) when Val < 0 andalso Val >= -256 ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, bnot (Val)),
    Stream1 = State1#state.stream,
    I = jit_armv6m_asm:bics(Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{available_regs = [Temp | AT], stream = Stream2};
and_(
    #state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0,
    Reg,
    Val
) ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_armv6m_asm:ands(Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{available_regs = [Temp | AT], stream = Stream2}.

or_(
    #state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0,
    Reg,
    Val
) ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_armv6m_asm:orrs(Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{available_regs = [Temp | AT], stream = Stream2}.

add(#state{stream_module = StreamModule, stream = Stream0} = State0, Reg, Val) ->
    try jit_armv6m_asm:adds(Reg, Val) of
        I ->
            Stream1 = StreamModule:append(Stream0, I),
            State0#state{stream = Stream1}
    catch
        error:{unencodable_immediate, Val} ->
            [Temp | AT] = State0#state.available_regs,
            State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
            Stream1 = State1#state.stream,
            I = jit_armv6m_asm:adds(Reg, Temp),
            Stream2 = StreamModule:append(Stream1, I),
            State1#state{available_regs = [Temp | AT], stream = Stream2}
    end.

mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) when
    Val >= 0 andalso Val =< 255
->
    I = jit_armv6m_asm:movs(Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) when
    Val >= -255 andalso Val < 0
->
    I1 = jit_armv6m_asm:movs(Reg, -Val),
    I2 = jit_armv6m_asm:negs(Reg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    %% Use a literal pool with a branch instruction (branch-over pattern)
    %% Calculate where literal will be placed (must be word-aligned)
    %% After LDR (2 bytes) + Branch (2 bytes) = 4 bytes from current position
    CurrentOffset = StreamModule:offset(Stream0),
    OffsetAfterInstructions = CurrentOffset + 4,
    %% Find next word-aligned position for literal
    LiteralPosition =
        case OffsetAfterInstructions rem 4 of
            % Already aligned
            0 -> OffsetAfterInstructions;
            % Add 2 bytes padding to align
            _ -> OffsetAfterInstructions + 2
        end,
    PaddingNeeded = LiteralPosition - OffsetAfterInstructions,

    %% Calculate LDR PC-relative offset
    %% PC = (current_instruction_address & ~3) + 4
    LdrInstructionAddr = CurrentOffset,
    LdrPC = (LdrInstructionAddr band (bnot 3)) + 4,
    LiteralOffset = LiteralPosition - LdrPC,

    %% Generate: ldr rTemp, [pc, #LiteralOffset]  ; Load from literal
    I1 = jit_armv6m_asm:ldr(Reg, {pc, LiteralOffset}),
    %% Calculate branch offset
    %% Branch is at CurrentOffset + 2, need to jump past literal
    BranchPosition = CurrentOffset + 2,
    % After the 4-byte literal
    TargetPosition = LiteralPosition + 4,
    BranchOffset = TargetPosition - BranchPosition,
    I2 = jit_armv6m_asm:b(BranchOffset),
    %% Generate padding if needed (just zeros)
    Padding = <<0:(PaddingNeeded * 8)>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, Padding/binary, Val:32/little>>),
    State#state{stream = Stream1}.

sub(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_armv6m_asm:sub(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{available_regs = [Temp | _]} = State, Reg, 3) ->
    I1 = jit_armv6m_asm:lsls(Temp, Reg, 1),
    I2 = jit_armv6m_asm:adds(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 5) ->
    I1 = jit_armv6m_asm:lsls(Temp, Reg, 2),
    I2 = jit_armv6m_asm:adds(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 7) ->
    I1 = jit_armv6m_asm:lsls(Temp, Reg, 3),
    I2 = jit_armv6m_asm:subs(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{available_regs = [Temp | _]} = State, Reg, 9) ->
    I1 = jit_armv6m_asm:lsls(Temp, Reg, 3),
    I2 = jit_armv6m_asm:adds(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 15) ->
    I1 = jit_armv6m_asm:lsls(Temp, Reg, 4),
    I2 = jit_armv6m_asm:subs(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(
    #state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0,
    Reg,
    Val
) ->
    % multiply by decomposing by power of 2
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_armv6m_asm:muls(Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{stream = Stream2, available_regs = [Temp | State1#state.available_regs]}.

%%
%% Analysis of AArch64 pattern and ARM Thumb mapping:
%%
%% AArch64 layout (from call_ext_only_test):
%%   0x0-0x8:  Decrement reductions, store back
%%   0xc:      b.ne 0x20   ; Branch if reductions != 0 to continuation
%%   0x10-0x1c: adr/str/ldr/br sequence for scheduling next process
%%   0x20:     [CONTINUATION POINT] - Actual function starts here
%%
%% ARM Thumb equivalent should be:
%%   0x0-0x6:  Decrement reductions, store back
%%   0x8:      bne continuation_after_prolog ; Branch OVER the prolog if reductions != 0
%%   0xa-0x?:  adr/str/ldr/blx sequence for scheduling
%%   continuation: push {r1,r4-r7,lr}        ; PROLOG (only executed when scheduled)
%%   continuation_after_prolog: [actual function body]
%%
%% Key insight: When reductions != 0, we branch PAST the prolog directly to the function.
%% When reductions == 0, we schedule next process, and when we resume, we execute the prolog
%% then continue to the function body.
%%
-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{
        stream_module = StreamModule, stream = Stream0, available_regs = [Temp, TempJitState | _]
    } = State0
) ->
    % Load jit_state pointer from stack
    I0 = jit_armv6m_asm:ldr(TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    % Load reduction count
    I1 = jit_armv6m_asm:ldr(Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    % Decrement reduction count
    I2 = jit_armv6m_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_armv6m_asm:str(Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    Stream1 = StreamModule:append(Stream0, <<I0/binary, I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch if reduction count is not zero
    I4 = jit_armv6m_asm:bcc(ne, 0),
    % Set continuation to the next instruction
    ADROffset = BNEOffset + byte_size(I4),
    I5 = jit_armv6m_asm:adr(Temp, 4),
    I6 = jit_armv6m_asm:str(Temp, ?JITSTATE_CONTINUATION(TempJitState)),
    % Append the instructions to the stream
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary, I6/binary>>),
    State1 = State0#state{stream = Stream2},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Add the prolog at the continuation point (where scheduled execution resumes)
    #state{stream = Stream3} = State2,
    CurrentOffset = StreamModule:offset(Stream3),
    % Ensure continuation point is 4-byte aligned by adding NOP if necessary
    {AlignedContinuationOffset, Stream3_5} =
        case CurrentOffset rem 4 of
            % Already 4-byte aligned
            0 ->
                {CurrentOffset, Stream3};
            2 ->
                % Add NOP to achieve 4-byte alignment
                NOPPadded = StreamModule:append(Stream3, jit_armv6m_asm:nop()),
                {StreamModule:offset(NOPPadded), NOPPadded};
            _ ->
                error({unexpected_alignment, CurrentOffset})
        end,
    Prolog = jit_armv6m_asm:push([r1, r4, r5, r6, r7, lr]),
    Stream4 = StreamModule:append(Stream3_5, Prolog),
    % Calculate offsets for rewriting
    ContinuationAfterPrologOffset = StreamModule:offset(Stream4),
    % Rewrite the branch to skip over the prolog (branch to continuation_after_prolog)
    NewI4 = jit_armv6m_asm:bcc(ne, ContinuationAfterPrologOffset - BNEOffset),
    % Rewrite the adr to point to the aligned continuation point (prolog location)
    % The ADR instruction uses PC aligned down to 4-byte boundary
    ADRAlignedOffset = ADROffset band (bnot 3),
    ADRImmediate = AlignedContinuationOffset - ADRAlignedOffset,
    NewI5 = jit_armv6m_asm:adr(Temp, ADRImmediate),
    Stream5 = StreamModule:replace(
        Stream4, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    merge_used_regs(State2#state{stream = Stream5}, State1#state.used_regs).

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        available_regs = [Temp, TempJitState | _]
    } = State0,
    Label
) ->
    % Load jit_state pointer from stack
    I0 = jit_armv6m_asm:ldr(TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    % Load reduction count
    I1 = jit_armv6m_asm:ldr(Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    % Decrement reduction count
    I2 = jit_armv6m_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_armv6m_asm:str(Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    Stream1 = StreamModule:append(Stream0, <<I0/binary, I1/binary, I2/binary, I3/binary>>),
    % Use trampoline technique: branch if zero (eq) to skip over the long branch
    % If not zero, we want to continue execution at Label
    % If zero, we want to fall through to scheduling code

    % Skip over the unconditional branch (2 bytes)
    I4 = jit_armv6m_asm:bcc(eq, 4),
    % Unconditional branch to label (will be patched later)
    I5 = jit_armv6m_asm:b(0),
    LongBranchOffset = StreamModule:offset(Stream1) + byte_size(I4),
    LongBranchReloc = {Label, LongBranchOffset, b},
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary>>),
    State1 = State0#state{stream = Stream2, branches = [LongBranchReloc | Branches]},
    State2 = set_continuation_to_label(State1, Label),
    call_primitive_last(State2, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

-spec set_cp(state()) -> {state(), non_neg_integer(), armv6m_register()}.
set_cp(State0) ->
    % get module index (dynamically)
    {
        #state{stream_module = StreamModule, stream = Stream0, available_regs = AvailRegs} = State1,
        Reg
    } = get_module_index(
        State0
    ),
    % Get a temporary register from available registers
    [TempReg | _] = AvailRegs,

    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_armv6m_asm:lsls(Reg, Reg, 24),
    % Emit a single nop as placeholder for offset load instruction
    I2 = jit_armv6m_asm:nop(),
    MOVOffset = Offset + byte_size(I1),
    % OR the module index with the offset (loaded in temp register)
    I3 = jit_armv6m_asm:orrs(Reg, TempReg),
    I4 = jit_armv6m_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, MOVOffset, TempReg}.

-spec rewrite_cp_offset(state(), non_neg_integer(), armv6m_register()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset,
    TempReg
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    OffsetImm = NewOffset bsl 2,

    % Check if offset fits in movs immediate (0-255)
    {NewMoveInstr, Stream1} =
        if
            OffsetImm =< 255 ->
                {jit_armv6m_asm:movs(TempReg, OffsetImm), Stream0};
            true ->
                % Need to emit literal pool with proper alignment
                CurrentOffset = StreamModule:offset(Stream0),
                % Ensure 4-byte alignment for literal pool
                AlignedOffset = (CurrentOffset + 3) band (bnot 3),
                PaddingSize = AlignedOffset - CurrentOffset,
                Padding = <<0:(PaddingSize * 8)>>,

                % Emit the 32-bit literal
                Literal = <<OffsetImm:32/little>>,
                StreamWithLiteral = StreamModule:append(
                    StreamModule:append(Stream0, Padding), Literal
                ),

                % Compute PC-relative offset for ldr instruction
                % PC is (RewriteOffset + 4) aligned to 4-byte boundary, literal is at AlignedOffset
                PCValue = (RewriteOffset + 4 + 3) band (bnot 3),
                PCRelOffset = AlignedOffset - PCValue,
                LdrInstr = jit_armv6m_asm:ldr(TempReg, {pc, PCRelOffset}),
                {LdrInstr, StreamWithLiteral}
        end,
    Stream2 = StreamModule:replace(Stream1, RewriteOffset, NewMoveInstr),
    State0#state{stream = Stream2}.

set_bs(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    TermReg
) ->
    I1 = jit_armv6m_asm:str(TermReg, ?BS),
    I2 = jit_armv6m_asm:movs(Temp, 0),
    I3 = jit_armv6m_asm:str(Temp, ?BS_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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
     || {Label, LabelOffset} <- Labels, is_integer(Label)
    ]),

    % Check if current offset is 4-byte aligned
    CurrentOffset = StreamModule:offset(Stream0),

    {I1, Padding} =
        case CurrentOffset rem 4 of
            0 ->
                % Aligned - use offset 4
                {jit_armv6m_asm:adr(r0, 4), <<>>};
            _ ->
                % Unaligned - use offset 8 with 2-byte padding
                {jit_armv6m_asm:adr(r0, 8), <<0:16>>}
        end,
    I2 = jit_armv6m_asm:bx(lr),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<I1/binary, I2/binary, Padding/binary, (length(SortedLabels)):16, LabelsTable/binary,
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
            (jit_state) -> jit_state;
            (jit_state_tail_call) -> jit_state;
            (stack) -> stack;
            (Reg) when is_atom(Reg) -> Reg;
            (Imm) when is_integer(Imm) -> imm;
            ({ptr, Reg}) -> Reg;
            ({x_reg, _}) -> ?CTX_REG;
            ({y_reg, _}) -> ?CTX_REG;
            ({fp_reg, _}) -> ?CTX_REG;
            ({free, {x_reg, _}}) -> ?CTX_REG;
            ({free, {y_reg, _}}) -> ?CTX_REG;
            ({free, {fp_reg, _}}) -> ?CTX_REG
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
