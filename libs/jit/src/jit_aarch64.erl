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
    flush/1,
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
    jump_to_continuation/2,
    jump_to_offset/2,
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
    div_/3,
    rem_/3,
    shift_right_arith/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3,
    xor_/3
]).

-ifdef(JIT_DWARF).
-export([
    dwarf_opcode/2,
    dwarf_label/2,
    dwarf_function/3,
    dwarf_line/2,
    dwarf_ctx_register/0
]).
-endif.

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").

-ifdef(JIT_DWARF).
-include("jit_dwarf.hrl").
-endif.

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
    jump_table_start :: non_neg_integer(),
    available_regs :: non_neg_integer(),
    used_regs :: non_neg_integer(),
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer(),
    regs :: jit_regs:regs()
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
    | {integer(), '<', maybe_free_aarch64_register()}
    | {maybe_free_aarch64_register(), '==', integer()}
    | {maybe_free_aarch64_register(), '!=', aarch64_register() | integer()}
    | {'(int)', maybe_free_aarch64_register(), '==', integer()}
    | {'(int)', maybe_free_aarch64_register(), '!=', aarch64_register() | integer()}
    | {'(bool)', maybe_free_aarch64_register(), '==', false}
    | {'(bool)', maybe_free_aarch64_register(), '!=', false}
    | {maybe_free_aarch64_register(), '&', non_neg_integer(), '!=', integer()}
    | {{free, aarch64_register()}, '==', {free, aarch64_register()}}.

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

-define(PARAMETER_REGS, [r0, r1, r2, r3, r4, r5]).

-define(REG_BIT_R0, (1 bsl 0)).
-define(REG_BIT_R1, (1 bsl 1)).
-define(REG_BIT_R2, (1 bsl 2)).
-define(REG_BIT_R3, (1 bsl 3)).
-define(REG_BIT_R4, (1 bsl 4)).
-define(REG_BIT_R5, (1 bsl 5)).
-define(REG_BIT_R6, (1 bsl 6)).
-define(REG_BIT_R7, (1 bsl 7)).
-define(REG_BIT_R8, (1 bsl 8)).
-define(REG_BIT_R9, (1 bsl 9)).
-define(REG_BIT_R10, (1 bsl 10)).
-define(REG_BIT_R11, (1 bsl 11)).
-define(REG_BIT_R12, (1 bsl 12)).
-define(REG_BIT_R13, (1 bsl 13)).
-define(REG_BIT_R14, (1 bsl 14)).
-define(REG_BIT_R15, (1 bsl 15)).
-define(REG_BIT_R16, (1 bsl 16)).
-define(REG_BIT_R17, (1 bsl 17)).

-define(AVAILABLE_REGS_MASK,
    (?REG_BIT_R7 bor ?REG_BIT_R8 bor ?REG_BIT_R9 bor ?REG_BIT_R10 bor ?REG_BIT_R11 bor
        ?REG_BIT_R12 bor ?REG_BIT_R13 bor ?REG_BIT_R14 bor ?REG_BIT_R15 bor
        ?REG_BIT_R3 bor ?REG_BIT_R4 bor ?REG_BIT_R5 bor ?REG_BIT_R6)
).
-define(SCRATCH_REGS_MASK,
    (?REG_BIT_R7 bor ?REG_BIT_R8 bor ?REG_BIT_R9 bor ?REG_BIT_R10 bor ?REG_BIT_R11 bor
        ?REG_BIT_R12 bor ?REG_BIT_R13 bor ?REG_BIT_R14 bor ?REG_BIT_R15 bor
        ?REG_BIT_R3 bor ?REG_BIT_R4 bor ?REG_BIT_R5 bor ?REG_BIT_R6 bor ?REG_BIT_R17)
).

-include("jit_backend_dwarf_impl.hrl").

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
        jump_table_start = 0,
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        labels = [],
        variant = Variant,
        regs = jit_regs:new()
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
%% @doc Flush the current state (unused on aarch64)
%% @end
%% @param State current backend state
%% @return The flushed state
%%-----------------------------------------------------------------------------
-spec flush(state()) -> state().
flush(#state{} = State) ->
    State.

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
used_regs(#state{used_regs = Used}) -> mask_to_list(Used).

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [aarch64_register()].
available_regs(#state{available_regs = Available}) -> mask_to_list(Available).

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
    Bit = reg_bit(Reg),
    State#state{
        available_regs = Available0 bor Bit,
        used_regs = Used0 band (bnot Bit)
    };
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
    0 = State#state.used_regs,
    ?AVAILABLE_REGS_MASK = State#state.available_regs,
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
jump_table(#state{stream_module = StreamModule, stream = Stream0} = State, LabelsCount) ->
    JumpTableStart = StreamModule:offset(Stream0),
    jump_table0(State#state{jump_table_start = JumpTableStart}, 0, LabelsCount).

-spec jump_table0(state(), non_neg_integer(), pos_integer()) -> state().
jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    N,
    LabelsCount
) ->
    % Placeholder jumps to next entry
    BranchInstr = jit_aarch64_asm:b(4),
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    jump_table0(State#state{stream = Stream1}, N + 1, LabelsCount).

%%-----------------------------------------------------------------------------
%% @doc Patch a single branch in the stream
%% @end
%% @param StreamModule stream module
%% @param Stream stream state
%% @param Offset offset of the branch to patch
%% @param Type type of the branch
%% @param LabelOffset target label offset
%% @return Updated stream
%%-----------------------------------------------------------------------------
-spec patch_branch(module(), stream(), non_neg_integer(), any(), non_neg_integer()) -> stream().
patch_branch(StreamModule, Stream, Offset, Type, LabelOffset) ->
    Rel = LabelOffset - Offset,
    NewInstr =
        case Type of
            {bcc, CC} -> jit_aarch64_asm:bcc(CC, Rel);
            {adr, Reg} -> jit_aarch64_asm:adr(Reg, Rel);
            b -> jit_aarch64_asm:b(Rel)
        end,
    StreamModule:replace(Stream, Offset, NewInstr).

%%-----------------------------------------------------------------------------
%% @doc Patch all branches targeting a specific label and return remaining branches
%% @end
%% @param StreamModule stream module
%% @param Stream stream state
%% @param TargetLabel label to patch branches for
%% @param LabelOffset offset of the target label
%% @param Branches list of pending branches
%% @return {UpdatedStream, RemainingBranches}
%%-----------------------------------------------------------------------------
-spec patch_branches_for_label(
    module(),
    stream(),
    integer(),
    non_neg_integer(),
    [{integer(), non_neg_integer(), any()}]
) -> {stream(), [{integer(), non_neg_integer(), any()}]}.
patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Branches) ->
    patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Branches, []).

patch_branches_for_label(_StreamModule, Stream, _TargetLabel, _LabelOffset, [], Acc) ->
    {Stream, lists:reverse(Acc)};
patch_branches_for_label(
    StreamModule,
    Stream0,
    TargetLabel,
    LabelOffset,
    [{Label, Offset, Type} | Rest],
    Acc
) when Label =:= TargetLabel ->
    Stream1 = patch_branch(StreamModule, Stream0, Offset, Type, LabelOffset),
    patch_branches_for_label(StreamModule, Stream1, TargetLabel, LabelOffset, Rest, Acc);
patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, [Branch | Rest], Acc) ->
    patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Rest, [Branch | Acc]).

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
    Stream1 = patch_branch(StreamModule, Stream0, Offset, Type, LabelOffset),
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
    ArgsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ScratchMask = ?AVAILABLE_REGS_MASK band (bnot (ArgsMask bor ParamMask)),
    Temp = first_avail(ScratchMask),
    TempBit = reg_bit(Temp),
    AvailableRegs1 = ScratchMask band (bnot TempBit),
    UsedRegs = ?AVAILABLE_REGS_MASK band (bnot AvailableRegs1),
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
            stream = Stream1,
            available_regs = AvailableRegs1,
            used_regs = UsedRegs,
            regs = jit_regs:invalidate_reg(State0#state.regs, Temp)
        },
        Args
    ),
    #state{stream = Stream2} = State1,
    Call = jit_aarch64_asm:br(Temp),
    Stream3 = StreamModule:append(Stream2, Call),
    State1#state{
        stream = Stream3,
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State1#state.regs)
    }.

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
    Bit = reg_bit(Reg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
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

jump_to_offset(#state{stream_module = StreamModule, stream = Stream0} = State, TargetOffset) ->
    Offset = StreamModule:offset(Stream0),
    Rel = TargetOffset - Offset,
    I1 = jit_aarch64_asm:b(Rel),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Jump to a continuation address stored in a register.
%% This is used for optimized intra-module returns.
%% @end
%% @param State current backend state
%% @param OffsetReg register containing the continuation offset
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_continuation(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        offset = BaseOffset,
        available_regs = Available
    } = State,
    {free, OffsetReg}
) ->
    TempReg = first_avail(Available),
    % Calculate absolute address: native_code_base + target_offset
    % where native_code_base = current_pc + (BaseOffset - CurrentStreamOffset)
    CurrentStreamOffset = StreamModule:offset(Stream0),
    NetOffset = BaseOffset - CurrentStreamOffset,

    % Get native code base address into temporary register
    I1 = jit_aarch64_asm:adr(TempReg, NetOffset),
    % Add target offset to get final absolute address
    I2 = jit_aarch64_asm:add(TempReg, TempReg, OffsetReg),
    % Indirect branch to the calculated absolute address
    I3 = jit_aarch64_asm:br(TempReg),

    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free all registers since this is a tail jump
    State#state{
        stream = Stream1,
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State#state.regs)
    }.

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
    State4 = merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs),
    MergedRegs = jit_regs:merge(State1#state.regs, State2#state.regs),
    State4#state{regs = MergedRegs};
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
    State3 = merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs),
    MergedRegs = jit_regs:merge(State1#state.regs, State2#state.regs),
    State3#state{regs = MergedRegs}.

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
        available_regs = State1#state.available_regs,
        regs = State1#state.regs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    %% Patch the unconditional branch to jump to the end
    FinalJumpOffset = OffsetFinal - ElseJumpOffset,
    NewElseJumpInstr = jit_aarch64_asm:b(FinalJumpOffset),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    State4 = merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs),
    MergedRegs = jit_regs:merge(State2#state.regs, State3#state.regs),
    State4#state{regs = MergedRegs}.

%% @private
-spec if_block_cond(state(), condition()) ->
    {
        state(),
        jit_aarch64_asm:cc() | {tbz | tbnz, atom(), 0..63} | {cbz, atom()},
        non_neg_integer()
    }.
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '<', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:tbz(Reg, 63, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {tbz, Reg, 63}, 0};
% Handle {Val, '<', Reg} - means Val < Reg, jump if false (i.e., if Val >= Reg or Reg <= Val)
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
    % le = less than or equal
    I2 = jit_aarch64_asm:bcc(le, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, le, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val), Val =/= 0 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
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
    {{free, Reg1}, '==', {free, Reg2}}
) ->
    % Compare two free registers
    I1 = jit_aarch64_asm:cmp(Reg1, Reg2),
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free both registers
    State1 = if_block_free_reg({free, Reg1}, State0),
    State2 = if_block_free_reg({free, Reg2}, State1),
    State3 = State2#state{stream = Stream1},
    {State3, ne, byte_size(I1)};
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
        available_regs = Available
    } = State0,
    {RegOrTuple, '&', Val, '!=', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    Temp = first_avail(Available),
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
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream1, regs = Regs1},
    {State2, eq, byte_size(TestCode)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    Temp = first_avail(Available),
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
    Regs1b = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream3, regs = Regs1b},
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
    {State1, Reg} = and_(State0, RegTuple, Mask),
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
    Bit = reg_bit(Reg),
    AvR1 = AvR0 bor Bit,
    UR1 = UR0 band (bnot Bit),
    State0#state{
        available_regs = AvR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

%% @private
-spec merge_used_regs(state(), non_neg_integer()) -> state().
merge_used_regs(#state{used_regs = UR} = State, OtherUR) ->
    MergedUR = UR bor OtherUR,
    MergedAvail = ?AVAILABLE_REGS_MASK band (bnot MergedUR),
    State#state{used_regs = MergedUR, available_regs = MergedAvail}.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register right by a fixed number of bits, effectively
%% dividing it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
-spec shift_right(#state{}, maybe_free_aarch64_register(), non_neg_integer()) ->
    {#state{}, aarch64_register()}.
shift_right(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_aarch64_asm:lsr(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    ResultReg = first_avail(Available),
    Bit = reg_bit(ResultReg),
    I = jit_aarch64_asm:lsr(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Available band (bnot Bit),
            used_regs = Used bor Bit,
            regs = Regs1
        },
        ResultReg
    }.

-spec shift_right_arith(#state{}, maybe_free_aarch64_register(), non_neg_integer()) ->
    {#state{}, aarch64_register()}.
shift_right_arith(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_aarch64_asm:asr(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right_arith(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    ResultReg = first_avail(Available),
    Bit = reg_bit(ResultReg),
    I = jit_aarch64_asm:asr(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Available band (bnot Bit),
            used_regs = Used bor Bit,
            regs = Regs1
        },
        ResultReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register left by a fixed number of bits, effectively
%% multiplying it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
-spec shift_left(state(), aarch64_register(), non_neg_integer()) -> state().
shift_left(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Shift
) when
    is_atom(Reg)
->
    I = jit_aarch64_asm:lsl(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

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
    FreeMask = jit_regs:regs_to_mask(FreeRegs, fun reg_bit/1),
    UsedRegs1 = UsedRegs0 band (bnot FreeMask),
    SavedRegs = [?LR_REG, ?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | mask_to_list(UsedRegs1)],
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
    FreeGPMask = FreeMask band ?AVAILABLE_REGS_MASK,
    AvailableRegs1 = AvailableRegs0 bor FreeGPMask,
    {Stream5, ResultReg} =
        case lists:member(r0, SavedRegs) of
            true ->
                Temp = first_avail(AvailableRegs1),
                {StreamModule:append(Stream4, jit_aarch64_asm:mov(Temp, r0)), Temp};
            false ->
                {Stream4, r0}
        end,

    Stream6 = pop_registers(SavedRegsOdd, lists:reverse(SavedRegs), StreamModule, Stream5),

    ResultBit = reg_bit(ResultReg),
    AvailableRegs2 = AvailableRegs1 band (bnot ResultBit),
    AvailableRegs3 = AvailableRegs2 band ?AVAILABLE_REGS_MASK,
    Regs1 = jit_regs:invalidate_all(State0#state.regs),
    UsedRegs2 = UsedRegs1 bor ResultBit,
    {
        State1#state{
            stream = Stream6,
            available_regs = AvailableRegs3,
            used_regs = UsedRegs2,
            regs = Regs1
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
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ArgsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    AvailableScratchMask =
        ?SCRATCH_REGS_MASK band (bnot (ParamMask bor ArgsMask bor UsedRegs)),
    AvailableScratchGP = mask_to_list(AvailableScratchMask),
    Offset = StreamModule:offset(Stream0),
    Args1 = [
        case Arg of
            offset -> Offset;
            _ -> Arg
        end
     || Arg <- Args
    ],
    SetArgsCode = set_args0(Args1, ArgsRegs, ParamRegs, AvailableScratchGP, #{}, []),
    Stream1 = StreamModule:append(Stream0, SetArgsCode),
    NewUsedMask = lists:foldl(
        fun
            ({free, {ptr, Reg}}, AccUsed) -> AccUsed band (bnot reg_bit(Reg));
            ({free, Reg}, AccUsed) when is_atom(Reg) -> AccUsed band (bnot reg_bit(Reg));
            (_, AccUsed) -> AccUsed
        end,
        UsedRegs,
        Args
    ),
    State0#state{
        stream = Stream1,
        available_regs = ?AVAILABLE_REGS_MASK band (bnot (ParamMask bor NewUsedMask)),
        used_regs = ParamMask bor NewUsedMask
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
-spec set_args0(
    [arg()], [aarch64_register() | imm], [aarch64_register()], [aarch64_register()], map(), [
        binary()
    ]
) -> binary().
set_args0([], [], [], _AvailGP, _LoadedImm, Acc) ->
    list_to_binary(lists:reverse(Acc));
set_args0([{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc) ->
    set_args0([FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc);
set_args0([ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, LoadedImm, Acc) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc);
set_args0(
    [jit_state | ArgsT],
    [?JITSTATE_REG | ArgsRegs],
    [?JITSTATE_REG | ParamRegs],
    AvailGP,
    LoadedImm,
    Acc
) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc);
set_args0(
    [jit_state | ArgsT], [?JITSTATE_REG | ArgsRegs], [ParamReg | ParamRegs], AvailGP, LoadedImm, Acc
) ->
    false = lists:member(ParamReg, ArgsRegs),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [
        jit_aarch64_asm:mov(ParamReg, ?JITSTATE_REG) | Acc
    ]);
% ctx is special as we need it to access x_reg/y_reg/fp_reg
set_args0([Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, LoadedImm, Acc) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    J = set_args1(Arg, ?CTX_REG),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [J | Acc]);
set_args0(
    [Arg | ArgsT],
    [_ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    [Avail | AvailGPT] = AvailGP,
    LoadedImm,
    Acc
) ->
    case is_integer(Arg) andalso maps:find(Arg, LoadedImm) of
        {ok, CachedReg} ->
            J = jit_aarch64_asm:mov(ParamReg, CachedReg),
            set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [J | Acc]);
        _ ->
            J = set_args1(Arg, ParamReg),
            NewLoadedImm =
                case is_integer(Arg) of
                    true -> LoadedImm#{Arg => ParamReg};
                    false -> LoadedImm
                end,
            case lists:member(ParamReg, ArgsRegs) of
                false ->
                    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, NewLoadedImm, [J | Acc]);
                true ->
                    I = jit_aarch64_asm:mov(Avail, ParamReg),
                    NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
                    set_args0(NewArgsT, ArgsRegs, ParamRegs, AvailGPT, NewLoadedImm, [J, I | Acc])
            end
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
move_to_vm_register(#state{regs = Regs0} = State, Src, Dest) ->
    VmLoc = jit_regs:vm_dest_to_contents(Dest, ?MAX_REG),
    Regs1 =
        case VmLoc of
            unknown -> Regs0;
            _ -> jit_regs:invalidate_vm_loc(Regs0, VmLoc)
        end,
    State1 = move_to_vm_register_emit(State#state{regs = Regs1}, Src, Dest),
    case {Src, VmLoc} of
        {Reg, Contents} when is_atom(Reg), Contents =/= unknown ->
            #state{regs = Regs2} = State1,
            State1#state{regs = jit_regs:set_contents(Regs2, Reg, Contents)};
        _ ->
            State1
    end.

% Native register to VM register
move_to_vm_register_emit(State0, Src, {x_reg, extra}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {x_reg, X}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(
    #state{available_regs = Available, regs = Regs0} = State0, Src, {y_reg, Y}
) when
    is_atom(Src)
->
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:str(Src, {Temp, Y * ?WORD_SIZE}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1};
% Source is an integer
move_to_vm_register_emit(State, 0, Dest) ->
    move_to_vm_register_emit(State, xzr, Dest);
move_to_vm_register_emit(#state{available_regs = AR0} = State0, N, Dest) when
    is_integer(N)
->
    Temp = first_avail(AR0),
    TempBit = reg_bit(Temp),
    AT = AR0 band (bnot TempBit),
    I1 = jit_aarch64_asm:mov(Temp, N),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register_emit(
        State0#state{stream = Stream1, available_regs = AT}, Temp, Dest
    ),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
% Source is a VM register
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {x_reg, extra}, Dest) ->
    Temp = first_avail(AR0),
    TempBit = reg_bit(Temp),
    AT = AR0 band (bnot TempBit),
    I1 = jit_aarch64_asm:ldr(Temp, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register_emit(
        State0#state{stream = Stream1, available_regs = AT}, Temp, Dest
    ),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {x_reg, X}, Dest) ->
    Temp = first_avail(AR0),
    TempBit = reg_bit(Temp),
    AT = AR0 band (bnot TempBit),
    I1 = jit_aarch64_asm:ldr(Temp, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register_emit(
        State0#state{stream = Stream1, available_regs = AT}, Temp, Dest
    ),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {ptr, Reg}, Dest) ->
    Temp = first_avail(AR0),
    TempBit = reg_bit(Temp),
    AT = AR0 band (bnot TempBit),
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register_emit(
        State0#state{stream = Stream1, available_regs = AT}, Temp, Dest
    ),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {y_reg, Y}, Dest) ->
    Temp = first_avail(AR0),
    TempBit = reg_bit(Temp),
    AT = AR0 band (bnot TempBit),
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Temp, {Temp, Y * ?WORD_SIZE}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State1 = move_to_vm_register_emit(
        State0#state{stream = Stream1, available_regs = AT}, Temp, Dest
    ),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
% term_to_float
move_to_vm_register_emit(
    #state{stream_module = StreamModule, available_regs = Available, stream = Stream0} = State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) ->
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, ?WORD_SIZE}),
    I2 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I3 = jit_aarch64_asm:str(Reg, {Temp, ?FP_REG_OFFSET(State0, F)}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = free_native_register(State0, Reg),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{stream = Stream1, regs = Regs1}.

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
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available, regs = Regs0} =
        State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, Index * ?WORD_SIZE}),
    I2 = jit_aarch64_asm:str(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available, regs = Regs0} =
        State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, Index * ?WORD_SIZE}),
    I2 = jit_aarch64_asm:str(Temp, {Dest, 0}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available, regs = Regs0} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp1 = first_avail(Available),
    Bit1 = reg_bit(Temp1),
    Avail1 = Available band (bnot Bit1),
    Temp2 = first_avail(Avail1),
    I1 = jit_aarch64_asm:ldr(Temp1, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Temp2, {Reg, Index * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Temp2, {Temp1, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp1),
    Regs3 = jit_regs:invalidate_reg(Regs2, Temp2),
    State#state{stream = Stream1, regs = Regs3};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available, regs = Regs0} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Index * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Reg, {Temp, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Reg),
    Regs3 = jit_regs:invalidate_reg(Regs2, Temp),
    State#state{stream = Stream1, regs = Regs3};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Dest, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Dest),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    I1 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I2 = jit_aarch64_asm:str(IndexReg, ?X_REG(X)),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, IndexReg),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs2
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {ptr, PtrReg}
) when is_atom(IndexReg) ->
    I1 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I2 = jit_aarch64_asm:str(IndexReg, {PtrReg, 0}),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, IndexReg),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs1
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when ?IS_GPR(IndexReg) ->
    Temp = first_avail(AvailableRegs0),
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I3 = jit_aarch64_asm:str(IndexReg, {Temp, Y * ?WORD_SIZE}),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary>>
    ),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp),
    Regs3 = jit_regs:invalidate_reg(Regs2, IndexReg),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs3
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
        stream = Stream0,
        regs = Regs0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    Index
) ->
    ElemReg = first_avail(Available),
    Bit = reg_bit(ElemReg),
    I1 = jit_aarch64_asm:ldr(ElemReg, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ElemReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Available band (bnot Bit),
            used_regs = UsedRegs0 bor Bit,
            regs = Regs1
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
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available, regs = Regs0} =
        State,
    ValueReg,
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:add(Temp, IndexReg, Offset),
    I2 = jit_aarch64_asm:str(ValueReg, {BaseReg, Temp, lsl, 3}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    State0,
    Value,
    BaseReg,
    IndexReg,
    Offset
) ->
    {State1, ValueReg} = copy_to_native_register(State0, Value),
    Temp = first_avail(State1#state.available_regs),
    I1 = jit_aarch64_asm:add(Temp, IndexReg, Offset),
    I2 = jit_aarch64_asm:str(ValueReg, {BaseReg, Temp, lsl, 3}),
    Stream1 = (State1#state.stream_module):append(State1#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream1, regs = Regs1},
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
-spec move_to_native_register(state(), value() | cp) -> {state(), aarch64_register()}.
move_to_native_register(State, Reg) when ?IS_GPR(Reg) ->
    {State, Reg};
move_to_native_register(#state{regs = Regs} = State, Value) ->
    Contents = jit_regs:value_to_contents(Value, ?MAX_REG),
    case Contents =/= unknown andalso jit_regs:find_reg_with_contents(Regs, Contents) of
        {ok, CachedReg} ->
            Bit = reg_bit(CachedReg),
            case State#state.used_regs band Bit of
                0 ->
                    case State#state.available_regs band Bit of
                        0 ->
                            move_to_native_register_emit(State, Value, Contents);
                        _ ->
                            {
                                State#state{
                                    used_regs = State#state.used_regs bor Bit,
                                    available_regs = State#state.available_regs band (bnot Bit)
                                },
                                CachedReg
                            }
                    end;
                _ ->
                    {State, CachedReg}
            end;
        _ ->
            move_to_native_register_emit(State, Value, Contents)
    end.

move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    cp,
    Contents
) ->
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?CP),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor Bit,
            available_regs = Available band (bnot Bit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {ptr, Reg},
    _Contents
) when is_atom(Reg) ->
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    Imm,
    Contents
) when
    is_integer(Imm)
->
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:mov(Reg, Imm),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor Bit,
            available_regs = Available band (bnot Bit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    {x_reg, extra},
    Contents
) ->
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor Bit,
            available_regs = Available band (bnot Bit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    {x_reg, X},
    Contents
) when
    X < ?MAX_REG
->
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor Bit,
            available_regs = Available band (bnot Bit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    {y_reg, Y},
    Contents
) ->
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            available_regs = Available band (bnot Bit),
            used_regs = Used bor Bit,
            regs = Regs1
        },
        Reg
    }.

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
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_atom(RegSrc) ->
    I = jit_aarch64_asm:mov(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    SrcContents = jit_regs:get_contents(Regs0, RegSrc),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, SrcContents),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_integer(RegSrc) ->
    I = jit_aarch64_asm:mov(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {imm, RegSrc}),
    State#state{stream = Stream1, regs = Regs1};
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
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    Reg
) when is_atom(Reg) ->
    SaveReg = first_avail(Available),
    Bit = reg_bit(SaveReg),
    I1 = jit_aarch64_asm:mov(SaveReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    SrcContents = jit_regs:get_contents(Regs0, Reg),
    Regs1 = jit_regs:set_contents(Regs0, SaveReg, SrcContents),
    {
        State#state{
            stream = Stream1,
            available_regs = Available band (bnot Bit),
            used_regs = Used bor Bit,
            regs = Regs1
        },
        SaveReg
    };
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used,
        regs = Regs0
    } = State,
    {ptr, Reg}
) when is_atom(Reg) ->
    SaveReg = first_avail(Available),
    Bit = reg_bit(SaveReg),
    I1 = jit_aarch64_asm:ldr(SaveReg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, SaveReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Available band (bnot Bit),
            used_regs = Used bor Bit,
            regs = Regs1
        },
        SaveReg
    };
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
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    {y_reg, Y}
) ->
    Reg = first_avail(Avail),
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Y * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Increment the stack pointer (SP) by a given offset.
%% @end
%% @param State current backend state
%% @param Offset offset to add to SP (in words, will be multiplied by 8)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec increment_sp(state(), integer()) -> state().
increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Offset
) ->
    Reg = first_avail(Avail),
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:add(Reg, Reg, Offset * ?WORD_SIZE),
    I3 = jit_aarch64_asm:str(Reg, ?Y_REGS),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

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
        available_regs = Avail,
        branches = Branches,
        labels = Labels,
        regs = Regs0
    } = State,
    Label
) ->
    Temp = first_avail(Avail),
    Offset = StreamModule:offset(Stream0),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    case lists:keyfind(Label, 1, Labels) of
        {Label, LabelOffset} ->
            Rel = LabelOffset - Offset,
            I1 = jit_aarch64_asm:adr(Temp, Rel),
            I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            State#state{stream = Stream1, regs = Regs1};
        false ->
            I1 = jit_aarch64_asm:adr(Temp, 0),
            Reloc = {Label, Offset, {adr, Temp}},
            I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            State#state{stream = Stream1, branches = [Reloc | Branches], regs = Regs1}
    end.

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
        available_regs = Avail,
        branches = Branches,
        regs = Regs0
    } = State
) ->
    Temp = first_avail(Avail),
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    I1 = jit_aarch64_asm:adr(Temp, 0),
    Reloc = {OffsetRef, Offset, {adr, Temp}},
    I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    {State#state{stream = Stream1, branches = [Reloc | Branches], regs = Regs1}, OffsetRef}.

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
        available_regs = Avail,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State
) ->
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?JITSTATE_MODULE),
    I2 = jit_aarch64_asm:ldr_w(Reg, ?MODULE_INDEX(Reg)),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot Bit),
            used_regs = UsedRegs0 bor Bit,
            regs = Regs1
        },
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
                Temp = first_avail(State#state.available_regs),
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
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {free, Reg},
    SrcReg
) when
    is_atom(SrcReg)
->
    I1 = jit_aarch64_asm:and_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
and_(#state{regs = Regs0} = State, {free, Reg}, Val) ->
    NewState = op_imm(State, and_, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {NewState#state{regs = Regs1}, Reg};
and_(
    #state{available_regs = Avail, used_regs = UR, regs = Regs0} = State,
    Reg,
    Val
) ->
    ResultReg = first_avail(Avail),
    Bit = reg_bit(ResultReg),
    NewState = op_imm(
        State#state{available_regs = Avail band (bnot Bit), used_regs = UR bor Bit},
        and_,
        ResultReg,
        Reg,
        Val
    ),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {NewState#state{regs = Regs1}, ResultReg}.

%%-----------------------------------------------------------------------------
%% @doc Perform bitwise OR of a register with an immediate value.
%% @end
%% @param State current backend state
%% @param Reg register to OR with value
%% @param Val immediate value to OR
%% @return Updated backend state
%%-----------------------------------------------------------------------------
or_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I1 = jit_aarch64_asm:orr(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
or_(#state{regs = Regs0} = State, Reg, Val) ->
    NewState = op_imm(State, orr, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1}.

xor_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I1 = jit_aarch64_asm:eor(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
xor_(#state{regs = Regs0} = State, Reg, Val) ->
    NewState = op_imm(State, eor, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Add an immediate value to a register.
%% @end
%% @param State current backend state
%% @param Reg register to add to
%% @param Val immediate value to add
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add(state(), aarch64_register(), integer()) -> state().
add(#state{regs = Regs0} = State, Reg, Val) ->
    NewState = op_imm(State, add, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Subtract an immediate value from a register.
%% @end
%% @param State current backend state
%% @param Reg register to subtract from
%% @param Val immediate value to subtract
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec sub(state(), aarch64_register(), integer() | aarch64_register()) -> state().
sub(#state{regs = Regs0} = State, Reg, Val) when is_integer(Val) ->
    NewState = op_imm(State, sub, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1};
sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    is_atom(Val)
->
    I1 = jit_aarch64_asm:sub(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Multiply a register by a constant value. Uses optimized instruction
%% sequences for common multipliers (powers of 2, small values).
%% @end
%% @param State current backend state
%% @param Reg register to multiply
%% @param Val multiplier (an integer constant or a register)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec mul(state(), aarch64_register(), integer() | aarch64_register()) -> state().
mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 3) ->
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 1),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 5) ->
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 2),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 7) ->
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 3),
    I2 = jit_aarch64_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 9) ->
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 3),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 15) ->
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 4),
    I2 = jit_aarch64_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Val
) when is_integer(Val) ->
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:mov(Temp, Val),
    I2 = jit_aarch64_asm:mul(Reg, Reg, Temp),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, DestReg, SrcReg
) when is_atom(SrcReg) ->
    I1 = jit_aarch64_asm:mul(DestReg, DestReg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, DestReg),
    State#state{stream = Stream1, regs = Regs1}.

-spec div_(state(), aarch64_register(), aarch64_register()) -> {state(), aarch64_register()}.
div_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I1 = jit_aarch64_asm:sdiv(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

-spec rem_(state(), aarch64_register(), aarch64_register()) -> {state(), aarch64_register()}.
rem_(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    DividendReg,
    DivisorReg
) ->
    %% rem = dividend - (dividend / divisor) * divisor
    %% Use msub: Rd = Ra - (Rn * Rm)
    %% First sdiv into a temp, then msub
    TempReg = first_avail(Avail band (bnot reg_bit(DividendReg)) band (bnot reg_bit(DivisorReg))),
    I1 = jit_aarch64_asm:sdiv(TempReg, DividendReg, DivisorReg),
    I2 = jit_aarch64_asm:msub(DividendReg, TempReg, DivisorReg, DividendReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

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
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State0
) ->
    Temp = first_avail(Avail),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
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
    State1 = State0#state{stream = Stream2, regs = Regs1},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite the branch and adr instructions
    #state{stream = Stream3} = State2,
    NewOffset = StreamModule:offset(Stream3),
    NewI4 = jit_aarch64_asm:bcc(ne, NewOffset - BNEOffset),
    NewI5 = jit_aarch64_asm:adr(Temp, NewOffset - ADROffset),
    Stream4 = StreamModule:replace(
        Stream3, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    State3 = merge_used_regs(State2#state{stream = Stream4}, State1#state.used_regs),
    %% The schedule_next path is a tail call (dead end), so the register tracking
    %% from the non-taken path (State1) is what matters at the continuation.
    State3#state{regs = State1#state.regs}.

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
        labels = Labels,
        available_regs = Avail
    } = State0,
    Label
) ->
    Temp = first_avail(Avail),
    % Load reduction count
    I1 = jit_aarch64_asm:ldr_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),

    case lists:keyfind(Label, 1, Labels) of
        {Label, LabelOffset} ->
            % Label is already known, emit direct branch with calculated offset
            % Calculate relative offset (must be 4-byte aligned)
            Rel = LabelOffset - BNEOffset,
            I4 = jit_aarch64_asm:bcc(ne, Rel),
            Stream2 = StreamModule:append(Stream1, I4),
            State1 = State0#state{stream = Stream2};
        false ->
            % Label not yet known, emit placeholder and add relocation
            I4 = jit_aarch64_asm:bcc(ne, 0),
            Reloc1 = {Label, BNEOffset, {bcc, ne}},
            Stream2 = StreamModule:append(Stream1, I4),
            State1 = State0#state{stream = Stream2, branches = [Reloc1 | Branches]}
    end,
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

reg_bit(r0) -> ?REG_BIT_R0;
reg_bit(r1) -> ?REG_BIT_R1;
reg_bit(r2) -> ?REG_BIT_R2;
reg_bit(r3) -> ?REG_BIT_R3;
reg_bit(r4) -> ?REG_BIT_R4;
reg_bit(r5) -> ?REG_BIT_R5;
reg_bit(r6) -> ?REG_BIT_R6;
reg_bit(r7) -> ?REG_BIT_R7;
reg_bit(r8) -> ?REG_BIT_R8;
reg_bit(r9) -> ?REG_BIT_R9;
reg_bit(r10) -> ?REG_BIT_R10;
reg_bit(r11) -> ?REG_BIT_R11;
reg_bit(r12) -> ?REG_BIT_R12;
reg_bit(r13) -> ?REG_BIT_R13;
reg_bit(r14) -> ?REG_BIT_R14;
reg_bit(r15) -> ?REG_BIT_R15;
reg_bit(r16) -> ?REG_BIT_R16;
reg_bit(r17) -> ?REG_BIT_R17.

%% first_avail returns the first available register from a bitmask.
%% Order: [r7, r8, r9, r10, r11, r12, r13, r14, r15, r3, r4, r5, r6]
first_avail(Mask) when Mask band ?REG_BIT_R7 =/= 0 -> r7;
first_avail(Mask) when Mask band ?REG_BIT_R8 =/= 0 -> r8;
first_avail(Mask) when Mask band ?REG_BIT_R9 =/= 0 -> r9;
first_avail(Mask) when Mask band ?REG_BIT_R10 =/= 0 -> r10;
first_avail(Mask) when Mask band ?REG_BIT_R11 =/= 0 -> r11;
first_avail(Mask) when Mask band ?REG_BIT_R12 =/= 0 -> r12;
first_avail(Mask) when Mask band ?REG_BIT_R13 =/= 0 -> r13;
first_avail(Mask) when Mask band ?REG_BIT_R14 =/= 0 -> r14;
first_avail(Mask) when Mask band ?REG_BIT_R15 =/= 0 -> r15;
first_avail(Mask) when Mask band ?REG_BIT_R3 =/= 0 -> r3;
first_avail(Mask) when Mask band ?REG_BIT_R4 =/= 0 -> r4;
first_avail(Mask) when Mask band ?REG_BIT_R5 =/= 0 -> r5;
first_avail(Mask) when Mask band ?REG_BIT_R6 =/= 0 -> r6.

%% Convert bitmask to list, matching the order
mask_to_list(0) -> [];
mask_to_list(Mask) -> mask_to_list_r7(Mask).

mask_to_list_r7(Mask) when Mask band ?REG_BIT_R7 =/= 0 -> [r7 | mask_to_list_r8(Mask)];
mask_to_list_r7(Mask) -> mask_to_list_r8(Mask).
mask_to_list_r8(Mask) when Mask band ?REG_BIT_R8 =/= 0 -> [r8 | mask_to_list_r9(Mask)];
mask_to_list_r8(Mask) -> mask_to_list_r9(Mask).
mask_to_list_r9(Mask) when Mask band ?REG_BIT_R9 =/= 0 -> [r9 | mask_to_list_r10(Mask)];
mask_to_list_r9(Mask) -> mask_to_list_r10(Mask).
mask_to_list_r10(Mask) when Mask band ?REG_BIT_R10 =/= 0 -> [r10 | mask_to_list_r11(Mask)];
mask_to_list_r10(Mask) -> mask_to_list_r11(Mask).
mask_to_list_r11(Mask) when Mask band ?REG_BIT_R11 =/= 0 -> [r11 | mask_to_list_r12(Mask)];
mask_to_list_r11(Mask) -> mask_to_list_r12(Mask).
mask_to_list_r12(Mask) when Mask band ?REG_BIT_R12 =/= 0 -> [r12 | mask_to_list_r13(Mask)];
mask_to_list_r12(Mask) -> mask_to_list_r13(Mask).
mask_to_list_r13(Mask) when Mask band ?REG_BIT_R13 =/= 0 -> [r13 | mask_to_list_r14(Mask)];
mask_to_list_r13(Mask) -> mask_to_list_r14(Mask).
mask_to_list_r14(Mask) when Mask band ?REG_BIT_R14 =/= 0 -> [r14 | mask_to_list_r15(Mask)];
mask_to_list_r14(Mask) -> mask_to_list_r15(Mask).
mask_to_list_r15(Mask) when Mask band ?REG_BIT_R15 =/= 0 -> [r15 | mask_to_list_r3(Mask)];
mask_to_list_r15(Mask) -> mask_to_list_r3(Mask).
mask_to_list_r3(Mask) when Mask band ?REG_BIT_R3 =/= 0 -> [r3 | mask_to_list_r4(Mask)];
mask_to_list_r3(Mask) -> mask_to_list_r4(Mask).
mask_to_list_r4(Mask) when Mask band ?REG_BIT_R4 =/= 0 -> [r4 | mask_to_list_r5(Mask)];
mask_to_list_r4(Mask) -> mask_to_list_r5(Mask).
mask_to_list_r5(Mask) when Mask band ?REG_BIT_R5 =/= 0 -> [r5 | mask_to_list_r6(Mask)];
mask_to_list_r5(Mask) -> mask_to_list_r6(Mask).
mask_to_list_r6(Mask) when Mask band ?REG_BIT_R6 =/= 0 -> [r6];
mask_to_list_r6(_Mask) -> [].

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
add_label(#state{stream_module = StreamModule, stream = Stream, regs = Regs0} = State, Label) ->
    Offset = StreamModule:offset(Stream),
    Regs1 = jit_regs:invalidate_all(Regs0),
    add_label(State#state{regs = Regs1}, Label, Offset).

%%-----------------------------------------------------------------------------
%% @doc Add a label at a specific offset
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @param Offset the explicit offset for this label
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference(), integer()) -> state().
add_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        jump_table_start = JumpTableStart,
        branches = Branches,
        labels = Labels
    } = State,
    Label,
    LabelOffset
) when is_integer(Label) ->
    % Patch the jump table entry immediately
    % Each b instruction is 4 bytes
    JumpTableEntryOffset = JumpTableStart + Label * 4,
    RelativeOffset = LabelOffset - JumpTableEntryOffset,
    BranchInstr = jit_aarch64_asm:b(RelativeOffset),
    Stream1 = StreamModule:replace(Stream0, JumpTableEntryOffset, BranchInstr),

    % Eagerly patch any branches targeting this label
    {Stream2, RemainingBranches} = patch_branches_for_label(
        StreamModule,
        Stream1,
        Label,
        LabelOffset,
        Branches
    ),

    State#state{
        stream = Stream2, branches = RemainingBranches, labels = [{Label, LabelOffset} | Labels]
    };
add_label(#state{labels = Labels} = State, Label, Offset) ->
    State#state{labels = [{Label, Offset} | Labels]}.

-ifdef(JIT_DWARF).
%%-----------------------------------------------------------------------------
%% @doc Return the DWARF register number for the ctx parameter
%% @returns The DWARF register number where ctx is passed (x0/r0 in aarch64)
%% @end
%%-----------------------------------------------------------------------------
-spec dwarf_ctx_register() -> non_neg_integer().
dwarf_ctx_register() ->
    ?DWARF_X0_REG_AARCH64.

-spec dwarf_register_number(atom()) -> non_neg_integer().
dwarf_register_number(r0) -> 0;
dwarf_register_number(r1) -> 1;
dwarf_register_number(r2) -> 2;
dwarf_register_number(r3) -> 3;
dwarf_register_number(r4) -> 4;
dwarf_register_number(r5) -> 5;
dwarf_register_number(r6) -> 6;
dwarf_register_number(r7) -> 7;
dwarf_register_number(r8) -> 8;
dwarf_register_number(r9) -> 9;
dwarf_register_number(r10) -> 10;
dwarf_register_number(r11) -> 11;
dwarf_register_number(r12) -> 12;
dwarf_register_number(r13) -> 13;
dwarf_register_number(r14) -> 14;
dwarf_register_number(r15) -> 15;
dwarf_register_number(r16) -> 16;
dwarf_register_number(r17) -> 17.
-endif.
