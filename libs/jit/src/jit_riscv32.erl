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

-module(jit_riscv32).

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
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3
]).

-ifdef(JIT_DWARF).
-export([
    dwarf_opcode/2,
    dwarf_label/2,
    dwarf_function/3,
    dwarf_line/2
]).
-endif.

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").
-include("term.hrl").

-define(ASSERT(Expr), true = Expr).

%% RISC-V32 ILP32 ABI: a0-a7 are used for argument passing (8 registers).
%% a0-a1 are used for return values (a0 for 32-bit, a0-a1 for 64-bit returns).
%% s0-s11 are callee-saved registers (must be preserved across calls).
%% t0-t6 are caller-saved temporary registers.
%% sp is the stack pointer.
%% ra is the return address register.
%% zero (x0) is hardwired to constant 0.
%% This implementation uses RV32IMC (base + multiply/compressed extensions).
%%
%% See: RISC-V Calling Convention
%% https://riscv.org/wp-content/uploads/2024/12/riscv-calling.pdf
%%
%% Registers used by the JIT backend (RISC-V32):
%%   - Argument/return: a0-a7 (up to 8 args in registers)
%%   - Callee-saved: s0-s11 (must preserve)
%%   - Temporaries: t0-t6 (caller-saved)
%%   - Stack pointer: sp
%%   - Return address: ra
%%   - Zero register: zero (always 0)
%%   - Available for JIT scratch: t0-t6 (7 temp registers)
%%
%% Note: RISC-V32 instructions are fixed 32-bit with uniform encoding,
%% allowing access to all 32 registers.
%%
%% For more details, refer to the RISC-V ILP32 Procedure Call Standard.

-type riscv32_register() ::
    a0
    | a1
    | a2
    | a3
    | a4
    | a5
    | a6
    | a7
    | t0
    | t1
    | t2
    | t3
    | t4
    | t5
    | t6
    | s0
    | s1
    | s2
    | s3
    | s4
    | s5
    | s6
    | s7
    | s8
    | s9
    | s10
    | s11
    | sp
    | ra.

-define(IS_GPR(Reg),
    (Reg =:= a0 orelse Reg =:= a1 orelse Reg =:= a2 orelse Reg =:= a3 orelse Reg =:= a4 orelse
        Reg =:= a5 orelse Reg =:= a6 orelse Reg =:= a7 orelse Reg =:= t0 orelse Reg =:= t1 orelse
        Reg =:= t2 orelse Reg =:= t3 orelse Reg =:= t4 orelse Reg =:= t5 orelse Reg =:= t6 orelse
        Reg =:= s0 orelse Reg =:= s1 orelse Reg =:= s2 orelse Reg =:= s3 orelse Reg =:= s4 orelse
        Reg =:= s5 orelse Reg =:= s6 orelse Reg =:= s7 orelse Reg =:= s8 orelse Reg =:= s9 orelse
        Reg =:= s10 orelse Reg =:= s11 orelse Reg =:= sp orelse Reg =:= ra)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    available_regs :: [riscv32_register()],
    used_regs :: [riscv32_register()],
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer()
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, riscv32_register()}.
-type value() :: immediate() | vm_register() | riscv32_register() | {ptr, riscv32_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_riscv32_register() ::
    {free, riscv32_register()} | riscv32_register().

-type condition() ::
    {riscv32_register(), '<', integer()}
    | {maybe_free_riscv32_register(), '<', riscv32_register()}
    | {maybe_free_riscv32_register(), '==', integer()}
    | {maybe_free_riscv32_register(), '!=', riscv32_register() | integer()}
    | {'(int)', maybe_free_riscv32_register(), '==', integer()}
    | {'(int)', maybe_free_riscv32_register(), '!=', riscv32_register() | integer()}
    | {'(bool)', maybe_free_riscv32_register(), '==', false}
    | {'(bool)', maybe_free_riscv32_register(), '!=', false}
    | {maybe_free_riscv32_register(), '&', non_neg_integer(), '!=', integer()}
    | {{free, riscv32_register()}, '==', {free, riscv32_register()}}.

% Context offsets (32-bit architecture)
% ctx->e is 0x14
% ctx->x is 0x18
-define(CTX_REG, a0).
-define(NATIVE_INTERFACE_REG, a2).
-define(Y_REGS, {?CTX_REG, 16#14}).
-define(X_REG(N), {?CTX_REG, 16#18 + (N * 4)}).
-define(CP, {?CTX_REG, 16#5C}).
-define(FP_REGS, {?CTX_REG, 16#60}).
-define(BS, {?CTX_REG, 16#64}).
-define(BS_OFFSET, {?CTX_REG, 16#68}).
% JITSTATE is in a1 register (no prolog, following aarch64 model)
-define(JITSTATE_REG, a1).
% Return address register (like LR in AArch64)
-define(RA_REG, ra).
-define(JITSTATE_MODULE_OFFSET, 0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#8).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * 4}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).

-define(JUMP_TABLE_ENTRY_SIZE, 8).

%% RISC-V32 register mappings

%% Use t3 as temporary for some operations
-define(IP_REG, t3).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).
-define(IS_SIGNED_OR_UNSIGNED_INT32_T(X),
    is_integer(X) andalso X >= -16#80000000 andalso X < 16#100000000
).

%% RISC-V32 ILP32 ABI register allocation:
%% - a0: context pointer (reserved, passed as first parameter)
%% - a1-a5: available for parameters to native functions (up to 6 params)
%% - a2: native interface pointer (reserved)
%% - t0-t6: temporaries, caller-saved, available for JIT use
%% - s0-s11: callee-saved (would need to be saved/restored)
-define(AVAILABLE_REGS, [t6, t5, t4, t3, t2, t1, t0]).
-define(PARAMETER_REGS, [a0, a1, a2, a3, a4, a5, a6, a7]).
-define(SCRATCH_REGS, [t6, t5, t4, t2, t1, t0]).

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
%% @doc Flush the stream.
%% @end
%% @param State current backend state
%% @return The new state
%%-----------------------------------------------------------------------------
-spec flush(state()) -> stream().
flush(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:flush(Stream0),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a debugger of breakpoint instruction. This is used for debugging
%% and not in production.
%% @end
%% @param State current backend state
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec debugger(state()) -> state().
debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:append(Stream0, jit_riscv32_asm:c_ebreak()),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [riscv32_register()].
used_regs(#state{used_regs = Used}) -> Used.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [riscv32_register()].
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
%% On this platform, each jump table entry is 12 bytes.
%% ```
%% ldr a3, pc+4
%% push {a1, r4, r5, r6, r7, lr}
%% add pc, pc, a3
%% nop()
%% offset_to_label0
%% ```
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
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State,
    N,
    LabelsCount
) ->
    % Create jump table entry: AUIPC + JALR (8 bytes total)
    % This will be patched later in update_branches/2
    Offset = StreamModule:offset(Stream0),
    JumpEntry = <<16#FFFFFFFF:32, 16#FFFFFFFF:32>>,
    Stream1 = StreamModule:append(Stream0, JumpEntry),

    % Record both AUIPC and JALR offsets for patching
    Reloc = {N, Offset, jump_table_auipc_jalr},
    UpdatedState = State#state{stream = Stream1, branches = [Reloc | Branches]},

    jump_table0(UpdatedState, N + 1, LabelsCount).

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
            {adr, Reg} when Rel rem 4 =:= 0 ->
                % Generate pc_relative_address and pad to 8 bytes with NOP
                I = pc_relative_address(Reg, Rel),
                case byte_size(I) of
                    4 -> <<I/binary, (jit_riscv32_asm:nop())/binary>>;
                    6 -> <<I/binary, (jit_riscv32_asm:c_nop())/binary>>;
                    8 -> I
                end;
            {adr, Reg} when Rel rem 4 =:= 2; Rel rem 4 =:= -2 ->
                % Handle 2-byte aligned offsets and pad to 8 bytes
                % Handle both positive and negative offsets (Erlang rem can be negative)
                I = pc_relative_address(Reg, Rel),
                case byte_size(I) of
                    4 -> <<I/binary, (jit_riscv32_asm:nop())/binary>>;
                    6 -> <<I/binary, (jit_riscv32_asm:c_nop())/binary>>;
                    8 -> I
                end;
            {far_branch, TempReg} ->
                % Check if branch can now be optimized to near branch
                if
                    Rel >= -1048576 andalso Rel =< 1048574 andalso (Rel rem 2) =:= 0 ->
                        % RISC-V jal has ±1MB range
                        % Optimize to near branch: jal + nops to fill original size
                        DirectBranch = jit_riscv32_asm:jal(zero, Rel),
                        case byte_size(DirectBranch) of
                            2 ->
                                <<DirectBranch/binary, (jit_riscv32_asm:c_nop())/binary,
                                    (jit_riscv32_asm:nop())/binary>>;
                            4 ->
                                <<DirectBranch/binary, (jit_riscv32_asm:nop())/binary>>
                        end;
                    true ->
                        % Keep far branch sequence: auipc + jalr (PC-relative, 8 bytes)
                        % Split the relative offset into upper 20 bits and lower 12 bits
                        Hi20 = (Rel + 16#800) bsr 12,
                        Lo12 = Rel - (Hi20 bsl 12),
                        I1 = jit_riscv32_asm:auipc(TempReg, Hi20),
                        I2 = jit_riscv32_asm:jalr(zero, TempReg, Lo12),
                        Entry = <<I1/binary, I2/binary>>,
                        case byte_size(Entry) of
                            6 -> <<Entry/binary, (jit_riscv32_asm:c_nop())/binary>>;
                            8 -> Entry
                        end
                end;
            jump_table_auipc_jalr ->
                % Calculate PC-relative offset from AUIPC instruction to target
                % AUIPC is at Offset, JALR is at Offset+4
                % Target is at LabelOffset
                % Offset from AUIPC PC to target
                PCRelOffset = LabelOffset - Offset,

                % Split into upper 20 bits and lower 12 bits
                % RISC-V encodes: target = PC + (upper20 << 12) + sign_ext(lower12)
                % If lower12 >= 0x800, it's negative when sign-extended, so add 1 to upper
                Upper20 = (PCRelOffset + 16#800) bsr 12,
                Lower12 = PCRelOffset band 16#FFF,
                % Sign-extend lower 12 bits for JALR immediate
                Lower12Signed =
                    if
                        Lower12 >= 16#800 -> Lower12 - 16#1000;
                        true -> Lower12
                    end,

                % Encode AUIPC and JALR with computed offsets
                I1 = jit_riscv32_asm:auipc(a3, Upper20),
                I2 = jit_riscv32_asm:jalr(zero, a3, Lower12Signed),
                % Map to 8 bytes
                JumpTableEntry = <<I1/binary, I2/binary>>,
                case byte_size(JumpTableEntry) of
                    6 -> <<JumpTableEntry/binary, (jit_riscv32_asm:c_nop())/binary>>;
                    8 -> JumpTableEntry
                end
        end,
    Stream1 = StreamModule:replace(Stream0, Offset, NewInstr),
    update_branches(State#state{stream = Stream1, branches = BranchesT}).

%%-----------------------------------------------------------------------------
%% @doc Generate code to load a primitive function pointer into a register
%% @param Primitive index to the primitive to call
%% @param TargetReg register to load the function pointer into
%% @return Binary instruction sequence
%%-----------------------------------------------------------------------------
-spec load_primitive_ptr(non_neg_integer(), riscv32_register()) -> binary().
load_primitive_ptr(Primitive, TargetReg) ->
    case Primitive of
        0 ->
            jit_riscv32_asm:lw(TargetReg, ?NATIVE_INTERFACE_REG, 0);
        N when N * 4 =< 124 ->
            jit_riscv32_asm:lw(TargetReg, ?NATIVE_INTERFACE_REG, N * 4);
        N when N * 4 < 256 ->
            % Can encode N * 4 directly in li instruction
            I1 = jit_riscv32_asm:li(TargetReg, N * 4),
            I2 = jit_riscv32_asm:add(TargetReg, TargetReg, ?NATIVE_INTERFACE_REG),
            I3 = jit_riscv32_asm:lw(TargetReg, TargetReg, 0),
            <<I1/binary, I2/binary, I3/binary>>;
        N ->
            % For very large primitive numbers, load N and shift left by 2 (multiply by 4)
            I1 = jit_riscv32_asm:li(TargetReg, N),
            I2 = jit_riscv32_asm:slli(TargetReg, TargetReg, 2),
            I3 = jit_riscv32_asm:add(TargetReg, TargetReg, ?NATIVE_INTERFACE_REG),
            I4 = jit_riscv32_asm:lw(TargetReg, TargetReg, 0),
            <<I1/binary, I2/binary, I3/binary, I4/binary>>
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
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), riscv32_register()}.
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
    call_func_ptr(StateCall, {free, TempReg}, Args);
call_primitive(
    #state{available_regs = []} = State,
    Primitive,
    Args
) ->
    call_func_ptr(State, {primitive, Primitive}, Args).

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

    State1 = State0#state{
        stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs
    },

    % Preprocess offset special arg
    Args1 = lists:map(
        fun(Arg) ->
            case Arg of
                offset -> StreamModule:offset(Stream1);
                _ -> Arg
            end
        end,
        Args
    ),

    % In RISC-V, all up to 8 arguments fit in registers (a0-a7)
    % Always use tail call when calling primitives in tail position
    State4 =
        case Args1 of
            [FirstArg, jit_state | ArgsT] ->
                % Use tail call
                ArgsForTailCall = [FirstArg, jit_state_tail_call | ArgsT],
                State2 = set_registers_args(State1, ArgsForTailCall, 0),
                tail_call_with_jit_state_registers_only(State2, Temp)
        end,
    State4#state{available_regs = ?AVAILABLE_REGS, used_regs = []}.

%%-----------------------------------------------------------------------------
%% @doc Tail call to address in register.
%% RA is preserved across regular calls (call_func_ptr saves/restores it),
%% so when the called C primitive returns, it returns to opcodesswitch.h.
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
    % Jump to address in register (tail call)
    I1 = jit_riscv32_asm:jr(Reg),
    Stream1 = StreamModule:append(Stream0, I1),
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
    % RISC-V doesn't have a separate cmp instruction, use beq directly
    I2 =
        case Reg of
            % Return value is already in a0
            a0 -> <<>>;
            % Move to a0 (return register)
            _ -> jit_riscv32_asm:mv(a0, Reg)
        end,
    I3 = jit_riscv32_asm:ret(),
    % Branch if equal (skip the return)
    % Offset must account for the beq instruction itself (4 bytes) plus I2 and I3
    I1 = jit_riscv32_asm:beq(Reg, ?CTX_REG, 4 + byte_size(I2) + byte_size(I3)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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
    #state{stream_module = StreamModule, stream = Stream0, labels = Labels} = State0, Label
) ->
    LabelLookupResult = lists:keyfind(Label, 1, Labels),
    Offset = StreamModule:offset(Stream0),
    {State1, CodeBlock} = branch_to_label_code(State0, Offset, Label, LabelLookupResult),
    Stream1 = StreamModule:append(Stream0, CodeBlock),
    State1#state{stream = Stream1}.

jump_to_offset(#state{stream_module = StreamModule, stream = Stream0} = State, TargetOffset) ->
    Offset = StreamModule:offset(Stream0),
    CodeBlock = branch_to_offset_code(State, Offset, TargetOffset),
    Stream1 = StreamModule:append(Stream0, CodeBlock),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Jump to address in continuation pointer register
%% Calculate absolute address and jump to it.
%% @end
%% @param State current backend state
%% @param {free, OffsetReg} register containing the offset value
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_continuation(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _],
        offset = BaseOffset
    } = State0,
    {free, OffsetReg}
) ->
    % Calculate absolute address: native_code_base + target_offset
    % where native_code_base = current_pc + (BaseOffset - CurrentStreamOffset)
    CurrentStreamOffset = StreamModule:offset(Stream0),
    NetOffset = BaseOffset - CurrentStreamOffset,

    % Get native code base address into temporary register
    I1 = pc_relative_address(Temp, NetOffset),
    % Add target offset to get final absolute address
    I2 = jit_riscv32_asm:add(Temp, Temp, OffsetReg),
    % Indirect branch to the calculated absolute address
    I3 = jit_riscv32_asm:jr(Temp),

    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free all registers since this is a tail jump
    State0#state{stream = Stream1, available_regs = ?AVAILABLE_REGS, used_regs = []}.

branch_to_offset_code(_State, Offset, TargetOffset) when
    TargetOffset - Offset =< 2050, TargetOffset - Offset >= -2044
->
    % Near branch: use direct J instruction
    Rel = TargetOffset - Offset,
    jit_riscv32_asm:j(Rel);
branch_to_offset_code(
    #state{available_regs = [TempReg | _]}, Offset, TargetOffset
) ->
    % Far branch: use auipc + jalr sequence for PC-relative addressing
    % This computes: PC + Immediate and jumps to it

    Rel = TargetOffset - Offset,
    % Split the relative offset into upper 20 bits and lower 12 bits
    % RISC-V PC-relative addressing: target = PC + (imm20 << 12) + sign_extend(imm12)
    % Since jalr's imm12 is sign-extended, if bit 11 of Rel is set,
    % we need to add 0x800 before splitting to compensate
    Hi20 = (Rel + 16#800) bsr 12,
    Lo12Unsigned = Rel band 16#FFF,
    % Convert to signed 12-bit value: if bit 11 is set, subtract 4096
    Lo12 =
        if
            Lo12Unsigned >= 16#800 -> Lo12Unsigned - 16#1000;
            true -> Lo12Unsigned
        end,

    % TempReg = PC + (Hi20 << 12)
    I1 = jit_riscv32_asm:auipc(TempReg, Hi20),
    % Jump to TempReg + sign_extend(Lo12)
    I2 = jit_riscv32_asm:jalr(zero, TempReg, Lo12),
    <<I1/binary, I2/binary>>.

branch_to_label_code(State, Offset, Label, {Label, LabelOffset}) ->
    CodeBlock = branch_to_offset_code(State, Offset, LabelOffset),
    {State, CodeBlock};
branch_to_label_code(
    #state{available_regs = [TempReg | _], branches = Branches} = State0, Offset, Label, false
) ->
    % RISC-V: Far branch sequence using PC-relative auipc + jalr (8 bytes)

    % Placeholder: auipc TempReg, 0
    % Placeholder: jalr zero, TempReg, 0
    CodeBlock = <<16#FFFFFFFF:32, 16#FFFFFFFF:32>>,
    % Add relocation entry
    Reloc = {Label, Offset, {far_branch, TempReg}},
    State1 = State0#state{branches = [Reloc | Branches]},
    {State1, CodeBlock};
branch_to_label_code(
    #state{available_regs = [], branches = Branches} = State0, Offset, Label, false
) ->
    % RISC-V: Use t6 as scratch (caller-saved, safe to clobber)
    % Far branch sequence using PC-relative auipc + jalr (8 bytes)

    % Placeholder: auipc t6, 0
    % Placeholder: jalr zero, t6, 0
    CodeBlock = <<16#FFFFFFFF:32, 16#FFFFFFFF:32>>,
    % Add relocation entry
    Reloc = {Label, Offset, {far_branch, t6}},
    State1 = State0#state{branches = [Reloc | Branches]},
    {State1, CodeBlock};
branch_to_label_code(#state{available_regs = []}, _Offset, _Label, _LabelLookup) ->
    error({no_available_registers, _LabelLookup}).

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
            {NewAccState, BranchInfo, ReplaceDelta} = if_block_cond(AccState, Cond),
            {[{Offset + ReplaceDelta, BranchInfo} | AccReplacements], NewAccState}
        end,
        {[], State0},
        CondList
    ),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({ReplacementOffset, {BranchFunc, Reg, Operand}}, AccStream) ->
            BranchOffset = OffsetAfter - ReplacementOffset,
            NewBranchInstr = apply(jit_riscv32_asm, BranchFunc, [Reg, Operand, BranchOffset]),
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
    {State1, {BranchFunc, Reg, Operand}, BranchInstrDelta} = if_block_cond(State0, Cond),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    %% Patch the conditional branch instruction to jump to the end of the block
    BranchInstrOffset = Offset + BranchInstrDelta,
    BranchOffset = OffsetAfter - BranchInstrOffset,
    NewBranchInstr = apply(jit_riscv32_asm, BranchFunc, [Reg, Operand, BranchOffset]),
    Stream3 = StreamModule:replace(Stream2, BranchInstrOffset, NewBranchInstr),
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
    {State1, {BranchFunc, Reg, Operand}, BranchInstrDelta} = if_block_cond(State0, Cond),
    BranchInstrOffset = Offset + BranchInstrDelta,
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    %% Emit unconditional branch to skip the else block (will be replaced)
    ElseJumpOffset = StreamModule:offset(Stream2),
    %% Use all-1s placeholder for flash compatibility (can only flip 1->0)
    ElseJumpInstr = <<16#FFFF:16>>,
    Stream3 = StreamModule:append(Stream2, ElseJumpInstr),
    %% Else block starts here.
    OffsetAfter = StreamModule:offset(Stream3),
    %% Patch the conditional branch to jump to the else block
    ElseBranchOffset = OffsetAfter - BranchInstrOffset,
    NewBranchInstr = apply(jit_riscv32_asm, BranchFunc, [Reg, Operand, ElseBranchOffset]),
    Stream4 = StreamModule:replace(Stream3, BranchInstrOffset, NewBranchInstr),
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
    NewElseJumpInstr = jit_riscv32_asm:j(FinalJumpOffset),
    %% Assert that replacement is 2 bytes (c.j range: -2048..2046)
    %% If this fails, the if/else blocks are too large
    2 = byte_size(NewElseJumpInstr),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs).

-spec if_block_cond(state(), condition()) ->
    {
        state(),
        {beq | bne | blt | bge, atom(), atom() | integer()},
        non_neg_integer()
    }.
if_block_cond(#state{stream_module = StreamModule, stream = Stream0} = State0, {Reg, '<', 0}) ->
    %% RISC-V: bge Reg, zero, offset (branch if Reg >= 0, i.e., NOT negative/NOT less than 0)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = State0#state{stream = Stream1},
    {State1, {bge, Reg, zero}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {Reg, '<', Val}
) when is_atom(Reg), is_integer(Val), Val >= 0, Val =< 255 ->
    % RISC-V: bge Reg, Val, offset (branch if Reg >= Val, i.e., NOT less than)
    % Load immediate into a temp register for comparison
    [Temp | _] = State0#state.available_regs,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = State1#state{stream = Stream2},
    {State2, {bge, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    {Reg, '<', Val}
) when is_atom(Reg), is_integer(Val) ->
    % RISC-V: bge Reg, Temp, offset (branch if Reg >= Temp, i.e., NOT less than)
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = State1#state{stream = Stream2},
    {State2, {bge, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % RISC-V: bge Reg, RegB, offset (branch if Reg >= RegB, i.e., NOT less than)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {bge, Reg, RegB}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: bne Reg, zero, offset (branch if Reg != 0, i.e., NOT equal to 0)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {bne, Reg, zero}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: bne Reg, RegB, offset (branch if Reg != RegB, i.e., NOT equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {bne, Reg, RegB}, 0};
%% Delegate (int) forms to regular forms since we only have 32-bit words
if_block_cond(State, {'(int)', RegOrTuple, '==', 0}) ->
    if_block_cond(State, {RegOrTuple, '==', 0});
if_block_cond(State, {'(int)', RegOrTuple, '==', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '==', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) andalso Val >= 0 andalso Val =< 255 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: Load immediate into temp, then beq Reg, Temp, offset
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    {State3, {beq, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: beq Reg, Val, offset (branch if Reg == Val, i.e., NOT not-equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {beq, Reg, Val}, 0};
if_block_cond(State, {'(int)', RegOrTuple, '!=', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '!=', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) andalso Val >= 0 andalso Val =< 255 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: Load immediate into temp, then bne Reg, Temp, offset
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    {State3, {bne, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {{free, RegA}, '==', {free, RegB}}
) ->
    %% RISC-V: bne RegA, RegB, offset (branch if RegA != RegB, i.e., NOT equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = State0#state{stream = Stream1},
    State2 = if_block_free_reg({free, RegA}, State1),
    State3 = if_block_free_reg({free, RegB}, State2),
    {State3, {bne, RegA, RegB}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    %% RISC-V: bne Reg, Temp, offset (branch if Reg != Temp, i.e., NOT equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    {State3, {bne, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    %% RISC-V: beq Reg, Temp, offset (branch if Reg == Temp, i.e., NOT not-equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    {State3, {beq, Reg, Temp}, BranchDelta};
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
    %% RISC-V: Test bit 0 by shifting to MSB, then branch if negative (bit was 1, NOT false)
    I1 = jit_riscv32_asm:slli(Temp, Reg, 31),
    Stream1 = StreamModule:append(Stream0, I1),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream2},
    {State2, {blt, Temp, zero}, byte_size(I1)};
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
    %% RISC-V: Test bit 0 by shifting to MSB, then branch if non-negative (bit was 0, NOT true)
    I1 = jit_riscv32_asm:slli(Temp, Reg, 31),
    Stream1 = StreamModule:append(Stream0, I1),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream2},
    {State2, {bge, Temp, zero}, byte_size(I1)};
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
    %% RISC-V: Test bits using ANDI or li+and
    TestCode =
        if
            Val >= -2048 andalso Val =< 2047 ->
                %% Can use ANDI instruction directly
                jit_riscv32_asm:andi(Temp, Reg, Val);
            true ->
                %% Need to load immediate into temp register first
                TestCode0 = jit_riscv32_asm:li(Temp, Val),
                TestCode1 = jit_riscv32_asm:and_(Temp, Reg, Temp),
                <<TestCode0/binary, TestCode1/binary>>
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    Stream1 = StreamModule:append(Stream0, TestCode),
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    %% Branch if result is zero (no bits set, NOT != 0)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream2},
    {State2, {beq, Temp, zero}, BranchDelta};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {Reg, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    %% RISC-V: Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = jit_riscv32_asm:not_(Temp, Reg),
    I2 = jit_riscv32_asm:slli(Temp, Temp, 28),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = State0#state{stream = Stream2},
    {State1, {beq, Temp, zero}, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    %% RISC-V: Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = jit_riscv32_asm:not_(Reg, Reg),
    I2 = jit_riscv32_asm:slli(Reg, Reg, 28),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = State0#state{stream = Stream2},
    State2 = if_block_free_reg(RegTuple, State1),
    {State2, {beq, Reg, zero}, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | AT]
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    %% RISC-V: AND with mask, then compare with value
    OffsetBefore = StreamModule:offset(Stream0),
    I1 = jit_riscv32_asm:mv(Temp, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State1 = State0#state{stream = Stream1},
    {State2, Temp} = and_(State1#state{available_regs = AT}, {free, Temp}, Mask),
    Stream2 = State2#state.stream,
    %% Compare Temp with Val and branch if equal (NOT != Val)
    case Val of
        0 ->
            %% Optimize comparison with zero
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream3 = StreamModule:append(Stream2, BranchInstr),
            State3 = State2#state{
                stream = Stream3, available_regs = [Temp | State2#state.available_regs]
            },
            {State3, {beq, Temp, zero}, BranchDelta};
        _ when ?IS_GPR(Val) ->
            %% Val is a register
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream3 = StreamModule:append(Stream2, BranchInstr),
            State3 = State2#state{
                stream = Stream3, available_regs = [Temp | State2#state.available_regs]
            },
            {State3, {beq, Temp, Val}, BranchDelta};
        _ ->
            %% Val is an immediate - need second temp register
            %% Reuse the mask register for the comparison value
            [MaskReg | AT2] = AT,
            State3 = mov_immediate(State2#state{available_regs = AT2}, MaskReg, Val),
            Stream3 = State3#state.stream,
            BranchDelta = StreamModule:offset(Stream3) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream4 = StreamModule:append(Stream3, BranchInstr),
            State4 = State3#state{
                stream = Stream4, available_regs = [Temp, MaskReg | State3#state.available_regs]
            },
            {State4, {beq, Temp, MaskReg}, BranchDelta}
    end;
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailRegs
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    %% RISC-V: AND with mask, then compare with value
    OffsetBefore = StreamModule:offset(Stream0),
    {State1, Reg} = and_(State0, RegTuple, Mask),
    Stream1 = State1#state.stream,
    %% Compare Reg with Val and branch if equal (NOT != Val)
    case Val of
        0 ->
            %% Optimize comparison with zero
            BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream2 = StreamModule:append(Stream1, BranchInstr),
            State2 = State1#state{stream = Stream2},
            State3 = if_block_free_reg(RegTuple, State2),
            {State3, {beq, Reg, zero}, BranchDelta};
        _ when ?IS_GPR(Val) ->
            %% Val is a register
            BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream2 = StreamModule:append(Stream1, BranchInstr),
            State2 = State1#state{stream = Stream2},
            State3 = if_block_free_reg(RegTuple, State2),
            {State3, {beq, Reg, Val}, BranchDelta};
        _ ->
            %% Val is an immediate - need temp register
            %% Reuse the mask register for the comparison value
            [MaskReg | AT] = State1#state.available_regs,
            State2 = mov_immediate(State1#state{available_regs = AT}, MaskReg, Val),
            Stream2 = State2#state.stream,
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream3 = StreamModule:append(Stream2, BranchInstr),
            State3 = State2#state{stream = Stream3, available_regs = AvailRegs},
            State4 = if_block_free_reg(RegTuple, State3),
            {State4, {beq, Reg, MaskReg}, BranchDelta}
    end.

-spec if_block_free_reg(riscv32_register() | {free, riscv32_register()}, state()) -> state().
if_block_free_reg({free, Reg}, State0) ->
    #state{available_regs = AvR0, used_regs = UR0} = State0,
    {AvR1, UR1} = free_reg(AvR0, UR0, Reg),
    State0#state{
        available_regs = AvR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

-spec merge_used_regs(state(), [riscv32_register()]) -> state().
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
-spec shift_right(#state{}, maybe_free_riscv32_register(), non_neg_integer()) ->
    {#state{}, riscv32_register()}.
shift_right(#state{stream_module = StreamModule, stream = Stream0} = State, {free, Reg}, Shift) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_riscv32_asm:srli(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    {State#state{stream = Stream1}, Reg};
shift_right(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [ResultReg | T],
        used_regs = UR
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_riscv32_asm:srli(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    {State#state{stream = Stream1, available_regs = T, used_regs = [ResultReg | UR]}, ResultReg}.

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
    I = jit_riscv32_asm:slli(Reg, Reg, Shift),
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
-spec call_func_ptr(state(), {free, riscv32_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), riscv32_register()}.
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
    % Save RA (like AArch64 saves LR) so it's preserved across jalr calls
    SavedRegs = [?RA_REG, ?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | UsedRegs1],

    % Calculate available registers
    FreeGPRegs = FreeRegs -- (FreeRegs -- ?AVAILABLE_REGS),
    AvailableRegs1 = FreeGPRegs ++ AvailableRegs0,

    % Calculate stack space: round up to 16-byte boundary for RISC-V ABI
    NumRegs = length(SavedRegs),
    StackBytes = NumRegs * 4,
    AlignedStackBytes = ((StackBytes + 15) div 16) * 16,

    Stream1 = push_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream0),

    % Set up arguments following RISC-V ILP32 calling convention
    % Arguments are passed in a0-a7 (up to 8 register arguments)
    Args1 = lists:map(
        fun(Arg) ->
            case Arg of
                offset -> StreamModule:offset(Stream1);
                _ -> Arg
            end
        end,
        Args
    ),

    RegArgs0 = Args1,
    RegArgsRegs = lists:flatmap(fun arg_to_reg_list/1, RegArgs0),

    % We pushed registers to stack, so we can use these registers we saved
    % and the currently available registers
    SetArgsRegsOnlyAvailableArgs = (UsedRegs1 -- RegArgsRegs) ++ AvailableRegs0,
    State1 = State0#state{
        available_regs = SetArgsRegsOnlyAvailableArgs,
        used_regs = ?AVAILABLE_REGS -- SetArgsRegsOnlyAvailableArgs,
        stream = Stream1
    },

    ParameterRegs = parameter_regs(RegArgs0),
    {Stream3, SetArgsAvailableRegs, FuncPtrReg, RegArgs} =
        case FuncPtrTuple of
            {free, FuncPtrReg0} ->
                % If FuncPtrReg is in parameter regs, we must swap it with a free reg.
                case lists:member(FuncPtrReg0, ParameterRegs) of
                    true ->
                        case SetArgsRegsOnlyAvailableArgs -- ParameterRegs of
                            [] ->
                                % Swap SetArgsRegsOnlyAvailableArgs with a reg used in RegArgs0
                                % that is not in ParameterRegs
                                [NewArgReg | _] = SetArgsRegsOnlyAvailableArgs,
                                [FuncPtrReg1 | _] = RegArgsRegs -- ParameterRegs,
                                MovInstr1 = jit_riscv32_asm:mv(NewArgReg, FuncPtrReg1),
                                MovInstr2 = jit_riscv32_asm:mv(FuncPtrReg1, FuncPtrReg0),
                                SetArgsAvailableArgs1 =
                                    (SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg1]) ++
                                        [FuncPtrReg0],
                                RegArgs1 = replace_reg(RegArgs0, FuncPtrReg1, NewArgReg),
                                {
                                    StreamModule:append(
                                        State1#state.stream, <<MovInstr1/binary, MovInstr2/binary>>
                                    ),
                                    SetArgsAvailableArgs1,
                                    FuncPtrReg1,
                                    RegArgs1
                                };
                            [FuncPtrReg1 | _] ->
                                MovInstr = jit_riscv32_asm:mv(FuncPtrReg1, FuncPtrReg0),
                                SetArgsAvailableArgs1 =
                                    (SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg1]) ++
                                        [FuncPtrReg0],
                                {
                                    StreamModule:append(State1#state.stream, MovInstr),
                                    SetArgsAvailableArgs1,
                                    FuncPtrReg1,
                                    RegArgs0
                                }
                        end;
                    false ->
                        SetArgsAvailableArgs1 = SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg0],
                        {State1#state.stream, SetArgsAvailableArgs1, FuncPtrReg0, RegArgs0}
                end;
            {primitive, Primitive} ->
                [FuncPtrReg0 | _] = SetArgsRegsOnlyAvailableArgs -- ParameterRegs,
                SetArgsAvailableRegs1 = SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg0],
                PrepCall = load_primitive_ptr(Primitive, FuncPtrReg0),
                Stream2 = StreamModule:append(State1#state.stream, PrepCall),
                {Stream2, SetArgsAvailableRegs1, FuncPtrReg0, RegArgs0}
        end,

    State3 = State1#state{
        available_regs = SetArgsAvailableRegs,
        used_regs = ?AVAILABLE_REGS -- SetArgsAvailableRegs,
        stream = Stream3
    },

    StackOffset = AlignedStackBytes,
    State4 = set_registers_args(State3, RegArgs, ParameterRegs, StackOffset),
    Stream4 = State4#state.stream,

    % Call the function pointer (using JALR for call with return)
    Call = jit_riscv32_asm:jalr(ra, FuncPtrReg, 0),
    Stream5 = StreamModule:append(Stream4, Call),

    % For result, we need a free register (including FuncPtrReg).
    % If none are available (all registers were pushed to the stack),
    % we write the result to the stack position of FuncPtrReg
    {Stream6, UsedRegs2} =
        case length(SavedRegs) of
            N when N >= 7 andalso element(1, FuncPtrTuple) =:= free ->
                % We use original FuncPtrReg then as we know it's available.
                % Calculate stack offset: find register index in SavedRegs * 4 bytes
                ResultReg = element(2, FuncPtrTuple),
                RegIndex = index_of(ResultReg, SavedRegs),
                case RegIndex >= 0 of
                    true ->
                        StoreResultStackOffset = RegIndex * 4,
                        StoreResult = jit_riscv32_asm:sw(sp, a0, StoreResultStackOffset),
                        {StreamModule:append(Stream5, StoreResult), [ResultReg | UsedRegs1]};
                    false ->
                        % FuncPtrReg was not in SavedRegs, use an available register
                        [ResultReg1 | _] = AvailableRegs1 -- SavedRegs,
                        MoveResult = jit_riscv32_asm:mv(ResultReg1, a0),
                        {StreamModule:append(Stream5, MoveResult), [ResultReg1 | UsedRegs1]}
                end;
            _ ->
                % Use any free that is not in SavedRegs
                [ResultReg | _] = AvailableRegs1 -- SavedRegs,
                MoveResult = jit_riscv32_asm:mv(ResultReg, a0),
                {StreamModule:append(Stream5, MoveResult), [ResultReg | UsedRegs1]}
        end,

    Stream8 = pop_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream6),

    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    {
        State4#state{
            stream = Stream8,
            available_regs = AvailableRegs3,
            used_regs = UsedRegs2
        },
        ResultReg
    }.

arg_to_reg_list({free, {ptr, Reg}}) -> [Reg];
arg_to_reg_list({free, Reg}) when is_atom(Reg) -> [Reg];
arg_to_reg_list(Reg) when is_atom(Reg) -> [Reg];
arg_to_reg_list(_) -> [].

index_of(Item, List) -> index_of(Item, List, 0).

index_of(_, [], _) -> -1;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Rest], Index) -> index_of(Item, Rest, Index + 1).

push_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    % RISC-V: addi sp, sp, -AlignedStackBytes then sw reg, offset(sp) for each reg
    StackAdjust = jit_riscv32_asm:addi(sp, sp, -AlignedStackBytes),
    Stream1 = StreamModule:append(Stream0, StackAdjust),
    {Stream2, _} = lists:foldl(
        fun(Reg, {StreamAcc, Offset}) ->
            Store = jit_riscv32_asm:sw(sp, Reg, Offset),
            {StreamModule:append(StreamAcc, Store), Offset + 4}
        end,
        {Stream1, 0},
        SavedRegs
    ),
    Stream2;
push_registers([], _AlignedStackBytes, _StreamModule, Stream0) ->
    Stream0.

pop_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    % RISC-V: lw reg, offset(sp) for each reg then addi sp, sp, AlignedStackBytes
    {Stream1, _} = lists:foldl(
        fun(Reg, {StreamAcc, Offset}) ->
            Load = jit_riscv32_asm:lw(Reg, sp, Offset),
            {StreamModule:append(StreamAcc, Load), Offset + 4}
        end,
        {Stream0, 0},
        SavedRegs
    ),
    StackAdjust = jit_riscv32_asm:addi(sp, sp, AlignedStackBytes),
    StreamModule:append(Stream1, StackAdjust);
pop_registers([], _AlignedStackBytes, _StreamModule, Stream0) ->
    Stream0.

set_registers_args(State0, Args, StackOffset) ->
    ParamRegs = parameter_regs(Args),
    set_registers_args(State0, Args, ParamRegs, StackOffset).

set_registers_args(
    #state{used_regs = UsedRegs} = State0,
    Args,
    ParamRegs,
    StackOffset
) ->
    ArgsRegs = args_regs(Args),
    AvailableScratchGP = ((?SCRATCH_REGS -- ParamRegs) -- ArgsRegs) -- UsedRegs,
    State1 = set_registers_args0(
        State0, Args, ArgsRegs, ParamRegs, AvailableScratchGP, StackOffset
    ),
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
    State1#state{
        stream = Stream1,
        available_regs = ?AVAILABLE_REGS -- ParamRegs -- NewUsedRegs,
        used_regs = ParamRegs ++ (NewUsedRegs -- ParamRegs)
    }.

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

% ILP32: 64-bit arguments require double-word alignment (even register number)
parameter_regs0([], _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([{avm_int64_t, _} | T], [a0, a1 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a1, a0 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [a1, a2 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a2, a1 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [a2, a3 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a3, a2 | Acc]);
parameter_regs0([_Other | T], [Reg | Rest], Acc) ->
    parameter_regs0(T, Rest, [Reg | Acc]).

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

set_registers_args0(State, [], [], [], _AvailGP, _StackOffset) ->
    State;
set_registers_args0(State, [{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, StackOffset) ->
    set_registers_args0(State, [FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, StackOffset);
set_registers_args0(
    State, [ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, StackOffset
) ->
    set_registers_args0(State, ArgsT, ArgsRegs, ParamRegs, AvailGP, StackOffset);
% Handle 64-bit arguments that need two registers according to ILP32
set_registers_args0(
    State,
    [{avm_int64_t, Value} | ArgsT],
    ArgsRegs,
    ParamRegs,
    AvailGP,
    StackOffset
) when is_integer(Value) ->
    LowPart = Value band 16#FFFFFFFF,
    HighPart = (Value bsr 32) band 16#FFFFFFFF,
    set_registers_args0(
        State, [LowPart, HighPart | ArgsT], [imm | ArgsRegs], ParamRegs, AvailGP, StackOffset
    );
% ctx is special as we need it to access x_reg/y_reg/fp_reg and we don't
% want to replace it
set_registers_args0(
    State, [Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, StackOffset
) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    State1 = set_registers_args1(State, Arg, ?CTX_REG, StackOffset),
    set_registers_args0(State1, ArgsT, ArgsRegs, ParamRegs, AvailGP, StackOffset);
set_registers_args0(
    #state{stream_module = StreamModule} = State0,
    [Arg | ArgsT],
    [_ArgReg | ArgsRegsT],
    [ParamReg | ParamRegsT],
    AvailGP,
    StackOffset
) ->
    case lists:member(ParamReg, ArgsRegsT) of
        false ->
            State1 = set_registers_args1(State0, Arg, ParamReg, StackOffset),
            set_registers_args0(State1, ArgsT, ArgsRegsT, ParamRegsT, AvailGP, StackOffset);
        true ->
            [Avail | AvailGPT] = AvailGP,
            I = jit_riscv32_asm:mv(Avail, ParamReg),
            Stream1 = StreamModule:append(State0#state.stream, I),
            State1 = set_registers_args1(
                State0#state{stream = Stream1}, Arg, ParamReg, StackOffset
            ),
            NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
            set_registers_args0(
                State1, NewArgsT, ArgsRegsT, ParamRegsT, AvailGPT, StackOffset
            )
    end.

set_registers_args1(State, Reg, Reg, _Offset) ->
    State;
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    jit_state,
    ParamReg,
    _StackOffset
) ->
    % jit_state is always in a1, so we only need to move it if the param reg is different
    case ParamReg of
        a1 ->
            State;
        _ ->
            I = jit_riscv32_asm:mv(ParamReg, a1),
            Stream1 = StreamModule:append(Stream0, I),
            State#state{stream = Stream1}
    end;
% For tail calls, jit_state is already in a1
set_registers_args1(State, jit_state_tail_call, a1, _StackOffset) ->
    State;
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {x_reg, extra},
    Reg,
    _StackOffset
) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I = jit_riscv32_asm:lw(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, Reg, _StackOffset
) ->
    {XReg, X_REGOffset} = ?X_REG(X),
    I = jit_riscv32_asm:lw(Reg, XReg, X_REGOffset),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Source}, Reg, _StackOffset
) ->
    I = jit_riscv32_asm:lw(Reg, Source, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = AvailRegs} = State,
    {y_reg, X},
    Reg,
    _StackOffset
) ->
    Code = ldr_y_reg(Reg, X, AvailRegs),
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, ArgReg, Reg, _StackOffset
) when
    ?IS_GPR(ArgReg)
->
    I = jit_riscv32_asm:mv(Reg, ArgReg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(State, Value, Reg, _StackOffset) when ?IS_SIGNED_OR_UNSIGNED_INT32_T(Value) ->
    mov_immediate(State, Reg, Value).

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
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_riscv32_asm:sw(BaseReg, Src, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {x_reg, X}) when is_atom(Src) ->
    {BaseReg, Off} = ?X_REG(X),
    I1 = jit_riscv32_asm:sw(BaseReg, Src, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_riscv32_asm:sw(Reg, Src, 0),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp1 | AT]} = State0, Src, {y_reg, Y}) when
    is_atom(Src)
->
    Code = str_y_reg(Src, Y, Temp1, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, Code),
    State0#state{stream = Stream1};
% Source is an integer to y_reg (optimized: ldr first, then movs)
move_to_vm_register(#state{available_regs = [Temp1, Temp2 | AT]} = State0, N, {y_reg, Y}) when
    is_integer(N), N >= 0, N =< 255
->
    I1 = jit_riscv32_asm:li(Temp2, N),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, YCode/binary>>),
    State0#state{stream = Stream1};
% Source is an integer (0-255 for movs, negative values need different handling)
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, N, Dest) when
    is_integer(N), N >= 0, N =< 255
->
    I1 = jit_riscv32_asm:li(Temp, N),
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
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_riscv32_asm:lw(Temp, BaseReg, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, X}, Dest) ->
    {XReg, X_REGOffset} = ?X_REG(X),
    I1 = jit_riscv32_asm:lw(Temp, XReg, X_REGOffset),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {ptr, Reg}, Dest) ->
    I1 = jit_riscv32_asm:lw(Temp, Reg, 0),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {y_reg, Y}, Dest) ->
    Code = ldr_y_reg(Temp, Y, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, Code),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
% term_to_float
move_to_vm_register(
    #state{
        stream_module = StreamModule,
        available_regs = [Temp1, Temp2 | _],
        stream = Stream0,
        variant = Variant
    } =
        State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) ->
    {BaseReg, Off} = ?FP_REGS,
    I1 = jit_riscv32_asm:lw(Temp1, BaseReg, Off),
    I2 = jit_riscv32_asm:lw(Temp2, Reg, 4),
    case Variant band ?JIT_VARIANT_FLOAT32 of
        0 ->
            % Double precision: write both 32-bit parts
            I3 = jit_riscv32_asm:sw(Temp1, Temp2, F * 8),
            I4 = jit_riscv32_asm:lw(Temp2, Reg, 8),
            I5 = jit_riscv32_asm:sw(Temp1, Temp2, F * 8 + 4),
            Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>;
        _ ->
            % Single precision: write only first 32-bit part
            I3 = jit_riscv32_asm:sw(Temp1, Temp2, F * 4),
            Code = <<I1/binary, I2/binary, I3/binary>>
    end,
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
    riscv32_register(),
    non_neg_integer() | riscv32_register(),
    vm_register() | riscv32_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_riscv32_asm:lw(Temp, Reg, Index * 4),
    {BaseReg, Off} = ?X_REG(X),
    I2 = jit_riscv32_asm:sw(BaseReg, Temp, Off),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_riscv32_asm:lw(Temp, Reg, Index * 4),
    I2 = jit_riscv32_asm:sw(Dest, Temp, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | AT]} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    I1 = jit_riscv32_asm:lw(Temp2, Reg, Index * 4),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | AT]} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    I1 = jit_riscv32_asm:lw(Reg, Reg, Index * 4),
    YCode = str_y_reg(Reg, Y, Temp, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_riscv32_asm:lw(Dest, Reg, Index * 4),
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
    I1 = jit_riscv32_asm:slli(IndexReg, IndexReg, 2),
    I2 = jit_riscv32_asm:add(IndexReg, Reg, IndexReg),
    I3 = jit_riscv32_asm:lw(IndexReg, IndexReg, 0),
    {BaseReg, Off} = ?X_REG(X),
    I4 = jit_riscv32_asm:sw(BaseReg, IndexReg, Off),
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
    I1 = jit_riscv32_asm:slli(IndexReg, IndexReg, 2),
    I2 = jit_riscv32_asm:add(IndexReg, Reg, IndexReg),
    I3 = jit_riscv32_asm:lw(IndexReg, IndexReg, 0),
    I4 = jit_riscv32_asm:sw(PtrReg, IndexReg, 0),
    {AvailableRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, UsedRegs0, IndexReg
    ),
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
        available_regs = [Temp | AT] = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when is_atom(IndexReg) ->
    I1 = jit_riscv32_asm:slli(IndexReg, IndexReg, 2),
    I2 = jit_riscv32_asm:add(IndexReg, Reg, IndexReg),
    I3 = jit_riscv32_asm:lw(IndexReg, IndexReg, 0),
    Code = str_y_reg(IndexReg, Y, Temp, AT),
    I4 = Code,
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
-spec get_array_element(
    state(), riscv32_register() | {free, riscv32_register()}, non_neg_integer()
) ->
    {state(), riscv32_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = jit_riscv32_asm:lw(Reg, Reg, Index * 4),
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
    I1 = jit_riscv32_asm:lw(ElemReg, Reg, Index * 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    {
        State#state{
            stream = Stream1, available_regs = AvailableT, used_regs = [ElemReg | UsedRegs0]
        },
        ElemReg
    }.

%% @doc move an integer, a vm or native register to reg[x]
-spec move_to_array_element(
    state(), integer() | vm_register() | riscv32_register(), riscv32_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_riscv32_asm:sw(Reg, ValueReg, Index * 4),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    ValueReg,
    Reg,
    IndexReg
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_riscv32_asm:mv(Temp, IndexReg),
    I2 = jit_riscv32_asm:slli(Temp, Temp, 2),
    I3 = jit_riscv32_asm:add(Temp, Reg, Temp),
    I4 = jit_riscv32_asm:sw(Temp, ValueReg, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
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
    I1 = jit_riscv32_asm:addi(Temp, IndexReg, Offset),
    I2 = jit_riscv32_asm:slli(Temp, Temp, 2),
    I3 = jit_riscv32_asm:add(Temp, BaseReg, Temp),
    I4 = jit_riscv32_asm:sw(Temp, ValueReg, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
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
    I1 = jit_riscv32_asm:addi(Temp, IndexReg, Offset),
    I2 = jit_riscv32_asm:slli(Temp, Temp, 2),
    I3 = jit_riscv32_asm:add(Temp, BaseReg, Temp),
    I4 = jit_riscv32_asm:sw(Temp, ValueReg, 0),
    Stream1 = (State1#state.stream_module):append(
        State1#state.stream, <<I1/binary, I2/binary, I3/binary, I4/binary>>
    ),
    State2 = State1#state{stream = Stream1},
    free_native_register(State2, ValueReg).

-spec move_to_native_register(state(), value() | cp) -> {state(), riscv32_register()}.
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    cp
) ->
    {BaseReg, Off} = ?CP,
    I1 = jit_riscv32_asm:lw(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, used_regs = [Reg | Used], available_regs = AvailT}, Reg};
move_to_native_register(State, Reg) when is_atom(Reg) ->
    {State, Reg};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}
) when is_atom(Reg) ->
    I1 = jit_riscv32_asm:lw(Reg, Reg, 0),
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
    {x_reg, extra}
) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_riscv32_asm:lw(Reg, BaseReg, Off),
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
    {BaseReg, Offset} = ?X_REG(X),
    I1 = jit_riscv32_asm:lw(Reg, BaseReg, Offset),
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
    Code = ldr_y_reg(Reg, Y, AvailT),
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
    {BaseReg, Off} = ?FP_REGS,
    I1 = jit_riscv32_asm:lw(RegB, BaseReg, Off),
    I2 = jit_riscv32_asm:lw(RegA, RegB, F * 8),
    I3 = jit_riscv32_asm:lw(RegB, RegB, F * 8 + 4),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_regs = AvailT, used_regs = [RegB, RegA | Used]},
        {fp, RegA, RegB}
    }.

-spec move_to_native_register(state(), value(), riscv32_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, RegSrc, RegDst
) when is_atom(RegSrc) ->
    I = jit_riscv32_asm:mv(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
move_to_native_register(State, ValSrc, RegDst) when is_integer(ValSrc) ->
    mov_immediate(State, RegDst, ValSrc);
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}, RegDst
) when ?IS_GPR(Reg) ->
    I1 = jit_riscv32_asm:lw(RegDst, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, extra}, RegDst
) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_riscv32_asm:lw(RegDst, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    {XReg, X_REGOffset} = ?X_REG(X),
    I1 = jit_riscv32_asm:lw(RegDst, XReg, X_REGOffset),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = AT} = State,
    {y_reg, Y},
    RegDst
) ->
    Code = ldr_y_reg(RegDst, Y, AT),
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
    {BaseReg, Off} = ?FP_REGS,
    I1 = jit_riscv32_asm:lw(RegB, BaseReg, Off),
    I2 = jit_riscv32_asm:lw(RegA, RegB, F * 8),
    I3 = jit_riscv32_asm:lw(RegB, RegB, F * 8 + 4),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec copy_to_native_register(state(), value()) -> {state(), riscv32_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [SaveReg | AvailT],
        used_regs = Used
    } = State,
    Reg
) when is_atom(Reg) ->
    I1 = jit_riscv32_asm:mv(SaveReg, Reg),
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
    I1 = jit_riscv32_asm:lw(SaveReg, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | AvailT]} = State,
    {y_reg, Y}
) ->
    I1 = ldr_y_reg(Reg, Y, AvailT),
    {BaseReg, Off} = ?CP,
    I2 = jit_riscv32_asm:sw(BaseReg, Reg, Off),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    Offset
) ->
    {BaseReg1, Off1} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(Reg, BaseReg1, Off1),
    I2 = jit_riscv32_asm:addi(Reg, Reg, Offset * 4),
    {BaseReg2, Off2} = ?Y_REGS,
    I3 = jit_riscv32_asm:sw(BaseReg2, Reg, Off2),
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
    % Similar to AArch64: use pc_relative_address with a relocation that will be
    % resolved to point directly to the label's actual address (not the jump table entry)
    Offset = StreamModule:offset(Stream0),
    % Emit placeholder for pc_relative_address (auipc + addi)
    % Reserve 8 bytes (2 x 32-bit instructions) with all-1s placeholder for flash programming
    % The relocation will replace these with the correct offset
    I1 = <<16#FFFFFFFF:32/little, 16#FFFFFFFF:32/little>>,
    Reloc = {Label, Offset, {adr, Temp}},
    % Store continuation (jit_state is in a1)
    I2 = jit_riscv32_asm:sw(?JITSTATE_REG, Temp, ?JITSTATE_CONTINUATION_OFFSET),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | Branches]}.

%% @doc Set the contination to a given offset
%% Return a reference so the offset will be updated with update_branches
%% This is only used with OP_WAIT_TIMEOUT and the offset is after the current
%% code and not too far, so on Thumb we can use adr instruction.
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
    % Reserve 8 bytes with all-1s placeholder for flash programming
    I1 = <<16#FFFFFFFF:32/little, 16#FFFFFFFF:32/little>>,
    Reloc = {OffsetRef, Offset, {adr, Temp}},
    % Store continuation (jit_state is in a1)
    I2 = jit_riscv32_asm:sw(?JITSTATE_REG, Temp, ?JITSTATE_CONTINUATION_OFFSET),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | Branches]}, OffsetRef}.

%% @doc Implement a continuation entry point.
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
    % Load module from jit_state (which is in a1)
    I1 = jit_riscv32_asm:lw(Reg, ?JITSTATE_REG, ?JITSTATE_MODULE_OFFSET),
    I2 = jit_riscv32_asm:lw(Reg, Reg, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{
            stream = Stream1,
            available_regs = AvailableT,
            used_regs = [Reg | UsedRegs0]
        },
        Reg
    }.

%% @doc Perform an AND of a register with an immediate.
%% JIT currentl calls this with two values: ?TERM_PRIMARY_CLEAR_MASK (-4) to
%% clear bits and ?TERM_BOXED_TAG_MASK (0x3F). We can avoid any literal pool
%% by using BICS for -4.
and_(#state{stream_module = StreamModule, stream = Stream0} = State0, {free, Reg}, 16#FFFFFF) ->
    I1 = jit_riscv32_asm:slli(Reg, Reg, 8),
    I2 = jit_riscv32_asm:srli(Reg, Reg, 8),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    {State0#state{stream = Stream1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0,
    {free, Reg},
    Val
) when Val < 0 andalso Val >= -256 ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, bnot (Val)),
    Stream1 = State1#state.stream,
    % RISC-V doesn't have bics, use not + and
    I1 = jit_riscv32_asm:not_(Temp, Temp),
    I2 = jit_riscv32_asm:and_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, I2/binary>>),
    {State1#state{available_regs = [Temp | AT], stream = Stream2}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0,
    {free, Reg},
    Val
) ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_riscv32_asm:and_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    {State1#state{available_regs = [Temp | AT], stream = Stream2}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = []} = State0,
    {free, Reg},
    Val
) when Val < 0 andalso Val >= -256 ->
    % No available registers, use a0 as temp and save it to t3
    Stream0 = State0#state.stream,
    % Save a0 to t3
    Save = jit_riscv32_asm:mv(?IP_REG, a0),
    Stream1 = StreamModule:append(Stream0, Save),
    % Load immediate value into a0
    State1 = mov_immediate(State0#state{stream = Stream1}, a0, bnot (Val)),
    Stream2 = State1#state.stream,
    % Perform BICS operation (RISC-V: not + and)
    I1 = jit_riscv32_asm:not_(a0, a0),
    I2 = jit_riscv32_asm:and_(Reg, Reg, a0),
    Stream3 = StreamModule:append(Stream2, <<I1/binary, I2/binary>>),
    % Restore a0 from t3
    Restore = jit_riscv32_asm:mv(a0, ?IP_REG),
    Stream4 = StreamModule:append(Stream3, Restore),
    {State0#state{stream = Stream4}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = []} = State0,
    {free, Reg},
    Val
) ->
    % No available registers, use a0 as temp and save it to t3
    Stream0 = State0#state.stream,
    % Save a0 to t3
    Save = jit_riscv32_asm:mv(?IP_REG, a0),
    Stream1 = StreamModule:append(Stream0, Save),
    % Load immediate value into a0
    State1 = mov_immediate(State0#state{stream = Stream1}, a0, Val),
    Stream2 = State1#state.stream,
    % Perform ANDS operation
    I = jit_riscv32_asm:and_(Reg, Reg, a0),
    Stream3 = StreamModule:append(Stream2, I),
    % Restore a0 from t3
    Restore = jit_riscv32_asm:mv(a0, ?IP_REG),
    Stream4 = StreamModule:append(Stream3, Restore),
    {State0#state{stream = Stream4}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = [ResultReg | AT], used_regs = UR} =
        State0,
    Reg,
    ?TERM_PRIMARY_CLEAR_MASK
) ->
    I = jit_riscv32_asm:andi(ResultReg, Reg, -4),
    Stream1 = StreamModule:append(State0#state.stream, I),
    {State0#state{stream = Stream1, available_regs = AT, used_regs = [ResultReg | UR]}, ResultReg}.

or_(
    #state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0,
    Reg,
    Val
) ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_riscv32_asm:or_(Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{available_regs = [Temp | AT], stream = Stream2}.

add(#state{stream_module = StreamModule, stream = Stream0} = State0, Reg, Val) when
    Val >= 0 andalso Val =< 255
->
    I = jit_riscv32_asm:addi(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State0#state{stream = Stream1};
add(#state{stream_module = StreamModule, stream = Stream0} = State0, Reg, Val) when
    is_atom(Val)
->
    I = jit_riscv32_asm:add(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State0#state{stream = Stream1};
add(#state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0, Reg, Val) ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_riscv32_asm:add(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{available_regs = [Temp | AT], stream = Stream2}.

mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) when
    Val >= -16#800, Val =< 16#7FF
->
    % RISC-V li can handle 12-bit signed immediates in a single instruction (addi)
    I = jit_riscv32_asm:li(Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    % For values outside 12-bit range, li will use lui + addi (2 instructions)
    % which is efficient enough, no need for literal pool
    I = jit_riscv32_asm:li(Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

sub(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) when
    Val >= 0 andalso Val =< 255
->
    I1 = jit_riscv32_asm:addi(Reg, Reg, -Val),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
sub(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) when
    is_atom(Val)
->
    I = jit_riscv32_asm:sub(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
sub(#state{stream_module = StreamModule, available_regs = [Temp | AT]} = State0, Reg, Val) ->
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_riscv32_asm:sub(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{available_regs = [Temp | AT], stream = Stream2}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{available_regs = [Temp | _]} = State, Reg, 3) ->
    I1 = jit_riscv32_asm:slli(Temp, Reg, 1),
    I2 = jit_riscv32_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 5) ->
    I1 = jit_riscv32_asm:slli(Temp, Reg, 2),
    I2 = jit_riscv32_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 7) ->
    I1 = jit_riscv32_asm:slli(Temp, Reg, 3),
    I2 = jit_riscv32_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{available_regs = [Temp | _]} = State, Reg, 9) ->
    I1 = jit_riscv32_asm:slli(Temp, Reg, 3),
    I2 = jit_riscv32_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{available_regs = [Temp | _]} = State, Reg, 15) ->
    I1 = jit_riscv32_asm:slli(Temp, Reg, 4),
    I2 = jit_riscv32_asm:sub(Reg, Temp, Reg),
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
    I = jit_riscv32_asm:mul(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    State1#state{stream = Stream2, available_regs = [Temp | State1#state.available_regs]}.

%%
%% Analysis of AArch64 pattern and RISC-V32 implementation:
%%
%% AArch64 layout (from call_ext_only_test):
%%   0x0-0x8:  Decrement reductions, store back
%%   0xc:      b.ne 0x20   ; Branch if reductions != 0 to continuation
%%   0x10-0x1c: adr/str/ldr/br sequence for scheduling next process
%%   0x20:     [CONTINUATION POINT] - Actual function starts here
%%
%% RISC-V32 implementation (no prolog/epilog needed due to 32 registers):
%%   0x0-0x8:  Decrement reductions, store back
%%   0xc:      bne continuation ; Branch if reductions != 0 to continuation
%%   0x10-0x?:  adr/sw/ldr/jalr sequence for scheduling next process
%%   continuation: [actual function body]
%%
%% Key insight: With 32 registers, RISC-V32 doesn't need prolog/epilog like ARM Thumb.
%% When reductions != 0, we branch directly to continue execution.
%% When reductions == 0, we schedule the next process, and resume at the continuation point.
%%
-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0
) ->
    % Load reduction count
    I1 = jit_riscv32_asm:lw(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    % Decrement reduction count
    I2 = jit_riscv32_asm:addi(Temp, Temp, -1),
    % Store back the decremented value
    I3 = jit_riscv32_asm:sw(?JITSTATE_REG, Temp, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch if reduction count is not zero
    I4 = <<16#FFFFFFFF:32/little>>,
    % Set continuation to the next instruction
    ADROffset = BNEOffset + byte_size(I4),
    % Use 8-byte placeholder (2 words of 0xFFFFFFFF) for pc_relative_address
    % This ensures we can always rewrite with either auipc alone (4 bytes) or auipc+addi (8 bytes)
    I5 = <<16#FFFFFFFF:32/little, 16#FFFFFFFF:32/little>>,
    I6 = jit_riscv32_asm:sw(?JITSTATE_REG, Temp, ?JITSTATE_CONTINUATION_OFFSET),
    % Append the instructions to the stream
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary, I6/binary>>),
    State1 = State0#state{stream = Stream2},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite the branch and adr instructions
    #state{stream = Stream3} = State2,
    NewOffset = StreamModule:offset(Stream3),
    NewI4 = jit_riscv32_asm:bne(Temp, zero, NewOffset - BNEOffset),
    NewI5Offset = NewOffset - ADROffset,
    % Generate the new pc_relative_address instruction, padding with NOP if needed
    NewI5 =
        case pc_relative_address(Temp, NewI5Offset) of
            I when byte_size(I) =:= 4 ->
                % Only auipc, pad with NOP (4 bytes)
                <<I/binary, (jit_riscv32_asm:nop())/binary>>;
            I when byte_size(I) =:= 6 ->
                % auipc + c.addi, pad with c.nop (2 bytes)
                <<I/binary, (jit_riscv32_asm:c_nop())/binary>>;
            I when byte_size(I) =:= 8 ->
                % auipc + addi, no padding needed
                I
        end,
    Stream4 = StreamModule:replace(
        Stream3, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    merge_used_regs(State2#state{stream = Stream4}, State1#state.used_regs).

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    Label
) ->
    % Load reduction count (jit_state is in a1)
    I1 = jit_riscv32_asm:lw(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    % Decrement reduction count
    I2 = jit_riscv32_asm:addi(Temp, Temp, -1),
    % Store back the decremented value
    I3 = jit_riscv32_asm:sw(?JITSTATE_REG, Temp, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    % Use trampoline technique: branch if zero (eq) to skip over the long branch
    % If not zero, we want to continue execution at Label
    % If zero, we want to fall through to scheduling code

    % Look up label once to avoid duplicate lookup in helper
    LabelLookupResult = lists:keyfind(Label, 1, State0#state.labels),

    BccOffset = StreamModule:offset(Stream1),

    State4 =
        case LabelLookupResult of
            {Label, LabelOffset} ->
                % Label is known, check if we can optimize the conditional branch
                % After branch instruction
                Rel = LabelOffset - BccOffset,

                if
                    Rel >= -4096 andalso Rel =< 4094 andalso (Rel rem 2) =:= 0 ->
                        % Near branch: use direct conditional branch (RISC-V has ±4KB range)

                        % Branch if NOT zero (temp != 0)
                        I4 = jit_riscv32_asm:bne(Temp, zero, Rel),
                        Stream2 = StreamModule:append(Stream1, I4),
                        State0#state{stream = Stream2};
                    true ->
                        % Far branch: use trampoline with helper
                        % Get the code block size for the far branch sequence that will follow

                        % RISC-V branch is 4 bytes
                        FarSeqOffset = BccOffset + 4,
                        {State1, FarCodeBlock} = branch_to_label_code(
                            State0, FarSeqOffset, Label, LabelLookupResult
                        ),
                        FarSeqSize = byte_size(FarCodeBlock),
                        % Skip over the far branch sequence if zero (temp == 0)
                        I4 = jit_riscv32_asm:beq(Temp, zero, FarSeqSize + 4),
                        Stream2 = StreamModule:append(Stream1, I4),
                        Stream3 = StreamModule:append(Stream2, FarCodeBlock),
                        State1#state{stream = Stream3}
                end;
            false ->
                % Label not known, get the far branch size for the skip

                % RISC-V branch is 4 bytes
                FarSeqOffset = BccOffset + 4,
                {State1, FarCodeBlock} = branch_to_label_code(State0, FarSeqOffset, Label, false),
                FarSeqSize = byte_size(FarCodeBlock),
                I4 = jit_riscv32_asm:beq(Temp, zero, FarSeqSize + 4),
                Stream2 = StreamModule:append(Stream1, I4),
                Stream3 = StreamModule:append(Stream2, FarCodeBlock),
                State1#state{stream = Stream3}
        end,
    State5 = set_continuation_to_label(State4, Label),
    call_primitive_last(State5, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

-spec set_cp(state()) -> {state(), non_neg_integer(), riscv32_register()}.
set_cp(#state{available_regs = [TempReg | AvailT], used_regs = UsedRegs} = State0) ->
    % Reserve a temporary register for the offset BEFORE calling get_module_index
    % to avoid running out of available registers
    State0b = State0#state{available_regs = AvailT, used_regs = [TempReg | UsedRegs]},
    % get module index (dynamically)
    {
        #state{stream_module = StreamModule, stream = Stream0} = State1,
        Reg
    } = get_module_index(
        State0b
    ),

    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_riscv32_asm:slli(Reg, Reg, 24),
    % Reserve space for offset load instruction
    % li can generate 1 instruction (4 bytes) for small immediates (< 2048)
    % or 2 instructions (8 bytes) for large immediates
    % Since we don't know the final CP value yet (it depends on code size),
    % we must always reserve 2 instructions (8 bytes) to be safe
    % The final CP value is (final_offset << 2), and final_offset is unknown
    % Use 0xFFFFFFFF placeholders for flash compatibility (can only flip 1->0)
    I2 = <<16#FFFFFFFF:32/little>>,
    I3 = <<16#FFFFFFFF:32/little>>,
    MOVOffset = Offset + byte_size(I1),
    % OR the module index with the offset (loaded in temp register)
    I4 = jit_riscv32_asm:or_(Reg, TempReg),
    {BaseReg, Off} = ?CP,
    I5 = jit_riscv32_asm:sw(BaseReg, Reg, Off),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    State4 = free_native_register(State3, TempReg),
    {State4, MOVOffset, TempReg}.

-spec rewrite_cp_offset(state(), non_neg_integer(), riscv32_register()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset,
    TempReg
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    CPValue = NewOffset bsl 2,
    NewMoveInstr = jit_riscv32_asm:li(TempReg, CPValue),
    % We reserved 8 bytes (2 instructions) for the CP value
    % Pad with NOP if needed to maintain alignment
    PaddedInstr =
        case byte_size(NewMoveInstr) of
            2 ->
                <<NewMoveInstr/binary, (jit_riscv32_asm:nop())/binary,
                    (jit_riscv32_asm:c_nop())/binary>>;
            4 ->
                <<NewMoveInstr/binary, (jit_riscv32_asm:nop())/binary>>;
            6 ->
                <<NewMoveInstr/binary, (jit_riscv32_asm:c_nop())/binary>>;
            8 ->
                NewMoveInstr
        end,
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, PaddedInstr),
    State0#state{stream = Stream1}.

set_bs(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    TermReg
) ->
    {BaseReg1, Off1} = ?BS,
    I1 = jit_riscv32_asm:sw(BaseReg1, TermReg, Off1),
    I2 = jit_riscv32_asm:li(Temp, 0),
    {BaseReg2, Off2} = ?BS_OFFSET,
    I3 = jit_riscv32_asm:sw(BaseReg2, Temp, Off2),
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

    I2 = jit_riscv32_asm:ret(),
    % Assume total size is 10 bytes (8-byte I1 + 2-byte c.ret)
    % If actual is 8 bytes (6-byte I1 + 2-byte c.ret), we'll pad with 2 bytes
    I1 = pc_relative_address(a0, 10),
    Prologue = <<I1/binary, I2/binary>>,
    ProloguePadded =
        case byte_size(Prologue) of
            10 -> Prologue;
            % 2-byte padding
            8 -> <<Prologue/binary, 16#FFFF:16>>
        end,
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<ProloguePadded/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

%% @doc Generate PC-relative address calculation using AUIPC + ADDI
%% This replaces the ARM-style 'adr' pseudo-instruction with native RISC-V instructions
-spec pc_relative_address(riscv32_register(), integer()) -> binary().
pc_relative_address(Rd, 0) ->
    % Simple case: just get current PC
    jit_riscv32_asm:auipc(Rd, 0);
pc_relative_address(Rd, Offset) ->
    % PC-relative address calculation
    % Split offset into upper 20 bits and lower 12 bits
    % AUIPC can represent offsets in range: (-524288 << 12) to (524287 << 12)
    % Combined with ADDI: (-524288 << 12) - 2048 to (524287 << 12) + 2047
    Lower = Offset band 16#FFF,
    % Sign extend lower 12 bits
    LowerSigned =
        if
            Lower >= 16#800 -> Lower - 16#1000;
            true -> Lower
        end,
    % Compute upper 20 bits, adjusting if lower is negative
    % Use arithmetic right shift (bsr) which preserves sign in Erlang
    Upper =
        if
            LowerSigned < 0 ->
                (Offset bsr 12) + 1;
            true ->
                Offset bsr 12
        end,
    % Validate that Upper is in valid range for AUIPC
    if
        Upper < -16#80000; Upper > 16#7FFFF ->
            error({offset_out_of_range, Offset, Upper, -16#80000, 16#7FFFF});
        true ->
            ok
    end,
    case {Upper, LowerSigned} of
        {0, 0} ->
            % Zero offset
            jit_riscv32_asm:auipc(Rd, 0);
        {0, _} ->
            % Only lower bits needed: auipc + addi
            AuipcInstr = jit_riscv32_asm:auipc(Rd, 0),
            AddiInstr = jit_riscv32_asm:addi(Rd, Rd, LowerSigned),
            <<AuipcInstr/binary, AddiInstr/binary>>;
        {_, 0} ->
            % Only upper bits needed
            jit_riscv32_asm:auipc(Rd, Upper);
        {_, _} ->
            % Both upper and lower bits
            AuipcInstr = jit_riscv32_asm:auipc(Rd, Upper),
            AddiInstr = jit_riscv32_asm:addi(Rd, Rd, LowerSigned),
            <<AuipcInstr/binary, AddiInstr/binary>>
    end.

%% Helper function to generate str instruction with y_reg offset, handling large offsets
str_y_reg(SrcReg, Y, TempReg, _AvailableRegs) when Y * 4 =< 124 ->
    % Small offset - use immediate addressing
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(TempReg, BaseReg, Off),
    I2 = jit_riscv32_asm:sw(TempReg, SrcReg, Y * 4),
    <<I1/binary, I2/binary>>;
str_y_reg(SrcReg, Y, TempReg1, [TempReg2 | _]) ->
    % Large offset - use register arithmetic with second available register
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(TempReg1, BaseReg, Off),
    I2 = jit_riscv32_asm:li(TempReg2, Offset),
    I3 = jit_riscv32_asm:add(TempReg2, TempReg2, TempReg1),
    I4 = jit_riscv32_asm:sw(TempReg2, SrcReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary>>;
str_y_reg(SrcReg, Y, TempReg1, []) ->
    % Large offset - no additional registers available, use IP_REG as second temp
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(TempReg1, BaseReg, Off),
    I2 = jit_riscv32_asm:mv(?IP_REG, TempReg1),
    I3 = jit_riscv32_asm:li(TempReg1, Offset),
    I4 = jit_riscv32_asm:add(TempReg1, TempReg1, ?IP_REG),
    I5 = jit_riscv32_asm:sw(TempReg1, SrcReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>.

%% Helper function to generate ldr instruction with y_reg offset, handling large offsets
ldr_y_reg(DstReg, Y, [TempReg | _]) when Y * 4 =< 124 ->
    % Small offset - use immediate addressing
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(TempReg, BaseReg, Off),
    I2 = jit_riscv32_asm:lw(DstReg, TempReg, Y * 4),
    <<I1/binary, I2/binary>>;
ldr_y_reg(DstReg, Y, [TempReg | _]) ->
    % Large offset - use DstReg as second temp register for arithmetic
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(TempReg, BaseReg, Off),
    I2 = jit_riscv32_asm:li(DstReg, Offset),
    I3 = jit_riscv32_asm:add(DstReg, DstReg, TempReg),
    I4 = jit_riscv32_asm:lw(DstReg, DstReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary>>;
ldr_y_reg(DstReg, Y, []) when Y * 4 =< 124 ->
    % Small offset, no registers available - use DstReg as temp
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(DstReg, BaseReg, Off),
    I2 = jit_riscv32_asm:lw(DstReg, DstReg, Y * 4),
    <<I1/binary, I2/binary>>;
ldr_y_reg(DstReg, Y, []) ->
    % Large offset, no registers available - use IP_REG as temp register
    % Note: IP_REG (t3) can only be used with mov, not ldr directly
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_riscv32_asm:lw(DstReg, BaseReg, Off),
    I2 = jit_riscv32_asm:mv(?IP_REG, DstReg),
    I3 = jit_riscv32_asm:li(DstReg, Offset),
    I4 = jit_riscv32_asm:add(DstReg, DstReg, ?IP_REG),
    I5 = jit_riscv32_asm:lw(DstReg, DstReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>.

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
            ({free, {fp_reg, _}}) -> ?CTX_REG;
            ({avm_int64_t, _}) -> imm
        end,
        Args
    ).

%%-----------------------------------------------------------------------------
%% @doc Add a label at the current offset.
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference()) -> state().
add_label(#state{stream_module = StreamModule, stream = Stream0} = State0, Label) ->
    Offset0 = StreamModule:offset(Stream0),
    add_label(State0, Label, Offset0).

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
