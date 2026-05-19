%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(jit_xtensa).

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
    cond_jump_to_label/3,
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
    add_label/3,
    get_regs_tracking/1,
    xor_/3,
    shift_right_arith/3,
    div_reg/3,
    rem_reg/3
]).

-ifdef(JIT_DWARF).
-export([
    dwarf_opcode/2,
    dwarf_label/2,
    dwarf_function/3,
    dwarf_line/2,
    dwarf_variables/2,
    dwarf_ctx_register/0
]).
-endif.

%% Exported for tests (see tests/libs/jit/jit_xtensa_tests.erl)
-export([
    mov_immediate/2,
    mov_immediate_large/2
]).

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").
-include("term.hrl").

-ifdef(JIT_DWARF).
-include("jit_dwarf.hrl").
-endif.

-define(ASSERT(Expr), true = Expr).

%% Xtensa (ESP32 LX6/LX7) windowed ABI:
%% C calls JIT via CALL8. JIT entry points start with ENTRY a1, 32.
%% JIT returns to C with RETW (not RET).
%% JIT calls C primitives via CALLX8 with args in a10-a15
%%   (callee sees them as a2-a7).
%% Return value from C comes back in a10 (callee's a2 mapped back).
%% CALLX8 preserves caller's a0-a7 automatically (register window).
%% a8-a15 are clobbered by CALLX8.
%% a0 contains encoded return address - MUST NOT be modified (needed for RETW).
%%
%% The assembler specifically targets ESP32 with instructions such as quos and
%% rems.
%%
%% Registers used by the JIT backend (Xtensa windowed):
%%   - Context: a2 (ctx pointer, first parameter from C via CALL8)
%%   - JITState: a3 (second parameter from C)
%%   - Native interface: a4 (third parameter from C)
%%   - Return address: a0 (encoded, must not be touched - needed for RETW)
%%   - Stack pointer: a1
%%   - Available for JIT scratch: a5-a15 (11 registers)
%%     a5-a7: preserved across CALLX8 (in caller's window)
%%     a8-a15: clobbered by CALLX8
%%   - A8_REG: a8 (special scratch, not in SCRATCH_REGS)

-type xtensa_register() ::
    a0
    | a1
    | a2
    | a3
    | a4
    | a5
    | a6
    | a7
    | a8
    | a9
    | a10
    | a11
    | a12
    | a13
    | a14
    | a15.

-define(IS_GPR(Reg),
    (Reg =:= a0 orelse Reg =:= a1 orelse Reg =:= a2 orelse Reg =:= a3 orelse Reg =:= a4 orelse
        Reg =:= a5 orelse Reg =:= a6 orelse Reg =:= a7 orelse Reg =:= a8 orelse Reg =:= a9 orelse
        Reg =:= a10 orelse Reg =:= a11 orelse Reg =:= a12 orelse Reg =:= a13 orelse
        Reg =:= a14 orelse Reg =:= a15)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: #{integer() | reference() => [{non_neg_integer(), non_neg_integer()}]},
    jump_table_start :: non_neg_integer(),
    available_regs :: non_neg_integer(),
    used_regs :: non_neg_integer(),
    labels :: #{integer() | reference() => integer()},
    variant :: non_neg_integer(),
    regs :: jit_regs:regs()
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, xtensa_register()}.
-type value() :: immediate() | vm_register() | xtensa_register() | {ptr, xtensa_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_xtensa_register() ::
    {free, xtensa_register()} | xtensa_register().

-type condition() ::
    {xtensa_register(), '<', integer()}
    | {maybe_free_xtensa_register(), '<', xtensa_register()}
    | {integer(), '<', maybe_free_xtensa_register()}
    | {maybe_free_xtensa_register(), '==', integer()}
    | {maybe_free_xtensa_register(), '!=', xtensa_register() | integer()}
    | {'(int)', maybe_free_xtensa_register(), '==', integer()}
    | {'(int)', maybe_free_xtensa_register(), '!=', xtensa_register() | integer()}
    | {'(bool)', maybe_free_xtensa_register(), '==', false}
    | {'(bool)', maybe_free_xtensa_register(), '!=', false}
    | {maybe_free_xtensa_register(), '&', non_neg_integer(), '!=', integer()}
    | {{free, xtensa_register()}, '==', {free, xtensa_register()}}.

% Context offsets (32-bit architecture)
% ctx->e is 0x14
% ctx->x is 0x18
-define(CTX_REG, a2).
-define(NATIVE_INTERFACE_REG, a4).
-define(Y_REGS, {?CTX_REG, 16#14}).
-define(X_REG(N), {?CTX_REG, 16#18 + (N * 4)}).
-define(CP, {?CTX_REG, 16#5C}).
-define(FP_REGS, {?CTX_REG, 16#60}).
-define(BS, {?CTX_REG, 16#64}).
-define(BS_OFFSET, {?CTX_REG, 16#68}).
-define(JITSTATE_REG, a3).
-define(JITSTATE_MODULE_OFFSET, 0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#8).
-define(JITSTATE_CODE_BASE_OFFSET, 16#C).

%% Each jump table entry: literal(4) + ENTRY(3) + L32R(3) + L32I(3) + ADD(3) + JX(3) + pad(1) = 20 bytes
-define(JUMP_TABLE_ENTRY_SIZE, 20).
%% Offset from start of entry to the ENTRY instruction (skip the 4-byte literal)
-define(JUMP_TABLE_OFFSET, 4).
%% Size of code_relative_address_padded: l32i(3) + mov_immediate(padded to 15) + add(3) = 21
-define(CODE_RELATIVE_ADDRESS_PADDED_SIZE, 21).
%% ENTRY frame size: 32 bytes for window save areas (base + extra) +
%% 16 bytes gap for alignment +
%% 48 bytes for local storage (push_registers at FRAME_LOCAL_OFFSET).
%% Total must be 16-byte aligned.
-define(ENTRY_FRAME_SIZE, 96).
%% Offset within the ENTRY frame where push_registers stores data.
%% This must be above the window save areas (32 bytes).
-define(FRAME_LOCAL_OFFSET, 48).

%% Use a8 as temporary for some operations
-define(A8_REG, a8).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_B4CONST(X),
    (X =:= -1 orelse X =:= 1 orelse X =:= 2 orelse X =:= 3 orelse X =:= 4 orelse
        X =:= 5 orelse X =:= 6 orelse X =:= 7 orelse X =:= 8 orelse X =:= 10 orelse
        X =:= 12 orelse X =:= 16 orelse X =:= 32 orelse X =:= 64 orelse X =:= 128 orelse
        X =:= 256)
).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).
-define(IS_SIGNED_OR_UNSIGNED_INT32_T(X),
    is_integer(X) andalso X >= -16#80000000 andalso X < 16#100000000
).

-define(AVAILABLE_REGS, [a15, a14, a13, a12, a11, a10, a9, a7, a6, a5]).
-define(PARAMETER_REGS, [a10, a11, a12, a13, a14, a15]).
-define(SCRATCH_REGS, [a15, a14, a13, a12, a11, a10, a9]).

-define(REG_BIT_A0, (1 bsl 0)).
-define(REG_BIT_A1, (1 bsl 1)).
-define(REG_BIT_A2, (1 bsl 2)).
-define(REG_BIT_A3, (1 bsl 3)).
-define(REG_BIT_A4, (1 bsl 4)).
-define(REG_BIT_A5, (1 bsl 5)).
-define(REG_BIT_A6, (1 bsl 6)).
-define(REG_BIT_A7, (1 bsl 7)).
-define(REG_BIT_A8, (1 bsl 8)).
-define(REG_BIT_A9, (1 bsl 9)).
-define(REG_BIT_A10, (1 bsl 10)).
-define(REG_BIT_A11, (1 bsl 11)).
-define(REG_BIT_A12, (1 bsl 12)).
-define(REG_BIT_A13, (1 bsl 13)).
-define(REG_BIT_A14, (1 bsl 14)).
-define(REG_BIT_A15, (1 bsl 15)).

-define(AVAILABLE_REGS_MASK,
    (?REG_BIT_A15 bor ?REG_BIT_A14 bor ?REG_BIT_A13 bor ?REG_BIT_A12 bor
        ?REG_BIT_A11 bor ?REG_BIT_A10 bor ?REG_BIT_A9 bor
        ?REG_BIT_A7 bor ?REG_BIT_A6 bor ?REG_BIT_A5)
).
-define(SCRATCH_REGS_MASK,
    (?REG_BIT_A15 bor ?REG_BIT_A14 bor ?REG_BIT_A13 bor ?REG_BIT_A12 bor
        ?REG_BIT_A11 bor ?REG_BIT_A10 bor ?REG_BIT_A9)
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
        branches = #{},
        jump_table_start = 0,
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        labels = #{},
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
    Stream1 = StreamModule:append(Stream0, jit_xtensa_asm:break(1, 15)),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [xtensa_register()].
used_regs(#state{used_regs = Used}) -> mask_to_list(Used).

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [xtensa_register()].
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
) when is_atom(Reg) ->
    Bit = reg_bit(Reg),
    State#state{
        available_regs = Available0 bor Bit, used_regs = Used0 band (bnot Bit)
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
%%
%% On Xtensa, each jump table entry is 20 bytes:
%% ```
%% .word label_offset         ; 4-byte literal (code-relative offset)
%% entry a1, 96               ; 3 bytes (windowed ABI frame setup)
%% l32r a5, [literal above]   ; 3 bytes (load label offset, imm16=-2)
%% l32i a8, a3, 0xC           ; 3 bytes (load code_base from jit_state)
%% add a8, a8, a5             ; 3 bytes (compute absolute address)
%% jx a8                      ; 3 bytes (indirect jump)
%% .byte 0xFF                 ; 1 byte padding (alignment)
%% ```
%%
%% @end
%% @param State current backend state
%% @param LabelsCount number of labels in the module.
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec jump_table(state(), pos_integer()) -> state().
jump_table(#state{stream_module = StreamModule, stream = Stream0} = State, LabelsCount) ->
    JumpTableStart = StreamModule:offset(Stream0),
    jump_table0(State#state{jump_table_start = JumpTableStart}, 0, LabelsCount).

jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    N,
    LabelsCount
) ->
    %% The 4-byte literal will be patched in add_label with the code-relative target offset.
    %% L32R imm16 = -2: at PC=entry_start+7, ((PC+3)&~3) = entry_start+8,
    %% target = -2*4 + entry_start+8 = entry_start = literal position.
    %% We use a5 for the offset (safe: a5 is unallocated at function entry from C).
    %% a8 (A8_REG) holds code_base then the final target address.
    %% a0 (return address) must NOT be clobbered, RETW needs it later.
    LiteralPlaceholder = <<16#FF, 16#FF, 16#FF, 16#FF>>,
    EntryInstr = jit_xtensa_asm:entry(a1, ?ENTRY_FRAME_SIZE),
    LoadOffset = jit_xtensa_asm:l32r(a5, -2),
    LoadCodeBase = jit_xtensa_asm:l32i(?A8_REG, ?JITSTATE_REG, ?JITSTATE_CODE_BASE_OFFSET),
    AddInstr = jit_xtensa_asm:add(?A8_REG, ?A8_REG, a5),
    JxInstr = jit_xtensa_asm:jx(?A8_REG),
    Padding = <<16#FF>>,
    JumpEntry =
        <<LiteralPlaceholder/binary, EntryInstr/binary, LoadOffset/binary, LoadCodeBase/binary,
            AddInstr/binary, JxInstr/binary, Padding/binary>>,
    20 = byte_size(JumpEntry),
    Stream1 = StreamModule:append(Stream0, JumpEntry),
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
    NewInstr =
        case Type of
            {adr, Reg, HeaderOffset} ->
                % Generate code_relative_address padded to 21 bytes
                % LabelOffset is a stream offset; subtract header to get code-relative
                code_relative_address_padded(Reg, LabelOffset - HeaderOffset);
            {far_branch, Temp, JumpTableStart} ->
                % 24-byte placeholder: J+NOPs for near, indirect jump for far
                Rel = LabelOffset - Offset - 4,
                case Rel >= -131072 andalso Rel =< 131071 of
                    true ->
                        J = jit_xtensa_asm:j(Rel),
                        Nops = list_to_binary([jit_xtensa_asm:nop() || _ <- lists:seq(1, 7)]),
                        <<J/binary, Nops/binary>>;
                    false ->
                        CodeRelativeTarget = LabelOffset - JumpTableStart,
                        I1 = jit_xtensa_asm:l32i(
                            ?A8_REG, ?JITSTATE_REG, ?JITSTATE_CODE_BASE_OFFSET
                        ),
                        I2 = mov_immediate(Temp, CodeRelativeTarget),
                        MovSize = byte_size(I2),
                        NopCount = (15 - MovSize) div 3,
                        Nops = list_to_binary([jit_xtensa_asm:nop() || _ <- lists:seq(1, NopCount)]),
                        PaddedMov = <<I2/binary, Nops/binary>>,
                        15 = byte_size(PaddedMov),
                        I3 = jit_xtensa_asm:add(?A8_REG, ?A8_REG, Temp),
                        I4 = jit_xtensa_asm:jx(?A8_REG),
                        <<I1/binary, PaddedMov/binary, I3/binary, I4/binary>>
                end
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
    integer() | reference(),
    non_neg_integer(),
    #{integer() | reference() => [{non_neg_integer(), non_neg_integer()}]}
) ->
    {stream(), #{integer() | reference() => [{non_neg_integer(), non_neg_integer()}]}}.
patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Branches) ->
    case Branches of
        #{TargetLabel := BrList} ->
            Stream1 = lists:foldl(
                fun({Offset, Type}, AccStream) ->
                    patch_branch(StreamModule, AccStream, Offset, Type, LabelOffset)
                end,
                Stream,
                BrList
            ),
            {Stream1, maps:remove(TargetLabel, Branches)};
        _ ->
            {Stream, Branches}
    end.

%%-----------------------------------------------------------------------------
%% @doc Rewrite stream to update all branches for labels.
%% @end
%% @param State current backend state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec update_branches(state()) -> state().
update_branches(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        labels = Labels
    } = State
) ->
    % all branches Labels are in Labels
    Stream1 = maps:fold(
        fun(Label, BrList, AccStream) ->
            #{Label := LabelOffset} = Labels,
            lists:foldl(
                fun({Offset, Type}, AccStream2) ->
                    patch_branch(StreamModule, AccStream2, Offset, Type, LabelOffset)
                end,
                AccStream,
                BrList
            )
        end,
        Stream0,
        Branches
    ),
    State#state{stream = Stream1, branches = #{}}.

%%-----------------------------------------------------------------------------
%% @doc Generate code to load a primitive function pointer into a register
%% @param Primitive index to the primitive to call
%% @param TargetReg register to load the function pointer into
%% @return Binary instruction sequence
%%-----------------------------------------------------------------------------
-spec load_primitive_ptr(non_neg_integer(), xtensa_register()) -> binary().
load_primitive_ptr(Primitive, TargetReg) ->
    case Primitive of
        0 ->
            jit_xtensa_asm:l32i(TargetReg, ?NATIVE_INTERFACE_REG, 0);
        N when N * 4 =< 1020 ->
            % There are definitely less than 256 primitives
            jit_xtensa_asm:l32i(TargetReg, ?NATIVE_INTERFACE_REG, N * 4)
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
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), xtensa_register()}.
call_primitive(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used
    } = State,
    Primitive,
    Args
) when Available =/= 0 ->
    TempReg = first_avail(Available),
    TempBit = reg_bit(TempReg),
    PrepCall = load_primitive_ptr(Primitive, TempReg),
    Stream1 = StreamModule:append(Stream0, PrepCall),
    StateCall = State#state{
        stream = Stream1,
        available_regs = Available band (bnot TempBit),
        used_regs = Used bor TempBit
    },
    call_func_ptr(StateCall, {free, TempReg}, Args);
call_primitive(
    #state{available_regs = 0} = State,
    Primitive,
    Args
) ->
    call_func_ptr(State, {primitive, Primitive}, Args).

%%-----------------------------------------------------------------------------
%% @doc Emit a jump (call without return) to a primitive with arguments. This
%% function converts arguments and pass them following the backend ABI
%% convention.
%%
%% Unlike other backends, which tail-jump to the primitive so its own return
%% returns directly to C, on Xtensa we emit a plain CALLX8 + RETW. Primitives
%% begin with an ENTRY instruction and follow the windowed ABI, and there is
%% no standard way to tail-jump into a windowed callee: ENTRY requires a fresh
%% rotated window (PS.CALLINC set by a preceding CALL4/8/12), so a bare JX
%% would either fault or corrupt WindowStart. GCC and LLVM likewise fall back
%% to call+retw when tail-calling a windowed callee.
%%
%% The functional cost is one extra register window plus ENTRY_FRAME_SIZE
%% bytes of stack that live only for the duration of the primitive call; it
%% is unwound before we return to C, so the overhead does not accumulate
%% across BEAM tail-call chains
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
    %% Xtensa windowed ABI: CALLX8 to the primitive, move its return value
    %% from a10 (our view of callee's a2) into our a2, then RETW to C.
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    ArgsRegs = args_regs(Args),
    ArgsRegsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ScratchMask = ?AVAILABLE_REGS_MASK band (bnot (ArgsRegsMask bor ParamMask)),
    Temp = first_avail(ScratchMask),
    TempBit = reg_bit(Temp),
    AvailableRegs1 = ScratchMask band (bnot TempBit),
    UsedMask = ?AVAILABLE_REGS_MASK band (bnot AvailableRegs1),
    PrepCall = load_primitive_ptr(Primitive, Temp),
    Stream1 = StreamModule:append(Stream0, PrepCall),

    State1 = State0#state{
        stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedMask
    },

    Args1 = lists:map(
        fun(Arg) ->
            case Arg of
                offset -> StreamModule:offset(Stream1);
                _ -> Arg
            end
        end,
        Args
    ),
    case Args1 of
        [FirstArg, jit_state | ArgsT] ->
            ArgsForCall = [FirstArg, jit_state_tail_call | ArgsT],
            ParameterRegs = parameter_regs(Args1),
            State2 = set_registers_args(State1, ArgsForCall, ParameterRegs, 0),
            Stream2 = State2#state.stream,
            I_call = jit_xtensa_asm:callx8(Temp),
            I_mv = jit_xtensa_asm:mov(a2, a10),
            I_retw = jit_xtensa_asm:retw(),
            Stream3 = StreamModule:append(Stream2, <<I_call/binary, I_mv/binary, I_retw/binary>>),
            State2#state{
                stream = Stream3,
                available_regs = ?AVAILABLE_REGS_MASK,
                used_regs = 0,
                regs = jit_regs:unreachable(State2#state.regs)
            }
    end.

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
    % Xtensa windowed ABI: return value in a2 (= CTX_REG)
    % RETW returns to the caller's register window
    I2 =
        case Reg of
            % Return value is already in a2
            a2 -> <<>>;
            % Move to a2 (return register)
            _ -> jit_xtensa_asm:mov(a2, Reg)
        end,
    I3 = jit_xtensa_asm:retw(),
    %% Branch if equal (skip the return)
    %% Offset accounts for beq instruction (3 bytes) plus I2 and I3
    I1 = jit_xtensa_asm:beq(Reg, ?CTX_REG, byte_size(I2) + byte_size(I3) - 1),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    RegBit = reg_bit(Reg),
    State#state{
        stream = Stream1,
        available_regs = AvailableRegs0 bor RegBit,
        used_regs = UsedRegs0 band (bnot RegBit),
        regs = State#state.regs
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
    LabelLookupResult =
        case Labels of
            #{Label := LabelOffset} -> {Label, LabelOffset};
            _ -> false
        end,
    Offset = StreamModule:offset(Stream0),
    {State1, CodeBlock} = branch_to_label_code(State0, Offset, Label, LabelLookupResult),
    Stream1 = StreamModule:append(Stream0, CodeBlock),
    State1#state{stream = Stream1, regs = jit_regs:unreachable(State1#state.regs)}.

jump_to_offset(#state{stream_module = StreamModule, stream = Stream0} = State, TargetOffset) ->
    Offset = StreamModule:offset(Stream0),
    CodeBlock = branch_to_offset_code(State, Offset, TargetOffset),
    Stream1 = StreamModule:append(Stream0, CodeBlock),
    State#state{stream = Stream1, regs = jit_regs:unreachable(State#state.regs)}.

cond_jump_to_label(State, Cond, Label) ->
    if_block(State, Cond, fun(S) -> jump_to_label(S, Label) end).

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
        available_regs = Available,
        offset = BaseOffset
    } = State0,
    {free, OffsetReg}
) ->
    Temp = first_avail(Available),
    % Calculate absolute address: native_code_base + target_offset
    % where native_code_base = current_pc + (BaseOffset - CurrentStreamOffset)
    CurrentStreamOffset = StreamModule:offset(Stream0),
    _NetOffset = BaseOffset - CurrentStreamOffset,

    I1 = jit_xtensa_asm:l32i(Temp, ?JITSTATE_REG, ?JITSTATE_CODE_BASE_OFFSET),
    I2 = jit_xtensa_asm:add(Temp, Temp, OffsetReg),
    %% Skip past the ENTRY instruction (3 bytes) at the continuation entry point.
    %% All continuation targets have an ENTRY instruction (needed when C code
    %% calls via CALL8 through jit_return). When jumping from within JIT code
    %% we are already in a windowed frame, so we must skip the ENTRY.
    I3 = jit_xtensa_asm:addi(Temp, Temp, 3),
    I4 = jit_xtensa_asm:jx(Temp),

    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free all registers since this is a tail jump
    State0#state{
        stream = Stream1,
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        regs = jit_regs:unreachable(State0#state.regs)
    }.

branch_to_offset_code(State, Offset, TargetOffset) ->
    %% J instruction has 18-bit signed offset range (+-131072 bytes)
    Rel = TargetOffset - Offset - 4,
    case Rel >= -131072 andalso Rel =< 131071 of
        true ->
            jit_xtensa_asm:j(Rel);
        false ->
            %% Far jump: use code_base + code-relative offset via indirect jump.
            %% Need two scratch regs: A8_REG (a8) for code_base/target, and one
            %% available register for the offset immediate.
            #state{jump_table_start = JumpTableStart, available_regs = Avail} = State,
            CodeRelativeTarget = TargetOffset - JumpTableStart,
            Temp = first_avail(Avail),
            I1 = jit_xtensa_asm:l32i(?A8_REG, ?JITSTATE_REG, ?JITSTATE_CODE_BASE_OFFSET),
            I2 = mov_immediate(Temp, CodeRelativeTarget),
            I3 = jit_xtensa_asm:add(?A8_REG, ?A8_REG, Temp),
            I4 = jit_xtensa_asm:jx(?A8_REG),
            <<I1/binary, I2/binary, I3/binary, I4/binary>>
    end.

branch_to_label_code(State, Offset, Label, {Label, LabelOffset}) ->
    CodeBlock = branch_to_offset_code(State, Offset, LabelOffset),
    {State, CodeBlock};
branch_to_label_code(
    #state{branches = Branches, available_regs = Avail, jump_table_start = JTS} = State0,
    Offset,
    Label,
    false
) ->
    %% Reserve 24 bytes for forward branch placeholder.
    %% Near targets will use J + NOPs, far targets use indirect jump.
    %% Pick a temp register now and encode it in the relocation.
    Temp = first_avail(Avail),
    CodeBlock = list_to_binary(lists:duplicate(24, 16#FF)),
    BrEntry = {Offset, {far_branch, Temp, JTS}},
    State1 = State0#state{branches = Branches#{Label => [BrEntry | maps:get(Label, Branches, [])]}},
    {State1, CodeBlock}.

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
            {NewAccState, JumpDelta} = if_block_cond(AccState, Cond),
            {[{Offset + JumpDelta} | AccReplacements], NewAccState}
        end,
        {[], State0},
        CondList
    ),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({JumpOffset}, AccStream) ->
            JumpRel = OffsetAfter - JumpOffset - 4,
            NewJumpInstr = jit_xtensa_asm:j(JumpRel),
            StreamModule:replace(AccStream, JumpOffset, NewJumpInstr)
        end,
        Stream2,
        Replacements
    ),
    State3 = merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs),
    MergedRegs = jit_regs:merge(State1#state.regs, State2#state.regs),
    State3#state{regs = MergedRegs};
if_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, JumpDelta} = if_block_cond(State0, Cond),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    %% Patch the J instruction in the trampoline to jump to the end of the block
    JumpOffset = Offset + JumpDelta,
    JumpRel = OffsetAfter - JumpOffset - 4,
    NewJumpInstr = jit_xtensa_asm:j(JumpRel),
    Stream3 = StreamModule:replace(Stream2, JumpOffset, NewJumpInstr),
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
    {State1, JumpDelta} = if_block_cond(State0, Cond),
    JumpInstrOffset = Offset + JumpDelta,
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    %% Emit unconditional J to skip the else block (will be replaced, 3 bytes)
    ElseJumpOffset = StreamModule:offset(Stream2),
    %% Use all-1s placeholder for flash compatibility (can only flip 1->0)
    ElseJumpInstr = <<16#FF, 16#FF, 16#FF>>,
    Stream3 = StreamModule:append(Stream2, ElseJumpInstr),
    %% Else block starts here.
    OffsetAfter = StreamModule:offset(Stream3),
    %% Patch the J in the trampoline to jump to the else block
    JumpRel = OffsetAfter - JumpInstrOffset - 4,
    NewJumpInstr = jit_xtensa_asm:j(JumpRel),
    Stream4 = StreamModule:replace(Stream3, JumpInstrOffset, NewJumpInstr),
    %% Build the else block
    StateElse = State2#state{
        stream = Stream4,
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    %% Patch the unconditional J to jump to the end (3 bytes)
    FinalJumpOffset = OffsetFinal - ElseJumpOffset - 4,
    NewElseJumpInstr = jit_xtensa_asm:j(FinalJumpOffset),
    3 = byte_size(NewElseJumpInstr),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    State4 = merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs),
    MergedRegs = jit_regs:merge(State2#state.regs, State3#state.regs),
    State4#state{regs = MergedRegs}.

-spec if_block_cond(state(), condition()) ->
    {state(), non_neg_integer()}.
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '<', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: bltz Reg, +2 (skip over J if less than 0) + J placeholder
    I1 = jit_xtensa_asm:bltz(Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', Val}
) when ?IS_B4CONST(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: blti Reg, Val, +2; J placeholder (skip J if less than Val)
    I1 = jit_xtensa_asm:blti(Reg, Val, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val), Val >= 0, Val =< 255 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: load Val, blt Reg, Temp, +2; J placeholder
    Temp =
        case State0#state.available_regs of
            0 -> ?A8_REG;
            _ -> first_avail(State0#state.available_regs)
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    I1 = jit_xtensa_asm:blt(Reg, Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val) ->
    Temp =
        case Available of
            0 -> ?A8_REG;
            _ -> first_avail(Available)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: blt Reg, Temp, +2; J placeholder
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    I1 = jit_xtensa_asm:blt(Reg, Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val), Val >= 0, Val =< 255 ->
    Temp =
        case Available of
            0 -> ?A8_REG;
            _ -> first_avail(Available)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: blt Temp, Reg, +2; J placeholder
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    I1 = jit_xtensa_asm:blt(Temp, Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val) ->
    Temp =
        case Available of
            0 -> ?A8_REG;
            _ -> first_avail(Available)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: blt Temp, Reg, +2; J placeholder
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    I1 = jit_xtensa_asm:blt(Temp, Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: blt Reg, RegB, +2; J placeholder
    I1 = jit_xtensa_asm:blt(Reg, RegB, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: beqz Reg, +2 (skip over J if equal to 0) + J placeholder
    I1 = jit_xtensa_asm:beqz(Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '!=', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: bnez Reg, +2 (skip over J if not equal to 0) + J placeholder
    I1 = jit_xtensa_asm:bnez(Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: beq Reg, RegB, +2; J placeholder
    I1 = jit_xtensa_asm:beq(Reg, RegB, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
%% Delegate (int) forms to regular forms since we only have 32-bit words
if_block_cond(State, {'(int)', RegOrTuple, '==', 0}) ->
    if_block_cond(State, {RegOrTuple, '==', 0});
if_block_cond(State, {'(int)', RegOrTuple, '==', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '==', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when ?IS_B4CONST(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: bnei Reg, Val, +2; J placeholder
    I1 = jit_xtensa_asm:bnei(Reg, Val, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) andalso Val >= 0 andalso Val =< 255 ->
    Temp =
        case Available of
            0 -> ?A8_REG;
            _ -> first_avail(Available)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: li Temp, Val; bne Reg, Temp, +2; J placeholder
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    I1 = jit_xtensa_asm:bne(Reg, Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: bne Reg, Val, +2; J placeholder
    I1 = jit_xtensa_asm:bne(Reg, Val, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(State, {'(int)', RegOrTuple, '!=', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '!=', Val});
%% b4const fast paths for ==, !=, < using beqi/bnei/bgei
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', Val}
) when ?IS_B4CONST(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: beqi Reg, Val, +2; J placeholder
    I1 = jit_xtensa_asm:beqi(Reg, Val, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) andalso Val >= 0 andalso Val =< 255 ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa: li Temp, Val; beq Reg, Temp, +2; J placeholder
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    I1 = jit_xtensa_asm:beq(Reg, Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {{free, RegA}, '==', {free, RegB}}
) ->
    %% Xtensa: beq RegA, RegB, +2; J placeholder
    I1 = jit_xtensa_asm:beq(RegA, RegB, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = State0#state{stream = Stream1},
    State2 = if_block_free_reg({free, RegA}, State1),
    State3 = if_block_free_reg({free, RegB}, State2),
    {State3, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    %% Xtensa: beq Reg, Temp, +2; J placeholder
    I1 = jit_xtensa_asm:beq(Reg, Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    MovSize = StreamModule:offset(Stream1) - OffsetBefore,
    %% Xtensa: bne Reg, Temp, +2; J placeholder
    I1 = jit_xtensa_asm:bne(Reg, Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I1/binary, JPlaceholder/binary>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    State3 = State2#state{stream = Stream2},
    JumpDelta = MovSize + byte_size(I1),
    {State3, JumpDelta};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Condition: Reg == false (== 0). Block executes when condition is met.
    %% Branch skips J when condition IS met (Reg == 0) to enter the block.
    %% When condition is NOT met (Reg != 0), fall through to J which skips block.
    I1 = jit_xtensa_asm:beqz(Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Condition: Reg != false (Reg != 0). Block executes when condition is met.
    %% Branch skips J when condition IS met (Reg != 0) to enter the block.
    %% When condition is NOT met (Reg == 0), fall through to J which skips block.
    I1 = jit_xtensa_asm:bnez(Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, JPlaceholder/binary>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        regs = Regs0
    } = State0,
    {RegOrTuple, '&', Val, '!=', 0}
) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Xtensa has no andi instruction - always use li+and_
    TestCode0 = mov_immediate(Temp, Val),
    TestCode1 = jit_xtensa_asm:and_(Temp, Reg, Temp),
    TestCode = <<TestCode0/binary, TestCode1/binary>>,
    OffsetBefore = StreamModule:offset(Stream0),
    Stream1 = StreamModule:append(Stream0, TestCode),
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    %% Xtensa: bnez Temp, +2; J placeholder (skip J if nonzero = IS != 0)
    I_beqz = jit_xtensa_asm:bnez(Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I_beqz/binary, JPlaceholder/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State1 = if_block_free_reg(RegOrTuple, State0#state{regs = Regs1}),
    State2 = State1#state{stream = Stream2},
    BranchDelta2 = BranchDelta + byte_size(I_beqz),
    {State2, BranchDelta2};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        regs = Regs0
    } = State0,
    {Reg, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    %% NOT(Reg) via movi -1 + xor, then slli to isolate low bits.
    I1 = <<(jit_xtensa_asm:movi(Temp, -1))/binary, (jit_xtensa_asm:xor_(Temp, Temp, Reg))/binary>>,
    I2 = jit_xtensa_asm:slli(Temp, Temp, 28),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    I3 = jit_xtensa_asm:bnez(Temp, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I3/binary, JPlaceholder/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State1 = State0#state{stream = Stream2, regs = Regs1},
    {State1, byte_size(I1) + byte_size(I2) + byte_size(I3)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    {{free, Reg} = RegTuple, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    %% NOT(Reg) in-place via neg + addi -1, then slli to isolate low bits.
    I1 = <<(jit_xtensa_asm:neg(Reg, Reg))/binary, (jit_xtensa_asm:addi(Reg, Reg, -1))/binary>>,
    I2 = jit_xtensa_asm:slli(Reg, Reg, 28),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    I3 = jit_xtensa_asm:bnez(Reg, 2),
    JPlaceholder = <<16#FF, 16#FF, 16#FF>>,
    Stream2 = StreamModule:append(Stream1, <<I3/binary, JPlaceholder/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State1 = State0#state{stream = Stream2, regs = Regs1},
    State2 = if_block_free_reg(RegTuple, State1),
    {State2, byte_size(I1) + byte_size(I2) + byte_size(I3)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg), Val =/= 0 ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    AT = Avail band (bnot reg_bit(Temp)),
    OffsetBefore = StreamModule:offset(Stream0),
    I1 = jit_xtensa_asm:mov(Temp, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State1 = State0#state{stream = Stream1},
    {State2, Temp} = and_(State1#state{available_regs = AT}, {free, Temp}, Mask),
    Stream2 = State2#state.stream,
    case Val of
        _ when ?IS_GPR(Val) ->
            I_beq = jit_xtensa_asm:bne(Temp, Val, 2),
            JPlaceholder2 = <<16#FF, 16#FF, 16#FF>>,
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore + byte_size(I_beq),
            Stream3 = StreamModule:append(Stream2, <<I_beq/binary, JPlaceholder2/binary>>),
            State3 = State2#state{
                stream = Stream3, available_regs = State2#state.available_regs bor reg_bit(Temp)
            },
            {State3, BranchDelta};
        _ ->
            %% Val is an immediate - need second temp register
            MaskReg =
                case AT of
                    0 -> ?A8_REG;
                    _ -> first_avail(AT)
                end,
            AT2 = AT band (bnot reg_bit(MaskReg)),
            State3 = mov_immediate(State2#state{available_regs = AT2}, MaskReg, Val),
            Stream3 = State3#state.stream,
            I_beq2 = jit_xtensa_asm:bne(Temp, MaskReg, 2),
            JPlaceholder3 = <<16#FF, 16#FF, 16#FF>>,
            BranchDelta = StreamModule:offset(Stream3) - OffsetBefore + byte_size(I_beq2),
            Stream4 = StreamModule:append(Stream3, <<I_beq2/binary, JPlaceholder3/binary>>),
            State4 = State3#state{
                stream = Stream4,
                available_regs = State3#state.available_regs bor reg_bit(Temp) bor reg_bit(MaskReg)
            },
            {State4, BranchDelta}
    end;
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailRegs
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg), Val =/= 0 ->
    OffsetBefore = StreamModule:offset(Stream0),
    {State1, Reg} = and_(State0, RegTuple, Mask),
    Stream1 = State1#state.stream,
    case Val of
        _ when ?IS_GPR(Val) ->
            I_beq3 = jit_xtensa_asm:bne(Reg, Val, 2),
            JPlaceholder5 = <<16#FF, 16#FF, 16#FF>>,
            BranchDelta = StreamModule:offset(Stream1) - OffsetBefore + byte_size(I_beq3),
            Stream2 = StreamModule:append(Stream1, <<I_beq3/binary, JPlaceholder5/binary>>),
            State2 = State1#state{stream = Stream2},
            State3 = if_block_free_reg(RegTuple, State2),
            {State3, BranchDelta};
        _ ->
            %% Val is an immediate - need temp register
            MaskReg = first_avail(State1#state.available_regs),
            AT = State1#state.available_regs band (bnot reg_bit(MaskReg)),
            State2 = mov_immediate(State1#state{available_regs = AT}, MaskReg, Val),
            Stream2 = State2#state.stream,
            I_beq4 = jit_xtensa_asm:bne(Reg, MaskReg, 2),
            JPlaceholder6 = <<16#FF, 16#FF, 16#FF>>,
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore + byte_size(I_beq4),
            Stream3 = StreamModule:append(Stream2, <<I_beq4/binary, JPlaceholder6/binary>>),
            State3 = State2#state{stream = Stream3, available_regs = AvailRegs},
            State4 = if_block_free_reg(RegTuple, State3),
            {State4, BranchDelta}
    end.

-spec if_block_free_reg(xtensa_register() | {free, xtensa_register()}, state()) -> state().
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
-spec shift_right(#state{}, maybe_free_xtensa_register(), non_neg_integer()) ->
    {#state{}, xtensa_register()}.
shift_right(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift) andalso Shift =< 15
->
    I = jit_xtensa_asm:srli(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    {free, Reg},
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:movi(Temp, Shift),
    I2 = jit_xtensa_asm:ssr(Temp),
    I3 = jit_xtensa_asm:srl(Reg, Reg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UR,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift) andalso Shift =< 15
->
    ResultReg = first_avail(Avail),
    ResultBit = reg_bit(ResultReg),
    I = jit_xtensa_asm:srli(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot ResultBit),
            used_regs = UR bor ResultBit,
            regs = Regs1
        },
        ResultReg
    };
shift_right(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UR,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    ResultReg = first_avail(Avail),
    ResultBit = reg_bit(ResultReg),
    I1 = jit_xtensa_asm:movi(ResultReg, Shift),
    I2 = jit_xtensa_asm:ssr(ResultReg),
    I3 = jit_xtensa_asm:srl(ResultReg, Reg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot ResultBit),
            used_regs = UR bor ResultBit,
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
shift_left(State, {free, Reg}, Shift) ->
    {shift_left(State, Reg, Shift), Reg};
shift_left(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Shift
) when
    is_atom(Reg)
->
    I = jit_xtensa_asm:slli(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

shift_right_arith(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_xtensa_asm:srai(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right_arith(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UR,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    ResultReg = first_avail(Avail),
    ResultBit = reg_bit(ResultReg),
    I = jit_xtensa_asm:srai(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot ResultBit),
            used_regs = UR bor ResultBit,
            regs = Regs1
        },
        ResultReg
    }.

div_reg(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I = jit_xtensa_asm:quos(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

rem_reg(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I = jit_xtensa_asm:rems(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a function pointer with arguments. This function converts
%% arguments and passes them following the backend ABI convention.
%% @end
%% @param State current backend state
%% @param FuncPtrTuple either {free, Reg} or {primitive, PrimitiveIndex}
%% @param Args arguments to pass to the function
%% @return Updated backend state and return register
%%-----------------------------------------------------------------------------
-spec call_func_ptr(state(), {free, xtensa_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), xtensa_register()}.
call_func_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0Mask,
        used_regs = UsedRegs0Mask
    } = State0,
    FuncPtrTuple,
    Args
) ->
    AvailableRegs0 = mask_to_list(AvailableRegs0Mask),
    UsedRegs0 = mask_to_list(UsedRegs0Mask),
    FreeRegs = lists:flatmap(
        fun
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [FuncPtrTuple | Args]
    ),
    UsedRegs1 = UsedRegs0 -- FreeRegs,

    %% Windowed ABI: a0-a7 are preserved across CALLX8 automatically.
    %% Only a8-a15 are clobbered.  We only need to save used regs in a8-a15
    %% that are not freed by this call.
    HighRegs = [a8, a9, a10, a11, a12, a13, a14, a15],
    RegsToSave = [R || R <- UsedRegs1, lists:member(R, HighRegs)],

    FreeGPRegs = FreeRegs -- (FreeRegs -- ?AVAILABLE_REGS),
    AvailableRegs1 = FreeGPRegs ++ AvailableRegs0,

    NumToSave = length(RegsToSave),
    AlignedStackBytes =
        if
            NumToSave > 0 -> ((NumToSave * 4 + 15) div 16) * 16;
            true -> 0
        end,
    Stream1 = push_registers(RegsToSave, AlignedStackBytes, StreamModule, Stream0),

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

    % Registers available for set_registers_args:
    % - saved regs (will be restored after call) that are not arg sources
    % - previously available regs
    %
    %% Only HIGH used regs (a8-a15) are pushed to the stack via
    %% push_registers and restored after the call. Low used regs (a5-a7)
    %% are preserved by the windowed ABI but NOT backed up, so if we
    %% use them as scratch here (e.g. to hold the primitive func ptr)
    %% their live contents are silently clobbered. Only RegsToSave is
    %% safe to reuse as scratch among the currently-used registers.
    SetArgsRegsOnlyAvailableArgs = (RegsToSave -- RegArgsRegs) ++ AvailableRegs0,
    State1 = State0#state{
        available_regs = jit_regs:regs_to_mask(SetArgsRegsOnlyAvailableArgs, fun reg_bit/1),
        used_regs = jit_regs:regs_to_mask(
            ?AVAILABLE_REGS -- SetArgsRegsOnlyAvailableArgs, fun reg_bit/1
        ),
        stream = Stream1
    },

    ParameterRegs = parameter_regs(RegArgs0),
    {Stream3, SetArgsAvailableRegs, FuncPtrReg, RegArgs} =
        case FuncPtrTuple of
            {free, FuncPtrReg0} ->
                % If FuncPtrReg is in parameter regs, we must move it out.
                case lists:member(FuncPtrReg0, ParameterRegs) of
                    true ->
                        case SetArgsRegsOnlyAvailableArgs -- ParameterRegs of
                            [] when SetArgsRegsOnlyAvailableArgs =:= [] ->
                                % No available registers at all, use ?A8_REG
                                MovInstr = jit_xtensa_asm:mov(?A8_REG, FuncPtrReg0),
                                SetArgsAvailableArgs1 = [FuncPtrReg0],
                                {
                                    StreamModule:append(State1#state.stream, MovInstr),
                                    SetArgsAvailableArgs1,
                                    ?A8_REG,
                                    RegArgs0
                                };
                            [] ->
                                % Swap with a reg used in RegArgs0 that is not in ParameterRegs
                                [NewArgReg | _] = SetArgsRegsOnlyAvailableArgs,
                                [FuncPtrReg1 | _] = RegArgsRegs -- ParameterRegs,
                                MovInstr1 = jit_xtensa_asm:mov(NewArgReg, FuncPtrReg1),
                                MovInstr2 = jit_xtensa_asm:mov(FuncPtrReg1, FuncPtrReg0),
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
                                MovInstr = jit_xtensa_asm:mov(FuncPtrReg1, FuncPtrReg0),
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
                FuncPtrReg0 =
                    case SetArgsRegsOnlyAvailableArgs -- ParameterRegs of
                        [] -> ?A8_REG;
                        [R | _] -> R
                    end,
                SetArgsAvailableRegs1 = SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg0],
                PrepCall = load_primitive_ptr(Primitive, FuncPtrReg0),
                Stream2 = StreamModule:append(State1#state.stream, PrepCall),
                {Stream2, SetArgsAvailableRegs1, FuncPtrReg0, RegArgs0}
        end,

    State3 = State1#state{
        available_regs = jit_regs:regs_to_mask(SetArgsAvailableRegs, fun reg_bit/1),
        used_regs = jit_regs:regs_to_mask(?AVAILABLE_REGS -- SetArgsAvailableRegs, fun reg_bit/1),
        stream = Stream3
    },

    StackOffset = AlignedStackBytes,
    State4 = set_registers_args(State3, RegArgs, ParameterRegs, StackOffset),
    Stream4 = State4#state.stream,

    %% Call the function pointer using callx8 (windowed ABI)
    Call = jit_xtensa_asm:callx8(FuncPtrReg),
    Stream5 = StreamModule:append(Stream4, Call),

    %% Return value is in a10 (callee's a2 mapped back to caller's a10).
    %% Pick a result register from the available registers.
    %% After CALLX8, a8-a15 are all clobbered, so they are all available
    %% (except for any we still need to restore from stack).
    %% a5-a7 are preserved and may still hold live values.
    PostCallAvail = (AvailableRegs1 -- HighRegs) ++ (HighRegs -- RegsToSave),
    {Stream6, UsedRegs2, ResultReg} =
        case PostCallAvail of
            [ResultReg0 | _] ->
                case ResultReg0 of
                    a10 ->
                        %% Result already in the right register
                        {Stream5, [a10 | UsedRegs1], a10};
                    _ ->
                        MoveResult = jit_xtensa_asm:mov(ResultReg0, a10),
                        {
                            StreamModule:append(Stream5, MoveResult),
                            [ResultReg0 | UsedRegs1],
                            ResultReg0
                        }
                end;
            [] ->
                %% Fallback: use ?A8_REG
                MoveResult = jit_xtensa_asm:mov(?A8_REG, a10),
                {StreamModule:append(Stream5, MoveResult), [?A8_REG | UsedRegs1], ?A8_REG}
        end,

    Stream8 = pop_registers(RegsToSave, AlignedStackBytes, StreamModule, Stream6),

    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    %% Invalidate ALL register tracking after a call. Even though a0-a7 are
    %% preserved by the windowed ABI, the called function may trigger GC which
    %% moves heap objects. Any cached heap pointers in registers become stale.
    Regs1 = jit_regs:invalidate_all(State0#state.regs),
    {
        State4#state{
            stream = Stream8,
            available_regs = jit_regs:regs_to_mask(AvailableRegs3, fun reg_bit/1),
            used_regs = jit_regs:regs_to_mask(UsedRegs2, fun reg_bit/1),
            regs = Regs1
        },
        ResultReg
    }.

arg_to_reg_list({free, {ptr, Reg}}) -> [Reg];
arg_to_reg_list({free, Reg}) when is_atom(Reg) -> [Reg];
arg_to_reg_list(Reg) when is_atom(Reg) -> [Reg];
arg_to_reg_list(_) -> [].

push_registers(SavedRegs, _AlignedStackBytes, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    %% Windowed ABI: store registers within the ENTRY frame at positive offsets
    %% from SP, starting at FRAME_LOCAL_OFFSET (above the window save areas).
    %% This avoids conflicts with the window overflow handler.
    {Stream1, _} = lists:foldl(
        fun(Reg, {StreamAcc, Offset}) ->
            Store = jit_xtensa_asm:s32i(Reg, a1, Offset),
            {StreamModule:append(StreamAcc, Store), Offset + 4}
        end,
        {Stream0, ?FRAME_LOCAL_OFFSET},
        SavedRegs
    ),
    Stream1;
push_registers([], _AlignedStackBytes, _StreamModule, Stream0) ->
    Stream0.

pop_registers(SavedRegs, _AlignedStackBytes, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    %% Windowed ABI: restore registers from the ENTRY frame at positive offsets.
    {Stream1, _} = lists:foldl(
        fun(Reg, {StreamAcc, Offset}) ->
            Load = jit_xtensa_asm:l32i(Reg, a1, Offset),
            {StreamModule:append(StreamAcc, Load), Offset + 4}
        end,
        {Stream0, ?FRAME_LOCAL_OFFSET},
        SavedRegs
    ),
    Stream1;
pop_registers([], _AlignedStackBytes, _StreamModule, Stream0) ->
    Stream0.

set_registers_args(
    #state{used_regs = UsedRegsMask} = State0,
    Args,
    ParamRegs,
    StackOffset
) ->
    UsedRegs = mask_to_list(UsedRegsMask),
    ArgsRegs = args_regs(Args),
    AvailableScratchGP = ((?AVAILABLE_REGS -- ParamRegs) -- ArgsRegs) -- UsedRegs,
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
        available_regs = jit_regs:regs_to_mask(
            ?AVAILABLE_REGS -- ParamRegs -- NewUsedRegs, fun reg_bit/1
        ),
        used_regs = jit_regs:regs_to_mask(ParamRegs ++ (NewUsedRegs -- ParamRegs), fun reg_bit/1)
    }.

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

% ILP32 (Xtensa windowed ABI): 64-bit args require even register number
% alignment. Caller-side parameter registers a10..a15 map to the callee's
% a2..a7 after CALL8, so even-aligned pairs are (a10,a11), (a12,a13),
% (a14,a15); a11/a13/a15 are odd (callee a3/a5/a7) and cannot start a pair,
% so the int64 is placed in the next aligned pair and the odd register is
% left as padding.
parameter_regs0([], _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([{avm_int64_t, _} | T], [a10, a11 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a11, a10 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [a11, a12, a13 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a13, a12 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [a12, a13 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a13, a12 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [a13, a14, a15 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a15, a14 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [a14, a15 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a15, a14 | Acc]);
parameter_regs0([{avm_int64_t, _} | _], Regs, _Acc) ->
    error({cannot_align_int64, Regs});
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
            I = jit_xtensa_asm:mov(Avail, ParamReg),
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
    ctx,
    ParamReg,
    _StackOffset
) ->
    %% ctx is always in a2 (?CTX_REG)
    case ParamReg of
        ?CTX_REG ->
            State;
        _ ->
            I = jit_xtensa_asm:mov(ParamReg, ?CTX_REG),
            Stream1 = StreamModule:append(Stream0, I),
            State#state{stream = Stream1}
    end;
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    jit_state,
    ParamReg,
    _StackOffset
) ->
    %% jit_state is always in a3 (?JITSTATE_REG)
    case ParamReg of
        ?JITSTATE_REG ->
            State;
        _ ->
            I = jit_xtensa_asm:mov(ParamReg, ?JITSTATE_REG),
            Stream1 = StreamModule:append(Stream0, I),
            State#state{stream = Stream1}
    end;
%% For tail calls, jit_state is in ?JITSTATE_REG (a3).
%% In windowed ABI, it still needs to be moved to the parameter register
%% (a11 etc.) since the callee will see it in a different window.
set_registers_args1(State, jit_state_tail_call, ?JITSTATE_REG, _StackOffset) ->
    State;
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    jit_state_tail_call,
    ParamReg,
    _StackOffset
) ->
    I = jit_xtensa_asm:mov(ParamReg, ?JITSTATE_REG),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {x_reg, extra},
    Reg,
    _StackOffset
) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I = jit_xtensa_asm:l32i(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, Reg, _StackOffset
) ->
    {XReg, X_REGOffset} = ?X_REG(X),
    I = jit_xtensa_asm:l32i(Reg, XReg, X_REGOffset),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Source}, Reg, _StackOffset
) ->
    I = jit_xtensa_asm:l32i(Reg, Source, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = AvailRegs, regs = Regs0} =
        State,
    {y_reg, X},
    Reg,
    _StackOffset
) ->
    Code = ldr_y_reg(Reg, X, AvailRegs),
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 =
        case AvailRegs of
            0 -> Regs0;
            _ -> jit_regs:invalidate_reg(Regs0, first_avail(AvailRegs))
        end,
    State#state{stream = Stream1, regs = Regs1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, ArgReg, Reg, _StackOffset
) when
    ?IS_GPR(ArgReg)
->
    I = jit_xtensa_asm:mov(Reg, ArgReg),
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
move_to_vm_register(#state{regs = Regs0} = State, Src, Dest) ->
    VmLoc = vm_dest_to_contents(Dest),
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
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_xtensa_asm:s32i(Src, BaseReg, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {x_reg, X}) when is_atom(Src) ->
    {BaseReg, Off} = ?X_REG(X),
    I1 = jit_xtensa_asm:s32i(Src, BaseReg, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_xtensa_asm:s32i(Src, Reg, 0),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(
    #state{available_regs = Avail, regs = Regs0} = State0, Src, {y_reg, Y}
) when
    is_atom(Src)
->
    Temp1 =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    AT = Avail band (bnot reg_bit(Temp1)),
    Code = str_y_reg(Src, Y, Temp1, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp1),
    Regs2 =
        case AT of
            0 -> Regs1;
            _ -> jit_regs:invalidate_reg(Regs1, first_avail(AT))
        end,
    State0#state{stream = Stream1, regs = Regs2};
% Source is an integer to y_reg (optimized: ldr first, then movs)
move_to_vm_register_emit(
    #state{available_regs = Avail, regs = Regs0} = State0, N, {y_reg, Y}
) when
    is_integer(N), N >= 0, N =< 255
->
    Temp1 =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    Avail2 = Avail band (bnot reg_bit(Temp1)),
    Temp2 =
        case Avail2 of
            0 -> ?A8_REG;
            _ -> first_avail(Avail2)
        end,
    AT = Avail2 band (bnot reg_bit(Temp2)),
    I1 = mov_immediate(Temp2, N),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, YCode/binary>>),
    Regs1a = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp1), Temp2),
    Regs1 =
        case AT of
            0 -> Regs1a;
            _ -> jit_regs:invalidate_reg(Regs1a, first_avail(AT))
        end,
    State0#state{stream = Stream1, regs = Regs1};
% Source is an integer (0-255 for movs, negative values need different handling)
move_to_vm_register_emit(#state{available_regs = AR0} = State0, N, Dest) when
    is_integer(N), N >= 0, N =< 255
->
    Temp =
        case AR0 of
            0 -> ?A8_REG;
            _ -> first_avail(AR0)
        end,
    AT = AR0 band (bnot reg_bit(Temp)),
    I1 = mov_immediate(Temp, N),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:set_contents(State1#state.regs, Temp, {imm, N}),
    State1#state{available_regs = AR0, regs = Regs1};
%% Handle large values using simple literal pool (branch-over pattern)
move_to_vm_register_emit(#state{available_regs = AR0} = State0, N, Dest) when
    is_integer(N)
->
    Temp =
        case AR0 of
            0 -> ?A8_REG;
            _ -> first_avail(AR0)
        end,
    AT = AR0 band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, N),
    State2 = move_to_vm_register(State1, Temp, Dest),
    Regs1 = jit_regs:set_contents(State2#state.regs, Temp, {imm, N}),
    State2#state{available_regs = AR0, regs = Regs1};
% Source is a VM register
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {x_reg, extra}, Dest) ->
    Temp =
        case AR0 of
            0 -> ?A8_REG;
            _ -> first_avail(AR0)
        end,
    AT = AR0 band (bnot reg_bit(Temp)),
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_xtensa_asm:l32i(Temp, BaseReg, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:set_contents(State1#state.regs, Temp, {x_reg, extra}),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {x_reg, X}, Dest) ->
    Temp =
        case AR0 of
            0 -> ?A8_REG;
            _ -> first_avail(AR0)
        end,
    AT = AR0 band (bnot reg_bit(Temp)),
    {XReg, X_REGOffset} = ?X_REG(X),
    I1 = jit_xtensa_asm:l32i(Temp, XReg, X_REGOffset),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:set_contents(State1#state.regs, Temp, {x_reg, X}),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {ptr, Reg}, Dest) ->
    Temp =
        case AR0 of
            0 -> ?A8_REG;
            _ -> first_avail(AR0)
        end,
    AT = AR0 band (bnot reg_bit(Temp)),
    I1 = jit_xtensa_asm:l32i(Temp, Reg, 0),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {y_reg, Y}, Dest) ->
    Temp =
        case AR0 of
            0 -> ?A8_REG;
            _ -> first_avail(AR0)
        end,
    AT = AR0 band (bnot reg_bit(Temp)),
    Code = ldr_y_reg(Temp, Y, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, Code),
    Regs1 =
        case AT of
            0 -> State0#state.regs;
            _ -> jit_regs:invalidate_reg(State0#state.regs, first_avail(AT))
        end,
    State1 = move_to_vm_register(
        State0#state{stream = Stream1, available_regs = AT, regs = Regs1}, Temp, Dest
    ),
    Regs2 = jit_regs:set_contents(State1#state.regs, Temp, {y_reg, Y}),
    State1#state{available_regs = AR0, regs = Regs2};
% term_to_float
move_to_vm_register_emit(
    #state{
        stream_module = StreamModule,
        available_regs = Avail,
        stream = Stream0,
        variant = Variant
    } =
        State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) ->
    Temp1 = first_avail(Avail),
    Temp2 = first_avail(Avail band (bnot reg_bit(Temp1))),
    {BaseReg, Off} = ?FP_REGS,
    I1 = jit_xtensa_asm:l32i(Temp1, BaseReg, Off),
    I2 = jit_xtensa_asm:l32i(Temp2, Reg, 4),
    case Variant band ?JIT_VARIANT_FLOAT32 of
        0 ->
            % Double precision: write both 32-bit parts
            I3 = jit_xtensa_asm:s32i(Temp2, Temp1, F * 8),
            I4 = jit_xtensa_asm:l32i(Temp2, Reg, 8),
            I5 = jit_xtensa_asm:s32i(Temp2, Temp1, F * 8 + 4),
            Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>;
        _ ->
            % Single precision: write only first 32-bit part
            I3 = jit_xtensa_asm:s32i(Temp2, Temp1, F * 4),
            Code = <<I1/binary, I2/binary, I3/binary>>
    end,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = free_native_register(State0, Reg),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(State1#state.regs, Temp1), Temp2),
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
    xtensa_register(),
    non_neg_integer() | xtensa_register(),
    vm_register() | xtensa_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Avail),
    I1 = jit_xtensa_asm:l32i(Temp, Reg, Index * 4),
    {BaseReg, Off} = ?X_REG(X),
    I2 = jit_xtensa_asm:s32i(Temp, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:set_contents(Regs1, Temp, {x_reg, X}),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Avail),
    I1 = jit_xtensa_asm:l32i(Temp, Reg, Index * 4),
    I2 = jit_xtensa_asm:s32i(Temp, Dest, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp1 = first_avail(Avail),
    Avail2 = Avail band (bnot reg_bit(Temp1)),
    Temp2 = first_avail(Avail2),
    AT = Avail2 band (bnot reg_bit(Temp2)),
    I1 = jit_xtensa_asm:l32i(Temp2, Reg, Index * 4),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs1, Temp1), Temp2),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    I1 = jit_xtensa_asm:l32i(Reg, Reg, Index * 4),
    YCode = str_y_reg(Reg, Y, Temp, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs1, Reg), Temp),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_xtensa_asm:l32i(Dest, Reg, Index * 4),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
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
    I1 = jit_xtensa_asm:slli(IndexReg, IndexReg, 2),
    I2 = jit_xtensa_asm:add(IndexReg, Reg, IndexReg),
    I3 = jit_xtensa_asm:l32i(IndexReg, IndexReg, 0),
    {BaseReg, Off} = ?X_REG(X),
    I4 = jit_xtensa_asm:s32i(IndexReg, BaseReg, Off),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
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
        used_regs = UsedRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {ptr, PtrReg}
) when is_atom(IndexReg) ->
    I1 = jit_xtensa_asm:slli(IndexReg, IndexReg, 2),
    I2 = jit_xtensa_asm:add(IndexReg, Reg, IndexReg),
    I3 = jit_xtensa_asm:l32i(IndexReg, IndexReg, 0),
    I4 = jit_xtensa_asm:s32i(IndexReg, PtrReg, 0),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
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
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when is_atom(IndexReg) ->
    Temp = first_avail(AvailableRegs0),
    AT = AvailableRegs0 band (bnot reg_bit(Temp)),
    I1 = jit_xtensa_asm:slli(IndexReg, IndexReg, 2),
    I2 = jit_xtensa_asm:add(IndexReg, Reg, IndexReg),
    I3 = jit_xtensa_asm:l32i(IndexReg, IndexReg, 0),
    Code = str_y_reg(IndexReg, Y, Temp, AT),
    I4 = Code,
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>
    ),
    Regs1a = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs1a, IndexReg), Temp),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs1
    }.

%% @doc move reg[x] to a vm or native register
-spec get_array_element(
    state(), xtensa_register() | {free, xtensa_register()}, non_neg_integer()
) ->
    {state(), xtensa_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = jit_xtensa_asm:l32i(Reg, Reg, Index * 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    {State#state{stream = Stream1}, Reg};
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UsedRegs0
    } = State,
    Reg,
    Index
) ->
    ElemReg =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    ElemBit = reg_bit(ElemReg),
    I1 = jit_xtensa_asm:l32i(ElemReg, Reg, Index * 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot ElemBit),
            used_regs = UsedRegs0 bor ElemBit
        },
        ElemReg
    }.

%% @doc move an integer, a vm or native register to reg[x]
-spec move_to_array_element(
    state(), integer() | vm_register() | xtensa_register(), xtensa_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_xtensa_asm:s32i(ValueReg, Reg, Index * 4),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State0,
    ValueReg,
    Reg,
    IndexReg
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:mov(Temp, IndexReg),
    I2 = jit_xtensa_asm:slli(Temp, Temp, 2),
    I3 = jit_xtensa_asm:add(Temp, Reg, Temp),
    I4 = jit_xtensa_asm:s32i(ValueReg, Temp, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1};
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
) when is_integer(IndexReg) andalso is_integer(Offset) ->
    move_to_array_element(State, Value, BaseReg, IndexReg + Offset);
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    ValueReg,
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:addi(Temp, IndexReg, Offset),
    I2 = jit_xtensa_asm:slli(Temp, Temp, 2),
    I3 = jit_xtensa_asm:add(Temp, BaseReg, Temp),
    I4 = jit_xtensa_asm:s32i(ValueReg, Temp, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
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
    Temp =
        case State1#state.available_regs of
            0 -> ?A8_REG;
            _ -> first_avail(State1#state.available_regs)
        end,
    I1 = jit_xtensa_asm:addi(Temp, IndexReg, Offset),
    I2 = jit_xtensa_asm:slli(Temp, Temp, 2),
    I3 = jit_xtensa_asm:add(Temp, BaseReg, Temp),
    I4 = jit_xtensa_asm:s32i(ValueReg, Temp, 0),
    Stream1 = (State1#state.stream_module):append(
        State1#state.stream, <<I1/binary, I2/binary, I3/binary, I4/binary>>
    ),
    State2 = State1#state{stream = Stream1},
    free_native_register(State2, ValueReg).

-spec move_to_native_register(state(), value() | cp) -> {state(), xtensa_register()}.
move_to_native_register(State, Reg) when ?IS_GPR(Reg) ->
    {State, Reg};
move_to_native_register(#state{regs = Regs} = State, Value) ->
    Contents = value_to_contents(Value),
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
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    cp,
    Contents
) ->
    Reg = first_avail(Avail),
    RegBit = reg_bit(Reg),
    {BaseReg, Off} = ?CP,
    I1 = jit_xtensa_asm:l32i(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor RegBit,
            available_regs = Avail band (bnot RegBit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {ptr, Reg},
    _Contents
) when is_atom(Reg) ->
    I1 = jit_xtensa_asm:l32i(Reg, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
move_to_native_register_emit(
    #state{
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State0,
    Imm,
    Contents
) when
    is_integer(Imm)
->
    Reg =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    RegBit = reg_bit(Reg),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    State1 = State0#state{
        used_regs = Used bor RegBit,
        available_regs = Avail band (bnot RegBit),
        regs = Regs1
    },
    {move_to_native_register(State1, Imm, Reg), Reg};
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {x_reg, extra},
    Contents
) ->
    Reg =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    RegBit = reg_bit(Reg),
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_xtensa_asm:l32i(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor RegBit,
            available_regs = Avail band (bnot RegBit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {x_reg, X},
    Contents
) when
    X < ?MAX_REG
->
    Reg =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    RegBit = reg_bit(Reg),
    {BaseReg, Offset} = ?X_REG(X),
    I1 = jit_xtensa_asm:l32i(Reg, BaseReg, Offset),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor RegBit,
            available_regs = Avail band (bnot RegBit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {y_reg, Y},
    Contents
) ->
    Reg =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    RegBit = reg_bit(Reg),
    AvailT = Avail band (bnot RegBit),
    Code = ldr_y_reg(Reg, Y, AvailT),
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1a = jit_regs:set_contents(Regs0, Reg, Contents),
    Regs1 =
        case AvailT of
            0 -> Regs1a;
            _ -> jit_regs:invalidate_reg(Regs1a, first_avail(AvailT))
        end,
    {
        State#state{
            stream = Stream1,
            available_regs = AvailT,
            used_regs = Used bor RegBit,
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used
    } = State,
    {fp_reg, F},
    _Contents
) ->
    RegA = first_avail(Avail),
    RegABit = reg_bit(RegA),
    Avail2 = Avail band (bnot RegABit),
    RegB = first_avail(Avail2),
    RegBBit = reg_bit(RegB),
    AvailT = Avail2 band (bnot RegBBit),
    {BaseReg, Off} = ?FP_REGS,
    I1 = jit_xtensa_asm:l32i(RegB, BaseReg, Off),
    I2 = jit_xtensa_asm:l32i(RegA, RegB, F * 8),
    I3 = jit_xtensa_asm:l32i(RegB, RegB, F * 8 + 4),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{
            stream = Stream1, available_regs = AvailT, used_regs = Used bor RegABit bor RegBBit
        },
        {fp, RegA, RegB}
    }.

-spec move_to_native_register(state(), value(), xtensa_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_atom(RegSrc) ->
    I = jit_xtensa_asm:mov(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    SrcContents = jit_regs:get_contents(Regs0, RegSrc),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, SrcContents),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(State, ValSrc, RegDst) when is_integer(ValSrc) ->
    State1 = mov_immediate(State, RegDst, ValSrc),
    #state{regs = Regs0} = State1,
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {imm, ValSrc}),
    State1#state{regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {ptr, Reg}, RegDst
) when ?IS_GPR(Reg) ->
    I1 = jit_xtensa_asm:l32i(RegDst, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, RegDst),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {x_reg, extra},
    RegDst
) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = jit_xtensa_asm:l32i(RegDst, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {x_reg, extra}),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    {XReg, X_REGOffset} = ?X_REG(X),
    I1 = jit_xtensa_asm:l32i(RegDst, XReg, X_REGOffset),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {x_reg, X}),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = AT, regs = Regs0} =
        State,
    {y_reg, Y},
    RegDst
) ->
    Code = ldr_y_reg(RegDst, Y, AT),
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 =
        case AT of
            0 -> Regs0;
            _ -> jit_regs:invalidate_reg(Regs0, first_avail(AT))
        end,
    Regs2 = jit_regs:set_contents(Regs1, RegDst, {y_reg, Y}),
    State#state{stream = Stream1, regs = Regs2};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {fp_reg, F},
    {fp, RegA, RegB}
) ->
    {BaseReg, Off} = ?FP_REGS,
    I1 = jit_xtensa_asm:l32i(RegB, BaseReg, Off),
    I2 = jit_xtensa_asm:l32i(RegA, RegB, F * 8),
    I3 = jit_xtensa_asm:l32i(RegB, RegB, F * 8 + 4),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec copy_to_native_register(state(), value()) -> {state(), xtensa_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    Reg
) when is_atom(Reg) ->
    SaveReg = first_avail(Avail),
    SaveBit = reg_bit(SaveReg),
    I1 = jit_xtensa_asm:mov(SaveReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    SrcContents = jit_regs:get_contents(Regs0, Reg),
    Regs1 = jit_regs:set_contents(Regs0, SaveReg, SrcContents),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot SaveBit),
            used_regs = Used bor SaveBit,
            regs = Regs1
        },
        SaveReg
    };
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {ptr, Reg}
) when is_atom(Reg) ->
    SaveReg = first_avail(Avail),
    SaveBit = reg_bit(SaveReg),
    I1 = jit_xtensa_asm:l32i(SaveReg, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, SaveReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot SaveBit),
            used_regs = Used bor SaveBit,
            regs = Regs1
        },
        SaveReg
    };
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    {y_reg, Y}
) ->
    Reg = first_avail(Avail),
    AvailT = Avail band (bnot reg_bit(Reg)),
    I1 = ldr_y_reg(Reg, Y, AvailT),
    {BaseReg, Off} = ?CP,
    I2 = jit_xtensa_asm:s32i(Reg, BaseReg, Off),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    Regs2 =
        case AvailT of
            0 -> Regs1;
            _ -> jit_regs:invalidate_reg(Regs1, first_avail(AvailT))
        end,
    State#state{stream = Stream1, regs = Regs2}.

increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Offset
) ->
    Reg = first_avail(Avail),
    {BaseReg1, Off1} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(Reg, BaseReg1, Off1),
    I2 = add_immediate_binary(Reg, Reg, Offset * 4),
    {BaseReg2, Off2} = ?Y_REGS,
    I3 = jit_xtensa_asm:s32i(Reg, BaseReg2, Off2),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

set_continuation_to_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        regs = Regs0
    } = State,
    Label
) ->
    Temp = first_avail(Avail),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    %% In windowed ABI, the continuation will be called by C via CALL8,
    %% so it must point to a location with an ENTRY instruction.
    %% We use the jump table entry for the label, which has ENTRY after the literal.
    %% code_base points past the chunk header, so the offset is relative to
    %% the jump table start (which is the first thing after the header).
    %% JUMP_TABLE_OFFSET skips past the 4-byte literal to the ENTRY instruction.
    CodeRelativeOffset = ?JUMP_TABLE_OFFSET + Label * ?JUMP_TABLE_ENTRY_SIZE,
    I1 = code_relative_address(Temp, CodeRelativeOffset),
    I2 = jit_xtensa_asm:s32i(Temp, ?JITSTATE_REG, ?JITSTATE_CONTINUATION_OFFSET),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, regs = Regs1}.

%% @doc Set the continuation to a given offset.
%% Returns a reference so the offset will be updated with update_branches.
%% Only used with OP_WAIT_TIMEOUT.
set_continuation_to_offset(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        branches = Branches,
        jump_table_start = JumpTableStart,
        regs = Regs0
    } = State
) ->
    Temp = first_avail(Avail),
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    %% Reserve 21 bytes placeholder for code_relative_address_padded
    I1 = list_to_binary(lists:duplicate(?CODE_RELATIVE_ADDRESS_PADDED_SIZE, 16#FF)),
    BrEntry = {Offset, {adr, Temp, JumpTableStart}},
    %% Store continuation
    I2 = jit_xtensa_asm:s32i(Temp, ?JITSTATE_REG, ?JITSTATE_CONTINUATION_OFFSET),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    {
        State#state{stream = Stream1, branches = Branches#{OffsetRef => [BrEntry]}, regs = Regs1},
        OffsetRef
    }.

%% @doc Implement a continuation entry point.
%% In windowed ABI, C calls the continuation via CALL8, so we need ENTRY
%% to establish a new register window frame.
-spec continuation_entry_point(#state{}) -> #state{}.
continuation_entry_point(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    I1 = jit_xtensa_asm:entry(a1, ?ENTRY_FRAME_SIZE),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

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
    RegBit = reg_bit(Reg),
    % Load module from jit_state (which is in a3)
    I1 = jit_xtensa_asm:l32i(Reg, ?JITSTATE_REG, ?JITSTATE_MODULE_OFFSET),
    I2 = jit_xtensa_asm:l32i(Reg, Reg, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, module_index),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot RegBit),
            used_regs = UsedRegs0 bor RegBit,
            regs = Regs1
        },
        Reg
    }.

and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    {free, Reg},
    SrcReg
) when
    is_atom(SrcReg)
->
    I = jit_xtensa_asm:and_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    {free, Reg},
    16#FFFFFF
) ->
    I1 = jit_xtensa_asm:slli(Reg, Reg, 8),
    I2 = jit_xtensa_asm:srli(Reg, Reg, 8),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, Reg};
% Xtensa has no andi instruction - small values fall through to general case
and_(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    {free, Reg},
    Val
) when
    Val >= -2048 andalso Val =< 2047 andalso Avail =/= 0
->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:and_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    {State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    {free, Reg},
    Val
) when Val < 0 andalso Val >= -256 andalso Avail =/= 0 ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:and_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    {State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    {free, Reg},
    Val
) when Avail =/= 0 ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:and_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    {State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{available_regs = 0} = State0,
    {free, Reg},
    Val
) when Val < 0 andalso Val >= -256 ->
    %% No available registers; use ?A8_REG (a8) directly as scratch.
    %% a8 is not in AVAILABLE_REGS so it is the dedicated implicit scratch.
    %% Avoid clobbering a0 (encoded return address required by RETW).
    State1 = mov_immediate(State0, ?A8_REG, Val),
    #state{stream_module = StreamModule, stream = Stream1} = State1,
    I = jit_xtensa_asm:and_(Reg, Reg, ?A8_REG),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Reg),
    {State1#state{stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{available_regs = 0, regs = Regs0} = State0,
    {free, Reg},
    Val
) ->
    %% No available registers; use ?A8_REG (a8) directly as scratch.
    State1 = mov_immediate(State0, ?A8_REG, Val),
    #state{stream_module = StreamModule, stream = Stream1} = State1,
    I = jit_xtensa_asm:and_(Reg, Reg, ?A8_REG),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State1#state{stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail, used_regs = UR, regs = Regs0} =
        State0,
    Reg,
    ?TERM_PRIMARY_CLEAR_MASK
) ->
    ResultReg =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    ResultBit = reg_bit(ResultReg),
    %% Xtensa has no andi - use movi+and_ for TERM_PRIMARY_CLEAR_MASK (-4)
    I1 = jit_xtensa_asm:movi(ResultReg, -4),
    I2 = jit_xtensa_asm:and_(ResultReg, Reg, ResultReg),
    Stream1 = StreamModule:append(State0#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State0#state{
            stream = Stream1,
            available_regs = Avail band (bnot ResultBit),
            used_regs = UR bor ResultBit,
            regs = Regs1
        },
        ResultReg
    }.

or_(State0, {free, Reg}, Val) ->
    {or_(State0, Reg, Val), Reg};
or_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I = jit_xtensa_asm:or_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
% Xtensa has no ori instruction - small values use li+or_ like general case
or_(
    #state{stream_module = StreamModule, available_regs = Avail} = State0, Reg, Val
) when
    Val >= -2048 andalso Val =< 2047 andalso Avail =/= 0
->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:or_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    %% Take the cache from State1 (mov_immediate already invalidated Temp),
    %% then also invalidate Reg whose value just got rewritten.
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1};
or_(
    #state{available_regs = 0} = State0, Reg, Val
) ->
    %% No available registers; use ?A8_REG (a8) directly as scratch.
    %% a8 is not in AVAILABLE_REGS so it is the dedicated implicit scratch.
    %% Avoid clobbering a0 (encoded return address required by RETW).
    State1 = mov_immediate(State0, ?A8_REG, Val),
    #state{stream_module = StreamModule, stream = Stream1} = State1,
    I = jit_xtensa_asm:or_(Reg, Reg, ?A8_REG),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{stream = Stream2, regs = Regs1};
or_(
    #state{stream_module = StreamModule, available_regs = Avail} = State0,
    Reg,
    Val
) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:or_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

xor_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, SrcReg
) when
    is_atom(SrcReg)
->
    I = jit_xtensa_asm:xor_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
% Xtensa has no xori instruction - small values use li+xor_ like general case
xor_(
    #state{stream_module = StreamModule, available_regs = Avail} = State0, Reg, Val
) when
    Val >= -2048 andalso Val =< 2047 andalso Avail =/= 0
->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:xor_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    %% mov_immediate already invalidated Temp in State1#state.regs; also
    %% invalidate Reg whose value just got rewritten.
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1};
xor_(
    #state{available_regs = 0} = State0, Reg, Val
) ->
    %% No available registers; use ?A8_REG (a8) directly as scratch.
    %% Avoid clobbering a0 (encoded return address required by RETW).
    State1 = mov_immediate(State0, ?A8_REG, Val),
    #state{stream_module = StreamModule, stream = Stream1} = State1,
    I = jit_xtensa_asm:xor_(Reg, Reg, ?A8_REG),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{stream = Stream2, regs = Regs1};
xor_(
    #state{stream_module = StreamModule, available_regs = Avail} = State0,
    Reg,
    Val
) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:xor_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

add(State0, {free, Reg}, Val) ->
    {add(State0, Reg, Val), Reg};
add(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, Val) when
    Val >= -128 andalso Val =< 127
->
    I = jit_xtensa_asm:addi(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
add(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, Val) when
    is_atom(Val)
->
    I = jit_xtensa_asm:add(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
add(#state{stream_module = StreamModule, available_regs = Avail} = State0, Reg, Val) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:add(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    %% mov_immediate already invalidated Temp in State1#state.regs.
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

%% mov_immediate/2: returns binary, for inline code generation.
%% Uses movi, movi+addmi, or movi+slli+addi sequences (no L32R).
mov_immediate(Reg, Val) when Val >= -2048, Val =< 2047 ->
    jit_xtensa_asm:movi(Reg, Val);
mov_immediate(Reg, Val) ->
    case split_movi_addmi(Val) of
        {ok, Low, High} ->
            <<
                (jit_xtensa_asm:movi(Reg, Low))/binary,
                (jit_xtensa_asm:addmi(Reg, Reg, High))/binary
            >>;
        false ->
            mov_immediate_large(Reg, Val band 16#FFFFFFFF)
    end.

%% Build arbitrary 32-bit value using MOVI + SLLI 8 + ADDI byte-by-byte sequence.
%% We solve for signed B3, B2, B1, B0 in [-128, 127] such that
%% ((B3*256 + B2)*256 + B1)*256 + B0 == Imm32 (mod 2^32).
%% Each ADDI sign-extends its 8-bit immediate, so we must absorb the borrow
%% caused by negative lower bytes into the byte directly above before encoding.
mov_immediate_large(Reg, Imm32) ->
    {B0, R1} = split_signed_byte(Imm32),
    {B1, R2} = split_signed_byte(R1),
    {B2, R3} = split_signed_byte(R2),
    B3 = sign_byte(R3),
    mov_immediate_build(Reg, B3, B2, B1, B0).

%% Split an integer V into {SignedByte, Higher} such that
%% V == SignedByte + Higher * 256, with SignedByte in [-128, 127].
split_signed_byte(V) ->
    Low = V band 16#FF,
    Signed =
        if
            Low >= 128 -> Low - 256;
            true -> Low
        end,
    Higher = (V - Signed) div 256,
    {Signed, Higher}.

sign_byte(V) ->
    V1 = V band 16#FF,
    if
        V1 >= 128 -> V1 - 256;
        true -> V1
    end.

mov_immediate_build(Reg, 0, 0, B1, B0) ->
    <<
        (jit_xtensa_asm:movi(Reg, B1))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, 8))/binary,
        (jit_xtensa_asm:addi(Reg, Reg, B0))/binary
    >>;
mov_immediate_build(Reg, 0, B2, B1, B0) ->
    <<
        (jit_xtensa_asm:movi(Reg, B2))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, 8))/binary,
        (jit_xtensa_asm:addi(Reg, Reg, B1))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, 8))/binary,
        (jit_xtensa_asm:addi(Reg, Reg, B0))/binary
    >>;
mov_immediate_build(Reg, B3, B2, B1, B0) ->
    <<
        (jit_xtensa_asm:movi(Reg, B3))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, 8))/binary,
        (jit_xtensa_asm:addi(Reg, Reg, B2))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, 8))/binary,
        (jit_xtensa_asm:addi(Reg, Reg, B1))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, 8))/binary,
        (jit_xtensa_asm:addi(Reg, Reg, B0))/binary
    >>.

%% mov_immediate/3: state-based, tries efficient sequences first then L32R.
mov_immediate(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when
    Val >= -2048, Val =< 2047
->
    %% Single movi instruction (3 bytes)
    I = jit_xtensa_asm:movi(Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
mov_immediate(#state{regs = Regs0} = State0, Reg, Val) ->
    State =
        case split_movi_addmi(Val) of
            {ok, Low, High} ->
                %% movi + addmi (6 bytes)
                mov_immediate_movi_addmi(State0, Reg, Low, High);
            false ->
                case find_movi_slli(Val band 16#FFFFFFFF) of
                    {ok, Core, Shift} ->
                        %% movi + slli (6 bytes)
                        mov_immediate_movi_slli(State0, Reg, Core, Shift);
                    false ->
                        case find_movi_slli_addi(Val band 16#FFFFFFFF) of
                            {ok, Core, Shift, Addend} ->
                                %% movi + slli + addi (9 bytes)
                                mov_immediate_movi_slli_addi(State0, Reg, Core, Shift, Addend);
                            false ->
                                case find_movi_slli_addmi(Val band 16#FFFFFFFF) of
                                    {ok, Core, Shift, High} ->
                                        %% movi + slli + addmi (9 bytes)
                                        mov_immediate_movi_slli_addmi(
                                            State0, Reg, Core, Shift, High
                                        );
                                    false ->
                                        %% Inline literal: j over value, then l32r (10-13 bytes)
                                        mov_immediate_l32r(State0, Reg, Val)
                                end
                        end
                end
        end,
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{regs = Regs1}.

mov_immediate_movi_addmi(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Low, High
) ->
    I = <<(jit_xtensa_asm:movi(Reg, Low))/binary, (jit_xtensa_asm:addmi(Reg, Reg, High))/binary>>,
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

mov_immediate_movi_slli(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Core, Shift
) ->
    I = <<(jit_xtensa_asm:movi(Reg, Core))/binary, (jit_xtensa_asm:slli(Reg, Reg, Shift))/binary>>,
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

mov_immediate_movi_slli_addi(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Core, Shift, Addend
) ->
    I = <<
        (jit_xtensa_asm:movi(Reg, Core))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, Shift))/binary,
        (jit_xtensa_asm:addi(Reg, Reg, Addend))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

mov_immediate_movi_slli_addmi(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Core, Shift, High
) ->
    I = <<
        (jit_xtensa_asm:movi(Reg, Core))/binary,
        (jit_xtensa_asm:slli(Reg, Reg, Shift))/binary,
        (jit_xtensa_asm:addmi(Reg, Reg, High))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

mov_immediate_l32r(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    O = StreamModule:offset(Stream0),
    Padding = (4 - ((O + 3) rem 4)) rem 4,
    %% j(N) targets PC+4+N; skip over Padding+literal(4) to land on l32r
    SkipDist = Padding + 3,
    JInstr = jit_xtensa_asm:j(SkipDist),
    PadBytes = <<16#FF:(Padding * 8)>>,
    ValBytes = <<(Val band 16#FFFFFFFF):32/little>>,
    %% L32R with imm16=-1 loads from 4 bytes before the aligned PC
    L32rInstr = jit_xtensa_asm:l32r(Reg, -1),
    Code = <<JInstr/binary, PadBytes/binary, ValBytes/binary, L32rInstr/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%% Split Val into Low (for movi, -2048..2047) and High (for addmi, multiple of 256,
%% with High/256 in -128..127)
split_movi_addmi(Val) ->
    Low0 = Val band 16#FF,
    LowSigned0 =
        if
            Low0 >= 128 -> Low0 - 256;
            true -> Low0
        end,
    High0 = Val - LowSigned0,
    HighByte0 = High0 bsr 8,
    {LowSigned, High} =
        if
            HighByte0 >= -128, HighByte0 =< 127 ->
                {LowSigned0, High0};
            HighByte0 > 127 ->
                {LowSigned0 + 256, High0 - 256};
            true ->
                {LowSigned0 - 256, High0 + 256}
        end,
    HighByte = High bsr 8,
    if
        LowSigned >= -2048,
        LowSigned =< 2047,
        HighByte >= -128,
        HighByte =< 127,
        High rem 256 =:= 0 ->
            {ok, LowSigned, High};
        true ->
            false
    end.

%% Find Core and Shift such that (Core << Shift) =:= Val (unsigned 32-bit)
%% where Core fits movi (-2048..2047) and Shift is 1..31
find_movi_slli(Val) when Val =:= 0 -> false;
find_movi_slli(Val) ->
    Tz = count_trailing_zeros(Val),
    if
        Tz >= 1 ->
            Core = sign_extend_from_width(Val bsr Tz, 32 - Tz),
            if
                Core >= -2048, Core =< 2047 ->
                    {ok, Core, Tz};
                true ->
                    false
            end;
        true ->
            false
    end.

%% Find Core, Shift, Addend such that (Core << Shift) + Addend =:= Val
%% where Core fits movi, Shift is 1..31, Addend fits addi (-128..127)
find_movi_slli_addi(Val) ->
    find_movi_slli_addi(Val, -128).

find_movi_slli_addi(_Val, Addend) when Addend > 127 -> false;
find_movi_slli_addi(Val, Addend) ->
    Shifted = (Val - Addend) band 16#FFFFFFFF,
    case find_movi_slli(Shifted) of
        {ok, Core, Shift} -> {ok, Core, Shift, Addend};
        false -> find_movi_slli_addi(Val, Addend + 1)
    end.

%% Find Core, Shift, High such that (Core << Shift) + High =:= Val
%% where Core fits movi, Shift is 1..31, High fits addmi (multiple of 256, High/256 in -128..127)
find_movi_slli_addmi(Val) ->
    find_movi_slli_addmi(Val, -128).

find_movi_slli_addmi(_Val, HighByte) when HighByte > 127 -> false;
find_movi_slli_addmi(Val, HighByte) ->
    High = HighByte * 256,
    Shifted = (Val - High) band 16#FFFFFFFF,
    case find_movi_slli(Shifted) of
        {ok, Core, Shift} -> {ok, Core, Shift, High};
        false -> find_movi_slli_addmi(Val, HighByte + 1)
    end.

count_trailing_zeros(0) -> 32;
count_trailing_zeros(N) -> count_trailing_zeros(N, 0).
count_trailing_zeros(N, Count) when N band 1 =:= 1 -> Count;
count_trailing_zeros(N, Count) -> count_trailing_zeros(N bsr 1, Count + 1).

%% Sign-extend a value from Width bits to full Erlang integer
sign_extend_from_width(Val, Width) ->
    Mask = 1 bsl (Width - 1),
    if
        Val band Mask =/= 0 -> Val - (1 bsl Width);
        true -> Val
    end.

sub(State, {free, Reg}, Val) ->
    {sub(State, Reg, Val), Reg};
sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    Val >= 0 andalso Val =< 127
->
    I1 = jit_xtensa_asm:addi(Reg, Reg, -Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    is_atom(Val)
->
    I = jit_xtensa_asm:sub(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
sub(#state{stream_module = StreamModule, available_regs = Avail} = State0, Reg, Val) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:sub(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    %% mov_immediate already invalidated Temp in State1#state.regs.
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 3) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:slli(Temp, Reg, 1),
    I2 = jit_xtensa_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 5) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:slli(Temp, Reg, 2),
    I2 = jit_xtensa_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 7) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:slli(Temp, Reg, 3),
    I2 = jit_xtensa_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 9) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:slli(Temp, Reg, 3),
    I2 = jit_xtensa_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 15) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    I1 = jit_xtensa_asm:slli(Temp, Reg, 4),
    I2 = jit_xtensa_asm:sub(Reg, Temp, Reg),
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
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    Reg,
    Val
) when is_integer(Val) ->
    Temp =
        case Avail of
            0 -> ?A8_REG;
            _ -> first_avail(Avail)
        end,
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_xtensa_asm:mull(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State1#state{
        stream = Stream2,
        available_regs = State1#state.available_regs bor reg_bit(Temp),
        regs = Regs1
    };
mul(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, DestReg, SrcReg
) when is_atom(SrcReg) ->
    I = jit_xtensa_asm:mull(DestReg, DestReg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DestReg),
    State#state{stream = Stream1, regs = Regs1}.

-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        jump_table_start = JumpTableStart,
        regs = Regs0
    } = State0
) ->
    Temp = first_avail(Avail),
    I1 = jit_xtensa_asm:l32i(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    I2 = jit_xtensa_asm:addi(Temp, Temp, -1),
    I3 = jit_xtensa_asm:s32i(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    %% Placeholder for bnez (3 bytes) patched below once the continuation offset is known.
    I4 = <<16#FF, 16#FF, 16#FF>>,
    _ADROffset = BNEOffset + byte_size(I4),
    I5 = list_to_binary(lists:duplicate(?CODE_RELATIVE_ADDRESS_PADDED_SIZE, 16#FF)),
    I6 = jit_xtensa_asm:s32i(Temp, ?JITSTATE_REG, ?JITSTATE_CONTINUATION_OFFSET),
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary, I6/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State1 = State0#state{stream = Stream2, regs = Regs1},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    #state{stream = Stream3} = State2,
    %% Emit ENTRY at the continuation point so C can call it via CALL8.
    %% The continuation offset stored in jit_state points here.
    EntryInstr = jit_xtensa_asm:entry(a1, ?ENTRY_FRAME_SIZE),
    NewOffset = StreamModule:offset(Stream3),
    Stream3b = StreamModule:append(Stream3, EntryInstr),
    %% The BNEZ from within JIT code must skip past the ENTRY instruction
    %% (we are already in a windowed frame, so hitting ENTRY again is wrong).
    NewI4 = jit_xtensa_asm:bnez(Temp, (NewOffset + 3) - BNEOffset - 4),
    %% Generate code_relative_address padded to 21 bytes
    %% The continuation address is NewOffset (stream offset) minus header
    NewI5 = code_relative_address_padded(Temp, NewOffset - JumpTableStart),
    Stream4 = StreamModule:replace(
        Stream3b, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    StreamN = Stream4,
    State3 = merge_used_regs(State2#state{stream = StreamN}, State1#state.used_regs),
    %% schedule_next clobbers caller-saved regs; invalidate cache at continuation.
    State3#state{regs = jit_regs:invalidate_all(State1#state.regs)}.

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        regs = Regs0
    } = State0a,
    Label
) ->
    Temp = first_avail(Avail),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0 = State0a#state{regs = Regs1},
    I1 = jit_xtensa_asm:l32i(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    I2 = jit_xtensa_asm:addi(Temp, Temp, -1),
    I3 = jit_xtensa_asm:s32i(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    LabelLookupResult =
        case State0#state.labels of
            #{Label := LabelOffset0} -> {Label, LabelOffset0};
            _ -> false
        end,

    BccOffset = StreamModule:offset(Stream1),

    State4 =
        case LabelLookupResult of
            {Label, LabelOffset} ->
                Rel = LabelOffset - BccOffset,
                %% BNEZ (BRI12 format): 12-bit signed offset relative to PC+4.
                %% Account for the -4 adjustment: Rel - 4 must fit in [-2048, 2047].
                BnezOffset = Rel - 4,
                if
                    BnezOffset >= -2048 andalso BnezOffset =< 2047 ->
                        I4 = jit_xtensa_asm:bnez(Temp, BnezOffset),
                        Stream2 = StreamModule:append(Stream1, I4),
                        State0#state{stream = Stream2};
                    true ->
                        FarSeqOffset = BccOffset + 3,
                        {State1, FarCodeBlock} = branch_to_label_code(
                            State0, FarSeqOffset, Label, LabelLookupResult
                        ),
                        FarSeqSize = byte_size(FarCodeBlock),
                        I4 = jit_xtensa_asm:beqz(Temp, FarSeqSize - 1),
                        Stream2 = StreamModule:append(Stream1, I4),
                        Stream3 = StreamModule:append(Stream2, FarCodeBlock),
                        State1#state{stream = Stream3}
                end;
            false ->
                FarSeqOffset = BccOffset + 3,
                {State1, FarCodeBlock} = branch_to_label_code(State0, FarSeqOffset, Label, false),
                FarSeqSize = byte_size(FarCodeBlock),
                I4 = jit_xtensa_asm:beqz(Temp, FarSeqSize - 1),
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

-spec set_cp(state()) -> {state(), non_neg_integer(), xtensa_register()}.
set_cp(#state{available_regs = Avail, used_regs = UsedRegs} = State0) ->
    TempReg = first_avail(Avail),
    TempBit = reg_bit(TempReg),
    %% Reserve TempReg for the offset BEFORE get_module_index consumes available registers.
    State1 = State0#state{
        available_regs = Avail band (bnot TempBit), used_regs = UsedRegs bor TempBit
    },
    {State2, Reg} = get_module_index(State1),
    #state{stream_module = StreamModule, stream = Stream0} = State2,

    Offset = StreamModule:offset(Stream0),
    I1 = jit_xtensa_asm:slli(Reg, Reg, 24),
    %% Reserve 15 bytes for offset load (li generates 3..21 bytes, patched by rewrite_cp_offset).
    I2 =
        <<16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
            16#FF, 16#FF>>,
    MOVOffset = Offset + byte_size(I1),
    I4 = jit_xtensa_asm:or_(Reg, Reg, TempReg),
    {BaseReg, Off} = ?CP,
    I5 = jit_xtensa_asm:s32i(Reg, BaseReg, Off),
    Code = <<I1/binary, I2/binary, I4/binary, I5/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State3 = State2#state{stream = Stream1},
    State4 = free_native_register(State3, Reg),
    State5 = free_native_register(State4, TempReg),
    {State5, MOVOffset, TempReg}.

-spec rewrite_cp_offset(state(), non_neg_integer(), xtensa_register()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset,
    TempReg
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    CPValue = NewOffset bsl 2,
    NewMoveInstr = mov_immediate(TempReg, CPValue),
    %% We reserved 15 bytes for the li instruction
    %% li generates 3 bytes (movi for small) or 6 bytes (movi+addmi) or more for large
    %% Pad with nops to fill 15 bytes
    NopCount = (15 - byte_size(NewMoveInstr)) div 3,
    Nops = list_to_binary([jit_xtensa_asm:nop() || _ <- lists:seq(1, NopCount)]),
    PaddedInstr = <<NewMoveInstr/binary, Nops/binary>>,
    15 = byte_size(PaddedInstr),
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, PaddedInstr),
    %% Emit ENTRY at the continuation point so that jump_to_continuation can
    %% skip past it with +3. This ENTRY is also needed when C code calls the
    %% continuation via CALL8 (through jit_return which converts ctx->cp to
    %% jit_state.continuation).
    EntryInstr = jit_xtensa_asm:entry(a1, ?ENTRY_FRAME_SIZE),
    Stream2 = StreamModule:append(Stream1, EntryInstr),
    State0#state{stream = Stream2}.

set_bs(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State0,
    TermReg
) ->
    Temp = first_avail(Avail),
    {BaseReg1, Off1} = ?BS,
    I1 = jit_xtensa_asm:s32i(TermReg, BaseReg1, Off1),
    I2 = mov_immediate(Temp, 0),
    {BaseReg2, Off2} = ?BS_OFFSET,
    I3 = jit_xtensa_asm:s32i(Temp, BaseReg2, Off2),
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
        labels = Labels,
        jump_table_start = JumpTableStart
    } = State,
    SortedLines
) ->
    SortedLabels = lists:keysort(2, [
        {Label, LabelOffset}
     || {Label, LabelOffset} <- maps:to_list(Labels), is_integer(Label)
    ]),

    I2 = jit_xtensa_asm:retw(),
    %% Xtensa: Return address of data that follows this prologue.
    %% code_relative_address loads code_base+offset into a2 (return register).
    %% We need the offset of the data after this prologue.
    %% Prologue = code_relative_address(21 bytes padded) + retw(3 bytes) = 24 bytes
    %% So the data starts at current_offset + 24
    %% Subtract header offset since code_base points past the header
    PrologueSize = ?CODE_RELATIVE_ADDRESS_PADDED_SIZE + 3,
    DataOffset = StreamModule:offset(Stream0) + PrologueSize - JumpTableStart,
    I1 = code_relative_address_padded(a2, DataOffset),
    Prologue = <<I1/binary, I2/binary>>,
    PrologueSize = byte_size(Prologue),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<Prologue/binary, (length(SortedLabels)):16, LabelsTable/binary, (length(SortedLines)):16,
            LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

%% @doc Generate code to load the absolute address of a code offset into Rd.
%% Loads code_base from JITState, then adds the absolute offset.
%% Returns a variable-length sequence to load code_base + offset into Rd.
-spec code_relative_address(xtensa_register(), non_neg_integer()) -> binary().
code_relative_address(Rd, CodeRelativeOffset) ->
    %% Load code_base from JITState (stored at offset 0xC)
    %% code_base points to the start of the native code (past the chunk header).
    %% CodeRelativeOffset must be relative to code_base, NOT a raw stream offset.
    I1 = jit_xtensa_asm:l32i(Rd, ?JITSTATE_REG, ?JITSTATE_CODE_BASE_OFFSET),
    I2 = add_immediate_binary(Rd, Rd, CodeRelativeOffset),
    <<I1/binary, I2/binary>>.

%% @doc Generate code_relative_address padded to exactly 21 bytes.
%% Loads code_base from JITState, then adds the offset using mov_immediate
%% into A8_REG as scratch, then ADD to combine.
%% Layout: l32i(3) + mov_immediate(padded to 15) + add(3) = 21 bytes.
%% Handles offsets up to 16MB (3-byte mov_immediate_large).
-spec code_relative_address_padded(xtensa_register(), non_neg_integer()) -> binary().
code_relative_address_padded(Rd, AbsOffset) when AbsOffset >= 0, AbsOffset =< 16#FFFFFF ->
    %% l32i Rd, JITSTATE_REG, CODE_BASE_OFFSET (3 bytes) - load code base
    I1 = jit_xtensa_asm:l32i(Rd, ?JITSTATE_REG, ?JITSTATE_CODE_BASE_OFFSET),
    %% mov_immediate into A8_REG, padded to 15 bytes with NOPs
    %% mov_immediate produces 3, 6, 9, or 15 bytes for offsets up to 16MB
    MovInstr = mov_immediate(?A8_REG, AbsOffset),
    MovSize = byte_size(MovInstr),
    NopCount = (15 - MovSize) div 3,
    Nops = list_to_binary([jit_xtensa_asm:nop() || _ <- lists:seq(1, NopCount)]),
    PaddedMov = <<MovInstr/binary, Nops/binary>>,
    15 = byte_size(PaddedMov),
    %% add Rd, Rd, A8_REG (3 bytes) - combine code_base + offset
    I3 = jit_xtensa_asm:add(Rd, Rd, ?A8_REG),
    Result = <<I1/binary, PaddedMov/binary, I3/binary>>,
    21 = byte_size(Result),
    Result.

%% @doc Generate an add-immediate instruction sequence.
%% For small values (+-128): addi (3 bytes)
%% For larger values: li + add (6 bytes)
-spec add_immediate_binary(xtensa_register(), xtensa_register(), integer()) -> binary().
add_immediate_binary(Rd, Rs, 0) ->
    case Rd of
        Rs -> <<>>;
        _ -> jit_xtensa_asm:mov(Rd, Rs)
    end;
add_immediate_binary(Rd, Rs, Imm) when Imm >= -128, Imm =< 127 ->
    jit_xtensa_asm:addi(Rd, Rs, Imm);
add_immediate_binary(Rd, Rs, Imm) ->
    %% For larger immediates, use li into Rd then add
    %% This works because we can use Rd as temp since we're computing Rd = Rs + Imm
    case Rd of
        Rs ->
            %% Can't use Rd as temp since Rd == Rs, use addmi+addi combo
            %% addmi handles multiples of 256 in range -32768..32512
            %% Then addi handles the remainder in range -128..127
            HiPart = (Imm div 256) * 256,
            LoPart = Imm - HiPart,
            if
                HiPart >= -32768, HiPart =< 32512, LoPart >= -128, LoPart =< 127 ->
                    I1 = jit_xtensa_asm:addmi(Rd, Rs, HiPart),
                    case LoPart of
                        0 ->
                            I1;
                        _ ->
                            I2 = jit_xtensa_asm:addi(Rd, Rd, LoPart),
                            <<I1/binary, I2/binary>>
                    end;
                true ->
                    %% Very large offset - must use a different temp
                    %% Use A8_REG as emergency temp
                    I1 = mov_immediate(?A8_REG, Imm),
                    I2 = jit_xtensa_asm:add(Rd, Rs, ?A8_REG),
                    <<I1/binary, I2/binary>>
            end;
        _ ->
            I1 = mov_immediate(Rd, Imm),
            I2 = jit_xtensa_asm:add(Rd, Rs, Rd),
            <<I1/binary, I2/binary>>
    end.

%% Helper function to generate str instruction with y_reg offset, handling large offsets
str_y_reg(SrcReg, Y, TempReg, _AvailableMask) when Y * 4 =< 1020 ->
    % Small offset - use immediate addressing
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(TempReg, BaseReg, Off),
    I2 = jit_xtensa_asm:s32i(SrcReg, TempReg, Y * 4),
    <<I1/binary, I2/binary>>;
str_y_reg(SrcReg, Y, TempReg1, AvailableMask) when AvailableMask =/= 0 ->
    % Large offset - use register arithmetic with second available register
    TempReg2 = first_avail(AvailableMask),
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(TempReg1, BaseReg, Off),
    I2 = mov_immediate(TempReg2, Offset),
    I3 = jit_xtensa_asm:add(TempReg2, TempReg2, TempReg1),
    I4 = jit_xtensa_asm:s32i(SrcReg, TempReg2, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary>>;
str_y_reg(SrcReg, Y, TempReg1, 0) ->
    % Large offset - no additional registers available, use A8_REG as second temp
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(TempReg1, BaseReg, Off),
    I2 = jit_xtensa_asm:mov(?A8_REG, TempReg1),
    I3 = mov_immediate(TempReg1, Offset),
    I4 = jit_xtensa_asm:add(TempReg1, TempReg1, ?A8_REG),
    I5 = jit_xtensa_asm:s32i(SrcReg, TempReg1, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>.

%% Helper function to generate ldr instruction with y_reg offset, handling large offsets
ldr_y_reg(DstReg, Y, AvailableMask) when AvailableMask =/= 0 andalso Y * 4 =< 1020 ->
    % Small offset - use immediate addressing
    TempReg = first_avail(AvailableMask),
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(TempReg, BaseReg, Off),
    I2 = jit_xtensa_asm:l32i(DstReg, TempReg, Y * 4),
    <<I1/binary, I2/binary>>;
ldr_y_reg(DstReg, Y, AvailableMask) when AvailableMask =/= 0 ->
    % Large offset - use DstReg as second temp register for arithmetic
    TempReg = first_avail(AvailableMask),
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(TempReg, BaseReg, Off),
    I2 = mov_immediate(DstReg, Offset),
    I3 = jit_xtensa_asm:add(DstReg, DstReg, TempReg),
    I4 = jit_xtensa_asm:l32i(DstReg, DstReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary>>;
ldr_y_reg(DstReg, Y, 0) when Y * 4 =< 1020 ->
    % Small offset, no registers available - use DstReg as temp
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(DstReg, BaseReg, Off),
    I2 = jit_xtensa_asm:l32i(DstReg, DstReg, Y * 4),
    <<I1/binary, I2/binary>>;
ldr_y_reg(DstReg, Y, 0) ->
    % Large offset, no registers available - use A8_REG as temp register
    Offset = Y * 4,
    {BaseReg, Off} = ?Y_REGS,
    I1 = jit_xtensa_asm:l32i(DstReg, BaseReg, Off),
    I2 = jit_xtensa_asm:mov(?A8_REG, DstReg),
    I3 = mov_immediate(DstReg, Offset),
    I4 = jit_xtensa_asm:add(DstReg, DstReg, ?A8_REG),
    I5 = jit_xtensa_asm:l32i(DstReg, DstReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>.

reg_bit(a0) -> ?REG_BIT_A0;
reg_bit(a1) -> ?REG_BIT_A1;
reg_bit(a2) -> ?REG_BIT_A2;
reg_bit(a3) -> ?REG_BIT_A3;
reg_bit(a4) -> ?REG_BIT_A4;
reg_bit(a5) -> ?REG_BIT_A5;
reg_bit(a6) -> ?REG_BIT_A6;
reg_bit(a7) -> ?REG_BIT_A7;
reg_bit(a8) -> ?REG_BIT_A8;
reg_bit(a9) -> ?REG_BIT_A9;
reg_bit(a10) -> ?REG_BIT_A10;
reg_bit(a11) -> ?REG_BIT_A11;
reg_bit(a12) -> ?REG_BIT_A12;
reg_bit(a13) -> ?REG_BIT_A13;
reg_bit(a14) -> ?REG_BIT_A14;
reg_bit(a15) -> ?REG_BIT_A15.

%% High registers (a15-a9) are clobbered by CALLX8 so prefer them to minimize saves.
first_avail(Mask) when Mask band ?REG_BIT_A15 =/= 0 -> a15;
first_avail(Mask) when Mask band ?REG_BIT_A14 =/= 0 -> a14;
first_avail(Mask) when Mask band ?REG_BIT_A13 =/= 0 -> a13;
first_avail(Mask) when Mask band ?REG_BIT_A12 =/= 0 -> a12;
first_avail(Mask) when Mask band ?REG_BIT_A11 =/= 0 -> a11;
first_avail(Mask) when Mask band ?REG_BIT_A10 =/= 0 -> a10;
first_avail(Mask) when Mask band ?REG_BIT_A9 =/= 0 -> a9;
first_avail(Mask) when Mask band ?REG_BIT_A8 =/= 0 -> a8;
first_avail(Mask) when Mask band ?REG_BIT_A7 =/= 0 -> a7;
first_avail(Mask) when Mask band ?REG_BIT_A6 =/= 0 -> a6;
first_avail(Mask) when Mask band ?REG_BIT_A5 =/= 0 -> a5.

mask_to_list(0) -> [];
mask_to_list(Mask) -> mask_to_list_a15(Mask).

mask_to_list_a15(Mask) when Mask band ?REG_BIT_A15 =/= 0 -> [a15 | mask_to_list_a14(Mask)];
mask_to_list_a15(Mask) -> mask_to_list_a14(Mask).
mask_to_list_a14(Mask) when Mask band ?REG_BIT_A14 =/= 0 -> [a14 | mask_to_list_a13(Mask)];
mask_to_list_a14(Mask) -> mask_to_list_a13(Mask).
mask_to_list_a13(Mask) when Mask band ?REG_BIT_A13 =/= 0 -> [a13 | mask_to_list_a12(Mask)];
mask_to_list_a13(Mask) -> mask_to_list_a12(Mask).
mask_to_list_a12(Mask) when Mask band ?REG_BIT_A12 =/= 0 -> [a12 | mask_to_list_a11(Mask)];
mask_to_list_a12(Mask) -> mask_to_list_a11(Mask).
mask_to_list_a11(Mask) when Mask band ?REG_BIT_A11 =/= 0 -> [a11 | mask_to_list_a10(Mask)];
mask_to_list_a11(Mask) -> mask_to_list_a10(Mask).
mask_to_list_a10(Mask) when Mask band ?REG_BIT_A10 =/= 0 -> [a10 | mask_to_list_a9(Mask)];
mask_to_list_a10(Mask) -> mask_to_list_a9(Mask).
mask_to_list_a9(Mask) when Mask band ?REG_BIT_A9 =/= 0 -> [a9 | mask_to_list_a8(Mask)];
mask_to_list_a9(Mask) -> mask_to_list_a8(Mask).
mask_to_list_a8(Mask) when Mask band ?REG_BIT_A8 =/= 0 -> [a8 | mask_to_list_a7(Mask)];
mask_to_list_a8(Mask) -> mask_to_list_a7(Mask).
mask_to_list_a7(Mask) when Mask band ?REG_BIT_A7 =/= 0 -> [a7 | mask_to_list_a6(Mask)];
mask_to_list_a7(Mask) -> mask_to_list_a6(Mask).
mask_to_list_a6(Mask) when Mask band ?REG_BIT_A6 =/= 0 -> [a6 | mask_to_list_a5(Mask)];
mask_to_list_a6(Mask) -> mask_to_list_a5(Mask).
mask_to_list_a5(Mask) when Mask band ?REG_BIT_A5 =/= 0 -> [a5 | mask_to_list_a4(Mask)];
mask_to_list_a5(Mask) -> mask_to_list_a4(Mask).
mask_to_list_a4(Mask) when Mask band ?REG_BIT_A4 =/= 0 -> [a4 | mask_to_list_a3(Mask)];
mask_to_list_a4(Mask) -> mask_to_list_a3(Mask).
mask_to_list_a3(Mask) when Mask band ?REG_BIT_A3 =/= 0 -> [a3 | mask_to_list_a2(Mask)];
mask_to_list_a3(Mask) -> mask_to_list_a2(Mask).
mask_to_list_a2(Mask) when Mask band ?REG_BIT_A2 =/= 0 -> [a2 | mask_to_list_a1(Mask)];
mask_to_list_a2(Mask) -> mask_to_list_a1(Mask).
mask_to_list_a1(Mask) when Mask band ?REG_BIT_A1 =/= 0 -> [a1 | mask_to_list_a0(Mask)];
mask_to_list_a1(Mask) -> mask_to_list_a0(Mask).
mask_to_list_a0(Mask) when Mask band ?REG_BIT_A0 =/= 0 -> [a0];
mask_to_list_a0(_Mask) -> [].

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
add_label(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Label) ->
    Offset0 = StreamModule:offset(Stream0),
    Regs1 = jit_regs:invalidate_all(Regs0),
    add_label(State0#state{regs = Regs1}, Label, Offset0).

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
    %% Patch the jump table entry literal
    %% Each entry is: literal(4) + ENTRY(3) + L32I(3) + L32R(3) + ADD(3) + JX(3) + pad(1)
    %% The 4-byte literal at the start of the entry stores the code-relative target offset.
    JumpTableEntryOffset = JumpTableStart + Label * ?JUMP_TABLE_ENTRY_SIZE,
    LiteralOffset = JumpTableEntryOffset,

    %% Code-relative offset: from code_base (= native_code start = JumpTableStart) to label
    CodeRelativeTarget = LabelOffset - JumpTableStart,
    LiteralData = <<CodeRelativeTarget:32/little>>,

    Stream1 = StreamModule:replace(Stream0, LiteralOffset, LiteralData),

    % Eagerly patch any branches targeting this label
    {Stream2, RemainingBranches} = patch_branches_for_label(
        StreamModule,
        Stream1,
        Label,
        LabelOffset,
        Branches
    ),

    State#state{
        stream = Stream2,
        branches = RemainingBranches,
        labels = Labels#{Label => LabelOffset},
        regs = jit_regs:invalidate_all(State#state.regs)
    };
add_label(#state{labels = Labels, regs = Regs0} = State, Label, Offset) ->
    State#state{labels = Labels#{Label => Offset}, regs = jit_regs:invalidate_all(Regs0)}.

%% @doc Get the register tracking state.
get_regs_tracking(#state{regs = Regs}) -> Regs.

value_to_contents(Value) ->
    jit_regs:value_to_contents(Value, ?MAX_REG).

vm_dest_to_contents(Dest) ->
    jit_regs:vm_dest_to_contents(Dest, ?MAX_REG).

-ifdef(JIT_DWARF).
%%-----------------------------------------------------------------------------
%% @doc Return the DWARF register number for the ctx parameter
%% @returns The DWARF register number where ctx is passed (a2 in Xtensa)
%% @end
%%-----------------------------------------------------------------------------
-spec dwarf_ctx_register() -> non_neg_integer().
dwarf_ctx_register() ->
    ?DWARF_A2_REG_XTENSA.

-spec dwarf_register_number(xtensa_register()) -> non_neg_integer().
dwarf_register_number(Reg) -> jit_xtensa_asm:reg_to_num(Reg).
-endif.
