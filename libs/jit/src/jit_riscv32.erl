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
    add_label/3,
    xor_/3,
    shift_right_arith/3,
    div_/3,
    rem_/3
]).

-ifdef(JIT_DWARF).
-export([
    dwarf_opcode/2,
    dwarf_label/2,
    dwarf_function/3,
    dwarf_line/2,
    dwarf_ctx_register/0,
    dwarf_register_number/1
]).
-endif.

-compile([warnings_as_errors, nowarn_unused_type]).

-include_lib("jit.hrl").

-include("primitives.hrl").
-include("term.hrl").

-ifdef(JIT_DWARF).
-include("jit_dwarf.hrl").
-endif.

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
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, riscv32_register()}.
-type value() :: immediate() | vm_register() | riscv32_register() | {ptr, riscv32_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_riscv32_register() ::
    {free, riscv32_register()} | riscv32_register().

-type condition() ::
    {riscv32_register(), '<', integer()}
    | {maybe_free_riscv32_register(), '<', riscv32_register()}
    | {integer(), '<', maybe_free_riscv32_register()}
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
-define(JITSTATE_REG, a1).
-define(RA_REG, ra).
-define(JITSTATE_MODULE_OFFSET, 0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#8).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * 4}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).

-define(JUMP_TABLE_ENTRY_SIZE, 8).

%% RISC-V32 register mappings

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
-define(PARAMETER_REGS, [a0, a1, a2, a3, a4, a5, a6, a7]).

-define(REG_BIT_A0, (1 bsl 0)).
-define(REG_BIT_A1, (1 bsl 1)).
-define(REG_BIT_A2, (1 bsl 2)).
-define(REG_BIT_A3, (1 bsl 3)).
-define(REG_BIT_A4, (1 bsl 4)).
-define(REG_BIT_A5, (1 bsl 5)).
-define(REG_BIT_A6, (1 bsl 6)).
-define(REG_BIT_A7, (1 bsl 7)).
-define(REG_BIT_T0, (1 bsl 8)).
-define(REG_BIT_T1, (1 bsl 9)).
-define(REG_BIT_T2, (1 bsl 10)).
-define(REG_BIT_T3, (1 bsl 11)).
-define(REG_BIT_T4, (1 bsl 12)).
-define(REG_BIT_T5, (1 bsl 13)).
-define(REG_BIT_T6, (1 bsl 14)).

%% AVAILABLE_REGS = [t6, t5, t4, t3, t2, t1, t0]
-define(AVAILABLE_REGS_MASK,
    (?REG_BIT_T6 bor ?REG_BIT_T5 bor ?REG_BIT_T4 bor ?REG_BIT_T3 bor
        ?REG_BIT_T2 bor ?REG_BIT_T1 bor ?REG_BIT_T0)
).

-include("jit_backend_dwarf_impl.hrl").
-define(ASM, jit_riscv32_asm).
-define(WORD_SIZE_BYTES, 4).
-define(BOOL_SHIFT, 31).
-define(Y_OFFSET_LIMIT, 124).
-define(PRIMITIVE_DIRECT_LOAD_LIMIT, 124).
-define(FLOAT_DATA_OFFSET, 4).
-define(IS_SIGNED_OR_UNSIGNED_WORD(X),
    is_integer(X) andalso X >= -16#80000000 andalso X < 16#100000000
).
-define(LOAD_WORD(Dst, Base, Off), jit_riscv32_asm:lw(Dst, Base, Off)).
-define(STORE_WORD(Base, Src, Off), jit_riscv32_asm:sw(Base, Src, Off)).
-define(DWARF_CTX_REG, ?DWARF_A0_REG_RISCV).
-define(ARRAY_OFFSET_FOLD_GUARD(Offset), Offset div 8 =:= 0).
-define(ARRAY_OFFSET_FOLD(IndexReg, Offset), IndexReg + (Offset div 8)).

-include("jit_riscv_impl.hrl").

-spec word_size() -> 4 | 8.
word_size() -> 4.

div_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I = jit_riscv32_asm:div_(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

rem_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I = jit_riscv32_asm:rem_(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

% ILP32: 64-bit arguments require double-word alignment (even register number)
parameter_regs0_avm_int64_t(T, [a0, a1 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a1, a0 | Acc]);
parameter_regs0_avm_int64_t(T, [a1, a2 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a2, a1 | Acc]);
parameter_regs0_avm_int64_t(T, [a2, a3 | Rest], Acc) ->
    parameter_regs0(T, Rest, [a3, a2 | Acc]).

% ILP32: 64-bit arguments need two registers
handle_avm_int64_t(State, Value, ArgsT, ArgsRegs, ParamRegs, AvailGP, StackOffset) ->
    LowPart = Value band 16#FFFFFFFF,
    HighPart = (Value bsr 32) band 16#FFFFFFFF,
    set_registers_args0(
        State, [LowPart, HighPart | ArgsT], [imm | ArgsRegs], ParamRegs, AvailGP, StackOffset
    ).

-ifdef(JIT_DWARF).
-spec dwarf_ctx_register() -> non_neg_integer().
dwarf_ctx_register() ->
    ?DWARF_A0_REG_RISCV.

%% RISC-V DWARF register numbers: a0-a7 = 10-17, t0-t6 = 5-7,28-31
-spec dwarf_register_number(atom()) -> non_neg_integer().
dwarf_register_number(a0) -> 10;
dwarf_register_number(a1) -> 11;
dwarf_register_number(a2) -> 12;
dwarf_register_number(a3) -> 13;
dwarf_register_number(a4) -> 14;
dwarf_register_number(a5) -> 15;
dwarf_register_number(a6) -> 16;
dwarf_register_number(a7) -> 17;
dwarf_register_number(t0) -> 5;
dwarf_register_number(t1) -> 6;
dwarf_register_number(t2) -> 7;
dwarf_register_number(t3) -> 28;
dwarf_register_number(t4) -> 29;
dwarf_register_number(t5) -> 30;
dwarf_register_number(t6) -> 31.
-endif.
