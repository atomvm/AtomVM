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

%% Shared native-register allocation bookkeeping for the register-based JIT
%% backends. The available/used scratch-register bitmasks live inside
%% `jit_regs:regs()' (set via `jit_regs:new/2', read via `jit_regs:available_regs/1'
%% and `jit_regs:used_regs/1'); these functions are the thin per-backend
%% wrappers that read/write the state record's `regs' field.
%%
%% Backends including this file must provide:
%%   - `#state{}' record with a `regs :: jit_regs:regs()' field (no separate
%%     `available_regs' / `used_regs' fields — they're inside `regs'),
%%   - `reg_bit/1'      : map a register atom to its bitmask,
%%   - `?AVAILABLE_REGS_MASK' : the mask of all allocatable scratch registers,
%%   - `?PARAMETER_REGS' : the C-ABI parameter registers, in order,
%%   - `?FIRST_AVAIL_REGS' : allocatable scratch registers in allocation-priority
%%     order (drives `first_avail/1'),
%%   - `?MASK_TO_LIST_REGS' : every register that can appear in a used/available
%%     mask, in canonical order (drives `mask_to_list/1'),
%%   - `?CTX_REG' : the register holding the execution context,
%%   - `?JITSTATE_ARG_REG' : what the `jit_state' argument maps to (a register
%%     atom on x86_64/aarch64, the atom `jit_state' on backends that keep it
%%     symbolic).
%%
%% These macros must be defined *before* this file is included.
%%
%% wasm32 does NOT include this file: it allocates dynamically-numbered WASM
%% locals, so its bit helpers use `local_bit/1' + `mask_to_locals/1' rather than
%% the register variants, and its scratch pool grows past the initial
%% `?AVAILABLE_REGS_MASK' at runtime. It defines these bookkeeping functions
%% inline.

%% Return the list of currently used native registers (debugging only).
used_regs(#state{regs = Regs}) -> mask_to_list(jit_regs:used_regs(Regs)).

%% Return the list of currently available native scratch registers (debugging
%% only).
available_regs(#state{regs = Regs}) -> mask_to_list(jit_regs:available_regs(Regs)).

%% Free a list of native registers. Entries that are not registers (pointers to
%% registers, immediates, etc.) are handled/ignored by free_native_register/2.
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Reg | Rest]) ->
    State1 = free_native_register(State, Reg),
    free_native_registers(State1, Rest).

%% Free a single native register. A bare register atom is returned to the
%% available pool; a {ptr, Reg} frees the underlying register; anything else is
%% ignored.
free_native_register(#state{regs = Regs} = State, Reg) when is_atom(Reg) ->
    State#state{regs = jit_regs:free_reg(Regs, reg_bit(Reg))};
free_native_register(State, {ptr, Reg}) ->
    free_native_register(State, Reg);
free_native_register(State, _Other) ->
    State.

%% Assert that all native scratch registers are available (debugging only).
assert_all_native_free(#state{regs = Regs}) ->
    0 = jit_regs:used_regs(Regs),
    ?AVAILABLE_REGS_MASK = jit_regs:available_regs(Regs),
    ok.

%% Return the first allocatable scratch register set in `Mask', following the
%% backend's allocation-priority order. Crashes if `Mask' has no allocatable
%% register set; callers check availability beforehand.
first_avail(Mask) -> jit_regs:first_set(Mask, ?FIRST_AVAIL_REGS, fun reg_bit/1).

%% Return the registers set in `Mask' in the backend's canonical order. The
%% order is significant: callers use this to build the saved-register push/pop
%% sequences around C calls and the argument-register lists, so it directly
%% affects the emitted machine code.
mask_to_list(Mask) -> jit_regs:mask_to_reg_list(Mask, ?MASK_TO_LIST_REGS, fun reg_bit/1).

%% Map a list of call arguments to the native registers (or `imm'/`stack') that
%% hold them, so the caller can compute which registers a call clobbers.
args_regs(Args) -> lists:map(fun arg_reg/1, Args).

arg_reg({free, {ptr, Reg}}) -> Reg;
arg_reg({free, Reg}) when is_atom(Reg) -> Reg;
arg_reg({free, Imm}) when is_integer(Imm) -> imm;
arg_reg(offset) -> imm;
arg_reg(ctx) -> ?CTX_REG;
arg_reg(jit_state) -> ?JITSTATE_ARG_REG;
arg_reg(jit_state_tail_call) -> ?JITSTATE_ARG_REG;
arg_reg(stack) -> stack;
arg_reg(Reg) when is_atom(Reg) -> Reg;
arg_reg(Imm) when is_integer(Imm) -> imm;
arg_reg({ptr, Reg}) -> Reg;
arg_reg({x_reg, _}) -> ?CTX_REG;
arg_reg({y_reg, _}) -> ?CTX_REG;
arg_reg({fp_reg, _}) -> ?CTX_REG;
arg_reg({free, {x_reg, _}}) -> ?CTX_REG;
arg_reg({free, {y_reg, _}}) -> ?CTX_REG;
arg_reg({free, {fp_reg, _}}) -> ?CTX_REG;
arg_reg({avm_int64_t, _}) -> imm.

%% Reserve a single scratch register for a non-returning (tail) call that passes
%% `Args'. Since the frame is not reused, every scratch register except those
%% holding parameters/arguments is free. Returns a map with the chosen scratch
%% register (`temp'), the resulting available/used masks with that register
%% removed from the available pool (`available_mask' / `used_mask'), and the
%% intermediate parameter/argument register lists and masks (some backends thread
%% these into their argument-setup code).
prepare_call_scratch(Args) ->
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    ArgsRegs = args_regs(Args),
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ArgsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    ScratchMask = ?AVAILABLE_REGS_MASK band (bnot (ArgsMask bor ParamMask)),
    Temp = first_avail(ScratchMask),
    AvailableMask = ScratchMask band (bnot reg_bit(Temp)),
    UsedMask = ?AVAILABLE_REGS_MASK band (bnot AvailableMask),
    #{
        temp => Temp,
        available_mask => AvailableMask,
        used_mask => UsedMask,
        param_regs => ParamRegs,
        args_regs => ArgsRegs,
        param_mask => ParamMask,
        args_mask => ArgsMask
    }.
