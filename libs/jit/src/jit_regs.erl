%
% This file is part of AtomVM.
%
% Copyright 2025-2026 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Track CPU register contents for the JIT backend.
%%
%% This module maintains knowledge about what each CPU register holds
%% (VM x/y register values, immediates, pointers), enabling the backend to
%% skip redundant loads.
%%
%% Tracking must be invalidated at:
%% - Labels (any offset can be a branch target)
%% - Function calls (ABI clobbers caller-saved registers)
%% - Branches (the target has unknown incoming register state)
%%
%% The tracked information includes:
%% - `contents`: maps cpu_reg -> what the register holds

-module(jit_regs).

-export([
    new/0,
    new/2,
    available_regs/1,
    used_regs/1,
    set_available_regs/2,
    set_masks/3,
    alloc_reg/2,
    free_reg/2,
    get_contents/2,
    get_all_contents/1,
    set_contents/3,
    unreachable/1,
    invalidate_reg/2,
    invalidate_all/1,
    invalidate_volatile/2,
    invalidate_vm_loc/2,
    find_reg_with_contents/2,
    merge/2,
    merge/3,
    stack_push/2,
    stack_pop/1,
    stack_clear/1,
    stack_contents/1,
    value_to_contents/2,
    vm_dest_to_contents/2,
    regs_to_mask/2,
    first_set/3,
    mask_to_reg_list/3
]).

-export_type([regs/0, contents/0]).

-type vm_loc() ::
    {x_reg, non_neg_integer()}
    | {y_reg, non_neg_integer()}.

%% What a CPU register holds
-type contents() ::
    vm_loc()
    | {ptr, vm_loc()}
    %% Register holds a known immediate value
    | {imm, integer()}
    %% Register holds the address of CP
    | cp
    %% Register holds the module index
    | module_index
    %% Unknown / clobbered
    | unknown.

-record(regs, {
    contents = #{} :: #{atom() => contents()},
    stack = [] :: [atom() | contents()],
    unreachable = false :: boolean(),
    available_regs = 0 :: non_neg_integer(),
    used_regs = 0 :: non_neg_integer()
}).

-opaque regs() :: #regs{}.

%% @doc Create a new empty register tracking state.
-spec new() -> regs().
new() ->
    #regs{}.

%% @doc Create a new register tracking state with initial available/used masks.
-spec new(non_neg_integer(), non_neg_integer()) -> regs().
new(Available, Used) ->
    #regs{available_regs = Available, used_regs = Used}.

%% @doc Get the available-registers bitmask.
-spec available_regs(regs()) -> non_neg_integer().
available_regs(#regs{available_regs = A}) -> A.

%% @doc Get the used-registers bitmask.
-spec used_regs(regs()) -> non_neg_integer().
used_regs(#regs{used_regs = U}) -> U.

%% @doc Set the available-registers bitmask.
-spec set_available_regs(regs(), non_neg_integer()) -> regs().
set_available_regs(#regs{} = Regs, A) ->
    Regs#regs{available_regs = A}.

%% @doc Set both available and used bitmasks at once.
-spec set_masks(regs(), non_neg_integer(), non_neg_integer()) -> regs().
set_masks(#regs{} = Regs, A, U) ->
    Regs#regs{available_regs = A, used_regs = U}.

%% @doc Mark the scratch register(s) in `Bit' as allocated: remove them from the
%% available pool and add them to the used set. `Bit' is usually a single
%% register's bit (`reg_bit(Reg)') but may be the bitwise-or of several. This is
%% the single place the allocation bit-law lives.
-spec alloc_reg(regs(), non_neg_integer()) -> regs().
alloc_reg(#regs{available_regs = A, used_regs = U} = Regs, Bit) ->
    Regs#regs{available_regs = A band (bnot Bit), used_regs = U bor Bit}.

%% @doc Mark the scratch register(s) in `Bit' as freed: return them to the
%% available pool and remove them from the used set. Inverse of alloc_reg/2.
-spec free_reg(regs(), non_neg_integer()) -> regs().
free_reg(#regs{available_regs = A, used_regs = U} = Regs, Bit) ->
    Regs#regs{available_regs = A bor Bit, used_regs = U band (bnot Bit)}.

%% @doc Merge two regs taking the intersection of available masks and union
%% of used masks (constrained by AllRegsMask).
-spec merge(regs(), regs(), non_neg_integer()) -> regs().
merge(R1, R2, AllRegsMask) ->
    Merged0 = merge(R1, R2),
    A1 = (R1#regs.available_regs band R2#regs.available_regs) band AllRegsMask,
    U1 = (R1#regs.used_regs bor R2#regs.used_regs) band AllRegsMask,
    Merged0#regs{available_regs = A1, used_regs = U1}.

%% @doc Get what a CPU register currently holds.
-spec get_contents(regs(), atom()) -> contents().
get_contents(#regs{contents = C}, Reg) ->
    maps:get(Reg, C, unknown).

%% @doc Get the full contents map (cpu_reg -> contents).
-spec get_all_contents(regs()) -> #{atom() => contents()}.
get_all_contents(#regs{contents = C}) -> C.

%% @doc Record that a CPU register now holds the given contents.
-spec set_contents(regs(), atom(), contents()) -> regs().
set_contents(#regs{contents = C} = Regs, Reg, Contents) ->
    Regs#regs{contents = C#{Reg => Contents}}.

%% @doc Mark register tracking as unreachable after terminal control flow.
-spec unreachable(regs()) -> regs().
unreachable(Regs) ->
    Regs#regs{contents = #{}, stack = [], unreachable = true}.

%% @doc Invalidate tracking for a single CPU register (e.g. it was clobbered).
-spec invalidate_reg(regs(), atom()) -> regs().
invalidate_reg(#regs{contents = C} = Regs, Reg) ->
    Regs#regs{contents = maps:remove(Reg, C)}.

%% @doc Invalidate all register tracking (e.g. at a label or unknown branch target).
-spec invalidate_all(regs()) -> regs().
invalidate_all(Regs) ->
    Regs#regs{contents = #{}, stack = [], unreachable = false}.

%% @doc Invalidate registers that are volatile across a C function call.
%% On x86-64 System V ABI, all our scratch registers (rax, rcx, rdx, rsi, rdi,
%% r8, r9, r10, r11) are caller-saved, so after a C call they're all clobbered.
%% However, the special registers (rdi=ctx, rsi=jit_state, rdx=native_interface)
%% are restored by the JIT after the call via push/pop, so we keep their tracking.
-spec invalidate_volatile(regs(), [atom()]) -> regs().
invalidate_volatile(#regs{contents = C0} = Regs, PreservedRegs) ->
    C1 = maps:filter(fun(Reg, _) -> lists:member(Reg, PreservedRegs) end, C0),
    Regs#regs{contents = C1}.

%% @doc Invalidate all CPU registers that reference a given VM location.
%% Call this when a VM register is written to, so that any CPU register
%% that was caching its old value is invalidated.
-spec invalidate_vm_loc(regs(), vm_loc()) -> regs().
invalidate_vm_loc(#regs{contents = C} = Regs, VmLoc) ->
    C1 = maps:filter(fun(_Reg, Val) -> Val =/= VmLoc end, C),
    Regs#regs{contents = C1}.

%% @doc Find a CPU register that holds the given contents.
%% Returns `{ok, Reg}` or `none`.
-spec find_reg_with_contents(regs(), contents()) -> {ok, atom()} | none.
find_reg_with_contents(#regs{contents = C}, Contents) ->
    find_in_map(maps:iterator(C), Contents).

find_in_map(Iterator, Contents) ->
    case maps:next(Iterator) of
        {Reg, Contents, _Next} -> {ok, Reg};
        {_Reg, _Other, Next} -> find_in_map(Next, Contents);
        none -> none
    end.

%% @doc Merge two register tracking states (for control flow merge points).
%% Only keeps information that is consistent in both states.
-spec merge(regs(), regs()) -> regs().
merge(#regs{unreachable = true}, #regs{unreachable = true}) ->
    unreachable(new());
merge(#regs{unreachable = true}, #regs{} = Regs) ->
    Regs#regs{stack = []};
merge(#regs{} = Regs, #regs{unreachable = true}) ->
    Regs#regs{stack = []};
merge(#regs{contents = C1}, #regs{contents = C2}) ->
    %% Keep only entries that match in both maps
    MergedContents = maps:filter(
        fun(Reg, Val) -> maps:get(Reg, C2, undefined) =:= Val end,
        C1
    ),
    #regs{contents = MergedContents, stack = []}.

%% @doc Record a push to the C stack.
-spec stack_push(regs(), atom() | contents()) -> regs().
stack_push(#regs{stack = S} = Regs, Value) ->
    Regs#regs{stack = [Value | S]}.

%% @doc Record a pop from the C stack.
-spec stack_pop(regs()) -> {atom() | contents(), regs()}.
stack_pop(#regs{stack = [Top | Rest]} = Regs) ->
    {Top, Regs#regs{stack = Rest}};
stack_pop(#regs{stack = []} = Regs) ->
    {unknown, Regs}.

%% @doc Clear the C stack tracking.
-spec stack_clear(regs()) -> regs().
stack_clear(Regs) ->
    Regs#regs{stack = []}.

%% @doc Get the current C stack contents.
-spec stack_contents(regs()) -> [atom() | contents()].
stack_contents(#regs{stack = S}) -> S.

%% @doc Convert a backend value to a contents descriptor for tracking.
%% MaxReg is the maximum number of x registers (typically ?MAX_REG from jit.hrl).
-spec value_to_contents(term(), non_neg_integer()) -> contents().
value_to_contents(cp, _MaxReg) -> cp;
value_to_contents({x_reg, N}, _MaxReg) when is_integer(N) -> {x_reg, N};
value_to_contents({x_reg, extra}, MaxReg) -> {x_reg, MaxReg};
value_to_contents({y_reg, N}, _MaxReg) -> {y_reg, N};
value_to_contents(Imm, _MaxReg) when is_integer(Imm) -> {imm, Imm};
value_to_contents({ptr, _}, _MaxReg) -> unknown;
value_to_contents(_, _MaxReg) -> unknown.

%% @doc Convert a VM destination register to a contents descriptor for tracking.
%% MaxReg is the maximum number of x registers (typically ?MAX_REG from jit.hrl).
-spec vm_dest_to_contents(term(), non_neg_integer()) -> contents().
vm_dest_to_contents({x_reg, X}, MaxReg) when is_integer(X), X < MaxReg -> {x_reg, X};
vm_dest_to_contents({x_reg, extra}, MaxReg) -> {x_reg, MaxReg};
vm_dest_to_contents({y_reg, Y}, _MaxReg) -> {y_reg, Y};
vm_dest_to_contents(_, _MaxReg) -> unknown.

%% @doc Convert a list of register atoms to a bitmask.
%% Skips non-register entries like `imm`, `jit_state`, and `stack`.
%% RegBitFn maps register atoms to their bit positions.
-spec regs_to_mask([atom()], fun((atom()) -> non_neg_integer())) -> non_neg_integer().
regs_to_mask([], _RegBitFn) -> 0;
regs_to_mask([imm | T], RegBitFn) -> regs_to_mask(T, RegBitFn);
regs_to_mask([jit_state | T], RegBitFn) -> regs_to_mask(T, RegBitFn);
regs_to_mask([stack | T], RegBitFn) -> regs_to_mask(T, RegBitFn);
regs_to_mask([Reg | T], RegBitFn) -> RegBitFn(Reg) bor regs_to_mask(T, RegBitFn).

%% @doc Return the first register in the ordered register list whose bit (per
%% `RegBitFn') is set in `Mask', following the list order. Crashes with
%% `function_clause' if no register in the list matches — callers must ensure at
%% least one register in the list has its bit set in `Mask' (matching the
%% previous per-backend guard-clause `first_avail/1').
-spec first_set(non_neg_integer(), [atom()], fun((atom()) -> non_neg_integer())) -> atom().
first_set(Mask, [Reg | Rest], RegBitFn) ->
    case Mask band RegBitFn(Reg) of
        0 -> first_set(Mask, Rest, RegBitFn);
        _ -> Reg
    end.

%% @doc Return the registers in the ordered register list whose bits (per
%% `RegBitFn') are set in `Mask', preserving the list order.
-spec mask_to_reg_list(non_neg_integer(), [atom()], fun((atom()) -> non_neg_integer())) ->
    [atom()].
mask_to_reg_list(_Mask, [], _RegBitFn) ->
    [];
mask_to_reg_list(Mask, [Reg | Rest], RegBitFn) ->
    case Mask band RegBitFn(Reg) of
        0 -> mask_to_reg_list(Mask, Rest, RegBitFn);
        _ -> [Reg | mask_to_reg_list(Mask, Rest, RegBitFn)]
    end.
