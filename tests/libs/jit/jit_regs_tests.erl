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

-module(jit_regs_tests).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
    Regs = jit_regs:new(),
    ?assertEqual(unknown, jit_regs:get_contents(Regs, rax)),
    ?assertEqual(none, jit_regs:find_reg_with_contents(Regs, {x_reg, 0})),
    ?assertEqual([], jit_regs:stack_contents(Regs)).

set_and_get_contents_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
    ?assertEqual({x_reg, 0}, jit_regs:get_contents(Regs1, rax)),
    ?assertEqual(unknown, jit_regs:get_contents(Regs1, r11)),
    Regs2 = jit_regs:set_contents(Regs1, r11, {imm, 42}),
    ?assertEqual({x_reg, 0}, jit_regs:get_contents(Regs2, rax)),
    ?assertEqual({imm, 42}, jit_regs:get_contents(Regs2, r11)).

find_reg_with_contents_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
    Regs2 = jit_regs:set_contents(Regs1, r11, {y_reg, 1}),
    ?assertEqual({ok, rax}, jit_regs:find_reg_with_contents(Regs2, {x_reg, 0})),
    ?assertEqual({ok, r11}, jit_regs:find_reg_with_contents(Regs2, {y_reg, 1})),
    ?assertEqual(none, jit_regs:find_reg_with_contents(Regs2, {x_reg, 5})).

invalidate_reg_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
    Regs2 = jit_regs:set_contents(Regs1, r11, {y_reg, 1}),
    Regs3 = jit_regs:invalidate_reg(Regs2, rax),
    ?assertEqual(unknown, jit_regs:get_contents(Regs3, rax)),
    ?assertEqual({y_reg, 1}, jit_regs:get_contents(Regs3, r11)),
    ?assertEqual(none, jit_regs:find_reg_with_contents(Regs3, {x_reg, 0})).

invalidate_all_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
    Regs2 = jit_regs:set_contents(Regs1, r11, {y_reg, 1}),
    Regs3 = jit_regs:invalidate_all(Regs2),
    ?assertEqual(unknown, jit_regs:get_contents(Regs3, rax)),
    ?assertEqual(unknown, jit_regs:get_contents(Regs3, r11)).

invalidate_vm_loc_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
    Regs2 = jit_regs:set_contents(Regs1, r11, {x_reg, 0}),
    Regs3 = jit_regs:set_contents(Regs2, r10, {y_reg, 1}),
    %% Invalidate all registers tracking x_reg 0
    Regs4 = jit_regs:invalidate_vm_loc(Regs3, {x_reg, 0}),
    ?assertEqual(unknown, jit_regs:get_contents(Regs4, rax)),
    ?assertEqual(unknown, jit_regs:get_contents(Regs4, r11)),
    %% y_reg 1 should be unaffected
    ?assertEqual({y_reg, 1}, jit_regs:get_contents(Regs4, r10)).

merge_test() ->
    Regs0 = jit_regs:new(),
    %% Path A: rax = x_reg 0, r11 = x_reg 1
    RegsA = jit_regs:set_contents(
        jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
        r11,
        {x_reg, 1}
    ),
    %% Path B: rax = x_reg 0, r11 = x_reg 2
    RegsB = jit_regs:set_contents(
        jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
        r11,
        {x_reg, 2}
    ),
    Merged = jit_regs:merge(RegsA, RegsB),
    %% rax = x_reg 0 in both paths: kept
    ?assertEqual({x_reg, 0}, jit_regs:get_contents(Merged, rax)),
    %% r11 differs: invalidated
    ?assertEqual(unknown, jit_regs:get_contents(Merged, r11)).

merge_with_unreachable_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
    Regs2 = jit_regs:set_contents(Regs1, r11, {imm, 42}),
    Unreachable = jit_regs:unreachable(Regs0),
    ?assertEqual({x_reg, 0}, jit_regs:get_contents(jit_regs:merge(Regs2, Unreachable), rax)),
    ?assertEqual({imm, 42}, jit_regs:get_contents(jit_regs:merge(Regs2, Unreachable), r11)),
    ?assertEqual({x_reg, 0}, jit_regs:get_contents(jit_regs:merge(Unreachable, Regs2), rax)),
    ?assertEqual({imm, 42}, jit_regs:get_contents(jit_regs:merge(Unreachable, Regs2), r11)),
    ?assertEqual(
        unknown,
        jit_regs:get_contents(jit_regs:merge(Unreachable, jit_regs:unreachable(Regs0)), rax)
    ).

stack_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:stack_push(Regs0, rdi),
    Regs2 = jit_regs:stack_push(Regs1, rsi),
    ?assertEqual([rsi, rdi], jit_regs:stack_contents(Regs2)),
    {rsi, Regs3} = jit_regs:stack_pop(Regs2),
    {rdi, Regs4} = jit_regs:stack_pop(Regs3),
    ?assertEqual([], jit_regs:stack_contents(Regs4)),
    Regs5 = jit_regs:stack_clear(Regs2),
    ?assertEqual([], jit_regs:stack_contents(Regs5)).

invalidate_volatile_test() ->
    Regs0 = jit_regs:new(),
    Regs1 = jit_regs:set_contents(Regs0, rax, {x_reg, 0}),
    Regs2 = jit_regs:set_contents(Regs1, rdi, cp),
    Regs3 = jit_regs:set_contents(Regs2, r11, {imm, 42}),
    %% Only rdi is preserved
    Regs4 = jit_regs:invalidate_volatile(Regs3, [rdi]),
    ?assertEqual(unknown, jit_regs:get_contents(Regs4, rax)),
    ?assertEqual(cp, jit_regs:get_contents(Regs4, rdi)),
    ?assertEqual(unknown, jit_regs:get_contents(Regs4, r11)).

value_to_contents_test() ->
    MaxReg = 16,
    ?assertEqual(cp, jit_regs:value_to_contents(cp, MaxReg)),
    ?assertEqual({x_reg, 0}, jit_regs:value_to_contents({x_reg, 0}, MaxReg)),
    ?assertEqual({x_reg, 5}, jit_regs:value_to_contents({x_reg, 5}, MaxReg)),
    ?assertEqual({x_reg, MaxReg}, jit_regs:value_to_contents({x_reg, extra}, MaxReg)),
    ?assertEqual({y_reg, 3}, jit_regs:value_to_contents({y_reg, 3}, MaxReg)),
    ?assertEqual({imm, 42}, jit_regs:value_to_contents(42, MaxReg)),
    ?assertEqual({imm, 0}, jit_regs:value_to_contents(0, MaxReg)),
    ?assertEqual(unknown, jit_regs:value_to_contents({ptr, rax}, MaxReg)),
    ?assertEqual(unknown, jit_regs:value_to_contents({fp_reg, 0}, MaxReg)).

vm_dest_to_contents_test() ->
    MaxReg = 16,
    ?assertEqual({x_reg, 0}, jit_regs:vm_dest_to_contents({x_reg, 0}, MaxReg)),
    ?assertEqual({x_reg, 15}, jit_regs:vm_dest_to_contents({x_reg, 15}, MaxReg)),
    ?assertEqual({x_reg, MaxReg}, jit_regs:vm_dest_to_contents({x_reg, extra}, MaxReg)),
    ?assertEqual({y_reg, 5}, jit_regs:vm_dest_to_contents({y_reg, 5}, MaxReg)),
    ?assertEqual(unknown, jit_regs:vm_dest_to_contents({fp_reg, 0}, MaxReg)),
    ?assertEqual(unknown, jit_regs:vm_dest_to_contents({ptr, rax}, MaxReg)).
