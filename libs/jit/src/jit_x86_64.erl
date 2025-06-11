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
    free_native_register/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/2,
    call_primitive/3,
    call_primitive_last/3,
    handle_error_if_false/2,
    handle_error_if_zero/2,
    return_if_not_null/2,
    jump_to_label/2,
    jump_to_label_if_zero/3,
    jump_to_label_if_and_non_zero_b/4,
    jump_to_label_if_and_not_equal/5,
    jump_to_label_if_equal/4,
    jump_to_label_if_not_equal/4,
    jump_to_offset_if_equal/3,
    jump_to_offset_if_and_equal/4,
    shift_right/3,
    shift_left/3,
    move_to_vm_register/3,
    move_to_native_register/2,
    move_to_cp/2,
    move_array_element/4,
    move_to_array_element/4,
    get_pointer_to_vm_register/2,
    get_array_element/3,
    save_to_native_register/2,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    get_continuation_address/1,
    get_module_index/1,
    and_/3,
    or_/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_ext_or_schedule_next/3,
    call_ext_onlylast_or_schedule_next/4,
    call_func_ptr/3
]).

-include_lib("jit.hrl").

-include("primitives.hrl").

%% System V X86_64 calling conventions which we apply here.
%% (Integer) parameters : rdi, rsi, rdx, rcx, r8, r9
%% (Integer) result : rax
%%
%% Function is called as (Context *, JITState *, ModuleNativeInterface *) so:
%% Context * is rdi
%% JITState * is rsi
%% ModuleNativeInterface * is rdx
%%
%% rax, r10 and r11 can be used as scratch registers.
%% rdi / rsi / rdx are pushed to stack before calling a primitive and popped back.
%% when returning (some push call pop push call pop sequences could be optimized)

-type x86_64_register() :: rax | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11.

-record(state, {
    stream_module :: module(),
    stream :: any(),
    offset :: non_neg_integer(),
    branches :: [],
    available_regs :: [x86_64_register()],
    used_regs :: [x86_64_register()]
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, x86_64_register()}.
-type value() :: immediate() | vm_register() | x86_64_register().
-type arg() :: ctx | jit_state | value() | {free, value()}.

% ctx->e is 0x28
% ctx->x is 0x30
-define(Y_REGS, {16#28, rdi}).
-define(X_REG(N), {16#30 + (N * 8), rdi}).
-define(CP, {16#B8, rdi}).
-define(PRIMITIVE(N), {N * 8, rdx}).
-define(JITSTATE_MODULE, {0, rsi}).
-define(JITSTATE_CONTINUATION, {16#8, rsi}).
-define(JITSTATE_REDUCTIONCOUNT, {16#10, rsi}).
-define(MODULE_INDEX(ModuleReg), {0, ModuleReg}).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-define(AVAILABLE_REGS, [rax, r11, r10, r9, r8, rcx]).
-define(PARAMETER_REGS, [rdi, rsi, rdx, rcx, r8, r9]).

-spec word_size() -> 4 | 8.
word_size() -> 8.

-spec new(any(), module(), any()) -> state().
new(_Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS,
        used_regs = []
    }.

-spec stream(state()) -> any().
stream(#state{stream = Stream}) ->
    Stream.

-spec offset(state()) -> non_neg_integer().
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:append(Stream0, <<16#CC>>),
    State#state{stream = Stream1}.

-spec free_native_register(state(), value()) -> state().
free_native_register(#state{available_regs = Available0, used_regs = Used0} = State, Reg) when
    is_atom(Reg)
->
    {AvailableRegs1, Used1} = free_reg(Available0, Used0, Reg),
    State#state{available_regs = AvailableRegs1, used_regs = Used1};
free_native_register(State, {ptr, Reg}) ->
    free_native_register(State, Reg);
free_native_register(State, _Other) ->
    State.

assert_all_native_free(#state{available_regs = ?AVAILABLE_REGS, used_regs = []}) -> ok.

jump_table(State, LabelsCount) ->
    jump_table0(State, 0, LabelsCount).

jump_table0(State, N, LabelsCount) when N >= LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State,
    N,
    LabelsCount
) ->
    Offset = StreamModule:offset(Stream0),
    {RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(-4),
    Reloc = {N, Offset + RelocOffset, 32},
    Stream1 = StreamModule:append(Stream0, I1),
    jump_table0(State#state{stream = Stream1, branches = [Reloc | Branches]}, N + 1, LabelsCount).

update_branches(#state{branches = []} = State, _Labels) ->
    State;
update_branches(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = [{Label, Offset, Size} | BranchesT]
    } = State,
    Labels
) ->
    {Label, LabelOffset} = lists:keyfind(Label, 1, Labels),
    Stream1 = StreamModule:map(Stream0, Offset, Size div 8, fun(<<Delta:Size/signed-little>>) ->
        <<(Delta + LabelOffset - Offset):Size/little>>
    end),
    update_branches(State#state{stream = Stream1, branches = BranchesT}, Labels).

-spec call_primitive(state(), non_neg_integer(), [any()]) -> {state(), x86_64_register()}.
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
    [Temp | _] = AvailableRegs0 -- ParamRegs,
    AvailableRegs1 = AvailableRegs0 -- [Temp],
    PrepCall =
        case Primitive of
            0 ->
                jit_x86_64_asm:movq({0, rdx}, Temp);
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
    ).

call_primitive_last(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State0,
    Primitive,
    Args
) ->
    % We need a register for the function pointer that should not be used as a parameter
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    [Temp | _] = AvailableRegs0 -- ParamRegs,
    AvailableRegs1 = AvailableRegs0 -- [Temp],
    PrepCall =
        case Primitive of
            0 ->
                jit_x86_64_asm:movq({0, rdx}, Temp);
            N ->
                jit_x86_64_asm:movq(?PRIMITIVE(N), Temp)
        end,
    Stream1 = StreamModule:append(Stream0, PrepCall),
    State1 = set_args(
        State0#state{
            stream = Stream1, available_regs = AvailableRegs1, used_regs = [Temp | UsedRegs0]
        },
        Args
    ),
    #state{stream = Stream2} = State1,
    Call = jit_x86_64_asm:jmpq({Temp}),
    Stream3 = StreamModule:append(Stream2, Call),
    State1#state{stream = Stream3, available_regs = ?AVAILABLE_REGS, used_regs = []}.

handle_error_if_false(#state{stream_module = StreamModule, stream = Stream0} = State, Reg) ->
    I1 = jit_x86_64_asm:testb(Reg, Reg),
    I3 = jit_x86_64_asm:movq(?PRIMITIVE(?PRIM_HANDLE_ERROR), Reg),
    I4 = jit_x86_64_asm:jmpq({Reg}),
    I2 = jit_x86_64_asm:jnz(byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    State#state{stream = Stream1}.

handle_error_if_zero(#state{stream_module = StreamModule, stream = Stream0} = State, Reg) ->
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    I3 = jit_x86_64_asm:movq(?PRIMITIVE(?PRIM_HANDLE_ERROR), Reg),
    I4 = jit_x86_64_asm:jmpq({Reg}),
    I2 = jit_x86_64_asm:jnz(byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    State#state{stream = Stream1}.

return_if_not_null(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    {free, Reg}
) ->
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    I3 =
        case Reg of
            rax -> <<>>;
            _ -> jit_x86_64_asm:movq(Reg, rax)
        end,
    I4 = jit_x86_64_asm:retq(),
    I2 = jit_x86_64_asm:jz(byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, Reg),
    State#state{stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs1}.

jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State, Label
) ->
    Offset = StreamModule:offset(Stream0),
    {RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(-4),
    Reloc = {Label, Offset + RelocOffset, 32},
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

jump_to_label_if_zero(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State,
    Reg,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocOffset, I3} = jit_x86_64_asm:jmp_rel32(-4),
    I2 = jit_x86_64_asm:jnz(byte_size(I3)),
    Sz = byte_size(I1) + byte_size(I2),
    Reloc = {Label, Offset + Sz + RelocOffset, 32},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

jump_to_label_if_and_non_zero_b(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State,
    Reg,
    Val,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:testb(Val, Reg),
    {RelocOffset, I3} = jit_x86_64_asm:jmp_rel32(-4),
    I2 = jit_x86_64_asm:jz(byte_size(I3)),
    Sz = byte_size(I1) + byte_size(I2),
    Reloc = {Label, Offset + Sz + RelocOffset, 32},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

jump_to_label_if_and_not_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = AccBranches,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    {free, Reg},
    Mask,
    Val,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:andq(Mask, Reg),
    I2 = jit_x86_64_asm:cmpl(Val, Reg),
    {RelocOffset, I4} = jit_x86_64_asm:jmp_rel32(-4),
    I3 = jit_x86_64_asm:jz(byte_size(I4)),
    Sz = byte_size(I1) + byte_size(I2) + byte_size(I3),
    Reloc = {Label, Offset + Sz + RelocOffset, 32},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary,
        I4/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {AvailableRegs1, UsedRegs1} = free_reg(AvailableRegs0, UsedRegs0, Reg),
    State#state{
        stream = Stream1,
        branches = [Reloc | AccBranches],
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1
    };
jump_to_label_if_and_not_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = AccBranches,
        available_regs = [Temp | _]
    } = State,
    Reg,
    Mask,
    Val,
    Label
) when is_atom(Reg) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:movq(Reg, Temp),
    I2 = jit_x86_64_asm:andq(Mask, Temp),
    I3 = jit_x86_64_asm:cmpl(Val, Temp),
    {RelocOffset, I5} = jit_x86_64_asm:jmp_rel32(-4),
    I4 = jit_x86_64_asm:jz(byte_size(I5)),
    Sz = byte_size(I1) + byte_size(I2) + byte_size(I3) + byte_size(I4),
    Reloc = {Label, Offset + Sz + RelocOffset, 32},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary,
        I4/binary,
        I5/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

jump_to_label_if_equal(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State,
    Reg,
    Val,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocOffset, I3} = jit_x86_64_asm:jmp_rel32(-4),
    I2 = jit_x86_64_asm:jnz(byte_size(I3)),
    Sz = byte_size(I1) + byte_size(I2),
    Reloc = {Label, Offset + Sz + RelocOffset, 32},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

jump_to_label_if_not_equal(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State,
    Reg,
    Val,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocOffset, I3} = jit_x86_64_asm:jmp_rel32(-4),
    I2 = jit_x86_64_asm:jz(byte_size(I3)),
    Sz = byte_size(I1) + byte_size(I2),
    Reloc = {Label, Offset + Sz + RelocOffset, 32},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

jump_to_offset_if_equal(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State,
    Reg,
    Val
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jz_rel8(-1),
    OffsetRef = make_ref(),
    Reloc = {OffsetRef, Offset + byte_size(I1) + RelocJNZOffset, 8},
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | AccBranches]}, OffsetRef}.

jump_to_offset_if_and_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = AccBranches,
        available_regs = [Temp | _]
    } = State,
    Reg,
    Mask,
    Val
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:movq(Reg, Temp),
    I2 = jit_x86_64_asm:andq(Mask, Temp),
    I3 = jit_x86_64_asm:cmpq(Val, Temp),
    {RelocJNZOffset, I4} = jit_x86_64_asm:jz_rel8(-1),
    OffsetRef = make_ref(),
    Reloc = {OffsetRef, Offset + byte_size(I1) + byte_size(I2) + byte_size(I3) + RelocJNZOffset, 8},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary,
        I4/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | AccBranches]}, OffsetRef}.

shift_right(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    is_atom(Reg)
->
    I = jit_x86_64_asm:shrq(Shift, Reg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

shift_left(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    is_atom(Reg)
->
    I = jit_x86_64_asm:shlq(Shift, Reg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

-spec call_func_ptr(state(), x86_64_register(), [arg()]) -> {state(), x86_64_register()}.
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
            ({free, Reg}) -> [Reg];
            (_) -> []
        end,
        [{free, FuncPtrReg} | Args]
    ),
    UsedRegs1 = UsedRegs0 -- FreeRegs,
    SavedRegs = [rdi, rsi, rdx | UsedRegs1],
    Stream1 = lists:foldl(
        fun(Reg, AccStream) ->
            StreamModule:append(AccStream, jit_x86_64_asm:pushq(Reg))
        end,
        Stream0,
        SavedRegs
    ),
    State1 = set_args(State0#state{stream = Stream1}, Args),
    #state{stream = Stream2} = State1,
    Call = jit_x86_64_asm:callq({FuncPtrReg}),
    Stream3 = StreamModule:append(Stream2, Call),
    % If rax is in used regs, save it to another temporary register
    {Stream4, ResultReg} =
        case lists:member(rax, SavedRegs) of
            true ->
                [Temp | _] = AvailableRegs0,
                {StreamModule:append(Stream3, jit_x86_64_asm:movq(rax, Temp)), Temp};
            false ->
                {Stream3, rax}
        end,
    Stream5 = lists:foldl(
        fun(Reg, AccStream) ->
            StreamModule:append(AccStream, jit_x86_64_asm:popq(Reg))
        end,
        Stream4,
        lists:reverse(SavedRegs)
    ),
    AvailableRegs1 = FreeRegs ++ AvailableRegs0,
    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    UsedRegs2 = [ResultReg | UsedRegs1],
    {
        State1#state{stream = Stream5, available_regs = AvailableRegs3, used_regs = UsedRegs2},
        ResultReg
    }.

-spec set_args(state(), [arg()]) -> state().
set_args(
    #state{stream = Stream0, stream_module = StreamModule, used_regs = UsedRegs} = State0, Args
) ->
    % Handle overlaps if any
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    ArgsRegs = lists:flatmap(
        fun
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) -> [Reg];
            (Reg) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        Args
    ),
    OverlapRegs = ArgsRegs -- (ArgsRegs -- ParamRegs),
    SetArgsCode =
        case OverlapRegs of
            [] ->
                set_args0(Args, ParamRegs, []);
            _ ->
                AvailableScratch = [rdi, rsi, rdx, rcx, r8, r9, r10, r11] -- ParamRegs -- ArgsRegs,
                {UpdatedArgs, _, SwapCode} = lists:foldl(
                    fun(OverlapReg, {AccArgs, [Avail | AccAvailT], AccSwapCode}) ->
                        I = jit_x86_64_asm:movq(OverlapReg, Avail),
                        NewArgs = replace_reg(AccArgs, OverlapReg, Avail),
                        {NewArgs, AccAvailT, [I | AccSwapCode]}
                    end,
                    {Args, AvailableScratch, <<>>},
                    OverlapRegs
                ),
                set_args0(UpdatedArgs, ParamRegs, SwapCode)
        end,
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

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

set_args0([], _, Acc) ->
    list_to_binary(lists:reverse(Acc));
set_args0([{free, FreeVal} | ArgsT], Regs, Acc) ->
    set_args0([FreeVal | ArgsT], Regs, Acc);
set_args0([ctx | ArgsT], [rdi | Regs], Acc) ->
    set_args0(ArgsT, Regs, Acc);
set_args0([jit_state | ArgsT], [rsi | Regs], Acc) ->
    set_args0(ArgsT, Regs, Acc);
set_args0([jit_state | ArgsT], [rdi | Regs], Acc) ->
    set_args0(ArgsT, Regs, [jit_x86_64_asm:movq(rsi, rdi) | Acc]);
set_args0([{x_reg, X} | ArgsT], [Reg | RegsT], Acc) ->
    set_args0(ArgsT, RegsT, [jit_x86_64_asm:movq({16#30 + 8 * X, rdi}, Reg) | Acc]);
set_args0([{ptr, Source} | ArgsT], [Reg | RegsT], Acc) ->
    set_args0(ArgsT, RegsT, [jit_x86_64_asm:movq({0, Source}, Reg) | Acc]);
set_args0([{y_reg, X} | ArgsT], [Reg | RegsT], Acc) ->
    set_args0(ArgsT, RegsT, [
        jit_x86_64_asm:movq({X * 8, Reg}, Reg), jit_x86_64_asm:movq(?Y_REGS, Reg) | Acc
    ]);
set_args0([ArgReg | ArgsT], [Reg | RegsT], Acc) when is_atom(ArgReg) ->
    set_args0(ArgsT, RegsT, [jit_x86_64_asm:movq(ArgReg, Reg) | Acc]);
set_args0([Arg | ArgsT], [Reg | RegsT], Acc) when
    is_integer(Arg) andalso Arg >= -16#80000000 andalso Arg < 16#80000000
->
    set_args0(ArgsT, RegsT, [jit_x86_64_asm:movq(Arg, Reg) | Acc]);
set_args0([Arg | ArgsT], [Reg | RegsT], Acc) when is_integer(Arg) ->
    set_args0(ArgsT, RegsT, [jit_x86_64_asm:movabsq(Arg, Reg) | Acc]).

move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, 0, {x_reg, X}
) when
    X < ?MAX_REG
->
    I1 = jit_x86_64_asm:andq(0, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_vm_register(#state{stream_module = StreamModule, stream = Stream0} = State, 0, {ptr, Reg}) ->
    I1 = jit_x86_64_asm:andq(0, {0, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, N, {x_reg, X}
) when
    X < ?MAX_REG andalso ?IS_SINT32_T(N)
->
    Stream1 = StreamModule:append(Stream0, jit_x86_64_asm:movq(N, ?X_REG(X))),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, N, {ptr, Reg}
) when
    ?IS_SINT32_T(N)
->
    Stream1 = StreamModule:append(Stream0, jit_x86_64_asm:movq(N, {0, Reg})),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    N,
    {x_reg, X}
) when
    X < ?MAX_REG andalso is_integer(N)
->
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    N,
    {ptr, Reg}
) when
    is_integer(N)
->
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {0, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, 0, {y_reg, X}
) when
    X < 32
->
    Code = <<
        % movq	    0x28(%rdi), %rax
        16#48,
        16#8b,
        16#47,
        16#28,
        % andq	    $0x0, N(%rax)
        16#48,
        16#83,
        16#60,
        (X * 8),
        16#00
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    N,
    {y_reg, Y}
) when ?IS_SINT32_T(N) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq(N, {Y * 8, Temp}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, N, {y_reg, X}
) when
    X < 32 andalso is_integer(N)
->
    Code = <<
        % movq	    0x28(%rdi), %rax
        16#48,
        16#8b,
        16#47,
        16#28,
        % movabsq	$N, %rcx
        16#48,
        16#b9,
        N:64/little,
        % movq      %rcx, M(%rax)
        16#48,
        16#89,
        16#48,
        (X * 8)
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    {x_reg, Y}
) when X < ?MAX_REG andalso Y < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(Y)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    {ptr, Reg}
) when X < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {0, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | _]} =
        State,
    {x_reg, X},
    {y_reg, Y}
) when X < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp1),
    I2 = jit_x86_64_asm:movq(?Y_REGS, Temp2),
    I3 = jit_x86_64_asm:movq(Temp1, {Y * 8, Temp2}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {y_reg, Y},
    {x_reg, X}
) when X < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {y_reg, Y},
    {ptr, Reg}
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {0, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | _]} =
        State,
    {y_reg, YS},
    {y_reg, YD}
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movq({YS * 8, Temp1}, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {YD * 8, Temp1}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, {x_reg, X}
) when is_atom(Reg) andalso X < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq(Reg, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, {ptr, Dest}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(Reg, {0, Dest}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, available_regs = [Temp | _], stream = Stream0} = State,
    Reg,
    {y_reg, Y}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq(Reg, {Y * 8, Temp}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

get_pointer_to_vm_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | AvailableT],
        used_regs = UsedRegs0
    } = State,
    {x_reg, X}
) when
    X < ?MAX_REG
->
    I1 = jit_x86_64_asm:leaq(?X_REG(X), Temp),
    Stream1 = StreamModule:append(Stream0, I1),
    {
        State#state{stream = Stream1, available_regs = AvailableT, used_regs = [Temp | UsedRegs0]},
        Temp
    };
get_pointer_to_vm_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | AvailableT],
        used_regs = UsedRegs0
    } = State,
    {y_reg, 0}
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    Stream1 = StreamModule:append(Stream0, I1),
    {
        State#state{stream = Stream1, available_regs = AvailableT, used_regs = [Temp | UsedRegs0]},
        Temp
    };
get_pointer_to_vm_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | AvailableT],
        used_regs = UsedRegs0
    } = State,
    {y_reg, X}
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:leaq({X * 8, Temp}, Temp),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    {
        State#state{stream = Stream1, available_regs = AvailableT, used_regs = [Temp | UsedRegs0]},
        Temp
    };
get_pointer_to_vm_register(State, {ptr, Reg}) ->
    {State, Reg}.

%% @doc move reg[x] to a vm or native register
-spec move_array_element(
    state(), x86_64_register(), non_neg_integer(), vm_register() | x86_64_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {ptr, Dest}
) ->
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
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Y * 8, Temp1}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Dest),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

%% @doc move reg[x] to a vm or native register
-spec get_array_element(state(), x86_64_register(), non_neg_integer()) ->
    {state(), x86_64_register()}.
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

%% @doc move an integer, a vm or native register to reg[x]
-spec move_to_array_element(
    state(), integer() | vm_register() | x86_64_register(), x86_64_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    Reg,
    Index
) when X < ?MAX_REG ->
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
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | _]} =
        State,
    {y_reg, Y},
    Reg,
    Index
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp1}, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Index * 8, Reg}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when is_atom(Source) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when is_integer(Source) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

-spec move_to_native_register(state(), value()) -> {state(), x86_64_register()}.
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
    {y_reg, Y}
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Reg),
    I2 = jit_x86_64_asm:movq({Y * 8, Reg}, Reg),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [Reg | Used]}, Reg};
move_to_native_register(State, Reg) when is_atom(Reg) ->
    {State, Reg}.

-spec save_to_native_register(state(), x86_64_register()) -> {state(), x86_64_register()}.
save_to_native_register(
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
save_to_native_register(
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
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg}.

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

increment_sp(#state{stream_module = StreamModule, stream = Stream0} = State, Offset) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, rax),
    I2 = jit_x86_64_asm:addq(Offset * 8, rax),
    I3 = jit_x86_64_asm:movq(rax, ?Y_REGS),
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

get_continuation_address(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailableT],
        used_regs = UsedRegs0,
        branches = Branches
    } = State
) ->
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    {RewriteLEAOffset, I1} = jit_x86_64_asm:leaq_rel32({-4, rip}, Reg),
    Reloc = {OffsetRef, Offset + RewriteLEAOffset, 32},
    Stream1 = StreamModule:append(Stream0, I1),
    {
        State#state{
            stream = Stream1,
            branches = [Reloc | Branches],
            available_regs = AvailableT,
            used_regs = [Reg | UsedRegs0]
        },
        Reg,
        OffsetRef
    }.

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

and_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Imm) ->
    I1 = jit_x86_64_asm:andq(Imm, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

or_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Imm) ->
    I1 = jit_x86_64_asm:orq(Imm, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:decl(?JITSTATE_REDUCTIONCOUNT),
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
    State2#state{stream = Stream4}.

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {RewriteOffset, State1} = set_cp(State0),
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
    I1 = jit_x86_64_asm:decl(?JITSTATE_REDUCTIONCOUNT),
    {RewriteJMPOffset, I3} = jit_x86_64_asm:jmp_rel32(-4),
    I2 = jit_x86_64_asm:jz(byte_size(I3)),
    Sz = byte_size(I1) + byte_size(I2),
    Reloc1 = {Label, Offset + Sz + RewriteJMPOffset, 32},
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1, branches = [Reloc1 | Branches]},
    State2 = set_continuation_to_label(State1, Label),
    call_primitive_last(State2, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

call_ext_or_schedule_next(State0, Arity, Index) ->
    {RewriteOffset, State1} = set_cp(State0),
    State2 = call_ext_onlylast_or_schedule_next(State1, Arity, Index, -1),
    rewrite_cp_offset(State2, RewriteOffset).

call_ext_onlylast_or_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0,
    Arity,
    Index,
    NWords
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:decl(?JITSTATE_REDUCTIONCOUNT),
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
    call_primitive_last(State2#state{stream = Stream4}, ?PRIM_CALL_EXT, [
        ctx, jit_state, Arity, Index, NWords
    ]).

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
    I3 = jit_x86_64_asm:movq(Reg, {16#B8, rdi}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {AddrOffset, State3}.

-spec rewrite_cp_offset(state(), non_neg_integer()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    % Encode ReturnAddrOffset << 2
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, <<(NewOffset bsl 2):32/little>>),
    State0#state{stream = Stream1}.

free_reg(AvailableRegs0, UsedRegs0, Reg) ->
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
