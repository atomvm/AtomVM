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
    offset/2,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/2,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    return_if_not_null/2,
    return_if_not_equal/3,
    jump_to_label/2,
    jump_to_offset/1,
    if_block/3,
    if_else_block/4,
    jump_to_offset_if_equal/3,
    jump_to_offset_if_and_equal/4,
    jump_to_offset_if_and_not_equal/4,
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
    get_continuation_address/1,
    get_module_index/1,
    and_/3,
    or_/3,
    add/3,
    sub/3,
    mul/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3
]).

-include_lib("jit.hrl").

-include("primitives.hrl").

-define(ASSERT(Expr), true = Expr).

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

-type x86_64_register() ::
    rax
    | rcx
    | rdx
    | rsi
    | rdi
    | r8
    | r9
    | r10
    | r11
    | xmm0
    | xmm1
    | xmm2
    | xmm3
    | xmm4
    | xmm5
    | xmm6
    | xmm7.

-define(IS_GPR(Reg),
    (Reg =:= rax orelse Reg =:= rcx orelse Reg =:= rdx orelse Reg =:= rsi orelse Reg =:= r8 orelse
        Reg =:= r9 orelse Reg =:= r10 orelse Reg =:= r11)
).
-define(IS_FPR(Reg),
    (Reg =:= xmm0 orelse Reg =:= xmm1 orelse Reg =:= xmm2 orelse Reg =:= xmm3 orelse Reg =:= xmm4 orelse
        Reg =:= xmm5 orelse Reg =:= xmm6 orelse Reg =:= xmm7)
).

-record(state, {
    stream_module :: module(),
    stream :: any(),
    offset :: non_neg_integer(),
    branches :: [],
    available_regs :: [x86_64_register()],
    available_fpregs :: [x86_64_register()],
    used_regs :: [x86_64_register()]
}).

-record(jump_token, {
    used_regs :: [x86_64_register()]
}).

-type jump_token() :: #jump_token{}.

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, x86_64_register()}.
-type value() :: immediate() | vm_register() | x86_64_register().
-type arg() :: ctx | jit_state | value() | {free, value()}.

-type maybe_free_x86_64_register() :: x86_64_register() | {free, x86_64_register()}.

-type condition() ::
    {x86_64_register(), '<', 0}
    | {maybe_free_x86_64_register(), '==', 0}
    | {maybe_free_x86_64_register(), '!=', integer()}
    | {'(uint8_t)', maybe_free_x86_64_register(), '==', false}
    | {'(uint8_t)', maybe_free_x86_64_register(), '!=', false}
    | {maybe_free_x86_64_register(), '&', non_neg_integer(), '!=', 0}.

% ctx->e is 0x28
% ctx->x is 0x30
-define(CTX_REG, rdi).
-define(JITSTATE_REG, rsi).
-define(NATIVE_INTERFACE_REG, rdx).
-define(Y_REGS, {16#28, ?CTX_REG}).
-define(X_REG(N), {16#30 + (N * 8), ?CTX_REG}).
-define(CP, {16#B8, ?CTX_REG}).
-define(FP_REGS, {16#C0, ?CTX_REG}).
-define(BS, {16#C8, ?CTX_REG}).
-define(BS_OFFSET, {16#D0, ?CTX_REG}).
-define(JITSTATE_MODULE, {0, ?JITSTATE_REG}).
-define(JITSTATE_CONTINUATION, {16#8, ?JITSTATE_REG}).
-define(JITSTATE_REDUCTIONCOUNT, {16#10, ?JITSTATE_REG}).
-define(PRIMITIVE(N), {N * 8, ?NATIVE_INTERFACE_REG}).
-define(MODULE_INDEX(ModuleReg), {0, ModuleReg}).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-define(AVAILABLE_REGS, [rax, r11, r10, r9, r8, rcx]).
-define(AVAILABLE_FPREGS, [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7]).
-define(PARAMETER_REGS, [rdi, rsi, rdx, rcx, r8, r9]).
-define(PARAMETER_FPREGS, ?AVAILABLE_FPREGS).

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
        available_fpregs = ?AVAILABLE_FPREGS,
        used_regs = []
    }.

-spec stream(state()) -> any().
stream(#state{stream = Stream}) ->
    Stream.

-spec offset(state()) -> non_neg_integer().
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

-spec offset(state(), [jump_token()]) -> {state(), non_neg_integer()}.
offset(
    #state{
        available_regs = AvailableRegs0, available_fpregs = AvailableFPRegs0, used_regs = UsedRegs0
    } = State0,
    JumpTokens
) ->
    Offset = offset(State0),
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = lists:foldl(
        fun(#jump_token{used_regs = JTUsedRegs}, {AccAvail, AccAvailFP, AccUsed}) ->
            lists:foldl(
                fun(UsedReg, {AccAvailIn, AccAvailFPIn, AccUsedIn}) ->
                    case lists:member(UsedReg, AccAvailIn) of
                        true ->
                            {lists:delete(UsedReg, AccAvailIn), AccAvailFPIn, [UsedReg | AccUsedIn]};
                        false ->
                            case lists:member(UsedReg, AccAvailFPIn) of
                                true ->
                                    {AccAvailIn, lists:delete(UsedReg, AccAvailFPIn), [
                                        UsedReg | AccUsedIn
                                    ]};
                                false ->
                                    {AccAvailIn, AccAvailFPIn, AccUsedIn}
                            end
                    end
                end,
                {AccAvail, AccAvailFP, AccUsed},
                JTUsedRegs
            )
        end,
        {AvailableRegs0, AvailableFPRegs0, UsedRegs0},
        JumpTokens
    ),
    {
        State0#state{
            available_regs = AvailableRegs1,
            available_fpregs = AvailableFPRegs1,
            used_regs = UsedRegs1
        },
        Offset
    }.

debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:append(Stream0, <<16#CC>>),
    State#state{stream = Stream1}.

-spec used_regs(state()) -> [x86_64_register()].
used_regs(#state{used_regs = Used}) -> Used.

-spec available_regs(state()) -> [x86_64_register()].
available_regs(#state{available_regs = Available}) -> Available.

-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Reg | Rest]) ->
    State1 = free_native_register(State, Reg),
    free_native_registers(State1, Rest).

-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_regs = Available0, available_fpregs = AvailableFP0, used_regs = Used0} = State,
    Reg
) when
    is_atom(Reg)
->
    {Available1, AvailableFP1, Used1} = free_reg(Available0, AvailableFP0, Used0, Reg),
    State#state{available_regs = Available1, available_fpregs = AvailableFP1, used_regs = Used1};
free_native_register(State, {ptr, Reg}) ->
    free_native_register(State, Reg);
free_native_register(State, _Other) ->
    State.

assert_all_native_free(#state{
    available_regs = ?AVAILABLE_REGS, available_fpregs = ?AVAILABLE_FPREGS, used_regs = []
}) ->
    ok.

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
    case AvailableRegs0 -- ParamRegs of
        [Temp | _] ->
            AvailableRegs1 = AvailableRegs0 -- [Temp],
            PrepCall =
                case Primitive of
                    0 ->
                        jit_x86_64_asm:movq({0, ?NATIVE_INTERFACE_REG}, Temp);
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
            );
        [] when length(Args) >= 3 ->
            % No register left, we'll use the stack to save NATIVE_INTERFACE_REG
            % and rax when calling function.
            call_func_ptr(State, {primitive, Primitive}, Args)
    end.

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
                jit_x86_64_asm:movq({0, ?NATIVE_INTERFACE_REG}, Temp);
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

return_if_not_null(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        available_fpregs = AvailableFPRegs0,
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
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, AvailableFPRegs0, UsedRegs0, Reg
    ),
    State#state{
        stream = Stream1,
        available_regs = AvailableRegs1,
        available_fpregs = AvailableFPRegs1,
        used_regs = UsedRegs1
    }.

return_if_not_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        available_fpregs = AvailableFPRegs0,
        used_regs = UsedRegs0
    } = State,
    ctx,
    {free, Reg}
) ->
    I1 = jit_x86_64_asm:cmpq(?CTX_REG, Reg),
    I3 =
        case Reg of
            rax -> <<>>;
            _ -> jit_x86_64_asm:movq(Reg, rax)
        end,
    I4 = jit_x86_64_asm:retq(),
    I2 = jit_x86_64_asm:jz(byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, AvailableFPRegs0, UsedRegs0, Reg
    ),
    State#state{
        stream = Stream1,
        available_regs = AvailableRegs1,
        available_fpregs = AvailableFPRegs1,
        used_regs = UsedRegs1
    }.

jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State, Label
) ->
    Offset = StreamModule:offset(Stream0),
    {RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(-4),
    Reloc = {Label, Offset + RelocOffset, 32},
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

jump_to_offset(
    #state{
        stream_module = StreamModule, stream = Stream0, branches = AccBranches, used_regs = UsedRegs
    } = State
) ->
    Offset = StreamModule:offset(Stream0),
    {RelocJMPOffset, I} = jit_x86_64_asm:jmp_rel8(-1),
    OffsetRef = make_ref(),
    Reloc = {OffsetRef, Offset + RelocJMPOffset, 8},
    Stream1 = StreamModule:append(Stream0, I),
    {State#state{stream = Stream1, branches = [Reloc | AccBranches]}, OffsetRef, #jump_token{
        used_regs = UsedRegs
    }}.

-spec if_block(state(), condition(), fun((state()) -> state())) -> state().
if_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, ReplaceDelta} = if_block_cond(State0, Cond),
    OffsetAfterCond = StreamModule:offset(State1#state.stream),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    ?ASSERT(OffsetAfter - OffsetAfterCond < 16#80),
    Stream3 = StreamModule:replace(Stream2, Offset + ReplaceDelta, <<
        (OffsetAfter - OffsetAfterCond)
    >>),
    merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs).

-spec if_else_block(state(), condition(), fun((state()) -> state()), fun((state()) -> state())) ->
    state().
if_else_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockTrueFn,
    BlockFalseFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, ReplaceDelta} = if_block_cond(State0, Cond),
    OffsetAfterCond = StreamModule:offset(State1#state.stream),
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    ElseJumpOffset = StreamModule:offset(Stream2),
    {RelocJMPOffset, I} = jit_x86_64_asm:jmp_rel8(-1),
    Stream3 = StreamModule:append(Stream2, I),
    OffsetAfter = StreamModule:offset(Stream3),
    ?ASSERT(OffsetAfter - OffsetAfterCond < 16#80),
    Stream4 = StreamModule:replace(Stream3, Offset + ReplaceDelta, <<
        (OffsetAfter - OffsetAfterCond)
    >>),
    StateElse = State2#state{
        stream = Stream4,
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs,
        available_fpregs = State1#state.available_fpregs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset + RelocJMPOffset, <<
        (OffsetFinal - OffsetAfter)
    >>),
    merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs).

if_block_cond(#state{stream_module = StreamModule, stream = Stream0} = State0, {Reg, '<', 0}) ->
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocJGEOffset, I2} = jit_x86_64_asm:jge_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, byte_size(I1) + RelocJGEOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegA, '<', RegB}
) when ?IS_GPR(RegA) ->
    I1 = jit_x86_64_asm:cmpq(RegB, RegA),
    {RelocJGEOffset, I2} = jit_x86_64_asm:jge_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, byte_size(I1) + RelocJGEOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJNZOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {'(int)', RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testl(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJNZOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJZOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpl(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJZOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jnz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJZOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpl(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jnz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJZOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(uint8_t)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJNZOffset};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(uint8_t)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Reg, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJZOffset};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    I1 = jit_x86_64_asm:movq(Reg, Temp),
    I2 = jit_x86_64_asm:andq(Mask, Temp),
    I3 = jit_x86_64_asm:cmpq(Val, Temp),
    {RelocJZOffset, I4} = jit_x86_64_asm:jz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary,
        I4/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, byte_size(I1) + byte_size(I2) + byte_size(I3) + RelocJZOffset};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    I1 = jit_x86_64_asm:andq(Mask, Reg),
    I2 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJZOffset, I3} = jit_x86_64_asm:jz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + byte_size(I2) + RelocJZOffset};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {'(uint8_t)', RegOrTuple, '&', Val}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(-1),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, byte_size(I1) + RelocJZOffset}.

if_block_free_reg({free, Reg}, State0) ->
    #state{available_regs = AvR0, available_fpregs = AvFR0, used_regs = UR0} = State0,
    {AvR1, AvFR1, UR1} = free_reg(AvR0, AvFR0, UR0, Reg),
    State0#state{
        available_regs = AvR1,
        available_fpregs = AvFR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

merge_used_regs(#state{used_regs = UR0, available_regs = AvR0, available_fpregs = AvFR0} = State, [
    Reg | T
]) ->
    case lists:member(Reg, UR0) of
        true ->
            merge_used_regs(State, T);
        false ->
            AvR1 = lists:delete(Reg, AvR0),
            AvFR1 = lists:delete(Reg, AvFR0),
            UR1 = [Reg | UR0],
            merge_used_regs(
                State#state{used_regs = UR1, available_regs = AvR1, available_fpregs = AvFR1}, T
            )
    end;
merge_used_regs(State, []) ->
    State.

jump_to_offset_if_equal(
    #state{
        stream_module = StreamModule, stream = Stream0, branches = AccBranches, used_regs = UsedRegs
    } = State,
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
    {State#state{stream = Stream1, branches = [Reloc | AccBranches]}, OffsetRef, #jump_token{
        used_regs = UsedRegs
    }}.

jump_to_offset_if_and_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = AccBranches
    } = State0,
    {free, Reg},
    Mask,
    Val
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:andq(Mask, Reg),
    I2 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJNZOffset, I3} = jit_x86_64_asm:jz_rel8(-1),
    OffsetRef = make_ref(),
    Reloc = {OffsetRef, Offset + byte_size(I1) + byte_size(I2) + RelocJNZOffset, 8},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1, branches = [Reloc | AccBranches]},
    State2 = free_native_register(State1, Reg),
    {State2, OffsetRef, #jump_token{used_regs = State2#state.used_regs}};
jump_to_offset_if_and_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = AccBranches,
        available_regs = [Temp | _],
        used_regs = UsedRegs
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
    {State#state{stream = Stream1, branches = [Reloc | AccBranches]}, OffsetRef, #jump_token{
        used_regs = UsedRegs
    }}.

jump_to_offset_if_and_not_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = AccBranches
    } = State0,
    {free, Reg},
    Mask,
    Val
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:andq(Mask, Reg),
    I2 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJNZOffset, I3} = jit_x86_64_asm:jnz_rel8(-1),
    OffsetRef = make_ref(),
    Reloc = {OffsetRef, Offset + byte_size(I1) + byte_size(I2) + RelocJNZOffset, 8},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1, branches = [Reloc | AccBranches]},
    State2 = free_native_register(State1, Reg),
    {State2, OffsetRef, #jump_token{used_regs = State2#state.used_regs}};
jump_to_offset_if_and_not_equal(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = AccBranches,
        available_regs = [Temp | _],
        used_regs = UsedRegs
    } = State,
    Reg,
    Mask,
    Val
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:movq(Reg, Temp),
    I2 = jit_x86_64_asm:andq(Mask, Temp),
    I3 = jit_x86_64_asm:cmpq(Val, Temp),
    {RelocJNZOffset, I4} = jit_x86_64_asm:jnz_rel8(-1),
    OffsetRef = make_ref(),
    Reloc = {OffsetRef, Offset + byte_size(I1) + byte_size(I2) + byte_size(I3) + RelocJNZOffset, 8},
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary,
        I4/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | AccBranches]}, OffsetRef, #jump_token{
        used_regs = UsedRegs
    }}.

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

-spec call_func_ptr(state(), {free, x86_64_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), x86_64_register()}.
call_func_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        available_fpregs = AvailableFP0,
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
    SavedRegs = [?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | UsedRegs1],
    Stream1 = lists:foldl(
        fun(Reg, AccStream) ->
            StreamModule:append(AccStream, jit_x86_64_asm:pushq(Reg))
        end,
        Stream0,
        SavedRegs
    ),
    PushOdds = length(SavedRegs) rem 2,
    Stream3 =
        case FuncPtrTuple of
            {free, _} ->
                Stream1;
            {primitive, Primitive} ->
                PrepCall0 =
                    case Primitive of
                        0 ->
                            jit_x86_64_asm:movq({0, ?NATIVE_INTERFACE_REG}, ?NATIVE_INTERFACE_REG);
                        N ->
                            jit_x86_64_asm:movq(?PRIMITIVE(N), ?NATIVE_INTERFACE_REG)
                    end,
                PrepCall1 = jit_x86_64_asm:pushq(?NATIVE_INTERFACE_REG),
                StreamModule:append(Stream1, <<PrepCall0/binary, PrepCall1/binary>>)
        end,
    % x86 64 stack should be aligned to 16 bytes when running callq instruction
    % It is therefore unaligned and we need to always push an odd number of
    % registers.
    % Align stack by pushing ?NATIVE_INTERFACE_REG
    % ?NATIVE_INTERFACE_REG may have been pushed as the function pointer
    Stream4 =
        case PushOdds of
            1 ->
                Stream3;
            0 ->
                PrepCall2 = jit_x86_64_asm:pushq(?NATIVE_INTERFACE_REG),
                StreamModule:append(Stream3, PrepCall2)
        end,
    State1 = set_args(State0#state{stream = Stream4}, Args),
    #state{stream = Stream5} = State1,
    Call =
        case FuncPtrTuple of
            {free, FuncPtrReg} ->
                jit_x86_64_asm:callq({FuncPtrReg});
            {primitive, _} ->
                Call0 = jit_x86_64_asm:popq(rax),
                Call1 = jit_x86_64_asm:callq({rax}),
                <<Call0/binary, Call1/binary>>
        end,
    Stream6 = StreamModule:append(Stream5, Call),
    % Unalign stack
    Stream7 =
        case PushOdds of
            1 ->
                Stream6;
            0 ->
                PostCall1 = jit_x86_64_asm:popq(?NATIVE_INTERFACE_REG),
                StreamModule:append(Stream6, PostCall1)
        end,
    % If rax is in used regs, save it to another temporary register
    AvailableRegs1 = FreeRegs ++ AvailableRegs0,
    {Stream8, ResultReg} =
        case lists:member(rax, SavedRegs) of
            true ->
                [Temp | _] = AvailableRegs1,
                {StreamModule:append(Stream7, jit_x86_64_asm:movq(rax, Temp)), Temp};
            false ->
                {Stream7, rax}
        end,
    Stream9 = lists:foldl(
        fun(Reg, AccStream) ->
            StreamModule:append(AccStream, jit_x86_64_asm:popq(Reg))
        end,
        Stream8,
        lists:reverse(SavedRegs)
    ),
    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    AvailableFP1 = FreeRegs ++ AvailableFP0,
    AvailableFP2 = lists:delete(ResultReg, AvailableFP1),
    AvailableFP3 = ?AVAILABLE_FPREGS -- (?AVAILABLE_FPREGS -- AvailableFP2),
    UsedRegs2 = [ResultReg | UsedRegs1],
    {
        State1#state{
            stream = Stream9,
            available_regs = AvailableRegs3,
            available_fpregs = AvailableFP3,
            used_regs = UsedRegs2
        },
        ResultReg
    }.

-spec set_args(state(), [arg()]) -> state().
set_args(
    #state{stream = Stream0, stream_module = StreamModule, used_regs = UsedRegs} = State0, Args
) ->
    ParamRegs = parameter_regs(Args),
    ArgsRegs = lists:map(
        fun
            ({free, {ptr, Reg}}) -> Reg;
            ({free, Reg}) when is_atom(Reg) -> Reg;
            ({free, Imm}) when is_integer(Imm) -> imm;
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
            ({free, {fp_reg, _}}) -> ?CTX_REG
        end,
        Args
    ),
    AvailableScratchGP =
        [rdi, rsi, rdx, rcx, r8, r9, r10, r11] -- ParamRegs -- ArgsRegs -- UsedRegs,
    AvailableScratchFP = ?AVAILABLE_FPREGS -- ParamRegs -- ArgsRegs -- UsedRegs,
    SetArgsCode = set_args0(Args, ArgsRegs, ParamRegs, AvailableScratchGP, AvailableScratchFP, []),
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
        available_fpregs = ?AVAILABLE_FPREGS -- ParamRegs -- NewUsedRegs,
        used_regs = ParamRegs ++ (NewUsedRegs -- ParamRegs)
    }.

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, ?PARAMETER_FPREGS, []).

parameter_regs0([], _, _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([Special | T], [GPReg | GPRegsT], FPRegs, Acc) when
    Special =:= ctx orelse Special =:= jit_state
->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([{free, Free} | T], GPRegs, FPRegs, Acc) ->
    parameter_regs0([Free | T], GPRegs, FPRegs, Acc);
parameter_regs0([{ptr, Reg} | T], [GPReg | GPRegsT], FPRegs, Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([Reg | T], [GPReg | GPRegsT], FPRegs, Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([Reg | T], GPRegs, [FPReg | FPRegsT], Acc) when ?IS_FPR(Reg) ->
    parameter_regs0(T, GPRegs, FPRegsT, [FPReg | Acc]);
parameter_regs0([{x_reg, _} | T], [GPReg | GPRegsT], FPRegs, Acc) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([{y_reg, _} | T], [GPReg | GPRegsT], FPRegs, Acc) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([{fp_reg, _} | T], GPRegs, [FPReg | FPRegsT], Acc) ->
    parameter_regs0(T, GPRegs, FPRegsT, [FPReg | Acc]);
parameter_regs0([Int | T], [GPReg | GPRegsT], FPRegs, Acc) when is_integer(Int) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]).

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

set_args0([], [], [], _AvailGP, _AvailFP, Acc) ->
    list_to_binary(lists:reverse(Acc));
set_args0([{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc) ->
    set_args0([FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc);
set_args0([ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, AvailFP, Acc) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc);
set_args0(
    [jit_state | ArgsT],
    [?JITSTATE_REG | ArgsRegs],
    [?JITSTATE_REG | ParamRegs],
    AvailGP,
    AvailFP,
    Acc
) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc);
set_args0(
    [jit_state | ArgsT], [?JITSTATE_REG | ArgsRegs], [ParamReg | ParamRegs], AvailGP, AvailFP, Acc
) ->
    false = lists:member(ParamReg, ArgsRegs),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, [
        jit_x86_64_asm:movq(?JITSTATE_REG, ParamReg) | Acc
    ]);
% ctx is special as we need it to access x_reg/y_reg/fp_reg
set_args0([Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, AvailFP, Acc) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    J = set_args1(Arg, ?CTX_REG),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, [J | Acc]);
set_args0(
    [Arg | ArgsT],
    [_ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    [Avail | AvailGPT] = AvailGP,
    AvailFP,
    Acc
) ->
    J = set_args1(Arg, ParamReg),
    case lists:member(ParamReg, ArgsRegs) of
        false ->
            set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, [J | Acc]);
        true ->
            I = jit_x86_64_asm:movq(ParamReg, Avail),
            NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
            set_args0(NewArgsT, ArgsRegs, ParamRegs, AvailGPT, AvailFP, [J, I | Acc])
    end.

set_args1(Reg, Reg) ->
    [];
set_args1({x_reg, extra}, Reg) ->
    jit_x86_64_asm:movq(?X_REG(?MAX_REG), Reg);
set_args1({x_reg, X}, Reg) ->
    jit_x86_64_asm:movq(?X_REG(X), Reg);
set_args1({ptr, Source}, Reg) ->
    jit_x86_64_asm:movq({0, Source}, Reg);
set_args1({y_reg, X}, Reg) ->
    [
        jit_x86_64_asm:movq(?Y_REGS, Reg),
        jit_x86_64_asm:movq({X * 8, Reg}, Reg)
    ];
set_args1(ArgReg, Reg) when ?IS_GPR(ArgReg) ->
    jit_x86_64_asm:movq(ArgReg, Reg);
set_args1(Arg, Reg) when is_integer(Arg) andalso Arg >= -16#80000000 andalso Arg < 16#80000000 ->
    jit_x86_64_asm:movq(Arg, Reg);
set_args1(Arg, Reg) when is_integer(Arg) ->
    jit_x86_64_asm:movabsq(Arg, Reg).

move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, 0, {x_reg, X}
) when
    X < ?MAX_REG
->
    I1 = jit_x86_64_asm:andq(0, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, 0, {x_reg, extra}
) ->
    I1 = jit_x86_64_asm:andq(0, ?X_REG(?MAX_REG)),
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
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, available_regs = [Temp1, Temp2 | _], stream = Stream0} =
        State,
    {ptr, Reg},
    {y_reg, Y}
) when ?IS_GPR(Reg) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movq({0, Reg}, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Y * 8, Temp1}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_vm_register(
    #state{stream_module = StreamModule, available_regs = [Temp | _], stream = Stream0} = State,
    Reg,
    {fp_reg, F}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(?FP_REGS, Temp),
    I2 = jit_x86_64_asm:movq(Reg, {F * 8, Temp}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%% @doc move reg[x] to a vm or native register
-spec move_array_element(
    state(),
    x86_64_register(),
    non_neg_integer() | x86_64_register(),
    vm_register() | x86_64_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {ptr, Dest}
) when is_integer(Index) ->
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
) when is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Y * 8, Temp1}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1 | _]} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp1),
    I2 = jit_x86_64_asm:movq({Index * 8, Reg}, Reg),
    I3 = jit_x86_64_asm:movq(Reg, {Y * 8, Temp1}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Dest),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        available_fpregs = AvailableFPRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    I1 = jit_x86_64_asm:shlq(3, IndexReg),
    I2 = jit_x86_64_asm:addq(Reg, IndexReg),
    I3 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I4 = jit_x86_64_asm:movq(IndexReg, ?X_REG(X)),
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, AvailableFPRegs0, UsedRegs0, IndexReg
    ),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    State#state{
        available_regs = AvailableRegs1,
        available_fpregs = AvailableFPRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _] = AvailableRegs0,
        used_regs = UsedRegs0,
        available_fpregs = AvailableFPRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when ?IS_GPR(IndexReg) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:shlq(3, IndexReg),
    I3 = jit_x86_64_asm:addq(Reg, IndexReg),
    I4 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I5 = jit_x86_64_asm:movq(IndexReg, {Y * 8, Temp}),
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, AvailableFPRegs0, UsedRegs0, IndexReg
    ),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>
    ),
    State#state{
        available_regs = AvailableRegs1,
        available_fpregs = AvailableFPRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    }.

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
) when X < ?MAX_REG andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    Reg,
    IndexReg
) when X < ?MAX_REG andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {0, Reg, IndexReg, 8}),
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
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} =
        State,
    {y_reg, Y},
    Reg,
    Index
) when ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} =
        State,
    {y_reg, Y},
    Reg,
    IndexReg
) when ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {0, Reg, IndexReg, 8}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_GPR(Source) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_SINT32_T(Source) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Source,
    Reg,
    Index
) when is_integer(Source) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movabsq(Source, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1}.

move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    BaseReg,
    IndexReg,
    Offset
) when X < ?MAX_REG andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {y_reg, Y},
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when
    ?IS_GPR(Source) andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset)
->
    I1 = jit_x86_64_asm:movq(Source, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when
    ?IS_SINT32_T(Source) andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso
        is_integer(Offset)
->
    I1 = jit_x86_64_asm:movq(Source, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, I1 / binary),
    State#state{stream = Stream1};
move_to_array_element(
    State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when is_integer(IndexReg) andalso is_integer(Offset) andalso Offset div 8 =:= 0 ->
    move_to_array_element(State, Source, BaseReg, IndexReg + (Offset div 8)).

-spec move_to_native_register(state(), value()) -> {state(), x86_64_register()}.
move_to_native_register(State, Reg) when is_atom(Reg) ->
    {State, Reg};
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
    Imm
) when
    is_integer(Imm)
->
    I1 = jit_x86_64_asm:movq(Imm, Reg),
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
    I1 = jit_x86_64_asm:movq(?X_REG(X), Reg),
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
    I1 = jit_x86_64_asm:movq(?Y_REGS, Reg),
    I2 = jit_x86_64_asm:movq({Y * 8, Reg}, Reg),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [Reg | Used]}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _],
        available_fpregs = [FPReg | AvailFT],
        used_regs = Used
    } = State,
    {fp_reg, F}
) ->
    I1 = jit_x86_64_asm:movq(?FP_REGS, Temp),
    I2 = jit_x86_64_asm:movsd({F * 8, Temp}, FPReg),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_fpregs = AvailFT, used_regs = [FPReg | Used]}, FPReg}.

-spec move_to_native_register(state(), value(), x86_64_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, RegSrc, RegDst
) when is_atom(RegSrc) orelse is_integer(RegSrc) ->
    I = jit_x86_64_asm:movq(RegSrc, RegDst),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}, RegDst
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq({0, Reg}, RegDst),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {x_reg, X},
    RegDst
) when
    X < ?MAX_REG
->
    I1 = jit_x86_64_asm:movq(?X_REG(X), RegDst),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {y_reg, Y}, RegDst
) ->
    I1 = jit_x86_64_asm:movq(?Y_REGS, RegDst),
    I2 = jit_x86_64_asm:movq({Y * 8, RegDst}, RegDst),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State,
    {fp_reg, F},
    RegDst
) ->
    I1 = jit_x86_64_asm:movq(?FP_REGS, Temp),
    I2 = jit_x86_64_asm:movsd({F * 8, Temp}, RegDst),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec copy_to_native_register(state(), value()) -> {state(), x86_64_register()}.
copy_to_native_register(
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
copy_to_native_register(
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
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

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

and_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:andq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

or_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:orq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

add(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:addq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

sub(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:subq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:imulq(Val, Reg),
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
    merge_used_regs(State2#state{stream = Stream4}, State1#state.used_regs).

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset} = set_cp(State0),
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

call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset).

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
    I3 = jit_x86_64_asm:movq(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, AddrOffset}.

-spec rewrite_cp_offset(state(), non_neg_integer()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    % Encode ReturnAddrOffset << 2
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, <<(NewOffset bsl 2):32/little>>),
    State0#state{stream = Stream1}.

set_bs(#state{stream_module = StreamModule, stream = Stream0} = State0, TermReg) ->
    I1 = jit_x86_64_asm:movq(TermReg, ?BS),
    I2 = jit_x86_64_asm:movq(0, ?BS_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1}.

free_reg(AvailableRegs0, AvailableFPRegs0, UsedRegs0, Reg) when ?IS_GPR(Reg) ->
    AvailableRegs1 = free_reg0(?AVAILABLE_REGS, AvailableRegs0, Reg, []),
    true = lists:member(Reg, UsedRegs0),
    UsedRegs1 = lists:delete(Reg, UsedRegs0),
    {AvailableRegs1, AvailableFPRegs0, UsedRegs1};
free_reg(AvailableRegs0, AvailableFPRegs0, UsedRegs0, Reg) when ?IS_FPR(Reg) ->
    AvailableFPRegs1 = free_reg0(?AVAILABLE_FPREGS, AvailableFPRegs0, Reg, []),
    true = lists:member(Reg, UsedRegs0),
    UsedRegs1 = lists:delete(Reg, UsedRegs0),
    {AvailableRegs0, AvailableFPRegs1, UsedRegs1}.

free_reg0([Reg | _SortedT], PrevRegs0, Reg, Acc) ->
    lists:reverse(Acc, [Reg | PrevRegs0]);
free_reg0([PrevReg | SortedT], [PrevReg | PrevT], Reg, Acc) ->
    free_reg0(SortedT, PrevT, Reg, [PrevReg | Acc]);
free_reg0([_Other | SortedT], PrevRegs, Reg, Acc) ->
    free_reg0(SortedT, PrevRegs, Reg, Acc).
