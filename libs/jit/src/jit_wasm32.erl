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

-module(jit_wasm32).

%% WASM32 JIT backend for AtomVM.
%%
%% This backend generates WebAssembly bytecode instead of native machine code.
%% Each BEAM label compiles to a separate WASM function with the standard
%% ModuleNativeEntryPoint signature:
%%   Context* (*)(Context* ctx, JITState* jit_state, const ModuleNativeInterface* p)
%%
%% All WASM function parameters and local variables are passed through
%% Emscripten's linear memory. The three parameters (ctx, jit_state, p) are
%% wasm32 pointers (i32 indices into linear memory).
%%
%% Intra-module branches (jump_to_label, cond_jump_to_label) set the
%% jit_state->continuation field and return to the C dispatch loop, which
%% then re-enters the target label's WASM function.
%%
%% The "jump table" is an array of function pointers (i32 table indices)
%% stored as data, not executable code.
%%
%% WASM local variables:
%%   local 0: ctx (i32) - parameter
%%   local 1: jit_state (i32) - parameter
%%   local 2: native_interface (i32) - parameter
%%   local 3+: scratch locals for temporary values

-export([
    word_size/0,
    supports_tail_cache/0,
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

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").
-include("term.hrl").

-define(ASSERT(Expr), true = Expr).

%% WASM32 uses 32-bit pointers in Emscripten linear memory.
%% The three function parameters are:
%%   local 0: ctx pointer (i32)
%%   local 1: jit_state pointer (i32)
%%   local 2: native_interface pointer (i32)
%% Additional locals are used as scratch space.

-define(CTX_LOCAL, local0).
-define(JITSTATE_LOCAL, local1).
-define(NATIVE_INTERFACE_LOCAL, local2).

%% Scratch locals start at index 3
-define(FIRST_SCRATCH_LOCAL, 3).
-define(NUM_SCRATCH_LOCALS, 8).

%% Context struct offsets (32-bit architecture, same as armv6m/riscv32)
-define(CTX_E_OFFSET, 16#14).
-define(CTX_X_OFFSET, 16#18).
-define(CTX_CP_OFFSET, 16#5C).
-define(CTX_FR_OFFSET, 16#60).
-define(CTX_BS_OFFSET, 16#64).
-define(CTX_BS_OFFSET_OFFSET, 16#68).

%% JITState struct offsets
-define(JITSTATE_MODULE_OFFSET, 16#0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#8).

%% Jump table entry size: sizeof(uint32_t) for a function pointer
-define(JUMP_TABLE_ENTRY_SIZE, 4).

%% WASM type section layout for call_indirect type indices:
%%   0 .. MAX_PRIMITIVE_ARGS-1:  (i32 × N) -> i32  for N=1..MAX_PRIMITIVE_ARGS
%%   VOID_TYPE_BASE .. :         (i32 × N) -> void  for N=1..MAX_PRIMITIVE_ARGS
%%   SPECIAL_TYPE_BASE .. :      special signatures (e.g. i64 args)
-define(MAX_PRIMITIVE_ARGS, 6).
-define(VOID_TYPE_BASE, ?MAX_PRIMITIVE_ARGS).
-define(SPECIAL_TYPE_BASE, ?MAX_PRIMITIVE_ARGS + ?MAX_PRIMITIVE_ARGS).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    jump_table_start :: non_neg_integer(),
    available_regs :: non_neg_integer(),
    used_regs :: non_neg_integer(),
    max_scratch :: non_neg_integer(),
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer(),
    regs :: jit_regs:regs(),
    func_bodies :: [{integer(), binary()}],
    current_body :: binary(),
    current_label :: integer() | reference() | undefined,
    labels_count :: non_neg_integer(),
    beam_label :: non_neg_integer() | undefined,
    cont_label_map :: [{non_neg_integer(), non_neg_integer()}]
}).

-type state() :: #state{}.
-type wasm_local() :: non_neg_integer().
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, wasm_local()}.
-type value() :: immediate() | vm_register() | wasm_local() | {ptr, wasm_local()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_local() ::
    {free, wasm_local()} | wasm_local().

-type condition() ::
    {wasm_local(), '<', integer()}
    | {maybe_free_local(), '<', wasm_local()}
    | {integer(), '<', maybe_free_local()}
    | {maybe_free_local(), '==', integer()}
    | {maybe_free_local(), '!=', wasm_local() | integer()}
    | {'(int)', maybe_free_local(), '==', integer()}
    | {'(int)', maybe_free_local(), '!=', wasm_local() | integer()}
    | {'(bool)', maybe_free_local(), '==', false}
    | {'(bool)', maybe_free_local(), '!=', false}
    | {maybe_free_local(), '&', non_neg_integer(), '!=', integer()}
    | {{free, wasm_local()}, '==', {free, wasm_local()}}.

-define(AVAILABLE_REGS_MASK, 16#FF).

%%=============================================================================
%% Backend API implementation
%%=============================================================================

-spec word_size() -> 4 | 8.
word_size() -> 4.

%% WASM does not support the tail cache optimization because each BEAM label
%% compiles to a separate WASM function.
-spec supports_tail_cache() -> boolean().
supports_tail_cache() -> false.

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
        max_scratch = ?NUM_SCRATCH_LOCALS,
        labels = [],
        variant = Variant,
        regs = jit_regs:new(),
        func_bodies = [],
        current_body = <<>>,
        current_label = undefined,
        labels_count = 0,
        beam_label = undefined,
        cont_label_map = []
    }.

-spec stream(state()) -> stream().
stream(#state{stream = Stream}) ->
    Stream.

-spec offset(state()) -> non_neg_integer().
offset(#state{current_label = Label, labels = Labels, jump_table_start = _JTStart}) ->
    %% For WASM, the "offset" is label * JUMP_TABLE_ENTRY_SIZE.
    %% This matches the CP format: cp_offset = (cp & 0xFFFFFF) >> 2
    %% = label * JUMP_TABLE_ENTRY_SIZE.
    %% Note: does NOT include JTStart, unlike the labels list which does.
    case Label of
        undefined ->
            0;
        _ when is_integer(Label) -> Label * ?JUMP_TABLE_ENTRY_SIZE;
        _ ->
            %% Reference labels - find if already registered
            case lists:keyfind(Label, 1, Labels) of
                {Label, Offset} -> Offset;
                false -> 0
            end
    end.

-spec flush(state()) -> state().
flush(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:flush(Stream0),
    State#state{stream = Stream1}.

-spec debugger(state()) -> state().
debugger(State) ->
    emit(State, jit_wasm32_asm:unreachable()).

-spec used_regs(state()) -> [wasm_local()].
used_regs(#state{used_regs = Used}) -> mask_to_locals(Used).

-spec available_regs(state()) -> [wasm_local()].
available_regs(#state{available_regs = Available}) -> mask_to_locals(Available).

-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Val | Rest]) ->
    State1 = free_native_register(State, Val),
    free_native_registers(State1, Rest).

-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_regs = Available0, used_regs = Used0} = State,
    Local
) when is_atom(Local) ->
    LocalIdx = jit_wasm32_asm:local_index(Local),
    case LocalIdx >= ?FIRST_SCRATCH_LOCAL of
        true ->
            Bit = local_bit(LocalIdx),
            State#state{
                available_regs = Available0 bor Bit, used_regs = Used0 band (bnot Bit)
            };
        false ->
            State
    end;
free_native_register(State, {ptr, Local}) ->
    free_native_register(State, Local);
free_native_register(State, _Other) ->
    State.

-spec assert_all_native_free(state()) -> ok.
assert_all_native_free(#state{max_scratch = MS} = State) ->
    0 = State#state.used_regs,
    AllFree = (1 bsl MS) - 1,
    AllFree = State#state.available_regs,
    ok.

%%=============================================================================
%% Jump table
%%=============================================================================

-spec jump_table(state(), pos_integer()) -> state().
jump_table(#state{stream_module = StreamModule, stream = Stream0} = State, LabelsCount) ->
    NumEntries = LabelsCount + 1,
    %% Stream header:
    %%   Bytes 0..3:   num_entries (uint32_t LE)
    %%   Bytes 4..7:   wasm_offset placeholder (uint32_t LE, patched in return_labels_and_lines)
    %%   Bytes 8..11:  lines_offset placeholder (uint32_t LE, patched in return_labels_and_lines)
    %%   Bytes 12..12+num_entries*4-1: Reserved jump table area
    Stream1 = StreamModule:append(Stream0, <<NumEntries:32/little, 0:32/little, 0:32/little>>),
    JumpTableStart = StreamModule:offset(Stream1),
    Placeholder = <<0:32/little>>,
    Stream2 = emit_n_times(StreamModule, Stream1, Placeholder, NumEntries),
    State#state{
        stream = Stream2,
        jump_table_start = JumpTableStart,
        labels_count = LabelsCount
    }.

emit_n_times(_StreamModule, Stream, _Binary, 0) ->
    Stream;
emit_n_times(StreamModule, Stream, Binary, N) when N > 0 ->
    Stream1 = StreamModule:append(Stream, Binary),
    emit_n_times(StreamModule, Stream1, Binary, N - 1).

%%=============================================================================
%% Branch patching
%%=============================================================================

-spec update_branches(state()) -> state().
update_branches(#state{branches = []} = State) ->
    State;
update_branches(State) ->
    %% Branches are resolved at label-add time in WASM.
    State#state{branches = []}.

%%=============================================================================
%% Primitive calls
%%
%% Primitives are C functions called through call_indirect via the
%% ModuleNativeInterface function pointer table.
%%=============================================================================

-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), wasm_local()}.
call_primitive(State0, Primitive, Args) ->
    {State1, ResultLocal} = alloc_local(State0),
    State2 = emit_call_primitive(State1, Primitive, Args, ResultLocal, false),
    Regs1 = jit_regs:invalidate_all(State2#state.regs),
    {State2#state{regs = Regs1}, ResultLocal}.

-spec call_primitive_last(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_last(State0, Primitive, Args) ->
    State1 = emit_call_primitive(State0, Primitive, Args, none, true),
    AllFree = (1 bsl State1#state.max_scratch) - 1,
    State1#state{
        available_regs = AllFree,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State1#state.regs)
    }.

call_primitive_with_cp(State0, Primitive, Args) ->
    %% Split the function: the return from the primitive lands in a continuation.
    ContLabel = State0#state.labels_count + 1,
    State0a = record_cont_label(State0, ContLabel),
    State1 = State0a#state{labels_count = ContLabel},
    State2 = emit_set_cp_for_label(State1, ContLabel),
    State3 = call_primitive_last(State2, Primitive, Args),

    %% Split: close current function, open continuation function
    #state{
        func_bodies = FuncBodies,
        current_body = CurrentBody,
        current_label = PrevLabel,
        jump_table_start = JumpTableStart,
        labels = Labels
    } = State3,
    FinalizedBody =
        <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
            (jit_wasm32_asm:return())/binary>>,
    NewFuncBodies = [{PrevLabel, FinalizedBody} | FuncBodies],
    ContLabelOff = JumpTableStart + ContLabel * ?JUMP_TABLE_ENTRY_SIZE,
    AllFree = (1 bsl State3#state.max_scratch) - 1,
    State3#state{
        func_bodies = NewFuncBodies,
        current_body = <<>>,
        current_label = ContLabel,
        labels = [{ContLabel, ContLabelOff} | Labels],
        available_regs = AllFree,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State3#state.regs)
    }.

%%=============================================================================
%% Control flow
%%=============================================================================

return_if_not_equal_to_ctx(
    #state{available_regs = Available0, used_regs = Used0} = State0,
    {free, Local}
) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_ne())/binary,
        (jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void()))/binary,
        %% Return the value (not equal to ctx)
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:return())/binary,
        (jit_wasm32_asm:end_())/binary
    >>,
    Bit = local_bit(Local),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{
        available_regs = Available0 bor Bit,
        used_regs = Used0 band (bnot Bit),
        regs = Regs1
    }.

jump_to_label(State0, Label) ->
    %% In WASM, jumping to a label means:
    %% 1. Look up the label's function table index
    %% 2. Store it in jit_state->continuation
    %% 3. Return ctx (let the C dispatch loop re-enter)
    State1 = emit_set_continuation_for_label(State0, Label),
    %% Return ctx
    State2 = emit(State1, <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:return())/binary
    >>),
    %% After unconditional jump, register tracking is dead until next label
    State2#state{regs = jit_regs:invalidate_all(State2#state.regs)}.

jump_to_offset(State0, TargetOffset) ->
    %% The tail cache in jit.erl stores offsets from offset(). Since we return
    %% unique negative values from offset(), this should never be called with
    %% a valid cached offset. If it is, just return ctx.
    #state{jump_table_start = JTStart} = State0,
    case TargetOffset of
        _ when is_integer(TargetOffset), TargetOffset >= JTStart ->
            %% Real offset from the jump table - resolve to label
            Label = (TargetOffset - JTStart) div ?JUMP_TABLE_ENTRY_SIZE,
            jump_to_label(State0, Label);
        _ ->
            %% Negative or unrecognized offset - return ctx
            State1 = emit(State0, <<
                (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
                (jit_wasm32_asm:return())/binary
            >>),
            State1#state{regs = jit_regs:invalidate_all(State1#state.regs)}
    end.

cond_jump_to_label(State, Cond, Label) ->
    if_block(State, Cond, fun(S) -> jump_to_label(S, Label) end).

jump_to_continuation(State0, {free, OffsetLocal}) ->
    %% Convert byte offset to continuation = offset / JUMP_TABLE_ENTRY_SIZE + 1
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:local_get(OffsetLocal))/binary,
        (jit_wasm32_asm:i32_const(?JUMP_TABLE_ENTRY_SIZE))/binary,
        (jit_wasm32_asm:i32_div_u())/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary,
        %% Return ctx
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:return())/binary
    >>,
    State1 = emit(State0, Code),
    AllFree = (1 bsl State1#state.max_scratch) - 1,
    State1#state{available_regs = AllFree, used_regs = 0}.

%%=============================================================================
%% Conditional blocks
%%=============================================================================

-spec if_block(state(), condition() | {'and', [condition()]}, fun((state()) -> state())) -> state().
if_block(State0, {'and', CondList}, BlockFn) ->
    %% For AND conditions, emit all conditions and AND them together
    State1 = emit_and_conditions(State0, CondList),
    %% Now emit: if ... block ... end
    State2 = emit(State1, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State3 = BlockFn(State2),
    State4 = emit(State3, jit_wasm32_asm:end_()),
    MergedRegs = jit_regs:merge(State1#state.regs, State4#state.regs),
    merge_used_regs(State4#state{regs = MergedRegs}, State1#state.used_regs);
if_block(State0, Cond, BlockFn) ->
    State1 = emit_condition(State0, Cond),
    State2 = emit(State1, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State3 = BlockFn(State2),
    State4 = emit(State3, jit_wasm32_asm:end_()),
    MergedRegs = jit_regs:merge(State1#state.regs, State4#state.regs),
    merge_used_regs(State4#state{regs = MergedRegs}, State1#state.used_regs).

-spec if_else_block(state(), condition(), fun((state()) -> state()), fun((state()) -> state())) ->
    state().
if_else_block(State0, Cond, BlockTrueFn, BlockFalseFn) ->
    State1 = emit_condition(State0, Cond),
    State2 = emit(State1, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State3 = BlockTrueFn(State2),
    State4 = emit(State3, jit_wasm32_asm:else_()),
    StateElse = State4#state{
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs
    },
    State5 = BlockFalseFn(StateElse),
    State6 = emit(State5, jit_wasm32_asm:end_()),
    MergedRegs = jit_regs:merge(State3#state.regs, State5#state.regs),
    merge_used_regs(State6#state{regs = MergedRegs}, State3#state.used_regs).

%%=============================================================================
%% Arithmetic and bitwise operations
%%=============================================================================

-spec shift_right(state(), maybe_free_local(), non_neg_integer()) ->
    {state(), wasm_local()}.
shift_right(State0, {free, Local}, Shift) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_u())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
shift_right(State0, Local, Shift) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_u())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

shift_left(State0, Local, Shift) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

shift_right_arith(State0, {free, Local}, Shift) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_s())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
shift_right_arith(State0, Local, Shift) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_s())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

and_(State0, {free, Local}, Mask) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(Mask))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
and_(State0, Local, Mask) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(Mask))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

or_(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_or())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

xor_(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_xor())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

add(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

sub(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_sub())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_mul())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

div_reg(State0, {free, Local}, Divisor) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_div_s())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
div_reg(State0, Local, Divisor) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_div_s())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

rem_reg(State0, {free, Local}, Divisor) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_rem_s())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
rem_reg(State0, Local, Divisor) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_rem_s())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

%%=============================================================================
%% Memory access (VM registers, context fields)
%%=============================================================================

move_to_vm_register(State0, Value, {ptr, Local}) ->
    %% Store value to memory at [Local]
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, 0))/binary
    >>,
    emit(State0, Code);
move_to_vm_register(State0, Value, {x_reg, N}) ->
    %% Store value to ctx->x[N]
    %% ctx->x is at CTX_X_OFFSET, each x register is 4 bytes (word_size)
    Offset = ?CTX_X_OFFSET + N * 4,
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, Offset))/binary
    >>,
    emit(State0, Code);
move_to_vm_register(State0, Value, {y_reg, N}) ->
    %% Store value to ctx->e[N]
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, N * 4))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal);
move_to_vm_register(State0, {free, {ptr, Reg, WordOffset}}, {fp_reg, N}) ->
    %% Store boxed float data to ctx->fr[N] (8 bytes per double)
    DataOffset = WordOffset * 4,
    {State1, FpRegsLocal} = alloc_local(State0),
    Code = <<
        %% Load ctx->fr pointer
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_FR_OFFSET))/binary,
        (jit_wasm32_asm:local_set(FpRegsLocal))/binary,
        %% Copy first 4 bytes of the double
        (jit_wasm32_asm:local_get(FpRegsLocal))/binary,
        (jit_wasm32_asm:local_get(Reg))/binary,
        (jit_wasm32_asm:i32_load(2, DataOffset))/binary,
        (jit_wasm32_asm:i32_store(2, N * 8))/binary,
        %% Copy second 4 bytes of the double
        (jit_wasm32_asm:local_get(FpRegsLocal))/binary,
        (jit_wasm32_asm:local_get(Reg))/binary,
        (jit_wasm32_asm:i32_load(2, DataOffset + 4))/binary,
        (jit_wasm32_asm:i32_store(2, N * 8 + 4))/binary
    >>,
    State2 = free_native_register(State1, Reg),
    State3 = free_native_register(State2, FpRegsLocal),
    emit(State3, Code).

move_to_native_register(State, Reg) when is_atom(Reg) ->
    case is_scratch_local(Reg) of
        true -> {State, Reg};
        false -> move_to_native_register_alloc(State, Reg)
    end;
move_to_native_register(State0, Value) ->
    move_to_native_register_alloc(State0, Value).

move_to_native_register_alloc(State0, Value) ->
    {State1, Local} = alloc_local(State0),
    Code = <<
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Local),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, Local}.

move_to_native_register(State0, Value, Local) ->
    Code = <<
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

move_to_cp(State0, {y_reg, Y}) ->
    %% Load y register and store to ctx->cp
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        %% Load ctx->e
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        %% Load e[Y]
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Y * 4))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        %% Store to ctx->cp
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_CP_OFFSET))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal).

move_array_element(State0, Base, Index, {ptr, Dest}) when is_integer(Index) ->
    %% Load Base[Index] and store to [Dest]
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(Dest))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:i32_store(2, 0))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, Index, {x_reg, N}) when is_integer(Index) ->
    %% Load word at Base[Index] and store into ctx->x[N]
    BaseLocal = unwrap_local(Base),
    Offset = ?CTX_X_OFFSET + N * 4,
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:i32_store(2, Offset))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, Index, {y_reg, N}) when is_integer(Index) ->
    %% Load word at Base[Index] and store into ctx->e[N]
    BaseLocal = unwrap_local(Base),
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:i32_store(2, N * 4))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal);
move_array_element(State0, Base, Index, Dest) when is_integer(Index), is_atom(Dest) ->
    %% Load Base[Index] into Dest
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:local_set(Dest))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, {free, IndexReg}, {ptr, Dest}) ->
    %% Load word at Base[IndexReg * 4] and store into memory at [Dest]
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(Dest))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:i32_store(2, 0))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, {free, IndexReg}, {x_reg, N}) ->
    %% Load word at Base[IndexReg * 4] and store into ctx->x[N]
    BaseLocal = unwrap_local(Base),
    Offset = ?CTX_X_OFFSET + N * 4,
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:i32_store(2, Offset))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, {free, IndexReg}, {y_reg, N}) ->
    %% Load word at Base[IndexReg * 4] and store into ctx->e[N]
    BaseLocal = unwrap_local(Base),
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:i32_store(2, N * 4))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal);
move_array_element(State0, Base, {free, IndexReg}, Dest) when is_atom(Dest) ->
    %% Load word at Base[IndexReg * 4] and store into local Dest
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:local_set(Dest))/binary
    >>,
    emit(State0, Code).

move_to_array_element(State0, Value, Base, Index) ->
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, Index * 4))/binary
    >>,
    emit(State0, Code).

move_to_array_element(State0, Value, Base, IndexLocal, Offset) ->
    %% Store value at Base + (IndexLocal + Offset) * 4
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexLocal))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, Offset * 4))/binary
    >>,
    emit(State0, Code).

set_bs(State0, TermLocal) ->
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TermLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_BS_OFFSET))/binary,
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_const(0))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_BS_OFFSET_OFFSET))/binary
    >>,
    emit(State0, Code).

copy_to_native_register(State0, Value) when is_atom(Value) ->
    move_to_native_register_alloc(State0, Value);
copy_to_native_register(State0, Value) ->
    move_to_native_register(State0, Value).

get_array_element(State0, Base, Index) ->
    BaseLocal = unwrap_local(Base),
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

increment_sp(State0, Offset) ->
    %% ctx->e += Offset * 4
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        %% Load current ctx->e
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        %% Add offset
        (jit_wasm32_asm:i32_const(Offset * 4))/binary,
        (jit_wasm32_asm:i32_add())/binary,
        %% Store back
        (jit_wasm32_asm:i32_store(2, ?CTX_E_OFFSET))/binary
    >>,
    emit(State0, Code).

%%=============================================================================
%% Continuation and module index
%%=============================================================================

set_continuation_to_label(State0, Label) ->
    emit_set_continuation_for_label(State0, Label).

set_continuation_to_offset(State0) ->
    %% Allocate a continuation label (WASM needs a real label, not a code offset).
    ContLabel = State0#state.labels_count + 1,
    State1 = record_cont_label(State0, ContLabel),
    State2 = State1#state{labels_count = ContLabel},
    State3 = emit_set_continuation_for_label(State2, ContLabel),
    %% Return a {wasm_cont, ContLabel} tag so add_label can resolve it.
    {State3, {wasm_cont, ContLabel}}.

-spec continuation_entry_point(state()) -> state().
continuation_entry_point(State) ->
    State.

get_module_index(State0) ->
    {State1, Local} = alloc_local(State0),
    %% Load jit_state->module, then load module->module_index
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?JITSTATE_MODULE_OFFSET))/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Local),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, Local}.

%%=============================================================================
%% Scheduling and reductions
%%=============================================================================

record_cont_label(#state{beam_label = undefined} = State, _ContLabel) ->
    State;
record_cont_label(#state{beam_label = BeamLabel, cont_label_map = Map} = State, ContLabel) ->
    State#state{cont_label_map = [{ContLabel, BeamLabel} | Map]}.

decrement_reductions_and_maybe_schedule_next(State0) ->
    %% Split the function: WASM cannot resume mid-function after yielding,
    %% so both paths (yield and non-yield) go through a continuation.
    ContLabel = State0#state.labels_count + 1,
    State0a = record_cont_label(State0, ContLabel),
    State1 = State0a#state{labels_count = ContLabel},

    {State2, TempLocal} = alloc_local(State1),
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_sub())/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_eqz())/binary
    >>,
    State3 = emit(State2, Code),
    State4 = emit(State3, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State5 = emit_set_continuation_for_label(State4, ContLabel),
    State6 = call_primitive_last(State5, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    State7 = emit(State6, jit_wasm32_asm:end_()),
    State8 = free_native_register(State7, TempLocal),
    %% Non-yield path: also go to ContLabel
    State9 = emit_set_continuation_for_label(State8, ContLabel),

    %% Finalize current function, open continuation
    #state{
        func_bodies = FuncBodies,
        current_body = CurrentBody,
        current_label = PrevLabel,
        jump_table_start = JumpTableStart,
        labels = Labels
    } = State9,
    FinalizedBody =
        <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
            (jit_wasm32_asm:return())/binary>>,
    NewFuncBodies = [{PrevLabel, FinalizedBody} | FuncBodies],
    ContLabelOff = JumpTableStart + ContLabel * ?JUMP_TABLE_ENTRY_SIZE,
    AllFree = (1 bsl State9#state.max_scratch) - 1,
    State9#state{
        func_bodies = NewFuncBodies,
        current_body = <<>>,
        current_label = ContLabel,
        labels = [{ContLabel, ContLabelOff} | Labels],
        available_regs = AllFree,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State9#state.regs)
    }.

call_or_schedule_next(State0, Label) ->
    %% WASM: Split the function at the call site.

    %% Allocate a new label number for the continuation
    ContLabel = State0#state.labels_count + 1,
    State0a = record_cont_label(State0, ContLabel),
    State1 = State0a#state{labels_count = ContLabel},

    %% Set CP to point to the continuation label
    State2 = emit_set_cp_for_label(State1, ContLabel),

    %% Do the call (sets continuation for scheduling, then jumps)
    State3 = call_only_or_schedule_next(State2, Label),

    %% Split: close current function, open continuation function
    #state{
        func_bodies = FuncBodies,
        current_body = CurrentBody,
        current_label = PrevLabel,
        jump_table_start = JumpTableStart,
        labels = Labels
    } = State3,
    FinalizedBody =
        <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
            (jit_wasm32_asm:return())/binary>>,
    NewFuncBodies = [{PrevLabel, FinalizedBody} | FuncBodies],
    ContLabelOff = JumpTableStart + ContLabel * ?JUMP_TABLE_ENTRY_SIZE,
    AllFree = (1 bsl State3#state.max_scratch) - 1,
    State3#state{
        func_bodies = NewFuncBodies,
        current_body = <<>>,
        current_label = ContLabel,
        labels = [{ContLabel, ContLabelOff} | Labels],
        available_regs = AllFree,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State3#state.regs)
    }.

call_only_or_schedule_next(State0, Label) ->
    {State1, TempLocal} = alloc_local(State0),
    %% Decrement reductions
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_sub())/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        %% Store back
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        %% Check if zero
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_eqz())/binary
    >>,
    State2 = emit(State1, Code),
    %% If zero: set continuation and schedule
    State3 = emit(State2, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State4 = emit_set_continuation_for_label(State3, Label),
    State5 = call_primitive_last(State4, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    State6 = emit(State5, jit_wasm32_asm:end_()),
    State7 = free_native_register(State6, TempLocal),
    %% If not zero: jump to label
    jump_to_label(State7, Label).

%%=============================================================================
%% Function pointer calls (for BIFs)
%%=============================================================================

call_func_ptr(State0, FuncPtrTuple, Args) ->
    {State1, ResultLocal} = alloc_local(State0),
    State2 = emit_push_args(State1, Args),
    FuncPtrCode =
        case FuncPtrTuple of
            {free, Local} ->
                jit_wasm32_asm:local_get(Local);
            {primitive, Primitive} ->
                <<
                    (jit_wasm32_asm:local_get(?NATIVE_INTERFACE_LOCAL))/binary,
                    (jit_wasm32_asm:i32_load(2, Primitive * 4))/binary
                >>
        end,
    State3 = emit(State2, FuncPtrCode),
    %% call_indirect with the appropriate type for the argument count
    WasmArgCount = count_wasm_args(Args),
    TypeIdx = WasmArgCount - 1,
    State4 = emit(State3, jit_wasm32_asm:call_indirect(TypeIdx, 0)),
    %% Store result
    State5 = emit(State4, jit_wasm32_asm:local_set(ResultLocal)),
    Regs1 = jit_regs:invalidate_all(State0#state.regs),
    State6 = free_func_ptr(State5, FuncPtrTuple),
    {State6#state{regs = Regs1}, ResultLocal}.

%%=============================================================================
%% Labels and metadata
%%=============================================================================

return_labels_and_lines(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        func_bodies = FuncBodies0,
        current_body = CurrentBody,
        current_label = CurLabel,
        labels_count = LabelsCount,
        max_scratch = MaxScratch
    } = State,
    SortedLines
) ->
    %% Save the last label's body
    FuncBodies1 =
        case CurLabel of
            undefined ->
                FuncBodies0;
            _ ->
                FinalizedBody =
                    <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
                        (jit_wasm32_asm:return())/binary>>,
                [{CurLabel, FinalizedBody} | FuncBodies0]
        end,
    %% Sort func_bodies by label number (ascending)
    SortedBodies = lists:keysort(1, [{L, B} || {L, B} <- FuncBodies1, is_integer(L)]),

    WasmModule = assemble_wasm_module(SortedBodies, LabelsCount, MaxScratch),

    WasmOffset = StreamModule:offset(Stream0),
    Stream1 = StreamModule:append(Stream0, WasmModule),

    %% Lines: count:16/LE, (line_ref:16/LE, offset:32/LE)*
    LinesOffset = StreamModule:offset(Stream1),
    LinesTable = <<<<Line:16/little, Offset:32/little>> || {Line, Offset} <- SortedLines>>,
    %% Cont label map: count:16/LE, (cont_label:16/LE, beam_label:16/LE)*
    ContMap = State#state.cont_label_map,
    ContMapTable = <<<<CL:16/little, BL:16/little>> || {CL, BL} <- ContMap>>,
    Stream2 = StreamModule:append(
        Stream1,
        <<
            (length(SortedLines)):16/little,
            LinesTable/binary,
            (length(ContMap)):16/little,
            ContMapTable/binary
        >>
    ),

    %% Patch jump table header: num_entries(4) + wasm_offset(4) + lines_offset(4)
    JumpTableDataStart = State#state.jump_table_start - 12,
    NumEntries = LabelsCount + 1,
    Stream3 = StreamModule:replace(Stream2, JumpTableDataStart, <<NumEntries:32/little>>),
    WasmOffsetPos = JumpTableDataStart + 4,
    RelativeWasmOffset = WasmOffset - JumpTableDataStart,
    Stream4 = StreamModule:replace(Stream3, WasmOffsetPos, <<RelativeWasmOffset:32/little>>),
    LinesOffsetPos = JumpTableDataStart + 8,
    RelativeLinesOffset = LinesOffset - JumpTableDataStart,
    Stream5 = StreamModule:replace(Stream4, LinesOffsetPos, <<RelativeLinesOffset:32/little>>),

    State#state{
        stream = Stream5,
        current_body = <<>>,
        current_label = undefined
    }.

-spec add_label(state(), integer() | reference()) -> state().
add_label(State0, Label) ->
    add_label(State0, Label, undefined).

-spec add_label(state(), integer() | reference(), integer() | undefined) -> state().
add_label(
    #state{
        jump_table_start = JumpTableStart,
        func_bodies = FuncBodies,
        current_body = CurrentBody,
        current_label = PrevLabel,
        labels = Labels
    } = State,
    Label,
    _LabelOffset
) when is_integer(Label) ->
    %% Save the previous label's body (if any)
    NewFuncBodies =
        case PrevLabel of
            undefined ->
                FuncBodies;
            _ ->
                %% Finalize previous body: set continuation to the NEXT label
                %% (fall-through) and return ctx. Without this, the dispatch
                %% loop sees NULL continuation and goes to schedule_in.
                FinalizedBody =
                    <<CurrentBody/binary, (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
                        (jit_wasm32_asm:i32_const(Label + 1))/binary,
                        (jit_wasm32_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary,
                        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
                        (jit_wasm32_asm:return())/binary>>,
                [{PrevLabel, FinalizedBody} | FuncBodies]
        end,
    %% Compute label offset (= position in jump table data)
    LabelOff = JumpTableStart + Label * ?JUMP_TABLE_ENTRY_SIZE,
    Regs1 = jit_regs:invalidate_all(State#state.regs),
    State#state{
        func_bodies = NewFuncBodies,
        current_body = <<>>,
        current_label = Label,
        labels = [{Label, LabelOff} | Labels],
        regs = Regs1,
        beam_label = Label
    };
add_label(
    #state{
        jump_table_start = JumpTableStart,
        func_bodies = FuncBodies,
        current_body = CurrentBody,
        current_label = PrevLabel,
        labels = Labels
    } = State,
    {wasm_cont, ContLabel},
    _Offset
) ->
    %% Continuation label: split the function here.
    NewFuncBodies =
        case PrevLabel of
            undefined ->
                FuncBodies;
            _ ->
                FinalizedBody =
                    <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
                        (jit_wasm32_asm:return())/binary>>,
                [{PrevLabel, FinalizedBody} | FuncBodies]
        end,
    ContLabelOff = JumpTableStart + ContLabel * ?JUMP_TABLE_ENTRY_SIZE,
    Regs1 = jit_regs:invalidate_all(State#state.regs),
    AllFree = (1 bsl State#state.max_scratch) - 1,
    State#state{
        func_bodies = NewFuncBodies,
        current_body = <<>>,
        current_label = ContLabel,
        labels = [{ContLabel, ContLabelOff} | Labels],
        available_regs = AllFree,
        used_regs = 0,
        regs = Regs1
    };
add_label(
    #state{
        jump_table_start = JumpTableStart,
        labels = Labels
    } = State,
    Label,
    _Offset
) ->
    %% Reference labels - record offset for branch resolution
    %% Use a computed offset based on the current label
    LabelOff =
        case State#state.current_label of
            undefined ->
                JumpTableStart;
            CurLabel when is_integer(CurLabel) ->
                JumpTableStart + CurLabel * ?JUMP_TABLE_ENTRY_SIZE;
            _ ->
                JumpTableStart
        end,
    State#state{labels = [{Label, LabelOff} | Labels]}.

get_regs_tracking(#state{regs = Regs}) -> Regs.

%%=============================================================================
%% Internal helpers
%%=============================================================================

emit(#state{current_body = Body} = State, Code) ->
    State#state{current_body = <<Body/binary, Code/binary>>}.

%% Allocate a scratch local, extending the pool if exhausted.
alloc_local(#state{available_regs = 0, used_regs = Used, max_scratch = MaxScratch} = State) ->
    LocalIdx = ?FIRST_SCRATCH_LOCAL + MaxScratch,
    Bit = 1 bsl MaxScratch,
    LocalAtom = index_to_local(LocalIdx),
    {
        State#state{
            used_regs = Used bor Bit,
            max_scratch = MaxScratch + 1
        },
        LocalAtom
    };
alloc_local(#state{available_regs = Available, used_regs = Used} = State) ->
    LocalIdx = first_avail_local(Available),
    Bit = local_bit(LocalIdx),
    LocalAtom = index_to_local(LocalIdx),
    {
        State#state{
            available_regs = Available band (bnot Bit),
            used_regs = Used bor Bit
        },
        LocalAtom
    }.

%% Get the first available local from bitmask
first_avail_local(Mask) ->
    first_avail_local(Mask, 0).
first_avail_local(Mask, N) ->
    case Mask band (1 bsl N) of
        0 -> first_avail_local(Mask, N + 1);
        _ -> N + ?FIRST_SCRATCH_LOCAL
    end.

%% Convert bitmask to list of local indices
mask_to_locals(Mask) ->
    mask_to_locals(Mask, 0, []).
mask_to_locals(0, _N, Acc) ->
    lists:reverse(Acc);
mask_to_locals(Mask, N, Acc) ->
    case Mask band 1 of
        1 -> mask_to_locals(Mask bsr 1, N + 1, [index_to_local(N + ?FIRST_SCRATCH_LOCAL) | Acc]);
        0 -> mask_to_locals(Mask bsr 1, N + 1, Acc)
    end.

is_scratch_local(Atom) when is_atom(Atom) ->
    case atom_to_list(Atom) of
        "local" ++ Rest ->
            try
                list_to_integer(Rest) >= ?FIRST_SCRATCH_LOCAL
            catch
                error:badarg -> false
            end;
        _ ->
            false
    end;
is_scratch_local(_) ->
    false.

unwrap_local({ptr, L}) -> unwrap_local(L);
unwrap_local({free, L}) -> unwrap_local(L);
unwrap_local(L) when is_atom(L) -> L.

index_to_local(N) when is_integer(N) ->
    list_to_atom("local" ++ integer_to_list(N)).

local_bit(LocalAtom) when is_atom(LocalAtom) ->
    local_bit(jit_wasm32_asm:local_index(LocalAtom));
local_bit(LocalIdx) when is_integer(LocalIdx), LocalIdx >= ?FIRST_SCRATCH_LOCAL ->
    1 bsl (LocalIdx - ?FIRST_SCRATCH_LOCAL).

merge_used_regs(#state{used_regs = UR, max_scratch = MS} = State, OtherUR) ->
    MergedUR = UR bor OtherUR,
    AllFree = (1 bsl MS) - 1,
    MergedAvail = AllFree band (bnot MergedUR),
    State#state{used_regs = MergedUR, available_regs = MergedAvail}.

emit_value_to_stack(cp) ->
    <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_CP_OFFSET))/binary
    >>;
emit_value_to_stack({free, Local}) when is_atom(Local) ->
    jit_wasm32_asm:local_get(Local);
emit_value_to_stack({free, Imm}) when is_integer(Imm) ->
    jit_wasm32_asm:i32_const(to_i32(Imm));
emit_value_to_stack({x_reg, N}) ->
    %% Load ctx->x[N]
    Offset = ?CTX_X_OFFSET + N * 4,
    <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, Offset))/binary
    >>;
emit_value_to_stack({y_reg, N}) ->
    %% Load ctx->e[N]
    <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:i32_load(2, N * 4))/binary
    >>;
emit_value_to_stack({ptr, Local}) ->
    <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary
    >>;
emit_value_to_stack(Local) when is_atom(Local) ->
    jit_wasm32_asm:local_get(Local);
emit_value_to_stack(Imm) when is_integer(Imm) ->
    jit_wasm32_asm:i32_const(to_i32(Imm)).

%% Set jit_state->continuation = Label + 1 (0 means NULL/unset).
emit_set_continuation_for_label(#state{} = State, Label) when is_integer(Label) ->
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_const(Label + 1))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>,
    emit(State, Code);
emit_set_continuation_for_label(State, _RefLabel) ->
    %% Unresolved reference label: store 0 (NULL).
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_const(0))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>,
    emit(State, Code).

%% Emit code to set up CP (continuation pointer for returns).
%% CP format: (module_index << 24) | (label_offset << 2)
emit_set_cp_for_label(State0, Label) ->
    LabelOffset = Label * ?JUMP_TABLE_ENTRY_SIZE,
    emit_set_cp_for_offset(State0, LabelOffset).

emit_set_cp_for_offset(State0, LabelOffset) ->
    {State1, ModIdxLocal} = get_module_index(State0),
    State2 = shift_left(State1, ModIdxLocal, 24),
    {State3, OffsetLocal} = alloc_local(State2),
    Code = <<
        (jit_wasm32_asm:i32_const(LabelOffset bsl 2))/binary,
        (jit_wasm32_asm:local_set(OffsetLocal))/binary
    >>,
    State4 = emit(State3, Code),
    State5 = or_(State4, ModIdxLocal, OffsetLocal),
    State6 = free_native_register(State5, OffsetLocal),
    %% Store CP to ctx->cp
    Code2 = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(ModIdxLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_CP_OFFSET))/binary
    >>,
    State7 = emit(State6, Code2),
    free_native_register(State7, ModIdxLocal).

emit_call_primitive(State0, Primitive, Args, ResultLocal, IsTailCall) ->
    WasmArgCount = count_wasm_args(Args),
    State1 = emit_push_args(State0, Args),
    %% Load function pointer from native_interface[Primitive]
    Code = <<
        (jit_wasm32_asm:local_get(?NATIVE_INTERFACE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, Primitive * 4))/binary
    >>,
    State2 = emit(State1, Code),
    IsVoid = primitive_returns_void(Primitive),
    TypeIdx =
        case primitive_special_type(Primitive) of
            none ->
                BaseTypeIdx = WasmArgCount - 1,
                case IsVoid of
                    true -> BaseTypeIdx + ?VOID_TYPE_BASE;
                    false -> BaseTypeIdx
                end;
            SpecialTypeIdx ->
                SpecialTypeIdx
        end,
    State3 = emit(State2, jit_wasm32_asm:call_indirect(TypeIdx, 0)),
    case {IsTailCall, IsVoid} of
        {true, false} ->
            %% Tail call with i32 return: return the result directly
            emit(State3, jit_wasm32_asm:return());
        {true, true} ->
            %% Tail call with void return: return ctx
            State4 = emit(State3, jit_wasm32_asm:local_get(?CTX_LOCAL)),
            emit(State4, jit_wasm32_asm:return());
        {false, false} ->
            %% Non-tail call with i32 return: store result in local
            emit(State3, jit_wasm32_asm:local_set(ResultLocal));
        {false, true} ->
            %% Non-tail call with void return: push dummy value and store
            State4 = emit(State3, jit_wasm32_asm:i32_const(0)),
            emit(State4, jit_wasm32_asm:local_set(ResultLocal))
    end.

emit_push_args(State, []) ->
    State;
emit_push_args(State0, [Arg | Rest]) ->
    Code =
        case Arg of
            ctx ->
                jit_wasm32_asm:local_get(?CTX_LOCAL);
            jit_state ->
                jit_wasm32_asm:local_get(?JITSTATE_LOCAL);
            jit_state_tail_call ->
                jit_wasm32_asm:local_get(?JITSTATE_LOCAL);
            offset ->
                LabelOffset =
                    case State0#state.current_label of
                        undefined -> 0;
                        CurLabel -> CurLabel * ?JUMP_TABLE_ENTRY_SIZE
                    end,
                jit_wasm32_asm:i32_const(LabelOffset);
            stack ->
                jit_wasm32_asm:i32_const(0);
            {free, {ptr, Local}} ->
                emit_value_to_stack({ptr, Local});
            {free, {x_reg, N}} ->
                emit_value_to_stack({x_reg, N});
            {free, {y_reg, N}} ->
                emit_value_to_stack({y_reg, N});
            {free, Local} when is_atom(Local) ->
                jit_wasm32_asm:local_get(Local);
            {free, Imm} when is_integer(Imm) ->
                jit_wasm32_asm:i32_const(to_i32(Imm));
            {ptr, Local} ->
                jit_wasm32_asm:local_get(Local);
            {avm_int64_t, Val} ->
                jit_wasm32_asm:i64_const(Val);
            {x_reg, N} ->
                emit_value_to_stack({x_reg, N});
            {y_reg, N} ->
                emit_value_to_stack({y_reg, N});
            Local when is_atom(Local) -> jit_wasm32_asm:local_get(Local);
            Local when is_integer(Local) -> jit_wasm32_asm:i32_const(to_i32(Local))
        end,
    State1 = emit(State0, Code),
    emit_push_args(State1, Rest).

%% Emit a condition test, leaving the result (i32 0 or 1) on the stack
emit_condition(State0, {Local, '<', 0}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(0))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '<', Val}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(to_i32(Val)))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '<', OtherLocal}) when is_atom(OtherLocal) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:local_get(OtherLocal))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Val, '<', Local}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (jit_wasm32_asm:i32_const(to_i32(Val)))/binary,
        (jit_wasm32_asm:local_get(L))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '==', Val}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(to_i32(Val)))/binary,
        (jit_wasm32_asm:i32_eq())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '!=', Val}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(to_i32(Val)))/binary,
        (jit_wasm32_asm:i32_ne())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '!=', OtherLocal}) when is_atom(OtherLocal) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:local_get(OtherLocal))/binary,
        (jit_wasm32_asm:i32_ne())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {'(int)', Local, '==', Val}) ->
    emit_condition(State0, {Local, '==', Val});
emit_condition(State0, {'(int)', Local, '!=', Val}) ->
    emit_condition(State0, {Local, '!=', Val});
emit_condition(State0, {'(bool)', Local, '==', false}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:i32_eqz())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {'(bool)', Local, '!=', false}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_and())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '&', Mask, '!=', Val}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(to_i32(Mask)))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:i32_const(to_i32(Val)))/binary,
        (jit_wasm32_asm:i32_ne())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {{free, L1}, '==', {free, L2}}) ->
    Code = <<
        (emit_unwrapped_to_stack(L1))/binary,
        (emit_unwrapped_to_stack(L2))/binary,
        (jit_wasm32_asm:i32_eq())/binary
    >>,
    State1 = free_native_register(State0, L1),
    State2 = free_native_register(State1, L2),
    emit(State2, Code).

emit_and_conditions(State0, []) ->
    %% Push 1 (true) if no conditions
    emit(State0, jit_wasm32_asm:i32_const(1));
emit_and_conditions(State0, [Cond]) ->
    emit_condition(State0, Cond);
emit_and_conditions(State0, [Cond | Rest]) ->
    State1 = emit_condition(State0, Cond),
    State2 = emit_and_conditions(State1, Rest),
    emit(State2, jit_wasm32_asm:i32_and()).

unwrap_free({free, L}) -> L;
unwrap_free(L) -> L.

%% Truncate to signed i32 (host Erlang uses 64-bit terms).
to_i32(Val) when is_integer(Val) ->
    Masked = Val band 16#FFFFFFFF,
    case Masked > 16#7FFFFFFF of
        true -> Masked - 16#100000000;
        false -> Masked
    end.

emit_unwrapped_to_stack(L) when is_atom(L) ->
    jit_wasm32_asm:local_get(L);
emit_unwrapped_to_stack(Imm) when is_integer(Imm) ->
    jit_wasm32_asm:i32_const(to_i32(Imm)).

%% Must be kept in sync with ModuleNativeInterface in jit.h.
primitive_returns_void(?PRIM_TRIM_LIVE_REGS) -> true;
primitive_returns_void(?PRIM_MAILBOX_REMOVE_MESSAGE) -> true;
primitive_returns_void(?PRIM_TIMEOUT) -> true;
primitive_returns_void(?PRIM_MAILBOX_NEXT) -> true;
primitive_returns_void(?PRIM_CANCEL_TIMEOUT) -> true;
primitive_returns_void(?PRIM_CLEAR_TIMEOUT_FLAG) -> true;
primitive_returns_void(?PRIM_CONTEXT_ENSURE_FPREGS) -> true;
primitive_returns_void(?PRIM_TERM_CONV_TO_FLOAT) -> true;
primitive_returns_void(?PRIM_FNEGATE) -> true;
primitive_returns_void(?PRIM_BITSTRING_COPY_MODULE_STR) -> true;
primitive_returns_void(?PRIM_FREE) -> true;
primitive_returns_void(_) -> false.

%% Must be kept in sync with ModuleNativeInterface in jit.h.
primitive_special_type(?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT) -> ?SPECIAL_TYPE_BASE + 0;
primitive_special_type(_) -> none.

count_wasm_args(Args) ->
    length(Args).

maybe_free(State, {free, Local}) -> free_native_register(State, Local);
maybe_free(State, _) -> State.

free_func_ptr(State, {free, Local}) -> free_native_register(State, Local);
free_func_ptr(State, _) -> State.

%%=============================================================================
%% WASM module assembly
%%
%% Assembles accumulated per-label function bodies into a complete WASM module.
%%
%% Type indices are defined by the macros MAX_PRIMITIVE_ARGS, VOID_TYPE_BASE,
%% and SPECIAL_TYPE_BASE. See their definitions above.
%%=============================================================================

assemble_wasm_module(SortedBodies, LabelsCount, MaxScratch) ->
    I32ReturnTypes = [
        jit_wasm32_asm:encode_func_type(
            lists:duplicate(N, jit_wasm32_asm:type_i32()), [jit_wasm32_asm:type_i32()]
        )
     || N <- lists:seq(1, ?MAX_PRIMITIVE_ARGS)
    ],
    VoidReturnTypes = [
        jit_wasm32_asm:encode_func_type(
            lists:duplicate(N, jit_wasm32_asm:type_i32()), []
        )
     || N <- lists:seq(1, ?MAX_PRIMITIVE_ARGS)
    ],
    SpecialTypes = [
        jit_wasm32_asm:encode_func_type([jit_wasm32_asm:type_i32(), jit_wasm32_asm:type_i64()], [
            jit_wasm32_asm:type_i32()
        ])
    ],
    Types = I32ReturnTypes ++ VoidReturnTypes ++ SpecialTypes,
    TypeSection = jit_wasm32_asm:encode_type_section(Types),

    MemoryImport = <<
        (jit_wasm32_asm:encode_name("env"))/binary,
        (jit_wasm32_asm:encode_name("memory"))/binary,
        %% import kind: memory
        16#02,
        %% limits: shared + has max (required for -pthread Emscripten builds)
        16#03,
        %% min pages
        (jit_wasm32_asm:encode_uleb128(256))/binary,
        %% max pages (65536 = 4GB max)
        (jit_wasm32_asm:encode_uleb128(65536))/binary
    >>,
    TableImport = <<
        (jit_wasm32_asm:encode_name("env"))/binary,
        (jit_wasm32_asm:encode_name("__indirect_function_table"))/binary,
        %% import kind: table
        16#01,
        (jit_wasm32_asm:type_funcref())/binary,
        %% limits: min only
        16#00,
        %% min entries
        (jit_wasm32_asm:encode_uleb128(0))/binary
    >>,
    ImportSection = jit_wasm32_asm:encode_section(
        2, jit_wasm32_asm:encode_vector([MemoryImport, TableImport])
    ),

    %% Function section: all functions use type 2 (3 i32 args -> i32)
    NumFunctions = LabelsCount + 1,
    FunctionTypeIndices = lists:duplicate(NumFunctions, 2),
    FunctionSection = jit_wasm32_asm:encode_function_section(FunctionTypeIndices),

    %% Export section: "f0", "f1", ...
    Exports = lists:map(
        fun(LabelIdx) ->
            FuncName = "f" ++ integer_to_list(LabelIdx),
            <<
                (jit_wasm32_asm:encode_name(FuncName))/binary,
                %% export kind: function
                16#00,
                %% function index
                (jit_wasm32_asm:encode_uleb128(LabelIdx))/binary
            >>
        end,
        lists:seq(0, NumFunctions - 1)
    ),
    ExportSection = jit_wasm32_asm:encode_export_section(Exports),

    %% Code section
    ScratchLocals = [{MaxScratch, jit_wasm32_asm:type_i32()}],
    FuncBodiesEncoded = build_func_bodies(SortedBodies, 0, NumFunctions, ScratchLocals),
    CodeSection = jit_wasm32_asm:encode_code_section(FuncBodiesEncoded),

    <<
        (jit_wasm32_asm:wasm_magic())/binary,
        (jit_wasm32_asm:wasm_version())/binary,
        TypeSection/binary,
        ImportSection/binary,
        FunctionSection/binary,
        ExportSection/binary,
        CodeSection/binary
    >>.

%% Labels without compiled code get a stub returning ctx.
build_func_bodies(SortedBodies, Idx, NumFunctions, ScratchLocals) ->
    build_func_bodies(SortedBodies, Idx, NumFunctions, ScratchLocals, []).

build_func_bodies(_SortedBodies, Idx, NumFunctions, _ScratchLocals, Acc) when
    Idx >= NumFunctions
->
    lists:reverse(Acc);
build_func_bodies(SortedBodies, Idx, NumFunctions, ScratchLocals, Acc) ->
    Body =
        case lists:keyfind(Idx, 1, SortedBodies) of
            {Idx, InstrBytes} ->
                Expr = <<InstrBytes/binary, (jit_wasm32_asm:end_())/binary>>,
                jit_wasm32_asm:encode_func_body(ScratchLocals, Expr);
            false ->
                %% Stub: return ctx
                StubExpr = <<
                    (jit_wasm32_asm:local_get(0))/binary,
                    (jit_wasm32_asm:end_())/binary
                >>,
                jit_wasm32_asm:encode_func_body(ScratchLocals, StubExpr)
        end,
    build_func_bodies(SortedBodies, Idx + 1, NumFunctions, ScratchLocals, [Body | Acc]).
