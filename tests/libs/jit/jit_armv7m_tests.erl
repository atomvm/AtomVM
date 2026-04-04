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

%% @doc Backend-level tests for jit_armv6m with the thumb2 variant enabled.
%% These tests exercise Thumb-2 specific code paths in the backend:
%% jump table entries, mov_immediate, and branch generation.

-module(jit_armv7m_tests).

-include_lib("eunit/include/eunit.hrl").

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_armv6m).
-define(THUMB2_VARIANT, ?JIT_VARIANT_PIC bor ?JIT_VARIANT_THUMB2).

%% Jump table entries are 6 bytes with Thumb-2 (push + b.w)
%% vs 12 bytes with Thumb-1 (ldr + push + add pc + nop + literal)
jump_table_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual((512 + 1) * 6, byte_size(Stream)).

%% mov_immediate with Thumb-2: values 256-65535 use movw (4 bytes)
%% instead of Thumb-1 movs+adds (4 bytes) or ldr from literal pool.
%% The third argument to call_primitive goes into r2.
mov_immediate_thumb2_movw_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state, 1000]),
    Stream = ?BACKEND:stream(State1),
    MovwR2_1000 = jit_armv7m_asm:movw(r2, 1000),
    ?assertNotEqual(nomatch, binary:match(Stream, MovwR2_1000)).

%% mov_immediate with Thumb-2: 32-bit values use movw+movt (8 bytes)
%% instead of ldr from literal pool (2 bytes + 4 bytes data, needs pool flush).
mov_immediate_thumb2_movw_movt_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state, 16#12345678]),
    Stream = ?BACKEND:stream(State1),
    MovwR2_Lo = jit_armv7m_asm:movw(r2, 16#5678),
    MovtR2_Hi = jit_armv7m_asm:movt(r2, 16#1234),
    MovwMovt = <<MovwR2_Lo/binary, MovtR2_Hi/binary>>,
    ?assertNotEqual(nomatch, binary:match(Stream, MovwMovt)).

%% Verify that call_primitive produces the same instructions as Thumb-1
%% for small values (movs path is shared)
call_primitive_0_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6817      	ldr	r7, [r2, #0]\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	9902      	ldr	r1, [sp, #8]\n"
            "   6:	47b8      	blx	r7\n"
            "   8:	4607      	mov	r7, r0\n"
            "   a:	bc05      	pop	{r0, r2}"
        >>,
    ?assertStream(arm_thumb2, Dump, Stream).
