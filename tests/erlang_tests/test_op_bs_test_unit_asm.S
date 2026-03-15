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

%% BEAM assembly file to test the bs_test_unit opcode.
%% Modern OTP compilers (26+) fold bs_test_unit into the bs_match opcode,
%% so we need hand-written assembly to exercise this code path.

{module, test_op_bs_test_unit_asm}.

{exports, [
    {get_tail_if_byte_aligned, 1},
    {get_tail_unit_16, 1}
]}.

{attributes, []}.

{labels, 9}.

%% get_tail_if_byte_aligned(Bin) ->
%%   Start a match, verify the remaining bits are byte-aligned (unit=8),
%%   then return the tail. Returns 'error' on failure.
{function, get_tail_if_byte_aligned, 1, 2}.
{label, 1}.
{func_info, {atom, test_op_bs_test_unit_asm}, {atom, get_tail_if_byte_aligned}, 1}.
{label, 2}.
{test, bs_start_match3, {f, 3}, 1, [{x, 0}], {x, 1}}.
{test, bs_test_unit, {f, 3}, [{x, 1}, 8]}.
{bs_get_tail, {x, 1}, {x, 0}, 2}.
return.
{label, 3}.
{move, {atom, error}, {x, 0}}.
return.

%% get_tail_unit_16(Bin) ->
%%   Start a match, skip 8 bits, verify remaining bits are 16-bit aligned,
%%   then return the tail. Returns 'error' on failure.
{function, get_tail_unit_16, 1, 5}.
{label, 4}.
{func_info, {atom, test_op_bs_test_unit_asm}, {atom, get_tail_unit_16}, 1}.
{label, 5}.
{test, bs_start_match3, {f, 6}, 1, [{x, 0}], {x, 1}}.
{test, bs_skip_bits2, {f, 6}, [{x, 1}, {integer, 8}, 1, {field_flags, [unsigned, big]}]}.
{test, bs_test_unit, {f, 6}, [{x, 1}, 16]}.
{bs_get_tail, {x, 1}, {x, 0}, 2}.
return.
{label, 6}.
{move, {atom, error}, {x, 0}}.
return.
