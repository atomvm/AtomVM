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

%% BEAM assembly file to test bs_get_binary2 opcode with {atom, all}.
%% Modern OTP compilers never generate bs_get_binary2 with all -- they use
%% bs_get_tail or bs_skip_bits2 instead.  This file exercises the JIT code
%% path at jit.erl line 1347 (Size =:= ?ALL_ATOM).

{module, bs_get_binary2_all_asm}.

{exports, [
    {extract_all_after_skip1, 1}
]}.

{attributes, []}.

{labels, 5}.

%% extract_all_after_skip1(Bin) ->
%%   Skip 1 byte, then get the remaining bytes using
%%   bs_get_binary2 with {atom, all}.
%%   Returns the extracted binary, or the atom 'error' on failure.
{function, extract_all_after_skip1, 1, 2}.
{label, 1}.
{func_info, {atom, bs_get_binary2_all_asm}, {atom, extract_all_after_skip1}, 1}.
{label, 2}.
{bs_start_match4, {atom, no_fail}, 1, {x, 0}, {x, 0}}.
{test, bs_skip_bits2,
      {f, 3},
      [{x, 0}, {integer, 8}, 1, {field_flags, [unsigned, big]}]}.
{test, bs_get_binary2,
      {f, 3},
      1,
      [{x, 0}, {atom, all}, 8, {field_flags, [unsigned, big]}],
      {x, 0}}.
return.
{label, 3}.
{move, {atom, error}, {x, 0}}.
return.
