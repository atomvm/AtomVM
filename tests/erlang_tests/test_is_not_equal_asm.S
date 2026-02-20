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

%% BEAM assembly file to test is_ne (OP_IS_NOT_EQUAL, opcode 42).
%% Modern OTP compilers never generate is_ne -- they use is_eq with inverted
%% branch logic instead.  This file exercises the JIT code path for
%% OP_IS_NOT_EQUAL.

{module, test_is_not_equal_asm}.

{exports, [
    {is_ne_test, 2}
]}.

{attributes, []}.

{labels, 5}.

%% is_ne_test(A, B) ->
%%   Tests A /= B using the is_ne opcode.
%%   Returns 'not_equal' if A /= B, 'equal' if A == B.
{function, is_ne_test, 2, 2}.
{label, 1}.
{func_info, {atom, test_is_not_equal_asm}, {atom, is_ne_test}, 2}.
{label, 2}.
{test, is_ne, {f, 3}, [{x, 0}, {x, 1}]}.
%% If we reach here, A /= B (the test did not jump)
{move, {atom, not_equal}, {x, 0}}.
return.
{label, 3}.
%% A == B (the test jumped because /= was false)
{move, {atom, equal}, {x, 0}}.
return.
