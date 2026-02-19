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

%% BEAM assembly file to test bs_create_bin with multi-segment binaries
%% where the first segment has a runtime-variable size (setting AccSizeReg),
%% followed by utf8 or float segments.
%%
%% This exercises utf8/float compute_size with AccSizeReg0 =/= undefined.

{module, test_bs_create_bin_accum_asm}.

{exports, [
    {create_int_utf8, 3},
    {create_int_float, 4}
]}.

{attributes, []}.

{labels, 7}.

%% create_int_utf8(IntValue, Utf8Char, IntSizeBytes) ->
%%   <<IntValue:IntSizeBytes/integer-unit:8, Utf8Char/utf8>>
%%   The integer segment has a runtime-variable size (x2), which sets
%%   AccSizeReg in compute_size. The utf8 segment then finds
%%   AccSizeReg =/= undefined.
{function, create_int_utf8, 3, 2}.
{label, 1}.
{func_info, {atom, test_bs_create_bin_accum_asm}, {atom, create_int_utf8}, 3}.
{label, 2}.
{bs_create_bin, {f, 0}, 0, 3, 1, {x, 0},
    {list, [
        {atom, integer}, 1, 8, nil, {x, 0}, {x, 2},
        {atom, utf8}, 2, 0, nil, {x, 1}, {atom, undefined}
    ]}}.
return.

%% create_int_float(IntValue, FloatValue, FloatSizeBits, IntSizeBytes) ->
%%   <<IntValue:IntSizeBytes/integer-unit:8, FloatValue:FloatSizeBits/float-unit:1>>
%%   The integer segment sets AccSizeReg (runtime size in x3).
%%   The float segment has a dynamic size (x2), and AccSizeReg =/= undefined.
{function, create_int_float, 4, 5}.
{label, 4}.
{func_info, {atom, test_bs_create_bin_accum_asm}, {atom, create_int_float}, 4}.
{label, 5}.
{bs_create_bin, {f, 0}, 0, 4, 1, {x, 0},
    {list, [
        {atom, integer}, 1, 8, nil, {x, 0}, {x, 3},
        {atom, float}, 2, 1, nil, {x, 1}, {x, 2}
    ]}}.
return.
