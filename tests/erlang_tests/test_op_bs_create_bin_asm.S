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

%% version = 0
{module, test_op_bs_create_bin_asm}.

{exports, [
    {bs_create_bin_utf8_no_fail, 3},
    {bs_create_bin_utf8_fail, 3},
    {bs_create_bin_utf16_no_fail, 3},
    {bs_create_bin_utf16_fail, 3},
    {bs_create_bin_utf32_no_fail, 3},
    {bs_create_bin_utf32_fail, 3},
    {bs_create_bin_integer_no_fail, 3},
    {bs_create_bin_integer_fail, 3},
    {bs_create_bin_binary_no_fail, 3},
    {bs_create_bin_binary_fail, 3},
    {bs_create_bin_alloc_list, 1}
]}.

{attributes, []}.

{labels, 28}.

{function, bs_create_bin_utf8_no_fail, 3, 2}.
{label, 1}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_utf8_no_fail}, 3}.
{label, 2}.
{bs_create_bin, {f, 0}, 0, 1, 8, {x, 0}, {list, [{atom, utf8}, 1, 0, {x, 1}, {x, 0}, {x, 2}]}}.
return.

{function, bs_create_bin_utf8_fail, 3, 5}.
{label, 4}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_utf8_fail}, 3}.
{label, 5}.
{bs_create_bin, {f, 6}, 0, 1, 8, {x, 0}, {list, [{atom, utf8}, 1, 0, {x, 1}, {x, 0}, {x, 2}]}}.
return.
{label, 6}.
{move, {atom, fail}, {x, 0}}.
return.

{function, bs_create_bin_utf16_no_fail, 3, 8}.
{label, 7}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_utf16_no_fail}, 3}.
{label, 8}.
{bs_create_bin, {f, 0}, 0, 1, 8, {x, 0}, {list, [{atom, utf16}, 1, 0, {x, 1}, {x, 0}, {x, 2}]}}.
return.

{function, bs_create_bin_utf16_fail, 3, 10}.
{label, 9}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_utf16_fail}, 3}.
{label, 10}.
{bs_create_bin, {f, 11}, 0, 1, 8, {x, 0}, {list, [{atom, utf16}, 1, 0, {x, 1}, {x, 0}, {x, 2}]}}.
return.
{label, 11}.
{move, {atom, fail}, {x, 0}}.
return.

{function, bs_create_bin_utf32_no_fail, 3, 13}.
{label, 12}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_utf32_no_fail}, 3}.
{label, 13}.
{bs_create_bin, {f, 0}, 0, 1, 8, {x, 0}, {list, [{atom, utf32}, 1, 0, {x, 1}, {x, 0}, {x, 2}]}}.
return.

{function, bs_create_bin_utf32_fail, 3, 15}.
{label, 14}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_utf32_fail}, 3}.
{label, 15}.
{bs_create_bin, {f, 16}, 0, 1, 8, {x, 0}, {list, [{atom, utf32}, 1, 0, {x, 1}, {x, 0}, {x, 2}]}}.
return.
{label, 16}.
{move, {atom, fail}, {x, 0}}.
return.

{function, bs_create_bin_integer_no_fail, 3, 18}.
{label, 17}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_integer_no_fail}, 3}.
{label, 18}.
{bs_create_bin, {f, 0}, 0, 1, 8, {x, 0}, {list, [{atom, integer}, 1, 8, {x, 1}, {x, 0}, {x, 2}]}}.
return.

{function, bs_create_bin_integer_fail, 3, 20}.
{label, 19}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_integer_fail}, 3}.
{label, 20}.
{bs_create_bin, {f, 21}, 0, 1, 8, {x, 0}, {list, [{atom, integer}, 1, 8, {x, 1}, {x, 0}, {x, 2}]}}.
return.
{label, 21}.
{move, {atom, fail}, {x, 0}}.
return.

{function, bs_create_bin_binary_no_fail, 3, 23}.
{label, 22}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_binary_no_fail}, 3}.
{label, 23}.
{bs_create_bin, {f, 0}, 0, 1, 8, {x, 0}, {list, [{atom, binary}, 1, 8, {x, 1}, {x, 0}, {x, 2}]}}.
return.

{function, bs_create_bin_binary_fail, 3, 25}.
{label, 24}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_binary_fail}, 3}.
{label, 25}.
{bs_create_bin, {f, 26}, 0, 1, 8, {x, 0}, {list, [{atom, binary}, 1, 8, {x, 1}, {x, 0}, {x, 2}]}}.
return.
{label, 26}.
{move, {atom, fail}, {x, 0}}.
return.

{function, bs_create_bin_alloc_list, 1, 28}.
{label, 27}.
{func_info, {atom, test_op_bs_create_bin_asm}, {atom, bs_create_bin_alloc_list}, 1}.
{label, 28}.
%% Test bs_create_bin with allocator list instead of simple literal
{bs_create_bin, {f, 0}, {alloc, [{words, 2}, {floats, 1}, {funs, 1}]}, 1, 1, {x, 0},
    {list, [{atom, integer}, 1, 1, nil, {x, 0}, {integer, 32}]}}.
return.
