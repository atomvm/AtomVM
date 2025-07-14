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
{module, test_op_bs_start_match_asm}.

{exports, [
    {bs_start_match3, 2},
    {bs_start_match4_label, 2},
    {bs_start_match4_nofail_resume, 1}
]}.

{function, bs_start_match3, 2, 2}.
{label, 1}.
{func_info, {atom, test_op_bs_start_match_asm}, {atom, bs_start_match3}, 2}.
{label, 2}.
{test, bs_start_match3, {f, 3}, 2, [{x, 0}], {x, 2}}.
{test, bs_start_match3, {f, 4}, 3, [{x, 1}], {x, 2}}.
{move, {integer, 2}, {x, 0}}.
return.
{label, 3}.
{move, {integer, 3}, {x, 0}}.
return.
{label, 4}.
{test, bs_test_tail2, {f, 5}, [{x, 2}, 24]}.
{bs_get_tail, {x, 2}, {x, 0}, 3}.
return.
{label, 5}.
{move, {integer, 5}, {x, 0}}.
return.

{function, bs_start_match4_label, 2, 7}.
{label, 6}.
{func_info, {atom, test_op_bs_start_match_asm}, {atom, bs_start_match4_label}, 2}.
{label, 7}.
{bs_start_match4, {f, 8}, 2, {x, 0}, {x, 2}}.
{bs_start_match4, {f, 9}, 3, {x, 1}, {x, 2}}.
{move, {integer, 2}, {x, 0}}.
return.
{label, 8}.
{move, {integer, 3}, {x, 0}}.
return.
{label, 9}.
{test, bs_test_tail2, {f, 10}, [{x, 2}, 24]}.
{bs_get_tail, {x, 2}, {x, 0}, 3}.
return.
{label, 10}.
{move, {integer, 5}, {x, 0}}.
return.

{function, bs_start_match4_nofail_resume, 1, 12}.
{label, 11}.
{func_info, {atom, test_op_bs_start_match_asm}, {atom, bs_start_match4_nofail_resume}, 1}.
{label, 12}.
{bs_start_match4, {atom, no_fail}, 1, {x, 0}, {x, 1}}.
{move, {x, 1}, {x, 2}}.
{move, nil, {x, 1}}.
{bs_start_match4, {atom, resume}, 3, {x, 2}, {x, 1}}.
{move, {x, 1}, {x, 2}}.
{test, bs_test_tail2, {f, 13}, [{x, 2}, 24]}.
{bs_get_tail, {x, 2}, {x, 0}, 3}.
return.
{label, 13}.
{move, {integer, 13}, {x, 0}}.
return.
