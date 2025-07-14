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

-module(test_op_bs_start_match).

-export([start/0]).

start() ->
    HasBSStartMatch4 =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(otp_release) >= "23";
            "ATOM" ->
                % If code was compiled with OTP < 23, we won't have bs_start_match asm file
                ?OTP_RELEASE >= 23
        end,
    ok =
        if
            HasBSStartMatch4 ->
                ok = test_bs_start_match3_fail_label(),
                ok = test_bs_start_match4_fail_label(),
                ok = test_bs_start_match4_nofail_resume();
            true ->
                ok
        end,
    0.

test_bs_start_match3_fail_label() ->
    2 = test_op_bs_start_match_asm:bs_start_match3(<<"foo">>, <<"bar">>),
    2 = test_op_bs_start_match_asm:bs_start_match3(<<"foo">>, <<>>),
    3 = test_op_bs_start_match_asm:bs_start_match3(foo, bar),
    <<"foo">> = test_op_bs_start_match_asm:bs_start_match3(<<"foo">>, bar),
    5 = test_op_bs_start_match_asm:bs_start_match3(<<"foot">>, bar),
    ok.

test_bs_start_match4_fail_label() ->
    2 = test_op_bs_start_match_asm:bs_start_match4_label(<<"foo">>, <<"bar">>),
    2 = test_op_bs_start_match_asm:bs_start_match4_label(<<"foo">>, <<>>),
    3 = test_op_bs_start_match_asm:bs_start_match4_label(foo, bar),
    <<"foo">> = test_op_bs_start_match_asm:bs_start_match4_label(<<"foo">>, bar),
    5 = test_op_bs_start_match_asm:bs_start_match4_label(<<"foot">>, bar),
    ok.

% Other arguments will crash VM
test_bs_start_match4_nofail_resume() ->
    <<"foo">> = test_op_bs_start_match_asm:bs_start_match4_nofail_resume(<<"foo">>),
    13 = test_op_bs_start_match_asm:bs_start_match4_nofail_resume(<<"foot">>),
    ok.
