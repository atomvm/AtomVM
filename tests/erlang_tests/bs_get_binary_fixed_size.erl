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

-module(bs_get_binary_fixed_size).

%% Force the compiler to use bs_get_binary2 opcode instead of the
%% newer bs_match opcode (OTP 25+). On older OTP this is ignored.
%% The no_bs_match option was removed in OTP 29.
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE =< 28).
-compile([no_bs_match]).
-endif.
-endif.

-export([start/0]).

start() ->
    Bin = id(<<1, 2, 3, 4, 5, 6, 7, 8>>),
    ok = test_match_4(Bin),
    ok = test_match_3(Bin),
    %% bs_get_binary2_all_asm uses bs_start_match4 which requires OTP 23+
    HasAsmModule =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(otp_release) >= "23";
            "ATOM" ->
                ?OTP_RELEASE >= 23
        end,
    ok =
        if
            HasAsmModule -> test_match_all(Bin);
            true -> ok
        end,
    ok = test_match_fail(Bin),
    0.

test_match_4(Bin) ->
    <<Head:4/binary, Rest/binary>> = Bin,
    4 = byte_size(Head),
    4 = byte_size(Rest),
    <<1, 2, 3, 4>> = Head,
    <<5, 6, 7, 8>> = Rest,
    ok.

test_match_3(Bin) ->
    <<Head:3/binary, Rest/binary>> = Bin,
    3 = byte_size(Head),
    5 = byte_size(Rest),
    <<1, 2, 3>> = Head,
    <<4, 5, 6, 7, 8>> = Rest,
    ok.

test_match_all(Bin) ->
    %% Uses bs_get_binary2 with {atom, all} via hand-written BEAM assembly,
    %% since the compiler generates bs_skip_bits2/bs_get_tail instead.
    <<2, 3, 4, 5, 6, 7, 8>> = bs_get_binary2_all_asm:extract_all_after_skip1(Bin),
    7 = byte_size(bs_get_binary2_all_asm:extract_all_after_skip1(Bin)),
    ok.

test_match_fail(Bin) ->
    case Bin of
        <<_Head:9/binary, _Rest/binary>> ->
            error;
        _ ->
            ok
    end.

id(X) -> X.
