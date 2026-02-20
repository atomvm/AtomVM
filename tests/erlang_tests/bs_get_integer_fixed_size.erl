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

-module(bs_get_integer_fixed_size).

%% Force the compiler to use bs_get_integer2 opcode instead of the
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
    ok = test_match_8bit(Bin),
    ok = test_match_16bit(Bin),
    ok = test_match_32bit(Bin),
    ok = test_match_fail(Bin),
    0.

test_match_8bit(Bin) ->
    <<X:8, Rest/binary>> = Bin,
    1 = X,
    7 = byte_size(Rest),
    <<2, 3, 4, 5, 6, 7, 8>> = Rest,
    ok.

test_match_16bit(Bin) ->
    <<X:16, Rest/binary>> = Bin,
    258 = X,
    6 = byte_size(Rest),
    <<3, 4, 5, 6, 7, 8>> = Rest,
    ok.

test_match_32bit(Bin) ->
    <<X:32, Rest/binary>> = Bin,
    16909060 = X,
    4 = byte_size(Rest),
    <<5, 6, 7, 8>> = Rest,
    ok.

test_match_fail(Bin) ->
    case Bin of
        <<_X:72, _Rest/binary>> ->
            error;
        _ ->
            ok
    end.

id(X) -> X.
