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

-module(bs_get_float_dynamic_size).

%% Force the compiler to use bs_get_float2 opcode instead of the
%% newer bs_match opcode. The no_bs_match option was removed in OTP 29.
-if(?OTP_RELEASE =< 28).
-compile([no_bs_match]).
-endif.

-export([start/0, ext_id/1]).

start() ->
    %% Test bs_get_float2 with runtime-variable size.
    Bin64 = id(<<3.14:64/float>>),
    ok = test_float64_dynamic(Bin64, id(64)),
    Bin32 = id(<<3.14:32/float>>),
    ok = test_float32_dynamic(Bin32, id(32)),
    {1.5, <<7:3>>} = match_float(id(<<62, 0, 7:3>>), id(8)),
    {1.5, <<7:3>>} = match_float(id(<<63, 192, 0, 0, 7:3>>), id(16)),
    {1.5, <<7:3>>} = match_float(id(<<63, 248, 0, 0, 0, 0, 0, 0, 7:3>>), id(32)),
    {1.5, <<3:2>>} = match_float_little(id(<<5:3, 0, 62, 3:2>>), id(8)),
    {1.5, <<3:2>>} = match_float_little(id(<<5:3, 0, 0, 192, 63, 3:2>>), id(16)),
    {1.5, <<3:2>>} = match_float_little(id(<<5:3, 0, 0, 0, 0, 0, 0, 248, 63, 3:2>>), id(32)),
    {+0.0, <<>>} = match_float(id(<<>>), id(0)),
    {+0.0, <<129, 7:3>>} = match_float(id(<<129, 7:3>>), id(0)),
    {+0.0, <<3:2>>} = match_float_little(id(<<5:3, 3:2>>), id(0)),
    nope = match_float(id(<<62>>), id(8)),
    nope = match_float(id(<<0, 0, 0, 0>>), id(12)),
    nope = match_float(id(<<0, 0, 0, 0>>), id(-1)),
    0.

test_float64_dynamic(Bin, Size) ->
    <<F:Size/float>> = Bin,
    true = (F > 3.13),
    true = (F < 3.15),
    ok.

test_float32_dynamic(Bin, Size) ->
    <<F:Size/float>> = Bin,
    true = (F > 3.13),
    true = (F < 3.15),
    ok.

match_float(Bin, Size) ->
    case Bin of
        <<F:Size/float-unit:2, Rest/bitstring>> -> {F, Rest};
        _ -> nope
    end.

match_float_little(Bin, Size) ->
    case Bin of
        <<5:3, F:Size/float-little-unit:2, Rest/bitstring>> -> {F, Rest};
        _ -> nope
    end.

id(X) -> ?MODULE:ext_id(X).

ext_id(X) -> X.
