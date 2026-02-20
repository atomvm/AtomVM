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
%% newer bs_match opcode (OTP 25+). On older OTP this is ignored.
%% The no_bs_match option was removed in OTP 29.
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE =< 28).
-compile([no_bs_match]).
-endif.
-endif.

-export([start/0]).

start() ->
    %% Test bs_get_float2 with runtime-variable size.
    Bin64 = id(<<3.14:64/float>>),
    ok = test_float64_dynamic(Bin64, id(64)),
    Bin32 = id(<<3.14:32/float>>),
    ok = test_float32_dynamic(Bin32, id(32)),
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

id(X) -> X.
