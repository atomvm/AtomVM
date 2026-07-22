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

-module(bs_get_float_gc).

%% Force the compiler to use bs_get_float2 opcode instead of the
%% newer bs_match opcode. The no_bs_match option was removed in OTP 29.
-if(?OTP_RELEASE =< 28).
-compile([no_bs_match]).
-endif.

-export([start/0, id/1]).

start() ->
    Bin = id(<<3.14:64/float, 2.5:32/float>>),
    Acc = loop(2000, Bin, []),
    2000 = length(Acc),
    ok = check(Acc),
    0.

loop(0, _Bin, Acc) ->
    Acc;
loop(N, Bin, Acc) ->
    <<F64:64/float, F32:32/float>> = Bin,
    _ = id(make_list(32, [])),
    loop(N - 1, Bin, [{F64, F32} | Acc]).

make_list(0, Acc) ->
    Acc;
make_list(N, Acc) ->
    make_list(N - 1, [N | Acc]).

check([]) ->
    ok;
check([{F64, F32} | Tail]) ->
    true = (F64 > 3.139) andalso (F64 < 3.141),
    true = (F32 > 2.49) andalso (F32 < 2.51),
    check(Tail).

id(X) -> X.
