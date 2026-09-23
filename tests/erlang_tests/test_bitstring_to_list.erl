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

-module(test_bitstring_to_list).

-export([start/0, id/1]).

-define(ID(X), ?MODULE:id(X)).

%% AtomVM does not support non-byte-aligned bitstrings, so only byte-aligned
%% binaries are exercised. This matches the compiler's use of
%% erlang:bitstring_to_list/1 (beam_core_to_ssa:pattern_bin/3), which only ever
%% passes byte-aligned binaries.
start() ->
    [1, 2, 3] = erlang:bitstring_to_list(?ID(<<1, 2, 3>>)),
    [] = erlang:bitstring_to_list(?ID(<<>>)),
    [$a, $b, $c] = erlang:bitstring_to_list(?ID(<<"abc">>)),
    Bytes = [0, 1, 2, 127, 128, 200, 254, 255],
    Bytes = erlang:bitstring_to_list(?ID(<<0, 1, 2, 127, 128, 200, 254, 255>>)),
    ok = raises_badarg(fun() -> erlang:bitstring_to_list(?ID([1, 2, 3])) end),
    ok = raises_badarg(fun() -> erlang:bitstring_to_list(?ID(not_a_bitstring)) end),
    0.

raises_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.

id(X) ->
    X.
