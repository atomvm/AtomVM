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

start() ->
    [1, 2, 3] = erlang:bitstring_to_list(?ID(<<1, 2, 3>>)),
    [] = erlang:bitstring_to_list(?ID(<<>>)),
    [$a, $b, $c] = erlang:bitstring_to_list(?ID(<<"abc">>)),
    Bytes = [0, 1, 2, 127, 128, 200, 254, 255],
    Bytes = erlang:bitstring_to_list(?ID(<<0, 1, 2, 127, 128, 200, 254, 255>>)),
    ok = raises_badarg(fun() -> erlang:bitstring_to_list(?ID([1, 2, 3])) end),
    ok = raises_badarg(fun() -> erlang:bitstring_to_list(?ID(not_a_bitstring)) end),
    %% Non-byte-aligned bitstrings: the trailing partial byte is returned as a
    %% final bitstring element. beam_core_to_ssa relies on this when compiling a
    %% match against a non-byte-aligned literal segment (e.g. <<1:1,_:63>>).
    [B1] = erlang:bitstring_to_list(?ID(<<1:1>>)),
    true = is_bitstring(B1),
    false = is_binary(B1),
    1 = bit_size(B1),
    1 = extract(B1),
    [B3] = erlang:bitstring_to_list(?ID(<<3:3>>)),
    3 = bit_size(B3),
    3 = extract(B3),
    [255, B7] = erlang:bitstring_to_list(?ID(<<255, 5:7>>)),
    7 = bit_size(B7),
    5 = extract(B7),
    0.

extract(Bitstring) ->
    Size = bit_size(Bitstring),
    <<Value:Size>> = Bitstring,
    Value.

raises_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.

id(X) ->
    X.
