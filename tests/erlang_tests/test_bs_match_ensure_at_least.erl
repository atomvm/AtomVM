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

%% Test bs_match ensure_at_least with stride > 0 and unit > 1
%% after non-byte-aligned bits have been consumed.
%%
%% When the compiler splits binary matching across multiple bs_match
%% instructions (e.g. because an extracted value is used in a guard),
%% the second bs_match may see ensure_at_least with unit > 1 when
%% the remaining bits are not byte-aligned. The unit check must apply
%% to (remaining - stride), not to remaining.
%%
%% This reproduces the bug where parse_dns_name matching
%% <<3:2, Ptr:14, Tail/binary>> failed because after consuming 2 bits,
%% remaining % 8 != 0, but (remaining - 14) % 8 == 0.

-module(test_bs_match_ensure_at_least).

-export([start/0]).

start() ->
    ok = test_parse_name(),
    ok = test_parse_name_fail(),
    0.

%% Multi-clause function that forces the compiler to split bs_match:
%% - Clause 1 tries <<0, Tail/binary>>
%% - Clause 2 tries <<3:2, Ptr:14, Tail/binary>> with guard using Ptr
%% - Clause 3 tries <<N, Data:N/binary, Rest/binary>> with guard
%% The guard on Ptr forces the compiler to extract the 2-bit value and
%% 14-bit Ptr in a first bs_match, check the guard, then issue a second
%% bs_match with ensure_at_least 14,8 for the remaining tail.
parse_name(Msg, <<0, Tail/binary>>) ->
    {ok, {done, Tail, Msg}};
parse_name(Msg, <<3:2, Ptr:14, Tail/binary>>) when byte_size(Msg) > Ptr ->
    {ok, {ptr, Ptr, Tail}};
parse_name(_Msg, <<N, Data:N/binary, Rest/binary>>) when N < 64 ->
    {ok, {label, Data, Rest}};
parse_name(_Msg, Other) ->
    {error, Other}.

test_parse_name() ->
    Msg = id(
        <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 95, 114, 100, 108, 105, 110, 107, 192, 28, 0, 12,
            0, 1>>
    ),
    %% Parse the 7-byte label "_rdlink"
    {ok, {label, <<"_rdlink">>, Rest}} = parse_name(
        Msg, id(<<7, 95, 114, 100, 108, 105, 110, 107, 192, 28, 0, 12, 0, 1>>)
    ),
    %% Rest is <<192, 28, 0, 12, 0, 1>> -- a DNS compression pointer
    %% 192 = 0xC0 = 11_000000, so 3:2 matches.
    %% Ptr:14 = 28. byte_size(Msg) = 24 > 28 is false, so guard fails.
    %% But with 24-byte Msg, let's use a bigger message.

    % 31 bytes
    BigMsg = id(<<0:240, 0>>),
    {ok, {ptr, 28, <<0, 12, 0, 1>>}} = parse_name(BigMsg, Rest),
    ok.

test_parse_name_fail() ->
    % 10 bytes, smaller than Ptr=28
    SmallMsg = id(<<0:80>>),
    %% <<192, 28>> is 3:2, Ptr=28, but guard byte_size(SmallMsg)=10 > 28 fails
    %% Falls through to clause 3: N=192, N >= 64, guard fails
    %% Falls through to clause 4: error
    {error, <<192, 28, 0, 12, 0, 1>>} = parse_name(SmallMsg, id(<<192, 28, 0, 12, 0, 1>>)),
    ok.

id(X) -> X.
