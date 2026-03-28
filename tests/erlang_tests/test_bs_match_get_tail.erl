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

%% Test that a variable bound in a binary pattern head can be used
%% in a binary match in the function body.
%%
%% OTP 26 compiles:
%%   parse(<<Len:16, Rest/binary>>) when byte_size(Rest) >= Len ->
%%       <<Packet:Len/binary, Tail/binary>> = Rest
%%
%% using get_tail within bs_match to extract Rest, byte_size on the
%% extracted bitstring, and then bs_get_binary2 on the original match
%% context.

-module(test_bs_match_get_tail).

-export([start/0]).

start() ->
    ok = test_parse_exact(),
    ok = test_parse_with_tail(),
    ok = test_parse_need_more(),
    ok = test_parse_empty(),
    0.

parse(<<Len:16, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Packet:Len/binary, Tail/binary>> = Rest,
    {ok, Packet, Tail};
parse(_) ->
    need_more.

test_parse_exact() ->
    {ok, <<"hello">>, <<>>} = parse(id(<<0, 5, "hello">>)),
    {ok, <<"abc">>, <<>>} = parse(id(<<0, 3, "abc">>)),
    {ok, <<>>, <<>>} = parse(id(<<0, 0>>)),
    ok.

test_parse_with_tail() ->
    {ok, <<"hello">>, <<"extra">>} = parse(id(<<0, 5, "hello", "extra">>)),
    {ok, <<"AB">>, <<1, 2, 3>>} = parse(id(<<0, 2, "AB", 1, 2, 3>>)),
    ok.

test_parse_need_more() ->
    need_more = parse(id(<<0, 5, "hel">>)),
    need_more = parse(id(<<0, 10, "short">>)),
    need_more = parse(id(<<0>>)),
    ok.

test_parse_empty() ->
    need_more = parse(id(<<>>)),
    ok.

id(X) -> X.
