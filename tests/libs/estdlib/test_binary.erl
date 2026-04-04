%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_binary).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_split(),
    ok = test_hex(),
    ok = test_list_to_bin(),
    ok.

test_split() ->
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"oo">>), [<<"f">>, <<"bar">>]),
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"ooz">>), [<<"foobar">>]),
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"o">>), [<<"f">>, <<"obar">>]),
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"o">>, [global]), [<<"f">>, <<>>, <<"bar">>]),
    ?ASSERT_MATCH(binary:split(<<"foobar">>, [<<"oo">>, <<"o">>]), [<<"f">>, <<"bar">>]),
    ?ASSERT_MATCH(binary:split(<<"aba">>, [<<"a">>, <<"ab">>], [global]), [<<>>, <<>>, <<>>]),
    ?ASSERT_MATCH(binary:split(<<":a::">>, <<":">>, [global, trim]), [<<>>, <<"a">>]),
    ?ASSERT_MATCH(binary:split(<<":a::">>, <<":">>, [global, trim_all]), [<<"a">>]),
    ?ASSERT_MATCH(binary:split(<<>>, <<":">>, [trim]), []),
    ?ASSERT_EXCEPTION(binary:split(<<"a">>, []), error, badarg),
    ?ASSERT_EXCEPTION(binary:split(<<"a">>, [<<>>]), error, badarg),
    ?ASSERT_EXCEPTION(binary:split(<<"a">>, [foo]), error, badarg),
    ?ASSERT_EXCEPTION(binary:split(<<"a">>, <<":">>, [foo]), error, badarg),
    ?ASSERT_MATCH(binary:split(<<"a">>, <<":">>, [global | foo]), [<<"a">>]),
    ?ASSERT_MATCH(binary:split(<<"aba">>, [<<"a">>, <<"ab">>]), [<<>>, <<"a">>]),
    ?ASSERT_MATCH(binary:split(<<"aba">>, [<<"ab">>, <<"a">>]), [<<>>, <<"a">>]),
    ?ASSERT_MATCH(binary:split(<<":">>, <<":">>, [trim]), []),
    ?ASSERT_MATCH(binary:split(<<":">>, <<":">>, [trim_all]), []),
    ok.

test_list_to_bin() ->
    ?ASSERT_MATCH(binary:list_to_bin([]), <<>>),
    ?ASSERT_MATCH(binary:list_to_bin([104, 101, 108, 108, 111]), <<"hello">>),
    ?ASSERT_MATCH(binary:list_to_bin([<<"hel">>, [108, 111]]), <<"hello">>),
    ok.

test_hex() ->
    RawBinary = <<"Hello, AtomVM!">>,
    ?ASSERT_MATCH(binary:encode_hex(RawBinary), <<"48656C6C6F2C2041746F6D564D21">>),
    ?ASSERT_MATCH(binary:encode_hex(RawBinary, lowercase), <<"48656c6c6f2c2041746f6d564d21">>),
    ?ASSERT_MATCH(RawBinary, binary:decode_hex(<<"48656C6C6F2C2041746F6D564D21">>)),
    ?ASSERT_EXCEPTION(binary:decode_hex(<<"48656C6C6F2C2041746F6D564D2">>), error, badarg),
    ?ASSERT_EXCEPTION(binary:decode_hex(<<"ABCDEFGH">>), error, badarg),
    ok.
