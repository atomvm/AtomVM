%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_mdns).
-include_lib("eunit/include/eunit.hrl").

-define(DNS_TYPE_A, 1).
-define(DNS_CLASS_IN, 1).
-define(DNS_OPCODE_STANDARD_QUERY, 0).
-define(DNS_QR_QUERY, 0).
-define(DNS_QR_REPLY, 1).
-define(MDNS_SEND_BROADCAST_RESPONSE, 0).
-define(MDNS_SEND_UNICAST_RESPONSE, 1).

parse_dns_name_test_() ->
    [
        ?_assertEqual(
            {ok, {[<<"atomvm">>, <<"local">>], <<>>}},
            mdns:parse_dns_name(<<6, "atomvm", 5, "local", 0>>, <<6, "atomvm", 5, "local", 0>>)
        ),
        ?_assertEqual({ok, {[], <<42>>}}, mdns:parse_dns_name(<<0, 42>>, <<0, 42>>)),
        ?_assertEqual({error, {invalid_name, <<42>>}}, mdns:parse_dns_name(<<42>>, <<42>>)),
        ?_assertEqual({error, {invalid_name, <<>>}}, mdns:parse_dns_name(<<1, 42>>, <<1, 42>>))
    ].

parse_dns_message_test_() ->
    [
        ?_assertMatch(
            {ok, {dns_message, 0, 0, 0, 0, [], [], [], []}},
            mdns:parse_dns_message(<<0:16, 0:16, 0:16, 0:16, 0:16, 0:16>>)
        ),
        ?_assertMatch(
            {ok,
                {dns_message, 0, 0, 0, 0,
                    [{dns_question, [<<"atomvm">>, <<"local">>], ?DNS_TYPE_A, 1, ?DNS_CLASS_IN}],
                    [], [], []}},
            mdns:parse_dns_message(
                <<0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 6, 97, 116, 111, 109, 118, 109, 5, 108, 111,
                    99, 97, 108, 0, 0, 1, 128, 1>>
            )
        ),
        ?_assertMatch(
            {ok,
                {dns_message, 0, 0, 0, 0,
                    [{dns_question, [<<"atomvm">>, <<"local">>], ?DNS_TYPE_A, 0, ?DNS_CLASS_IN}],
                    [], [], []}},
            mdns:parse_dns_message(
                <<0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 6, 97, 116, 111, 109, 118, 109, 5, 108, 111,
                    99, 97, 108, 0, 0, 1, 0, 1>>
            )
        ),
        ?_assertMatch(
            {ok,
                {dns_message, 0, 0, 0, 0,
                    [
                        {dns_question, [<<"_companion-link">>, <<"_tcp">>, <<"local">>], 12, 0, 1},
                        {dns_question, [<<"_rdlink">>, <<"_tcp">>, <<"local">>], 12, 0, 1}
                    ],
                    [
                        {dns_rrecord, [<<"_companion-link">>, <<"_tcp">>, <<"local">>], 12, 1, 4488,
                            <<6, 227, 131, 166, 227, 130, 186, 192, 12>>}
                    ],
                    [], []}},
            mdns:parse_dns_message(
                <<0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 15, 95, 99, 111, 109, 112, 97, 110, 105, 111,
                    110, 45, 108, 105, 110, 107, 4, 95, 116, 99, 112, 5, 108, 111, 99, 97, 108, 0,
                    0, 12, 0, 1, 7, 95, 114, 100, 108, 105, 110, 107, 192, 28, 0, 12, 0, 1, 192, 12,
                    0, 12, 0, 1, 0, 0, 17, 136, 0, 9, 6, 227, 131, 166, 227, 130, 186, 192, 12>>
            )
        ),
        ?_assertMatch(
            {error, {invalid_dns_header, <<>>}},
            mdns:parse_dns_message(<<>>)
        ),
        ?_assertMatch(
            {error, {invalid_dns_header, <<0>>}},
            mdns:parse_dns_message(<<0>>)
        ),
        ?_assertMatch(
            {error, {invalid_dns_header, <<0:16, 0, 255, 0:16, 0:16, 0:16, 0:16>>}},
            mdns:parse_dns_message(<<0:16, 0, 255, 0:16, 0:16, 0:16, 0:16>>)
        ),
        ?_assertMatch(
            {error, {invalid_name, <<>>}},
            mdns:parse_dns_message(<<0:16, 0:16, 0:16, 0:16, 0:16, 1:16>>)
        ),
        ?_assertMatch(
            {error, {invalid_name, <<>>}},
            mdns:parse_dns_message(<<0:16, 0:16, 1:16, 0:16, 0:16, 0:16>>)
        ),
        ?_assertMatch(
            {error, {invalid_question, <<0>>}},
            mdns:parse_dns_message(<<0:16, 0:16, 1:16, 0:16, 0:16, 0:16, 0>>)
        ),
        ?_assertMatch(
            {error, {invalid_name, <<1>>}},
            mdns:parse_dns_message(<<0:16, 0:16, 1:16, 0:16, 0:16, 0:16, 1>>)
        ),
        ?_assertMatch(
            {error, {invalid_question, <<0, 0, 0>>}},
            mdns:parse_dns_message(<<0:16, 0:16, 1:16, 0:16, 0:16, 0:16, 0, 0:16>>)
        ),
        ?_assertMatch(
            {error, {invalid_question, <<0, 0, 0, 0>>}},
            mdns:parse_dns_message(<<0:16, 0:16, 1:16, 0:16, 0:16, 0:16, 0, 0:16, 0>>)
        ),
        ?_assertMatch(
            {error, {extra_bytes_in_dns_message, <<0>>}},
            mdns:parse_dns_message(<<0:16, 0:16, 1:16, 0:16, 0:16, 0:16, 0, 0:16, 0:16, 0>>)
        ),
        ?_assertMatch(
            {error, {extra_bytes_in_dns_message, <<"*">>}},
            mdns:parse_dns_message(<<0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 42>>)
        )
    ].

serialize_dns_message_test_() ->
    [
        ?_assertEqual(
            <<0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 6, 97, 116, 111, 109, 118, 109, 5, 108, 111, 99,
                97, 108, 0, 0, 1, 0, 1>>,
            mdns:serialize_dns_message(
                {dns_message, 0, 0, 0, 0,
                    [{dns_question, [<<"atomvm">>, <<"local">>], ?DNS_TYPE_A, 1, ?DNS_CLASS_IN}],
                    [], [], []}
            )
        )
    ].

serialize_dns_name_test_() ->
    [
        ?_assertEqual(
            <<6, "atomvm", 5, "local", 0>>, mdns:serialize_dns_name([<<"atomvm">>, <<"local">>])
        )
    ].
