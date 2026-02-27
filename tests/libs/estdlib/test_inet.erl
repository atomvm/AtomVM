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

-module(test_inet).

-export([test/0]).

test() ->
    ok = test_getaddr(),
    ok = test_ntoa(),
    ok = test_parse_address(),
    ok = test_parse_ipv4_address(),
    ok = test_parse_ipv4strict_address(),
    ok.

test_getaddr() ->
    {ok, {127, 0, 0, 1}} = inet:getaddr(localhost, inet),
    {ok, {127, 0, 0, 1}} = inet:getaddr("localhost", inet),
    {ok, {_, _, _, _}} = inet:getaddr("test.atomvm.org", inet),
    % RFC8880
    {ok, {192, 0, 0, LastByte}} = inet:getaddr("ipv4only.arpa", inet),
    true = LastByte =:= 170 orelse LastByte =:= 171,
    {error, einval} = inet:getaddr(127, inet),
    {error, einval} = inet:getaddr({127.0, 0, 0, 1.0}, inet),
    {error, einval} = inet:getaddr({312, 0, 0, 1}, inet),
    {error, einval} = inet:getaddr({foo, bar}, inet),
    {error, einval} = inet:getaddr(<<"localhost">>, inet),
    {error, _} = inet:getaddr("localhost.invalid", inet),
    ok.

test_ntoa() ->
    "127.0.0.1" = inet:ntoa({127, 0, 0, 1}),
    "192.168.0.1" = inet:ntoa({192, 168, 0, 1}),
    "0.0.0.0" = inet:ntoa({0, 0, 0, 0}),
    "255.255.255.255" = inet:ntoa({255, 255, 255, 255}),
    case get_otp_version() of
        OTPVersion when
            (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
        ->
            {error, einval} = inet:ntoa({256, 0, 0, 1}),
            {error, einval} = inet:ntoa({0, 0, 0, -1}),
            {error, einval} = inet:ntoa({1, 2, 3}),
            {error, einval} = inet:ntoa(not_an_address);
        _ ->
            ok
    end,
    ok.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" -> list_to_integer(erlang:system_info(otp_release));
        _ -> atomvm
    end.

test_parse_address() ->
    {ok, {127, 0, 0, 1}} = inet:parse_address("127.0.0.1"),
    {ok, {192, 168, 0, 1}} = inet:parse_address("192.168.0.1"),
    {ok, {0, 0, 0, 0}} = inet:parse_address("0.0.0.0"),
    {ok, {255, 255, 255, 255}} = inet:parse_address("255.255.255.255"),
    {error, einval} = inet:parse_address("256.0.0.1"),
    {error, einval} = inet:parse_address("not_an_address"),
    case erlang:system_info(machine) of
        "BEAM" ->
            % BEAM accepts short-form IPv4 addresses
            {ok, {127, 0, 0, 1}} = inet:parse_address("127.1"),
            {ok, {127, 0, 0, 1}} = inet:parse_address("0x7f000001"),
            {ok, {127, 0, 0, 1}} = inet:parse_address("127.0.000.001");
        "ATOM" ->
            % AtomVM only supports strict dotted-decimal IPv4 addresses
            {error, einval} = inet:parse_address("127.1"),
            {error, einval} = inet:parse_address("0x7f000001"),
            % AtomVM doesn't allow leading zeros as well
            {error, einval} = inet:parse_address("127.0.000.001")
    end,
    ok.

test_parse_ipv4_address() ->
    {ok, {127, 0, 0, 1}} = inet:parse_ipv4_address("127.0.0.1"),
    {ok, {10, 0, 0, 1}} = inet:parse_ipv4_address("10.0.0.1"),
    {ok, {0, 0, 0, 0}} = inet:parse_ipv4_address("0.0.0.0"),
    {ok, {255, 255, 255, 255}} = inet:parse_ipv4_address("255.255.255.255"),
    {error, einval} = inet:parse_ipv4_address("256.0.0.1"),
    {error, einval} = inet:parse_ipv4_address("not_an_address"),
    case erlang:system_info(machine) of
        "BEAM" ->
            % BEAM accepts short-form IPv4 addresses
            {ok, {127, 0, 0, 1}} = inet:parse_ipv4_address("127.1"),
            {ok, {127, 0, 0, 1}} = inet:parse_ipv4_address("0x7f000001");
        "ATOM" ->
            % AtomVM only supports strict dotted-decimal IPv4 addresses
            {error, einval} = inet:parse_ipv4_address("127.1"),
            {error, einval} = inet:parse_ipv4_address("0x7f000001")
    end,
    ok.

test_parse_ipv4strict_address() ->
    {ok, {127, 0, 0, 1}} = inet:parse_ipv4strict_address("127.0.0.1"),
    {ok, {10, 0, 0, 1}} = inet:parse_ipv4strict_address("10.0.0.1"),
    {ok, {0, 0, 0, 0}} = inet:parse_ipv4strict_address("0.0.0.0"),
    {ok, {255, 255, 255, 255}} = inet:parse_ipv4strict_address("255.255.255.255"),
    {error, einval} = inet:parse_ipv4strict_address("256.0.0.1"),
    {error, einval} = inet:parse_ipv4strict_address("127.0.0"),
    {error, einval} = inet:parse_ipv4strict_address("127.1"),
    {error, einval} = inet:parse_ipv4strict_address("0x7f000001"),
    {error, einval} = inet:parse_ipv4strict_address("not_an_address"),
    ok.
