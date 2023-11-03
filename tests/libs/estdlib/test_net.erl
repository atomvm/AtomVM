%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
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

-module(test_net).

-export([test/0]).

test() ->
    ok = test_getaddrinfo(),
    ok = test_getaddrinfo2(),
    ok.

test_getaddrinfo() ->
    ok = test_getaddrinfo("www.atomvm.net"),
    ok.

test_getaddrinfo(Host) ->
    {ok, AddrInfos} = net:getaddrinfo(Host),

    lists:foreach(
        fun(AddrInfo) ->
            inet = maps:get(family, AddrInfo),
            Protocol = maps:get(protocol, AddrInfo, undefined),
            Type = maps:get(type, AddrInfo, undefined),
            Addr = get_addr(AddrInfo),

            case erlang:system_info(machine) of
                "BEAM" ->
                    ok;
                _ ->
                    true = (Protocol =:= tcp orelse Protocol =:= udp orelse Protocol =:= undefined),
                    true = (Type =:= dgram orelse Type =:= stream orelse Type =:= undefined)
            end,

            {A, B, C, D} = Addr,
            true = is_integer(A) andalso 0 =< A andalso A =< 255,
            true = is_integer(B) andalso 0 =< B andalso B =< 255,
            true = is_integer(C) andalso 0 =< C andalso C =< 255,
            true = is_integer(D) andalso 0 =< D andalso D =< 255,

            ok
        end,
        maybe_filter_addrinfos(AddrInfos)
    ),

    expect_failure(fun() -> net:getaddrinfo(not_a_list) end, function_clause),
    expect_failure(fun() -> net:getaddrinfo([isnot, a, string]) end, badarg),
    expect_failure(fun() -> net:getaddrinfo([$i, $m, $p, $r, $o, $p, $e | $r]) end, badarg),

    {error, _Reason} = net:getaddrinfo("ewkrkerkwe.oksjds.wee.org"),

    ok.

test_getaddrinfo2() ->
    ok = test_getaddrinfo2("www.atomvm.net", "https"),
    ok = test_getaddrinfo2("www.atomvm.net", "443"),
    ok = test_getaddrinfo2(undefined, "443"),
    ok.

test_getaddrinfo2(Host, Service) ->
    {ok, AddrInfos} = net:getaddrinfo(Host, Service),

    lists:foreach(
        fun(AddrInfo) ->
            inet = maps:get(family, AddrInfo),
            Protocol = maps:get(protocol, AddrInfo, undefined),
            Type = maps:get(type, AddrInfo, undefined),
            Addr = get_addr(AddrInfo),

            case erlang:system_info(machine) of
                "BEAM" ->
                    ok;
                _ ->
                    true = (Protocol =:= tcp orelse Protocol =:= udp orelse Protocol =:= undefined),
                    true = (Type =:= dgram orelse Type =:= stream orelse Type =:= undefined)
            end,

            {A, B, C, D} = Addr,
            true = is_integer(A) andalso 0 =< A andalso A =< 255,
            true = is_integer(B) andalso 0 =< B andalso B =< 255,
            true = is_integer(C) andalso 0 =< C andalso C =< 255,
            true = is_integer(D) andalso 0 =< D andalso D =< 255,

            ok
        end,
        maybe_filter_addrinfos(AddrInfos)
    ),

    expect_failure(fun() -> net:getaddrinfo(Host, not_a_list) end, function_clause),
    expect_failure(fun() -> net:getaddrinfo(Host, [isnot, a, string]) end, badarg),
    expect_failure(fun() -> net:getaddrinfo(Host, [$i, $m, $p, $r, $o, $p, $e | $r]) end, badarg),
    expect_failure(fun() -> net:getaddrinfo(undefined, undefined) end, function_clause),

    {error, _Reason} = net:getaddrinfo("ewkrkerkwe.oksjds.wee.org", Service),

    ok.

expect_failure(F, Expected) ->
    try
        F(),
        fail
    catch
        _:E when E =:= Expected ->
            ok
    end.

maybe_filter_addrinfos(AddrInfos) ->
    case erlang:system_info(machine) of
        "BEAM" ->
            lists:filter(
                fun(AddrInfo) ->
                    inet =:= maps:get(family, AddrInfo)
                end,
                AddrInfos
            );
        _ ->
            AddrInfos
    end.

get_addr(AddrInfo) ->
    case erlang:system_info(machine) of
        "BEAM" ->
            maps:get(addr, maps:get(addr, AddrInfo));
        _ ->
            maps:get(addr, maps:get(address, AddrInfo))
    end.
