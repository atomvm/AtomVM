%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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
-export([start/0]).

-define(DHCP_TIMEOUT_MS, 45000).
-define(RESOLVER_TIMEOUT_MS, 10000).
-define(DNS_TIMEOUT_MS, 10000).
-define(DNS_RETRY_INTERVAL_MS, 250).
-define(ECHO_PORT, 8080).
-define(QEMU_DNS, {10, 0, 2, 3}).
-define(CLOUDFLARE_DNS, {1, 1, 1, 1}).

start() ->
    erlang:display({atomvm_rtems_net, atomvm:platform()}),
    case atomvm_rtems:wait_dhcp(?DHCP_TIMEOUT_MS) of
        {error, enotsup} ->
            erlang:display({net, enotsup}),
            ok = expect_socket_unsupported(),
            ok;
        ok ->
            ok = dhcp_ok(),
            ok = echo_server(),
            ok = tcp_outbound_ok(),
            ok = atomvm_rtems:wait_resolver(?RESOLVER_TIMEOUT_MS),
            erlang:display({resolver, ok}),
            ok = dns_ok(),
            ok;
        Other ->
            erlang:error({dhcp_failed, Other})
    end.

expect_socket_unsupported() ->
    try socket:open(inet, stream, tcp) of
        {error, Reason} ->
            erlang:display({socket, unsupported, Reason}),
            ok;
        Unexpected ->
            erlang:error({socket_unexpected, Unexpected})
    catch
        error:undef ->
            erlang:display({socket, undef}),
            ok;
        error:Reason ->
            erlang:display({socket, error, Reason}),
            ok
    end.

dhcp_ok() ->
    case atomvm_rtems:ifaddrs() of
        {ok, Addrs} ->
            case find_if(Addrs, "ffec0") of
                {ok, Addr} ->
                    erlang:display({dhcp, ok, Addr}),
                    ok;
                not_found ->
                    erlang:error({dhcp_no_ffec0, Addrs})
            end;
        Error ->
            erlang:error({ifaddrs_failed, Error})
    end.

find_if([], _Name) ->
    not_found;
find_if([{Name, Addr, _Flags} | Rest], Name) ->
    case Addr of
        {0, 0, 0, 0} -> find_if(Rest, Name);
        _ -> {ok, Addr}
    end;
find_if([_ | Rest], Name) ->
    find_if(Rest, Name).

dns_ok() ->
    case resolve_dns("example.com", ?DNS_TIMEOUT_MS) of
        {ok, [Info | _]} ->
            Addr = maps:get(addr, Info),
            IP = maps:get(addr, Addr),
            erlang:display({dns, ok, IP}),
            ok;
        Other ->
            erlang:error({dns_failed, Other})
    end.

resolve_dns(Name, RemainingMs) ->
    case net:getaddrinfo(Name) of
        {error, Reason} when
            RemainingMs > 0 andalso (Reason =:= eainoname orelse Reason =:= eaiagain)
        ->
            SleepMs = dns_retry_interval(RemainingMs),
            timer:sleep(SleepMs),
            resolve_dns(Name, RemainingMs - SleepMs);
        Result ->
            Result
    end.

dns_retry_interval(RemainingMs) when RemainingMs < ?DNS_RETRY_INTERVAL_MS ->
    RemainingMs;
dns_retry_interval(_RemainingMs) ->
    ?DNS_RETRY_INTERVAL_MS.

tcp_outbound_ok() ->
    case try_connect(?QEMU_DNS, 53) of
        ok ->
            erlang:display({tcp, outbound, ok}),
            ok;
        {error, Reason} ->
            case try_connect(?CLOUDFLARE_DNS, 53) of
                ok ->
                    erlang:display({tcp, outbound, cloudflare, ok}),
                    ok;
                Error2 ->
                    erlang:error({tcp_outbound_failed, Reason, Error2})
            end
    end.

try_connect(Addr, Port) ->
    case socket:open(inet, stream, tcp) of
        {ok, Socket} ->
            Result = socket:connect(Socket, #{family => inet, addr => Addr, port => Port}),
            socket:close(Socket),
            Result;
        Error ->
            Error
    end.

echo_server() ->
    {ok, ListeningSocket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(ListeningSocket, {socket, reuseaddr}, true),
    ok = socket:bind(ListeningSocket, #{family => inet, addr => any, port => ?ECHO_PORT}),
    ok = socket:listen(ListeningSocket),
    erlang:display({tcp, listen, ?ECHO_PORT}),
    Self = self(),
    Acceptor = spawn(fun() -> accept_loop(ListeningSocket, Self) end),
    {ok, Client} = socket:open(inet, stream, tcp),
    ok = socket:connect(Client, #{family => inet, addr => loopback, port => ?ECHO_PORT}),
    ok = socket:send(Client, <<"ping">>),
    receive
        {echo, <<"ping">>} ->
            erlang:display({tcp, echo, ok}),
            socket:close(Client),
            wait_hostfwd(20000),
            socket:close(ListeningSocket),
            ok
    after 10000 ->
        socket:close(Client),
        socket:close(ListeningSocket),
        exit(Acceptor, kill),
        erlang:error(echo_timeout)
    end.

wait_hostfwd(Timeout) ->
    receive
        {echo, Data} ->
            erlang:display({tcp, hostfwd, Data}),
            wait_hostfwd(1000)
    after Timeout ->
        ok
    end.

accept_loop(ListeningSocket, Parent) ->
    case socket:accept(ListeningSocket, 25000) of
        {ok, Conn} ->
            case socket:recv(Conn, 0, 5000) of
                {ok, Data} ->
                    socket:send(Conn, Data),
                    Parent ! {echo, Data},
                    socket:close(Conn);
                RecvErr ->
                    Parent ! {echo_error, RecvErr},
                    socket:close(Conn)
            end,
            accept_loop(ListeningSocket, Parent);
        {error, timeout} ->
            ok;
        {error, closed} ->
            ok;
        AcceptErr ->
            Parent ! {echo_error, AcceptErr}
    end.
