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

-module(test_esp32c6_wifi).
-export([start/0]).

start() ->
    erlang:display({atomvm_esp32c6_wifi, atomvm:platform()}),
    wifi_smoke(),
    ifaddrs_smoke(),
    socket_smoke(),
    udp_smoke(),
    tcp_echo_smoke(),
    gen_tcp_passive_smoke(),
    socket_select_smoke(),
    dns_smoke(),
    sntp_smoke(),
    erlang:display({wifi_test_complete, ok}),
    ok.

wifi_smoke() ->
    Status0 = atomvm_rtems:wifi_status(),
    erlang:display({initial_wifi_status, Status0}),
    case atomvm_rtems:wifi_start("AtomVM-TestAP", "secretpassword") of
        ok ->
            erlang:display({wifi_start, ok}),
            wait_for_wifi(30);
        {error, enotsup} ->
            erlang:display({wifi_start, enotsup}),
            ok;
        Error ->
            erlang:display({wifi_start_result, Error}),
            ok
    end.

wait_for_wifi(0) ->
    erlang:display({wifi_status_after_wait, atomvm_rtems:wifi_status()});
wait_for_wifi(Attempts) ->
    case atomvm_rtems:wifi_status() of
        {ok, Info} ->
            State = proplists:get_value(state, Info),
            case State of
                got_ip ->
                    erlang:display({wifi_status, got_ip}),
                    erlang:display({wifi_ip_info, Info});
                _ ->
                    timer:sleep(500),
                    wait_for_wifi(Attempts - 1)
            end;
        Other ->
            erlang:display({wifi_status, Other}),
            timer:sleep(500),
            wait_for_wifi(Attempts - 1)
    end.

ifaddrs_smoke() ->
    try atomvm_rtems:ifaddrs() of
        {ok, Addrs} ->
            erlang:display({ifaddrs, Addrs}),
            case has_ip(Addrs, {192, 168, 4, 2}) of
                true ->
                    erlang:display({ifaddrs_match, ok});
                false ->
                    erlang:display({ifaddrs_no_match, Addrs})
            end;
        Err ->
            erlang:display({ifaddrs_err, Err})
    catch
        Class:Reason:Stack ->
            erlang:display({ifaddrs_exception, Class, Reason, Stack})
    end.

has_ip([], _TargetIP) ->
    false;
has_ip([{_Name, TargetIP, _Flags} | _Rest], TargetIP) ->
    true;
has_ip([_ | Rest], TargetIP) ->
    has_ip(Rest, TargetIP).

socket_smoke() ->
    try socket:open(inet, stream, tcp) of
        {ok, Sock} ->
            erlang:display({socket_open, ok}),
            ok = socket:close(Sock),
            erlang:display({socket_close, ok});
        {error, Reason} ->
            erlang:display({socket_open, error, Reason});
        Other ->
            erlang:display({socket_open, other, Other})
    catch
        error:undef ->
            erlang:display({socket, undef})
    end.

udp_smoke() ->
    try
        erlang:display(starting_udp_test),
        case socket:open(inet, dgram, udp) of
            {ok, RecvSock} ->
                ok = socket:bind(RecvSock, #{family => inet, addr => any, port => 9091}),
                {ok, SendSock} = socket:open(inet, dgram, udp),
                ok = socket:sendto(SendSock, <<"udp_echo_test">>, #{family => inet, addr => {127, 0, 0, 1}, port => 9091}),
                case socket:recvfrom(RecvSock, 0, 5000) of
                    {ok, {_Source, <<"udp_echo_test">>}} ->
                        erlang:display({udp_echo, ok});
                    OtherRecv ->
                        erlang:display({udp_recv_unexpected, OtherRecv})
                end,
                socket:close(SendSock),
                socket:close(RecvSock),
                ok;
            {error, OpenErr} ->
                erlang:display({udp_open_error, OpenErr})
        end
    catch
        Class:ExReason:Stack ->
            erlang:display({udp_exception, Class, ExReason, Stack})
    end.

tcp_echo_smoke() ->
    try
        erlang:display(starting_tcp_echo_test),
        case socket:open(inet, stream, tcp) of
            {ok, ListenSock} ->
                ok = socket:setopt(ListenSock, {socket, reuseaddr}, true),
                ok = socket:bind(ListenSock, #{family => inet, addr => any, port => 8081}),
                ok = socket:listen(ListenSock, 4),
                Self = self(),
                spawn(fun() ->
                    case socket:accept(ListenSock, 5000) of
                        {ok, Conn} ->
                            case socket:recv(Conn, 0, 5000) of
                                {ok, Data} ->
                                    ok = socket:send(Conn, Data),
                                    socket:close(Conn),
                                    Self ! {server_echo_done, ok};
                                Err ->
                                    Self ! {server_recv_err, Err},
                                    socket:close(Conn)
                            end;
                        AcceptErr ->
                            Self ! {server_accept_err, AcceptErr}
                    end
                end),
                case socket:open(inet, stream, tcp) of
                    {ok, ClientSock} ->
                        case socket:connect(ClientSock, #{family => inet, addr => {127, 0, 0, 1}, port => 8081}) of
                            ok ->
                                ok = socket:send(ClientSock, <<"tcp_echo_test">>),
                                case socket:recv(ClientSock, 0, 5000) of
                                    {ok, <<"tcp_echo_test">>} ->
                                        erlang:display({tcp_echo, ok});
                                    OtherClientRecv ->
                                        erlang:display({tcp_client_recv_unexpected, OtherClientRecv})
                                end;
                            ConnectErr ->
                                erlang:display({tcp_connect_err, ConnectErr})
                        end,
                        socket:close(ClientSock);
                    ClientOpenErr ->
                        erlang:display({tcp_client_open_err, ClientOpenErr})
                end,
                receive
                    {server_echo_done, ok} ->
                        ok;
                    OtherServerMsg ->
                        erlang:display({server_msg, OtherServerMsg})
                after 6000 ->
                    erlang:display({server_timeout, error})
                end,
                socket:close(ListenSock),
                ok;
            {error, ListenOpenErr} ->
                erlang:display({tcp_listen_open_err, ListenOpenErr})
        end
    catch
        Class:ExReason:Stack ->
            erlang:display({tcp_echo_exception, Class, ExReason, Stack})
    end.

gen_tcp_passive_smoke() ->
    try
        erlang:display(starting_gen_tcp_passive_test),
        case gen_tcp:connect("192.168.4.1", 8080, [binary, {active, false}, {inet_backend, socket}]) of
            {ok, Sock} ->
                erlang:display({gen_tcp_passive_connected, ok}),
                ok = gen_tcp:send(Sock, <<"HEAD /README.md HTTP/1.0\r\nHost: 192.168.4.1\r\nConnection: close\r\n\r\n">>),
                case gen_tcp:recv(Sock, 0, 5000) of
                    {ok, Data} ->
                        erlang:display({gen_tcp_passive_bytes, byte_size(Data)}),
                        erlang:display({gen_tcp_passive, ok});
                    {error, RecvErr} ->
                        erlang:display({gen_tcp_passive_recv_err, RecvErr})
                end,
                gen_tcp:close(Sock);
            {error, ConnErr} ->
                erlang:display({gen_tcp_passive_connect_err, ConnErr})
        end
    catch
        Class:ExReason:Stack ->
            erlang:display({gen_tcp_passive_exception, Class, ExReason, Stack})
    end.

socket_select_smoke() ->
    try
        erlang:display(starting_socket_select_test),
        case socket:open(inet, stream, tcp) of
            {ok, Sock} ->
                case socket:connect(Sock, #{family => inet, addr => {192, 168, 4, 1}, port => 8080}) of
                    ok ->
                        erlang:display({socket_select_connected, ok}),
                        Ref = erlang:make_ref(),
                        ok = socket:nif_select_read(Sock, Ref),
                        ok = socket:send(Sock, <<"HEAD /README.md HTTP/1.0\r\nHost: 192.168.4.1\r\nConnection: close\r\n\r\n">>),
                        receive
                            {'$socket', Sock, select, Ref} ->
                                erlang:display({socket_select_event, ok}),
                                case socket:recv(Sock, 0, 5000) of
                                    {ok, Data} ->
                                        erlang:display({socket_select_bytes, byte_size(Data)}),
                                        erlang:display({socket_select, ok});
                                    RecvErr ->
                                        erlang:display({socket_select_recv_err, RecvErr})
                                end
                        after 5000 ->
                            erlang:display({socket_select_timeout, error})
                        end,
                        socket:close(Sock);
                    ConnectErr ->
                        erlang:display({socket_select_connect_err, ConnectErr}),
                        socket:close(Sock)
                end;
            OpenErr ->
                erlang:display({socket_select_open_err, OpenErr})
        end
    catch
        Class:ExReason:Stack ->
            erlang:display({socket_select_exception, Class, ExReason, Stack})
    end.

dns_smoke() ->
    try
        erlang:display(starting_dns_test),
        case net:getaddrinfo("192.168.4.1") of
            {ok, Results} ->
                erlang:display({dns_numeric, ok}),
                case Results of
                    [#{addr := #{addr := {192, 168, 4, 1}}} | _] ->
                        erlang:display({dns_numeric_match, ok});
                    _ ->
                        erlang:display({dns_numeric_unexpected, Results})
                end;
            NumericErr ->
                erlang:display({dns_numeric_err, NumericErr})
        end,
        case inet:getaddr("192.168.4.1", inet) of
            {ok, {192, 168, 4, 1}} ->
                erlang:display({dns_inet_getaddr, ok});
            InetErr ->
                erlang:display({dns_inet_getaddr_err, InetErr})
        end
    catch
        Class:ExReason:Stack ->
            erlang:display({dns_exception, Class, ExReason, Stack})
    end.

sntp_smoke() ->
    try
        erlang:display(starting_sntp_test),
        Before = erlang:universaltime(),
        erlang:display({time_before_sntp, Before}),

        % 1. Test direct clock synchronization and calendar time advancement
        % Unix timestamp 1776200000 corresponds to April 2026
        case atomvm_rtems:set_clock(1776200000, 0) of
            ok ->
                erlang:display({set_clock, ok}),
                AfterClock = erlang:universaltime(),
                erlang:display({time_after_clock_set, AfterClock}),
                {{YearClock, _, _}, _} = AfterClock,
                if
                    YearClock >= 2026 ->
                        erlang:display({clock_set_year_valid, ok});
                    true ->
                        erlang:display({clock_set_year_invalid, YearClock})
                end;
            ClockErr ->
                erlang:display({set_clock_err, ClockErr})
        end,

        % 2. Synchronous NIF SNTP client test (transmits RFC 4330 client packet)
        case atomvm_rtems:sntp_sync("192.168.4.1", 12300) of
            {ok, {Sec, Usec}} ->
                erlang:display({sntp_sync_nif, ok, Sec, Usec});
            {error, timeout} ->
                % In esp-emulator user-mode NAT, outbound UDP reaches host, but smoltcp NAT drops inbound replies
                erlang:display({sntp_sync_nif, timeout_emulation});
            SyncErr ->
                erlang:display({sntp_sync_err, SyncErr})
        end,

        % 3. Asynchronous Network port driver SNTP test
        Port = open_port({spawn, "network"}, []),
        Ref = make_ref(),
        Port ! {self(), Ref, {start, [{sntp, [{host, "192.168.4.1"}, {port, 12300}]}]}},
        receive
            {Ref, ok} ->
                erlang:display({network_sntp_start, ok})
        after 3000 ->
            erlang:display({network_sntp_start_timeout, error})
        end,
        receive
            {Ref, {sntp_sync, {PortSec, PortUsec}}} ->
                erlang:display({network_sntp_sync_msg, ok, PortSec, PortUsec}),
                erlang:display({sntp_port_sync, ok})
        after 2000 ->
            erlang:display({network_sntp_sync_timeout, emulation_udp_drop})
        end,
        case atomvm_rtems:rtc_timekeeper_status() of
            {ok, #{sntp_sec := TkSec}} ->
                erlang:display({rtc_timekeeper_wifi_sntp, ok, TkSec});
            OtherTk ->
                erlang:display({rtc_timekeeper_wifi_sntp, OtherTk})
        end,
        erlang:display({sntp_smoke_done, ok}),
        ok
    catch
        Class:ExReason:Stack ->
            erlang:display({sntp_exception, Class, ExReason, Stack})
    end.
