%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_ssl).
-export([start/0]).

% TLS terminates at github.com; HTTP Host targets test.atomvm.org (ESP32 qemu pattern).
-define(HOST, "github.com").
-define(SELECT_TIMEOUT, 15000).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            ok = start_network(),
            try
                run_ssl()
            after
                network:stop()
            end;
        Error ->
            Error
    end.

start_network() ->
    Self = self(),
    Config = [
        {sta, [
            {connected, fun() -> Self ! sta_connected end},
            {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end},
            {disconnected, fun() -> Self ! sta_disconnected end},
            {ssid, "Wokwi-GUEST"},
            {psk, ""}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            wait_for_ip(20000);
        Error ->
            Error
    end.

wait_for_ip(Timeout) ->
    receive
        {got_ip, IpInfo} ->
            io:format("Got IP: ~p~n", [IpInfo]),
            ok
    after Timeout ->
        {error, timeout}
    end.

verify_platform(esp32) ->
    ok;
verify_platform(zephyr) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.

run_ssl() ->
    % start SSL
    io:format("test_ssl: init rng.~n"),
    Entropy = ssl:nif_entropy_init(),
    CtrDrbg = ssl:nif_ctr_drbg_init(),
    ok = ssl:nif_ctr_drbg_seed(CtrDrbg, Entropy, <<"AtomVM">>),
    % Get address of github.com
    io:format("test_ssl: dns.~n"),
    {ok, Results} = net:getaddrinfo_nif(?HOST, undefined),
    [TCPAddr | _] = [
        Addr
     || #{addr := #{addr := Addr}, type := stream, protocol := tcp, family := inet} <- Results
    ],
    % Connect to github.com:443
    io:format("test_ssl: tcp connect.~n"),
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{family => inet, addr => TCPAddr, port => 443}),
    % Initialize SSL Socket and config
    io:format("test_ssl: ssl init.~n"),
    SSLContext = ssl:nif_init(),
    ok = ssl:nif_set_bio(SSLContext, Socket),
    SSLConfig = ssl:nif_config_init(),
    ok = ssl:nif_config_defaults(SSLConfig, client, stream),
    ok = ssl:nif_set_hostname(SSLContext, ?HOST),
    ok = ssl:nif_conf_authmode(SSLConfig, none),
    ok = ssl:nif_conf_rng(SSLConfig, CtrDrbg),
    io:format("test_ssl: ssl setup.~n"),
    ok = ssl:nif_setup(SSLContext, SSLConfig),
    % Handshake
    io:format("test_ssl: handshake.~n"),
    ok = handshake_loop(SSLContext, Socket),
    % Write
    io:format("test_ssl: send.~n"),
    ok = send_loop(SSLContext, Socket, request()),
    % Read
    io:format("test_ssl: recv.~n"),
    {ok, Prefix} = recv_loop(SSLContext, Socket, 8, []),
    ok = assert_http_response(Prefix),
    % Close
    io:format("test_ssl: close.~n"),
    ok = close(SSLContext, Socket),
    ok.

request() ->
    <<"GET / HTTP/1.1\r\nHost: github.com\r\nConnection: close\r\nUser-Agent: AtomVM Zephyr Wokwi\r\n\r\n">>.

assert_http_response(<<"HTTP/", _/binary>>) ->
    ok;
assert_http_response(Response) ->
    {error, {unexpected_response, Response}}.

close(SSLContext, Socket) ->
    Result =
        case close_notify_loop(SSLContext, Socket) of
            ok -> ok;
            {error, closed} -> ok;
            Error -> Error
        end,
    case socket:close(Socket) of
        ok -> Result;
        {error, closed} -> Result;
        CloseError when Result =:= ok -> CloseError;
        _CloseError -> Result
    end.

handshake_loop(SSLContext, Socket) ->
    case ssl:nif_handshake_step(SSLContext) of
        ok ->
            handshake_loop(SSLContext, Socket);
        done ->
            ok;
        want_read ->
            case wait_read(Socket) of
                ok ->
                    handshake_loop(SSLContext, Socket);
                {error, _Reason} = Error ->
                    socket:close(Socket),
                    Error
            end;
        want_write ->
            handshake_loop(SSLContext, Socket);
        {error, _Reason} = Error ->
            socket:close(Socket),
            Error
    end.

send_loop(SSLContext, Socket, Binary) ->
    case ssl:nif_write(SSLContext, Binary) of
        ok ->
            ok;
        {ok, Rest} ->
            send_loop(SSLContext, Socket, Rest);
        want_read ->
            case wait_read(Socket) of
                ok ->
                    send_loop(SSLContext, Socket, Binary);
                {error, _Reason} = Error ->
                    Error
            end;
        want_write ->
            send_loop(SSLContext, Socket, Binary);
        {error, _Reason} = Error ->
            Error
    end.

recv_loop(_SSLContext, _Socket, 0, Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
recv_loop(SSLContext, Socket, Remaining, Acc) ->
    case ssl:nif_read(SSLContext, Remaining) of
        {ok, Data} ->
            Len = byte_size(Data),
            recv_loop(SSLContext, Socket, Remaining - Len, [Data | Acc]);
        want_read ->
            case wait_read(Socket) of
                ok ->
                    recv_loop(SSLContext, Socket, Remaining, Acc);
                {error, _Reason} = Error ->
                    Error
            end;
        want_write ->
            recv_loop(SSLContext, Socket, Remaining, Acc);
        {error, _Reason} = Error ->
            Error
    end.

close_notify_loop(SSLContext, Socket) ->
    case ssl:nif_close_notify(SSLContext) of
        ok ->
            ok;
        want_read ->
            case wait_read(Socket) of
                ok ->
                    close_notify_loop(SSLContext, Socket);
                {error, _Reason} = Error ->
                    Error
            end;
        want_write ->
            close_notify_loop(SSLContext, Socket);
        {error, _Reason} = Error ->
            Error
    end.

wait_read(Socket) ->
    Ref = erlang:make_ref(),
    case socket:nif_select_read(Socket, Ref) of
        ok ->
            receive
                {'$socket', Socket, select, Ref} ->
                    ok;
                {'$socket', Socket, abort, {Ref, closed}} ->
                    {error, closed}
            after ?SELECT_TIMEOUT ->
                {error, timeout}
            end;
        {error, _Reason} = Error ->
            Error
    end.
