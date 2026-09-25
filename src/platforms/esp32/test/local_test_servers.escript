#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% This file is part of AtomVM.
%%
%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%

%% Local endpoints for the qemu network tests (test_socket, test_ssl).
%%
%% The qemu guest reaches this host at 10.0.2.2 (SLIRP user networking), so
%% test_socket and test_ssl talk to these servers instead of an external site:
%% CI runner egress is slow and lossy enough that external connections pile up
%% in lwIP until the board runs out of memory.
%%
%% - port 80:  answers any request with an HTTP/1.1 301, like http://github.com
%% - port 443: TLS (self-signed, clients don't verify), answers HTTP/1.1 200
%% - port 53:  minimal UDP DNS responder, so the UDP test needs no real resolver
%%
%% Usage: escript local_test_servers.escript [--certdir DIR]

main(Args) ->
    CertDir = cert_dir(Args),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, HTTPListen} = gen_tcp:listen(80, [binary, {active, false}, {reuseaddr, true}]),
    {ok, TLSListen} = ssl:listen(443, [binary, {active, false}, {reuseaddr, true} | tls_opts(CertDir)]),
    {ok, DNSSocket} = gen_udp:open(53, [binary, {active, false}, {reuseaddr, true}]),
    spawn_link(fun() -> http_loop(HTTPListen) end),
    spawn_link(fun() -> tls_loop(TLSListen) end),
    io:format("listening on 0.0.0.0:80 (http), 0.0.0.0:443 (tls) and 0.0.0.0:53 (dns)~n"),
    dns_loop(DNSSocket).

cert_dir(["--certdir", Dir | _]) -> Dir;
cert_dir([_ | Rest]) -> cert_dir(Rest);
cert_dir([]) -> "/tmp".

%% A self-signed RSA server certificate the client accepts without validation
%% (authmode none). Generated with openssl, like the retired python server.
tls_opts(CertDir) ->
    Cert = filename:join(CertDir, "cert.pem"),
    Key = filename:join(CertDir, "key.pem"),
    case filelib:is_file(Cert) andalso filelib:is_file(Key) of
        true ->
            ok;
        false ->
            Command = lists:flatten(io_lib:format(
                "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts"
                " -days 30 -subj /CN=10.0.2.2 2>&1",
                [Key, Cert]
            )),
            Output = os:cmd(Command),
            filelib:is_file(Cert) andalso filelib:is_file(Key) orelse
                fail("openssl could not generate a certificate: ~ts", [Output])
    end,
    [{certfile, Cert}, {keyfile, Key}].

http_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    try
        handle_http(Socket)
    catch
        _:_ -> ok
    end,
    http_loop(Listen).

handle_http(Socket) ->
    _ = gen_tcp:recv(Socket, 0, 10000),
    ok = gen_tcp:send(Socket, [
        <<"HTTP/1.1 301 Moved Permanently\r\n">>,
        <<"Content-Length: 0\r\n">>,
        <<"Location: https://10.0.2.2/\r\n">>,
        <<"Connection: close\r\n\r\n">>
    ]),
    gen_tcp:close(Socket).

tls_loop(Listen) ->
    case ssl:transport_accept(Listen, 30000) of
        {ok, Transport} ->
            try
                {ok, Socket} = ssl:handshake(Transport, 10000),
                handle_tls(Socket)
            catch
                _:_ -> ok
            end;
        _ ->
            ok
    end,
    tls_loop(Listen).

handle_tls(Socket) ->
    Body = <<"ok">>,
    _ = ssl:recv(Socket, 0, 10000),
    ok = ssl:send(Socket, [
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Content-Type: text/plain\r\n">>,
        [<<"Content-Length: ">>, integer_to_binary(byte_size(Body)), <<"\r\n">>],
        <<"Connection: close\r\n\r\n">>,
        Body
    ]),
    %% Let the client send its close_notify first: closing right after the
    %% reply can turn into an RST that races the client's shutdown.
    drain_tls(Socket),
    ssl:close(Socket).

drain_tls(Socket) ->
    case ssl:recv(Socket, 0, 5000) of
        {ok, _} -> drain_tls(Socket);
        _ -> ok
    end.

dns_loop(Socket) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Address, Port, <<Id:2/binary, _Flags:2/binary, Rest/binary>>}} ->
            %% The guest only checks the transaction id and the QR bit, so echo
            %% the id and rewrite the header flags to a response (QR=1, RD=1,
            %% RA=1) without synthesizing answer records.
            ok = gen_udp:send(Socket, Address, Port, <<Id/binary, 16#81, 16#80, Rest/binary>>);
        _ ->
            ok
    end,
    dns_loop(Socket).

fail(Format, Args) ->
    io:format(standard_error, Format ++ "~n", Args),
    halt(1).
