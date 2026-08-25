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

-export([test/0]).

-include("etest.hrl").

test() ->
    case is_ssl_available() of
        true ->
            test_ssl();
        false ->
            io:format("Warning: skipping test_ssl as ssl is not available\n"),
            ok
    end.

is_ssl_available() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            true;
        _ ->
            try
                ssl:nif_init(),
                true
            catch
                error:undef ->
                    false
            end
    end.

test_ssl() ->
    ok = ssl:start(),
    ok = test_start_twice(),
    ok = test_verify_peer_ip_requires_verification_name(),
    ok = test_cacert_errors(),
    ok = test_cacerts_override_cacertfile(),
    ok = test_connect_close(),
    ok = test_connect_error(),
    ok = test_send_recv(),
    ok = test_send_recv_zero(),
    ok = test_verify_peer_cacertfile(),
    ok = ssl:stop(),
    ok.

test_start_twice() ->
    ok = ssl:start().

test_verify_peer_ip_requires_verification_name() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            ok;
        _ ->
            {error, {options, missing_verification_name}} = ssl:connect({127, 0, 0, 1}, 443, [
                {verify, verify_peer}, {cacerts, []}, {active, false}
            ]),
            ok
    end.

test_cacert_errors() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            ok;
        _ ->
            {error, enoent} = ssl:connect("test.atomvm.org", 443, [
                {verify, verify_peer},
                {cacertfile, "/atomvm-test-missing/cacert.pem"},
                {active, false}
            ]),
            {error, invalid_cacert} = ssl:connect("test.atomvm.org", 443, [
                {verify, verify_peer}, {cacerts, [<<"invalid">>]}, {active, false}
            ]),
            ok
    end.

test_cacerts_override_cacertfile() ->
    case default_cacertfile() of
        undefined ->
            io:format("Warning: skipping CA precedence test, no system CA file~n"),
            ok;
        Path ->
            {ok, PemOrDer} = read_file(Path),
            CACerts =
                case erlang:system_info(machine) of
                    "BEAM" ->
                        [
                            Der
                         || {'Certificate', Der, not_encrypted} <- public_key:pem_decode(PemOrDer)
                        ];
                    _ ->
                        [PemOrDer]
                end,
            {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [
                {verify, verify_peer},
                {cacerts, CACerts},
                {cacertfile, "unused"},
                {active, false}
            ]),
            ok = ssl:close(SSLSocket),
            ok
    end.

test_connect_close() ->
    {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [{verify, verify_none}, {active, false}]),
    ok = ssl:close(SSLSocket).

test_connect_error() ->
    {error, _Error} = ssl:connect("test.atomvm.org", 80, [{verify, verify_none}, {active, false}]),
    ok.

test_send_recv() ->
    {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    UserAgent = erlang:system_info(machine),
    ok = ssl:send(SSLSocket, [
        <<"GET / HTTP/1.1\r\nHost: test.atomvm.org\r\nUser-Agent: ">>, UserAgent, <<"\r\n\r\n">>
    ]),
    {ok, <<"HTTP/1.1 200 OK">>} = ssl:recv(SSLSocket, 15),
    ok = ssl:close(SSLSocket),
    ok.

test_send_recv_zero() ->
    {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    UserAgent = erlang:system_info(machine),
    ok = ssl:send(SSLSocket, [
        <<"GET / HTTP/1.1\r\nHost: test.atomvm.org\r\nUser-Agent: ">>, UserAgent, <<"\r\n\r\n">>
    ]),
    {ok, <<"HTTP/1.1 200 OK", _/binary>>} = ssl:recv(SSLSocket, 0),
    ok = ssl:close(SSLSocket),
    ok.

test_verify_peer_cacertfile() ->
    case default_cacertfile() of
        undefined ->
            io:format("Warning: skipping verify_peer, no system CA file~n"),
            ok;
        Path ->
            {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [
                {verify, verify_peer}, {cacertfile, Path}, {active, false}, {binary, true}
            ]),
            ok = ssl:close(SSLSocket),
            ok
    end.

default_cacertfile() ->
    Candidates = [
        "/etc/ssl/cert.pem",
        "/etc/ssl/certs/ca-certificates.crt",
        "/etc/pki/tls/certs/ca-bundle.crt",
        "/etc/ssl/ca-bundle.pem"
    ],
    first_readable(Candidates).

first_readable([]) ->
    undefined;
first_readable([Path | Rest]) ->
    case readable_file(Path) of
        true -> Path;
        false -> first_readable(Rest)
    end.

readable_file(Path) ->
    case erlang:system_info(machine) of
        "BEAM" ->
            filelib:is_regular(Path);
        _ ->
            case atomvm:posix_open(Path, [o_rdonly]) of
                {ok, Fd} ->
                    _ = atomvm:posix_close(Fd),
                    true;
                {error, _} ->
                    false
            end
    end.

read_file(Path) ->
    case erlang:system_info(machine) of
        "BEAM" ->
            file:read_file(Path);
        _ ->
            {ok, Fd} = atomvm:posix_open(Path, [o_rdonly]),
            try
                read_file(Fd, [])
            after
                _ = atomvm:posix_close(Fd)
            end
    end.

read_file(Fd, Acc) ->
    case atomvm:posix_read(Fd, 4096) of
        {ok, Chunk} -> read_file(Fd, [Chunk | Acc]);
        eof -> {ok, iolist_to_binary(lists:reverse(Acc))}
    end.
