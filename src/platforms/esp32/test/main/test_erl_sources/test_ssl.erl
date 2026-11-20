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

start() ->
    % start SSL
    ok = ssl:start(),
    try
        ok = test_negotiated_tls_version()
    after
        ok = ssl:stop()
    end,
    Entropy = ssl:nif_entropy_init(),
    CtrDrbg = ssl:nif_ctr_drbg_init(),
    ok = ssl:nif_ctr_drbg_seed(CtrDrbg, Entropy, <<"AtomVM">>),
    % Get address of github.com
    {ok, Results} = net:getaddrinfo_nif("github.com", undefined),
    [TCPAddr | _] = [
        Addr
     || #{addr := #{addr := Addr}, type := stream, protocol := tcp, family := inet} <- Results
    ],
    % Connect to github.com:443
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{family => inet, addr => TCPAddr, port => 443}),
    % Initialize SSL Socket and config
    SSLContext = ssl:nif_init(),
    ok = ssl:nif_set_bio(SSLContext, Socket),
    SSLConfig = ssl:nif_config_init(),
    ok = ssl:nif_config_defaults(SSLConfig, client, stream),
    ok = ssl:nif_set_hostname(SSLContext, "github.com"),
    ok = ssl:nif_conf_authmode(SSLConfig, none),
    ok = ssl:nif_conf_rng(SSLConfig, CtrDrbg),
    ok = ssl:nif_setup(SSLContext, SSLConfig),
    % Handshake
    ok = handshake_loop(SSLContext, Socket),
    % Write
    ok = send_loop(
        SSLContext,
        Socket,
        <<"GET / HTTP/1.1\r\nHost: test.atomvm.org\r\nUser-Agent: AtomVM within qemu\r\n\r\n">>
    ),
    % Read
    {ok, <<"HTTP/1.1">>} = recv_loop(SSLContext, Socket, 8, []),
    % Close
    ok = close_notify_loop(SSLContext, Socket),
    ok = socket:close(Socket),
    ok.

test_negotiated_tls_version() ->
    % Bound DNS, handshake and response reads: AtomVM's SSL API has no timeout.
    {Pid, Ref} = spawn_monitor(fun check_negotiated_tls_version/0),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok;
        {'DOWN', Ref, process, Pid, Reason} -> erlang:error({tls_test_failed, Reason})
    after 30000 ->
        exit(Pid, kill),
        erlang:error(tls_test_timeout)
    end.

check_negotiated_tls_version() ->
    Expected = expected_tls_version(),
    {ok, SSLSocket} = ssl:connect("check-tls.akamai.io", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    try
        % HTTP/1.0 avoids chunked transfer encoding and closes after the response.
        ok = ssl:send(SSLSocket, [
            <<"GET /v1/tlsinfo.json HTTP/1.0\r\nHost: check-tls.akamai.io\r\n">>,
            <<"Connection: close\r\n\r\n">>
        ]),
        Response = recv_response(SSLSocket, []),
        [Headers, Body] = binary:split(Response, <<"\r\n\r\n">>),
        [Status | HeaderLines] = binary:split(Headers, <<"\r\n">>, [global]),
        [_, <<"200">>, _] = binary:split(Status, <<" ">>, [global]),
        [Length] = [Value || <<"Content-Length: ", Value/binary>> <- HeaderLines],
        BodySize = binary_to_integer(Length),
        BodySize = byte_size(Body),
        #{<<"tls_version">> := Negotiated} = json:decode(Body),
        io:format("TLS negotiated ~s; expected ~s~n", [Negotiated, Expected]),
        Expected = Negotiated,
        ok
    after
        ok = ssl:close(SSLSocket)
    end.

recv_response(SSLSocket, Acc) ->
    case ssl:recv(SSLSocket, 0) of
        {ok, Data} -> recv_response(SSLSocket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc));
        % AtomVM exposes MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY as an integer.
        {error, -16#7880} -> iolist_to_binary(lists:reverse(Acc));
        {error, Reason} -> erlang:error({tls_recv_failed, Reason})
    end.

% The flag reflects the build configuration, not the observed negotiation.
-ifdef(TEST_SSL_TLS13).
expected_tls_version() ->
    {<<"mbedtls">>, Version, _} = lists:keyfind(<<"mbedtls">>, 1, crypto:info_lib()),
    if
        Version >= 16#03060100 -> <<"tls1.3">>;
        true -> <<"tls1.2">>
    end.
-else.
expected_tls_version() ->
    <<"tls1.2">>.
-endif.

handshake_loop(SSLContext, Socket) ->
    case ssl:nif_handshake_step(SSLContext) of
        ok ->
            handshake_loop(SSLContext, Socket);
        done ->
            ok;
        want_read ->
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {'$socket', Socket, select, Ref} ->
                            handshake_loop(SSLContext, Socket);
                        {'$socket', Socket, abort, {Ref, closed}} ->
                            ok = socket:close(Socket),
                            {error, closed}
                    end;
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
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {'$socket', Socket, select, Ref} ->
                            send_loop(SSLContext, Socket, Binary);
                        {'$socket', Socket, abort, {Ref, closed}} ->
                            {error, closed}
                    end;
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
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {'$socket', Socket, select, Ref} ->
                            recv_loop(SSLContext, Socket, Remaining, Acc);
                        {'$socket', Socket, abort, {Ref, closed}} ->
                            {error, closed}
                    end;
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
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {'$socket', Socket, select, Ref} ->
                            close_notify_loop(SSLContext, Socket);
                        {'$socket', Socket, abort, {Ref, closed}} ->
                            {error, closed}
                    end;
                {error, _Reason} = Error ->
                    Error
            end;
        want_write ->
            close_notify_loop(SSLContext, Socket);
        {error, _Reason} = Error ->
            Error
    end.
