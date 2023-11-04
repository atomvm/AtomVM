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
        <<"GET / HTTP/1.1\r\nHost: atomvm.net\r\nUser-Agent: AtomVM within qemu\r\n\r\n">>
    ),
    % Read
    {ok, <<"HTTP/1.1">>} = recv_loop(SSLContext, Socket, 8, []),
    % Close
    ok = close_notify_loop(SSLContext, Socket),
    ok = socket:close(Socket),
    ok.

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
                        {select, _SocketResource, Ref, ready_input} ->
                            handshake_loop(SSLContext, Socket);
                        {closed, Ref} ->
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
                        {select, _SocketResource, Ref, ready_input} ->
                            send_loop(SSLContext, Socket, Binary);
                        {closed, Ref} ->
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
                        {select, _SocketResource, Ref, ready_input} ->
                            recv_loop(SSLContext, Socket, Remaining, Acc);
                        {closed, Ref} ->
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
                        {select, _SocketResource, Ref, ready_input} ->
                            close_notify_loop(SSLContext, Socket);
                        {closed, Ref} ->
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
