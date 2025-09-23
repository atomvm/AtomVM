%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(http_client).
-export([start/0]).

-define(ACTIVE, false).

start() ->
    ssl:start(),
    ConnectResult = ahttp_client:connect(https, "test.atomvm.org", 443, [
        {active, ?ACTIVE}, {verify, verify_none}, {parse_headers, [<<"Location">>]}
    ]),
    case ConnectResult of
        {ok, Conn} ->
            case ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined) of
                {ok, Conn2, _Ref} ->
                    loop(Conn2);
                {error, _} = RequestError ->
                    io:format("Request failed: ~p~n", [RequestError]),
                    RequestError
            end;
        {error, _} = ConnectError ->
            io:format("Request failed: ~p~n", [ConnectError]),
            ConnectError
    end.

-if(?ACTIVE).
loop(Conn) ->
    receive
        Message ->
            case ahttp_client:stream(Conn, Message) of
                {ok, _Conn, closed} ->
                    io:format("Connection closed.~n"),
                    ok;
                {ok, UpdatedConn, Responses} ->
                    io:format("Got: ~p~n", [Responses]),
                    case maybe_terminate(Responses, UpdatedConn) of
                        ok -> loop(UpdatedConn);
                        closed -> ok
                    end;
                unknown ->
                    io:format("Unexpected message: ~p~n", [Message]),
                    error
            end
    end.
-else.
loop(Conn) ->
    case ahttp_client:recv(Conn, 0) of
        {ok, UpdatedConn, Responses} ->
            io:format("Got: ~p~n", [Responses]),
            case maybe_terminate(Responses, UpdatedConn) of
                ok -> loop(UpdatedConn);
                closed -> ok
            end;
        Other ->
            io:format("Unexpected reply: ~p~n", [Other]),
            error
    end.
-endif.

maybe_terminate([], _Conn) ->
    ok;
maybe_terminate([{done, _Ref}], Conn) ->
    ahttp_client:close(Conn),
    closed;
maybe_terminate([_H | T], Conn) ->
    maybe_terminate(T, Conn).
