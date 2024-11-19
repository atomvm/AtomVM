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

-module(test_http_server).
-export([test/0, handle_req/3]).

test() ->
    ok = test_chunk().

test_chunk() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    Pid = http_server:start_server(8080, Router),
    {ok, Conn} = connect_client(5),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    ok = loop_passive(Conn2, []),
    exit(Pid, kill),
    ok.

connect_client(Retries) when Retries > 0 ->
    ConnectResult = ahttp_client:connect(http, "localhost", 8080, [{active, false}]),
    case ConnectResult of
        {ok, Conn} ->
            {ok, Conn};
        {error, _} = ConnectError ->
            io:format("Request failed: ~p~n", [ConnectError]),
            connect_client(Retries - 1)
    end.

handle_req("GET", [], Conn) ->
    Body = [34, <<"hello">>],
    http_server:reply(200, Body, Conn).

% http_server doesn't send Content-Length, so ahttp_client doesn't know when it's done and reports closed connection
loop_passive(Conn, AccResp) ->
    case ahttp_client:recv(Conn, 0) of
        {ok, UpdatedConn, Responses} ->
            loop_passive(UpdatedConn, lists:reverse(Responses, AccResp));
        {error, {gen_tcp, closed}} ->
            [{data, _DataRef, <<"\"hello">>}, {status, _StatusRef, 200}] = AccResp,
            ok
    end.
