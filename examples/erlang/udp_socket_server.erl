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

-module(udp_socket_server).

-export([start/0]).

-spec start() -> no_return().
start() ->
    {ok, Socket} = socket:open(inet, dgram, udp),

    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:setopt(Socket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(Socket, #{family => inet, addr => any, port => 44405}),
    io:format("Bound to ~p.~n", [socket:sockname(Socket)]),

    spawn(fun() -> loop(Socket) end),
    timer:sleep(infinity).

loop(Socket) ->
    io:format("Waiting to receive data...~n"),
    case socket:recvfrom(Socket) of
        {ok, {Source, Data}} ->
            io:format("Received packet ~p from ~p.~n", [
                Data, Source
            ]);
        {error, Reason} ->
            io:format("An error occurred receiving data on a socket: ~p~n", [Reason])
    end,
    loop(Socket).
