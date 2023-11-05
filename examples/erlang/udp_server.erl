%
% This file is part of AtomVM.
%
% Copyright 2018-2020 Fred Dushin <fred@dushin.net>
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

-module(udp_server).

-export([start/0]).

start() ->
    case gen_udp:open(44404, [{active, true}, binary]) of
        {ok, Socket} ->
            io:format("Opened UDP socket on ~s.~n", [local_address(Socket)]),
            active_loop();
        Error ->
            io:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.

active_loop() ->
    io:format("Waiting to receive data...~n"),
    receive
        {udp, _Socket, Address, Port, Packet} ->
            io:format("Received UDP packet ~p from ~s~n", [Packet, to_string({Address, Port})])
    end,
    active_loop().

local_address(Socket) ->
    {ok, SockName} = inet:sockname(Socket),
    to_string(SockName).

to_string({{A, B, C, D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]).
