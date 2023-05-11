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

-module(udp_socket_client).

-export([start/0]).

start() ->
    {ok, Socket} = socket:open(inet, dgram, udp),

    loop(Socket).

loop(Socket) ->
    Dest = #{addr => loopback, port => 44405},
    io:format("Sending packet to ~p...~n", [Dest]),
    Data = <<"Hello AtomVM!\n">>,
    case socket:sendto(Socket, Data, Dest) of
        {ok, _Rest} ->
            io:format("Send packet ~p to ~p.~n", [
                Data, Dest
            ]),
            timer:sleep(1000),
            loop(Socket);
        {error, closed} ->
            io:format("closed connection.~n");
        Error ->
            io:format("An error occurred sending a packet: ~p~n", [Error])
    end.
