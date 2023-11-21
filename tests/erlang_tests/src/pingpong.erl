%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(pingpong).

-export([start/0, ping/2, pong/2]).

start() ->
    Echo = echo:start(),
    Pong = spawn_opt(?MODULE, pong, [Echo, self()], []),
    spawn_opt(?MODULE, ping, [Echo, Pong], []),
    receive
        exit -> 1;
        _Any -> 0
    end.

ping(Echo, Pong) ->
    ping(Echo, Pong, 0).

ping(_Echo, Pong, 10000) ->
    Pong ! exit;
ping(Echo, Pong, N) ->
    Pong ! {self(), ping, f(N, N)},
    receive
        {Pong, pong, M} ->
            echo:puts("+"),
            ping(Echo, Pong, M)
    end.

pong(Echo, Main) ->
    receive
        {Ping, ping, N} ->
            echo:puts("-"),
            Ping ! {self(), pong, f(N, N + 1)},
            pong(Echo, Main);
        exit ->
            Main ! exit
    end.

f(N, M) when N < 1 ->
    M;
f(N, M) ->
    f((N - 1) rem 16, M).
