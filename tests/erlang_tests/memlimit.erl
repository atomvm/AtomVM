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

-module(memlimit).

-export([start/0, loop/1]).

start() ->
    Pid = spawn_opt(?MODULE, loop, [[]], [{max_heap_size, 1024}]),
    grow(Pid, 0).

grow(Pid, LastSize) ->
    Pid ! {self(), LastSize},
    receive
        NewSize ->
            grow(Pid, NewSize)
    after 1000 -> LastSize
    end.

loop(Data) ->
    case handle_request(Data) of
        terminate ->
            terminate;
        NewData ->
            loop(NewData)
    end.

handle_request(Data) ->
    receive
        terminate ->
            terminate;
        {Pid, Item} ->
            NewData = [Item | Data],
            Pid ! erts_debug:flat_size(NewData),
            NewData
    end.
