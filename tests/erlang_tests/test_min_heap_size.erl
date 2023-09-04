%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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

-module(test_min_heap_size).

-export([start/0, loop/1]).

start() ->
    Self = self(),
    Pid1 = spawn_opt(?MODULE, loop, [Self], []),
    receive
        ok -> ok
    end,
    receive
    after 100 ->
        ok
    end,
    {memory, Pid1MemorySize} = process_info(Pid1, memory),
    case erlang:system_info(machine) of
        "BEAM" -> ok;
        _ -> assert(Pid1MemorySize < 1024)
    end,
    Pid2 = spawn_opt(?MODULE, loop, [Self], [{min_heap_size, 1024}]),
    receive
        ok -> ok
    end,
    receive
    after 100 ->
        ok
    end,
    {memory, Pid2MemorySize} = process_info(Pid2, memory),
    assert(1024 =< Pid2MemorySize),
    Pid1 ! {Self, stop},
    receive
        ok -> ok
    end,
    Pid2 ! {Self, stop},
    receive
        ok -> ok
    end,
    0.

loop(undefined) ->
    receive
        {Pid, stop} ->
            Pid ! ok
    after 10 ->
        erlang:garbage_collect(),
        loop(undefined)
    end;
loop(Pid) ->
    Pid ! ok,
    loop(undefined).

assert(true) -> ok.
