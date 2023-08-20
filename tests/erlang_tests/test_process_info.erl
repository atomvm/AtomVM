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

-module(test_process_info).

-export([start/0, loop/2]).

start() ->
    Self = self(),
    {Pid, Ref} = spawn_opt(?MODULE, loop, [Self, []], [monitor]),
    receive
        ok -> ok
    end,
    test_message_queue_len(Pid, Self),
    {links, []} = process_info(Pid, links),
    link(Pid),
    {links, [Self]} = process_info(Pid, links),
    unlink(Pid),
    {links, []} = process_info(Pid, links),
    Pid ! {Self, stop},
    _Accum =
        receive
            {Pid, result, X} -> X
        end,
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            assert(Reason =:= normal)
    end,
    MessageQueueLen = process_info(Pid, message_queue_len),
    assert(MessageQueueLen =:= undefined),
    0.

test_message_queue_len(Pid, Self) ->
    {message_queue_len, MessageQueueLen} = process_info(Pid, message_queue_len),
    {memory, Memory} = process_info(Pid, memory),
    {heap_size, HeapSize} = process_info(Pid, heap_size),
    {total_heap_size, TotalHeapSize} = process_info(Pid, total_heap_size),
    Pid ! incr,
    Pid ! incr,
    Pid ! incr,
    {message_queue_len, MessageQueueLen2} = process_info(Pid, message_queue_len),
    {memory, Memory2} = process_info(Pid, memory),
    Pid ! unlock,
    Pid ! {Self, ping},
    receive
        pong -> ok
    end,
    {total_heap_size, TotalHeapSize2} = process_info(Pid, total_heap_size),
    {heap_size, HeapSize2} = process_info(Pid, heap_size),
    assert(MessageQueueLen < MessageQueueLen2),
    case erlang:system_info(machine) of
        "BEAM" ->
            assert(Memory =< Memory2),
            assert(HeapSize =< TotalHeapSize),
            assert(HeapSize2 =< TotalHeapSize2),
            assert(TotalHeapSize =< TotalHeapSize2);
        _ ->
            assert(Memory < Memory2),
            assert(HeapSize =< TotalHeapSize),
            assert(HeapSize2 =< TotalHeapSize2),
            assert(TotalHeapSize < TotalHeapSize2)
    end.

loop(undefined, Accum) ->
    receive
        {Pid, stop} ->
            Pid ! {self(), result, Accum};
        incr ->
            loop(undefined, [incr | Accum]);
        {Pid, ping} ->
            Pid ! pong,
            loop(undefined, Accum)
    end;
loop(locked, Accum) ->
    receive
        unlock -> loop(undefined, Accum)
    end;
loop(Pid, Accum) ->
    Pid ! ok,
    loop(locked, Accum).

assert(true) -> ok.
