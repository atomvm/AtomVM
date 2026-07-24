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

% process_info/1 is implemented in estdlib's erlang module, so it is
% covered by tests/libs/estdlib/test_process_info.erl; only the native
% process_info/2 paths are tested here.
start() ->
    ok = test_registered_name(),
    ok = test_heap_size(),
    ok = test_memory(),
    ok = test_total_heap_size(),
    ok = test_message_queue_len(),
    ok = test_links(),
    ok = test_monitored_by(),

    ok = test_list_semantics(),
    ok = test_badargs(),
    ok = test_external_pid(),

    0.

with_other_pid(Fun) ->
    {Pid, Ref} = spawn_opt(
        fun() ->
            receive
                quit -> ok
            end
        end,
        [monitor]
    ),
    Fun(Pid),
    Pid ! quit,
    normal =
        receive
            {'DOWN', Ref, process, Pid, Reason} -> Reason
        end.

repeat(0, _Item) ->
    [];
repeat(N, Item) ->
    [Item | repeat(N - 1, Item)].

with_dead_pid(Fun) ->
    {DeadPid, Ref} = spawn_opt(fun() -> ok end, [monitor]),
    normal =
        receive
            {'DOWN', Ref, process, DeadPid, Reason} -> Reason
        end,
    Fun(DeadPid).

get_item(Pid, Item) ->
    {Item, Val} = process_info(Pid, Item),
    [{Item, Val}] = process_info(Pid, [Item]),
    Val.

test_registered_name() ->
    Check = fun(Pid) ->
        [] = process_info(Pid, registered_name),
        [{registered_name, []}] = process_info(Pid, [registered_name]),

        erlang:register(has_name, Pid),
        has_name = get_item(Pid, registered_name),

        erlang:unregister(has_name),
        [] = process_info(Pid, registered_name),
        [{registered_name, []}] = process_info(Pid, [registered_name])
    end,

    Check(self()),
    with_other_pid(Check),

    ok.

test_heap_size() ->
    Self = self(),
    HS = get_item(Self, heap_size),
    true = is_integer(HS) andalso HS > 0,

    with_other_pid(fun(Pid) ->
        HS2 = get_item(Pid, heap_size),
        true = is_integer(HS2) andalso HS2 > 0
    end),

    with_dead_pid(fun(DeadPid) ->
        undefined = process_info(DeadPid, heap_size)
    end),

    ok.

test_memory() ->
    Self = self(),
    M = get_item(Self, memory),
    true = is_integer(M) andalso M > 0,

    with_other_pid(fun(Pid) ->
        M2 = get_item(Pid, memory),
        true = is_integer(M2) andalso M2 > 0
    end),

    with_dead_pid(fun(DeadPid) ->
        undefined = process_info(DeadPid, memory)
    end),

    ok.

test_total_heap_size() ->
    Self = self(),
    HS = get_item(Self, heap_size),
    THS = get_item(Self, total_heap_size),
    true = HS =< THS,

    with_other_pid(fun(Pid) ->
        HS2 = get_item(Pid, heap_size),
        THS2 = get_item(Pid, total_heap_size),
        true = HS2 =< THS2,
        verify_stable_total_heap_size(Pid, THS2, 50)
    end),

    with_dead_pid(fun(DeadPid) ->
        undefined = process_info(DeadPid, total_heap_size)
    end),

    ok.

verify_stable_total_heap_size(_Pid, _THS, 0) ->
    ok;
verify_stable_total_heap_size(Pid, THS, N) ->
    {total_heap_size, THS} = process_info(Pid, total_heap_size),
    verify_stable_total_heap_size(Pid, THS, N - 1).

test_message_queue_len() ->
    Self = self(),
    {Pid, Ref} = spawn_opt(?MODULE, loop, [Self, []], [monitor]),
    receive
        ok -> ok
    end,

    Q0 = get_item(Pid, message_queue_len),

    Pid ! incr,
    Pid ! incr,
    Pid ! incr,

    Q1 = get_item(Pid, message_queue_len),
    true = Q0 < Q1,

    Pid ! unlock,
    Pid ! {Self, ping},
    receive
        pong -> ok
    end,
    Q2 = get_item(Pid, message_queue_len),
    true = Q2 < Q1,

    Pid ! {Self, stop},
    receive
        {Pid, result, _} -> ok
    end,
    normal =
        receive
            {'DOWN', Ref, process, Pid, Reason} -> Reason
        end,

    undefined = process_info(Pid, message_queue_len),

    ok.

test_links() ->
    Self = self(),

    with_other_pid(fun(Pid) ->
        [] = get_item(Pid, links),

        link(Pid),
        [Self] = get_item(Pid, links),

        unlink(Pid),
        [] = get_item(Pid, links)
    end),

    ok.

test_monitored_by() ->
    Self = self(),
    [] = get_item(Self, monitored_by),

    {Pid, Ref} = spawn_opt(
        fun() ->
            receive
                quit -> ok
            end
        end,
        [monitor]
    ),

    % spawn_opt with [monitor] counts as one monitor from Self
    [Self] = get_item(Pid, monitored_by),

    Mon = monitor(process, Pid),
    [Self, Self] = get_item(Pid, monitored_by),

    demonitor(Mon),
    [Self] = get_item(Pid, monitored_by),

    Pid ! quit,
    normal =
        receive
            {'DOWN', Ref, process, Pid, Reason} -> Reason
        end,

    ok.

test_list_semantics() ->
    Self = self(),

    [] = process_info(Self, []),

    [{heap_size, _}, {memory, _}] = process_info(Self, [heap_size, memory]),
    [{memory, _}, {heap_size, _}] = process_info(Self, [memory, heap_size]),

    [{total_heap_size, THS}, {total_heap_size, THS}] =
        process_info(Self, [total_heap_size, total_heap_size]),

    LongRequest = repeat(100, total_heap_size),
    [{total_heap_size, SelfTHS} | SelfRest] = process_info(Self, LongRequest),
    99 = length(SelfRest),
    [] = [Item || Item <- SelfRest, Item =/= {total_heap_size, SelfTHS}],

    with_other_pid(fun(Pid) ->
        [] = process_info(Pid, []),
        [{message_queue_len, _}, {heap_size, _}, {memory, _}] =
            process_info(Pid, [message_queue_len, heap_size, memory]),

        [{total_heap_size, OtherTHS} | OtherRest] = process_info(Pid, LongRequest),
        99 = length(OtherRest),
        [] = [Item || Item <- OtherRest, Item =/= {total_heap_size, OtherTHS}]
    end),

    with_dead_pid(fun(DeadPid) ->
        undefined = process_info(DeadPid, []),
        undefined = process_info(DeadPid, [heap_size]),
        undefined = process_info(DeadPid, [heap_size, memory])
    end),

    ok.

test_badargs() ->
    Self = self(),

    assert_badarg(fun() -> process_info(bad_pid, heap_size) end),
    assert_badarg(fun() -> process_info(Self, bad_item) end),

    assert_badarg(fun() -> process_info(Self, 42) end),
    assert_badarg(fun() -> process_info(Self, {heap_size}) end),

    assert_badarg(fun() -> process_info(bad_pid, [heap_size]) end),
    assert_badarg(fun() -> process_info(Self, [heap_size, 42]) end),
    assert_badarg(fun() -> process_info(Self, [heap_size, invalid_key]) end),
    assert_badarg(fun() -> process_info(Self, [heap_size | not_a_list]) end),

    % An invalid argument is a badarg even when the process is dead
    with_dead_pid(fun(DeadPid) ->
        assert_badarg(fun() -> process_info(DeadPid, bad_item) end),
        assert_badarg(fun() -> process_info(DeadPid, 42) end),
        assert_badarg(fun() -> process_info(DeadPid, [bad_item]) end),
        assert_badarg(fun() -> process_info(DeadPid, [heap_size, bad_item]) end),
        assert_badarg(fun() -> process_info(DeadPid, [heap_size | not_a_list]) end)
    end),

    ok.

test_external_pid() ->
    ExternalPid = binary_to_term(<<131, 88, 119, 10, "other@node", 1:32, 0:32, 42:32>>),
    true = is_pid(ExternalPid),
    assert_badarg(fun() -> process_info(ExternalPid, heap_size) end),
    assert_badarg(fun() -> process_info(ExternalPid, [heap_size]) end),
    ok.

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

assert_badarg(Fun) ->
    try
        Fun(),
        erlang:error(no_throw)
    catch
        error:badarg ->
            ok;
        OtherClass:OtherError ->
            erlang:error({OtherClass, OtherError})
    end.
