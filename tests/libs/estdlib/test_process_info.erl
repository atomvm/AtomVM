%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-export([test/0]).

%% process_info/1 is not a nif but implemented in the erlang module on top
%% of process_info/2 with a list argument.

test() ->
    ok = test_process_info_1(),
    ok = test_registered_name_first(),
    ok = test_badargs(),
    ok.

test_process_info_1() ->
    Check = fun(Pid) ->
        Info = process_info(Pid),
        true = is_list(Info),

        {heap_size, HS} = lists:keyfind(heap_size, 1, Info),
        true = is_integer(HS) andalso HS > 0,

        {total_heap_size, THS} = lists:keyfind(total_heap_size, 1, Info),
        true = THS >= HS,

        {stack_size, SS} = lists:keyfind(stack_size, 1, Info),
        true = is_integer(SS) andalso SS >= 0,

        {message_queue_len, MQL} = lists:keyfind(message_queue_len, 1, Info),
        true = is_integer(MQL) andalso MQL >= 0,

        {links, Links} = lists:keyfind(links, 1, Info),
        true = is_list(Links),

        {trap_exit, TE} = lists:keyfind(trap_exit, 1, Info),
        true = is_boolean(TE),

        false = lists:keyfind(registered_name, 1, Info)
    end,

    Check(self()),
    with_other_pid(Check),

    with_dead_pid(fun(DeadPid) ->
        undefined = process_info(DeadPid)
    end),

    ok.

test_registered_name_first() ->
    erlang:register(process_info_1_test, self()),
    Info = process_info(self()),
    {registered_name, process_info_1_test} = lists:keyfind(registered_name, 1, Info),
    assert_registered_name_first(process_info_1_test, Info),
    erlang:unregister(process_info_1_test),
    false = lists:keyfind(registered_name, 1, process_info(self())),

    with_other_pid(fun(Pid) ->
        erlang:register(process_info_1_other, Pid),
        OtherInfo = process_info(Pid),
        {registered_name, process_info_1_other} = lists:keyfind(registered_name, 1, OtherInfo),
        assert_registered_name_first(process_info_1_other, OtherInfo),
        erlang:unregister(process_info_1_other)
    end),

    ok.

%% OTP documents no order for the process_info/1 result, so the
%% first-position check runs on AtomVM only
assert_registered_name_first(Name, Info) ->
    case erlang:system_info(machine) of
        "BEAM" -> ok;
        _ -> [{registered_name, Name} | _] = Info
    end.

test_badargs() ->
    assert_badarg(fun() -> process_info(bad_pid) end),

    ExternalPid = binary_to_term(<<131, 88, 119, 10, "other@node", 1:32, 0:32, 42:32>>),
    true = is_pid(ExternalPid),
    assert_badarg(fun() -> process_info(ExternalPid) end),

    ok.

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

with_dead_pid(Fun) ->
    {DeadPid, Ref} = spawn_opt(fun() -> ok end, [monitor]),
    normal =
        receive
            {'DOWN', Ref, process, DeadPid, Reason} -> Reason
        end,
    Fun(DeadPid).

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
