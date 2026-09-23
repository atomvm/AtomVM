%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

-module(test_monitor).

-export([start/0]).

start() ->
    ok = test_monitor_normal(),
    ok = test_monitor_demonitor(),
    ok = test_monitor_noproc(),
    ok = test_monitor_demonitor_flush(),
    ok = test_monitor_demonitor_info(),
    ok = test_monitor_demonitor_flush_info_true(),
    ok = test_monitor_demonitor_flush_info_false(),
    ok = test_monitor_self(),
    ok = test_monitor_demonitor_from_other(),
    ok = test_monitor_registered(),
    ok = test_monitor_registered_noproc(),

    ok = test_alias(),
    ok = test_multiple_aliases(),
    ok = test_multiple_unaliases(),
    ok = test_unalias_from_wrong_process(),
    ok = test_monitor_alias_dead_process(),
    ok = test_monitor_multiple_aliases_monitors(fun spawn_opt_monitor/2),
    ok = test_monitor_multiple_aliases_monitors(fun spawn_and_monitor/2),
    ok = test_monitor_alias_demonitor(fun spawn_opt_monitor/2),
    ok = test_monitor_alias_demonitor(fun spawn_and_monitor/2),
    ok = test_monitor_alias_explicit_unalias(fun spawn_opt_monitor/2),
    ok = test_monitor_alias_explicit_unalias(fun spawn_and_monitor/2),
    ok = test_monitor_alias_reply_demonitor(fun spawn_opt_monitor/2),
    ok = test_monitor_alias_reply_demonitor(fun spawn_and_monitor/2),
    ok = test_reply_demonitor_removes_monitor(fun spawn_opt_monitor/2),
    ok = test_reply_demonitor_removes_monitor(fun spawn_and_monitor/2),
    ok = test_monitor_down_alias(fun spawn_opt_monitor/2),
    ok = test_monitor_down_alias(fun spawn_and_monitor/2),
    ok = test_monitor_alias_demonitor_deactivates_on_down(fun spawn_opt_monitor/2),
    ok = test_monitor_alias_demonitor_deactivates_on_down(fun spawn_and_monitor/2),
    ok = test_alias_pid_send_order(),
    ok = test_reply_demonitor_same_batch_order(),
    ok = test_monitor_alias_noproc_returns_alias(),
    ok = test_monitor_alias_self_installs_nothing(),
    ok = test_spawn_opt_link_monitor_badarg_is_atomic(),
    ok = test_spawn_opt_monitor_non_list_badarg(),
    ok = test_monitor_alias_down_before_send_same_batch(),
    ok = test_unalias_and_send_non_local_refs(),
    ok = test_io_request_alias_reply(),
    ok = test_alias_as_key(),
    ok = test_monitor_alias_demonitor_flush(),
    ok = test_monitor_alias_duplicate_option(),
    ok = test_monitor_alias_registered_self_installs_nothing(),
    ok = test_alias_1(),
    ok = test_alias_reply_mode(),
    ok = test_alias_send_after_owner_died(),
    ok = test_alias_multi_sender_unalias(),
    ok = test_alias_duplicate_options(),
    ok = test_unalias_non_reference_badarg(),
    ok = test_alias_count_saturation(),
    ok = test_binary_to_term_invalid_process_ref(),
    ok = test_alias_ref_ordering(),
    0.

%% An alias sorts after every plain reference whichever was created first. Two
%% owners' aliases are distinct and strictly ordered, but that direction follows
%% the internal pid (not pid term order) and is implementation defined, so it is
%% not pinned here.
test_alias_ref_ordering() ->
    R0 = make_ref(),
    A0 = erlang:alias(),
    true = R0 < A0,
    A1 = erlang:alias(),
    R1 = make_ref(),
    true = R1 < A1,
    Ea = erlang:alias(),
    Eb = erlang:alias(),
    true = Ea < Eb,
    true = Eb =:= binary_to_term(term_to_binary(Eb)),
    Parent = self(),
    Child = spawn_opt(
        fun() ->
            receive
                {get, P} -> P ! {child_alias, erlang:alias()}
            end
        end,
        []
    ),
    Child ! {get, Parent},
    ChildAlias =
        receive
            {child_alias, Ca} -> Ca
        after 5000 -> error(child_alias_timeout)
        end,
    SelfAlias = erlang:alias(),
    true = ChildAlias =/= SelfAlias,
    true = (ChildAlias < SelfAlias) xor (SelfAlias < ChildAlias),
    _ = [erlang:unalias(R) || R <- [A0, A1, Ea, Eb, SelfAlias]],
    ok.

test_monitor_normal() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok;
            Other2 -> {unexpected, Other2}
        after 5000 -> timeout
        end,
    ok.

test_monitor_demonitor() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    true = demonitor(Ref),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_noproc() ->
    {Pid, Monitor} = spawn_opt(fun() -> ok end, [monitor]),
    ok =
        receive
            {'DOWN', Monitor, process, Pid, normal} -> ok
        after 500 -> timeout
        end,
    Ref = monitor(process, Pid),
    ok =
        receive
            {'DOWN', Ref, process, Pid, noproc} -> ok;
            Other -> {unexpected, Other}
        after 5000 -> timeout
        end,
    ok.

test_monitor_registered() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    true = register(name, Pid),
    Ref = monitor(process, name),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref, process, {name, nonode@nohost}, normal} -> ok;
            Other2 -> {unexpected, Other2}
        after 5000 -> timeout
        end,
    ok.

test_monitor_registered_noproc() ->
    Ref = monitor(process, foobar),
    ok =
        receive
            {'DOWN', Ref, process, {foobar, nonode@nohost}, noproc} -> ok;
            Other -> {unexpected, Other}
        after 5000 -> timeout
        end,
    ok.

test_monitor_demonitor_flush() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    Pid ! {self(), quit},
    receive
    after 100 -> ok
    end,
    true = demonitor(Ref, [flush]),
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_demonitor_info() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    true = demonitor(Ref, [info]),
    Pid ! {self(), quit},
    false = demonitor(Ref, [info]),
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_demonitor_flush_info_true() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    Pid ! {self(), quit},
    receive
    after 100 -> ok
    end,
    false = demonitor(Ref, [flush, info]),
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_demonitor_flush_info_false() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    true = demonitor(Ref, [flush, info]),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_self() ->
    Ref = monitor(process, self()),
    false = demonitor(Ref, [info]),
    ok.

test_monitor_demonitor_from_other() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    {OtherPid, OtherRef} = spawn_opt(
        fun() ->
            false = demonitor(Ref, [info])
        end,
        [monitor]
    ),
    normal =
        receive
            {'DOWN', OtherRef, process, OtherPid, Reason} -> Reason
        after 500 -> timeout
        end,
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok
        after 500 -> timeout
        end,
    ok.

test_alias() ->
    P = spawn_opt(fun echo_loop/0, []),
    Alias = erlang:alias(),
    do_test_alias(P, Alias),
    P ! quit,
    ok.

test_multiple_aliases() ->
    P = spawn_opt(fun echo_loop/0, []),
    A1 = erlang:alias(),
    A2 = erlang:alias(),
    A3 = erlang:alias(),
    do_test_alias(P, A1),
    do_test_alias(P, A3),
    do_test_alias(P, A2),
    P ! quit,
    ok.

test_multiple_unaliases() ->
    A = erlang:alias(),
    true = erlang:unalias(A),
    false = erlang:unalias(A),
    false = erlang:unalias(A),
    ok.

test_unalias_from_wrong_process() ->
    A = erlang:alias(),
    TestProcess = self(),
    spawn_opt(fun() -> TestProcess ! erlang:unalias(A) end, [link]),
    false = recv_one(),
    P = spawn_opt(fun echo_loop/0, []),
    do_test_alias(P, A),
    P ! quit,
    ok.

do_test_alias(P, Alias) ->
    do_test_alias(P, Alias, fun erlang:unalias/1).

do_test_alias(P, Alias, UnaliasFun) ->
    Ref = make_ref(),
    P ! {{m1, Ref}, Alias},
    {m1, Ref} = recv_one(),
    UnaliasFun(Alias),
    P ! {{m2, Ref}, Alias},
    P ! {{m3, Ref}, self()},
    {m3, Ref} = recv_one(),
    ok.

test_monitor_alias_demonitor(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, demonitor}]),
    do_test_alias(P, Mon, fun demonitor/1),
    P ! quit,
    ok.

test_monitor_alias_explicit_unalias(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, explicit_unalias}]),
    P ! {m1, Mon},
    m1 = recv_one(),
    demonitor(Mon),
    do_test_alias(P, Mon),
    P ! quit,
    ok.

test_monitor_alias_reply_demonitor(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, reply_demonitor}]),
    do_test_alias(P, Mon, fun(_Mon) -> ok end),
    P ! quit,
    ok.

test_reply_demonitor_removes_monitor(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, reply_demonitor}]),
    Ref = make_ref(),
    P ! {{reply, Ref}, Mon},
    {reply, Ref} = recv_one(),
    %% Monitors fire in installation order, so a stale 'DOWN' from a not-removed monitor
    %% would arrive before this fence's. On the BEAM no such 'DOWN' exists.
    Fence = monitor(process, P),
    P ! quit,
    {'DOWN', Fence, process, P, normal} = recv_one(),
    ok = assert_no_message(),
    ok.

%% Self-sending to our own alias and only then receiving guarantees both alias signals
%% drain in a single outer-list batch, so this is deterministic on SMP and non-SMP.
test_reply_demonitor_same_batch_order() ->
    P = spawn_opt(fun echo_loop/0, []),
    Mon = erlang:monitor(process, P, [{alias, reply_demonitor}]),
    Mon ! first,
    Mon ! second,
    first = recv_one(),
    ok = assert_no_message(),
    P ! quit,
    ok.

test_monitor_down_alias(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, demonitor}]),
    erlang:unalias(Mon),
    P ! {m1, Mon},
    P ! {m2, self()},
    m2 = recv_one(),
    P ! quit,
    {'DOWN', Mon, process, P, normal} = recv_one(),
    ok.

test_monitor_alias_demonitor_deactivates_on_down(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, demonitor}]),
    P ! quit,
    {'DOWN', Mon, process, P, normal} = recv_one(),
    Echo = spawn_opt(fun echo_loop/0, []),
    Echo ! {should_drop, Mon},
    %% Sends from one process keep their order, so once the fence reply arrives a dropped
    %% alias message can no longer show up afterwards.
    Echo ! {fence, self()},
    fence = recv_one(),
    ok = assert_no_message(),
    Echo ! quit,
    ok.

test_alias_pid_send_order() ->
    Parent = self(),
    P = spawn_opt(
        fun() ->
            Alias = erlang:alias(),
            Parent ! {ready, self(), Alias},
            receive
                A -> Parent ! {got, A}
            end,
            receive
                B -> Parent ! {got, B}
            end
        end,
        []
    ),
    {ready, P, Alias} = recv_one(),
    Alias ! m1,
    P ! m2,
    {got, m1} = recv_one(),
    {got, m2} = recv_one(),
    ok.

test_monitor_alias_noproc_returns_alias() ->
    {P, _} = spawn_opt(fun() -> ok end, [monitor]),
    ok =
        receive
            {'DOWN', _, _, P, _} -> ok
        after 5000 -> timeout
        end,
    Mon = erlang:monitor(process, P, [{alias, explicit_unalias}]),
    {'DOWN', Mon, process, P, noproc} = recv_one(),
    Echo = spawn_opt(fun echo_loop/0, []),
    Echo ! {via_alias, Mon},
    via_alias = recv_one(),
    true = erlang:unalias(Mon),
    Echo ! quit,
    ok.

test_monitor_alias_self_installs_nothing() ->
    Mon = erlang:monitor(process, self(), [{alias, explicit_unalias}]),
    Mon ! hello,
    ok = assert_no_message(),
    false = erlang:unalias(Mon),
    false = erlang:demonitor(Mon, [info]),
    ok.

%% The link is installed before the monitor options are parsed, so the badarg must
%% still unwind it: a surviving link would later deliver a spurious {'EXIT', Pid, normal}.
test_spawn_opt_link_monitor_badarg_is_atomic() ->
    %% On the BEAM the test process is linked to init, so compare against the initial links
    %% instead of [].
    {links, LinksBefore} = erlang:process_info(self(), links),
    false = erlang:process_flag(trap_exit, true),
    ok =
        try spawn_opt(fun() -> ok end, [link, {monitor, [bad_option]}]) of
            Result -> {unexpected, Result}
        catch
            error:badarg -> ok
        end,
    {links, LinksBefore} = erlang:process_info(self(), links),
    ok =
        receive
            Other -> {unexpected_message, Other}
        after 200 -> ok
        end,
    true = erlang:process_flag(trap_exit, false),
    ok.

test_monitor_multiple_aliases_monitors(SpawnFun) ->
    {P, Mon1} = SpawnFun(fun echo_loop/0, [{alias, demonitor}]),
    Mon2 = erlang:monitor(process, P, [{alias, reply_demonitor}]),
    Mon3 = erlang:monitor(process, P, [{alias, explicit_unalias}]),
    Mon4 = erlang:monitor(process, P),
    A1 = erlang:alias(),
    A2 = erlang:alias(),
    do_test_alias(P, A2),
    do_test_alias(P, Mon3),
    do_test_alias(P, A1),
    do_test_alias(P, Mon1, fun demonitor/1),
    P ! quit,
    {'DOWN', Mon2, process, P, normal} = recv_one(),
    {'DOWN', Mon3, process, P, normal} = recv_one(),
    {'DOWN', Mon4, process, P, normal} = recv_one(),
    ok.

test_monitor_alias_dead_process() ->
    {P, Mon0} = spawn_opt(fun() -> ok end, [monitor]),
    {'DOWN', Mon0, process, P, normal} = recv_one(),
    Mon1 = erlang:monitor(process, P, [{alias, demonitor}]),
    {'DOWN', Mon1, process, P, noproc} = recv_one(),
    Mon2 = erlang:monitor(process, P, [{alias, reply_demonitor}]),
    {'DOWN', Mon2, process, P, noproc} = recv_one(),
    Mon3 = erlang:monitor(process, P, [{alias, explicit_unalias}]),
    {'DOWN', Mon3, process, P, noproc} = recv_one(),
    ok.

%% A non-list, non-'true' monitor value fails before the monitor-option parser, unlike
%% {monitor, [BadOption]}, so it is exercised separately here.
test_spawn_opt_monitor_non_list_badarg() ->
    ok =
        try spawn_opt(fun() -> ok end, [{monitor, foo}]) of
            R1 -> {unexpected, R1}
        catch
            error:badarg -> ok
        end,
    ok =
        try spawn_opt(fun() -> ok end, [{monitor, 123}]) of
            R2 -> {unexpected, R2}
        catch
            error:badarg -> ok
        end,
    ok.

%% A 'DOWN' that deactivates a {alias, demonitor} alias must drop an alias send that lands
%% in the SAME mailbox drain. The relay sends the alias message only after seeing the owner's
%% 'DOWN', so both reach the owner in one batch. The owner busy-waits on whereis/1 rather than
%% receiving, because a receive would drain its mailbox before the batch is assembled.
%% On a single scheduler the owner may drain the 'DOWN' alone first. The test then passes
%% through the cross-batch deactivation path instead. The same-batch path is reliably
%% exercised only on SMP builds.
test_monitor_alias_down_before_send_same_batch() ->
    P = spawn_opt(
        fun() ->
            receive
                quit -> ok
            end
        end,
        []
    ),
    %% Monitor P before the relay does, so the owner's 'DOWN' is posted before the relay's.
    Mon = erlang:monitor(process, P, [{alias, demonitor}]),
    Relay = spawn_opt(
        fun() ->
            erlang:monitor(process, P),
            receive
                {'DOWN', _, process, P, _} ->
                    Mon ! should_drop,
                    register(down_batch_relay, self()),
                    receive
                        release -> ok
                    end
            end
        end,
        []
    ),
    P ! quit,
    %% The huge spin bound absorbs valgrind's unfair scheduling, which can starve the relay
    %% while this process spins. It must busy-wait: receiving would drain its mailbox.
    ok = wait_registered(down_batch_relay, 50000000),
    {'DOWN', Mon, process, P, normal} = recv_one(),
    ok = assert_no_message(),
    Relay ! release,
    ok.

test_unalias_and_send_non_local_refs() ->
    %% NEWER_REFERENCE_EXT (90): Len:16, Node atom, Creation:32, Len x 4-byte words.
    ExtRef = binary_to_term(
        <<131, 90, 2:16/integer-unsigned-big, 119, 3, "x@x", 1:32/integer-unsigned-big,
            1:32/integer-unsigned-big, 2:32/integer-unsigned-big>>
    ),
    true = is_reference(ExtRef),
    false = unalias(ExtRef),
    hello = (ExtRef ! hello),
    false = unalias(make_ref()),
    hello = (make_ref() ! hello),
    ok =
        receive
            Unexpected -> {unexpected_message, Unexpected}
        after 100 -> ok
        end,
    ok.

%% An alias passed as ReplyAs must come back verbatim. A short reference rebuilt from its
%% ticks would not match it, so the receive below would not fire.
test_io_request_alias_reply() ->
    %% On the BEAM the group leader is a full io server. On AtomVM the test process has no
    %% group leader, so talk to the console port driver directly.
    IoServer =
        case erlang:system_info(machine) of
            "BEAM" -> group_leader();
            _ -> open_port({spawn, "console"}, [])
        end,
    Alias = erlang:alias(),
    IoServer ! {io_request, self(), Alias, {put_chars, unicode, <<>>}},
    ok =
        receive
            {io_reply, Alias, ok} -> ok
        after 5000 -> io_reply_did_not_match_alias
        end,
    true = erlang:unalias(Alias),
    ok.

test_alias_as_key() ->
    Alias = erlang:alias(),
    Plain = make_ref(),
    Map = #{Alias => alias_value, Plain => plain_value},
    alias_value = maps:get(Alias, Map),
    plain_value = maps:get(Plain, Map),
    Tid = ets:new(alias_key_table, []),
    true = ets:insert(Tid, {Alias, alias_value}),
    true = ets:insert(Tid, {Plain, plain_value}),
    [{Alias, alias_value}] = ets:lookup(Tid, Alias),
    [{Plain, plain_value}] = ets:lookup(Tid, Plain),
    true = ets:delete(Tid, Alias),
    [] = ets:lookup(Tid, Alias),
    [{Plain, plain_value}] = ets:lookup(Tid, Plain),
    true = erlang:unalias(Alias),
    ok.

test_monitor_alias_demonitor_flush() ->
    P = spawn_opt(
        fun() ->
            receive
                quit -> ok
            end
        end,
        []
    ),
    Mon = erlang:monitor(process, P, [{alias, demonitor}]),
    Fence = monitor(process, P),
    P ! quit,
    %% Mon's 'DOWN' was enqueued first (installation order). This selective receive leaves it
    %% queued for the flush below to remove.
    ok =
        receive
            {'DOWN', Fence, process, P, normal} -> ok
        after 5000 -> timeout
        end,
    true = demonitor(Mon, [flush]),
    ok = assert_no_message(),
    Echo = spawn_opt(fun echo_loop/0, []),
    Echo ! {should_drop, Mon},
    Echo ! {fence, self()},
    fence = recv_one(),
    ok = assert_no_message(),
    Echo ! quit,
    ok.

%% With duplicate {alias, _} options the last one wins, like OTP 29.
test_monitor_alias_duplicate_option() ->
    P = spawn_opt(fun echo_loop/0, []),
    Mon = erlang:monitor(process, P, [{alias, demonitor}, {alias, explicit_unalias}]),
    true = demonitor(Mon),
    do_test_alias(P, Mon),
    P ! quit,
    ok.

test_monitor_alias_registered_self_installs_nothing() ->
    true = register(alias_self_name, self()),
    Mon = erlang:monitor(process, alias_self_name, [{alias, explicit_unalias}]),
    Mon ! hello,
    ok = assert_no_message(),
    false = erlang:unalias(Mon),
    false = erlang:demonitor(Mon, [info]),
    true = unregister(alias_self_name),
    ok.

test_alias_1() ->
    A1 = alias([]),
    A1 ! x1,
    x1 = recv_one(),
    true = unalias(A1),
    A2 = alias([explicit_unalias]),
    A2 ! x2,
    x2 = recv_one(),
    true = unalias(A2),
    ok =
        try alias([bogus]) of
            R1 -> {unexpected, R1}
        catch
            error:badarg -> ok
        end,
    ok =
        try alias(explicit_unalias) of
            R2 -> {unexpected, R2}
        catch
            error:badarg -> ok
        end,
    ok.

%% A reply alias is deactivated when its first message is delivered, so a second message in
%% the same batch is dropped too, not just delayed.
test_alias_reply_mode() ->
    A = alias([reply]),
    A ! m1,
    A ! m2,
    m1 = recv_one(),
    ok = assert_no_message(),
    A ! m3,
    %% A dropped refc binary exercises the mso sweep of the immediately-freed signal.
    A ! <<0:1600>>,
    ok = assert_no_message(),
    false = unalias(A),
    ok.

%% AtomVM assigns process ids monotonically and ref ticks are globally unique, so a dead
%% owner's stale alias stays unmatchable even by a later process. The churn loop spawns such
%% processes to confirm a stale-alias send never surfaces anywhere.
test_alias_send_after_owner_died() ->
    Parent = self(),
    {P, Fence} = spawn_opt(fun() -> Parent ! {alias, erlang:alias()} end, [monitor]),
    {alias, A} = recv_one(),
    {'DOWN', Fence, process, P, normal} = recv_one(),
    hello = (A ! hello),
    ok = churn_and_send_stale(A, 20),
    ok = assert_no_message(),
    ok.

churn_and_send_stale(_A, 0) ->
    ok;
churn_and_send_stale(A, N) ->
    Parent = self(),
    {Q, Mon} = spawn_opt(fun() -> stale_alias_probe(Parent) end, [monitor]),
    drop = (A ! drop),
    Q ! quit,
    {'DOWN', Mon, process, Q, normal} = recv_one(),
    churn_and_send_stale(A, N - 1).

%% A stale-alias signal is dropped against this process's empty alias list, so it must never
%% surface here as a plain message. Any non-quit message is reported as a misdelivery.
stale_alias_probe(Parent) ->
    receive
        quit ->
            ok;
        Other ->
            Parent ! {misdelivered, Other},
            stale_alias_probe(Parent)
    end.

%% Several senders hammer one alias while the owner unaliases mid-stream. Synchronization is
%% per-sender send order plus explicit acks, with no sleeps or timing windows, so the test
%% cannot flake on slow hosts. A sender's alias messages all precede its sent_all fence in the
%% owner's queue, so once the last phase-1 fence is consumed Count1 has counted them all.
test_alias_multi_sender_unalias() ->
    NSenders = 4,
    NMsgs = 25,
    A = erlang:alias(),
    Senders = spawn_alias_senders(self(), A, NMsgs, NSenders),
    ok = send_to_each(Senders, go),
    Count1 = drain_alias_msgs(NSenders, sent_all, 0),
    Count1 = NSenders * NMsgs,
    ok = send_to_each(Senders, go2),
    true = erlang:unalias(A),
    Count2 = drain_alias_msgs(NSenders, sent_all2, 0),
    true = Count2 =< NSenders * NMsgs,
    false = erlang:unalias(A),
    dead = (A ! dead),
    ok = assert_no_message(),
    ok.

spawn_alias_senders(_Owner, _A, _NMsgs, 0) ->
    [];
spawn_alias_senders(Owner, A, NMsgs, K) ->
    Pid = spawn_opt(fun() -> alias_sender(Owner, A, NMsgs) end, []),
    [Pid | spawn_alias_senders(Owner, A, NMsgs, K - 1)].

alias_sender(Owner, A, NMsgs) ->
    receive
        go -> ok
    end,
    ok = alias_blast(A, NMsgs),
    Owner ! sent_all,
    receive
        go2 -> ok
    end,
    ok = alias_blast(A, NMsgs),
    Owner ! sent_all2,
    ok.

alias_blast(_A, 0) ->
    ok;
alias_blast(A, N) ->
    {am, N} = (A ! {am, N}),
    alias_blast(A, N - 1).

send_to_each([], _Msg) ->
    ok;
send_to_each([Pid | Rest], Msg) ->
    Pid ! Msg,
    send_to_each(Rest, Msg).

drain_alias_msgs(0, _FenceMsg, Count) ->
    Count;
drain_alias_msgs(FencesLeft, FenceMsg, Count) ->
    case recv_one() of
        {am, _} -> drain_alias_msgs(FencesLeft, FenceMsg, Count + 1);
        FenceMsg -> drain_alias_msgs(FencesLeft - 1, FenceMsg, Count);
        Other -> {unexpected, Other}
    end.

%% With duplicate alias/1 options the last one wins, like OTP 29. Both orders are checked.
test_alias_duplicate_options() ->
    A1 = alias([explicit_unalias, reply]),
    A1 ! r1,
    A1 ! r2,
    %% A dropped refc binary exercises the mso sweep on the received-order drop path too.
    A1 ! <<0:1600>>,
    r1 = recv_one(),
    ok = assert_no_message(),
    false = unalias(A1),
    A2 = alias([reply, explicit_unalias]),
    A2 ! e1,
    A2 ! e2,
    e1 = recv_one(),
    e2 = recv_one(),
    true = unalias(A2),
    ok.

test_unalias_non_reference_badarg() ->
    ok =
        try unalias(42) of
            R -> {unexpected, R}
        catch
            error:badarg -> ok
        end,
    ok.

%% The active alias count saturates at 255 aliases and stays saturated until the last monitor
%% is removed. Sends must keep delivering while saturated (a wrapped count would drop every
%% alias of the process) and through a fresh alias after the count recovered.
test_alias_count_saturation() ->
    Parent = self(),
    spawn_opt(fun() -> saturation_worker(Parent) end, []),
    {survivor, Survivor} = recv_one(),
    ping1 = (Survivor ! ping1),
    {fresh, Stale, Fresh} = recv_one(),
    drop = (Stale ! drop),
    ping2 = (Fresh ! ping2),
    done = recv_one(),
    ok.

%% The worker owns nothing but its aliases, so unaliasing the last one empties its monitor
%% list. It must stay unlinked and unmonitored for that to hold.
saturation_worker(Parent) ->
    [Survivor | Rest] = make_aliases(300),
    ok = unalias_all(Rest),
    Parent ! {survivor, Survivor},
    receive
        ping1 -> ok
    end,
    true = unalias(Survivor),
    Fresh = erlang:alias(),
    Parent ! {fresh, Survivor, Fresh},
    receive
        ping2 -> ok
    end,
    ok = assert_no_message(),
    Parent ! done.

make_aliases(0) ->
    [];
make_aliases(N) ->
    [erlang:alias() | make_aliases(N - 1)].

unalias_all([]) ->
    ok;
unalias_all([A | Rest]) ->
    true = unalias(A),
    unalias_all(Rest).

%% The owner pid word of a wire-format alias is untrusted input: decoding must reject pid 0
%% (the short-ref sentinel) and pids above the 28-bit maximum. The BEAM treats reference words
%% as opaque payload, so it decodes the patched binaries as plain references instead of
%% rejecting them. Only AtomVM gives the third word pid semantics, so the test forks on machine.
test_binary_to_term_invalid_process_ref() ->
    A = erlang:alias(),
    B = term_to_binary(A),
    A = binary_to_term(B),
    PrefixSize = byte_size(B) - 4,
    <<Prefix:PrefixSize/binary, _Pid:32>> = B,
    TooBigPid = 1 bsl 28,
    BadZero = <<Prefix/binary, 0:32>>,
    BadBig = <<Prefix/binary, TooBigPid:32>>,
    case erlang:system_info(machine) of
        "BEAM" ->
            true = is_reference(binary_to_term(BadZero)),
            true = is_reference(binary_to_term(BadBig));
        _ ->
            %% On AtomVM the alias serializes as a len-3 reference whose last word is the pid.
            <<131, 90, 3:16, _/binary>> = B,
            ok =
                try binary_to_term(BadZero) of
                    R1 -> {unexpected, R1}
                catch
                    error:badarg -> ok
                end,
            ok =
                try binary_to_term(BadBig) of
                    R2 -> {unexpected, R2}
                catch
                    error:badarg -> ok
                end
    end,
    true = erlang:unalias(A),
    ok.

spawn_opt_monitor(LoopFun, Opts) ->
    spawn_opt(LoopFun, [{monitor, Opts}]).

spawn_and_monitor(LoopFun, Opts) ->
    P = spawn_opt(LoopFun, []),
    Mon = erlang:monitor(process, P, Opts),
    {P, Mon}.

normal_loop() ->
    receive
        {Caller, quit} -> Caller ! {self(), finished}
    end.

echo_loop() ->
    receive
        quit ->
            ok;
        {Msg, ReplyTo} ->
            ReplyTo ! Msg,
            echo_loop()
    end.

recv_one() ->
    receive
        Msg -> Msg
    after 5000 -> timeout
    end.

%% Only call this once the would-be message is already settled, behind a fence reply or after
%% a same-process send, so the short timeout window is not a race.
assert_no_message() ->
    receive
        Msg -> {unexpected_message, Msg}
    after 100 -> ok
    end.

wait_registered(_Name, 0) ->
    timeout;
wait_registered(Name, N) ->
    case whereis(Name) of
        undefined -> wait_registered(Name, N - 1);
        _ -> ok
    end.
