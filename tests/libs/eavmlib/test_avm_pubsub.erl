%
% This file is part of AtomVM.
%
% Copyright 2026 Dennis Palmer <dodava@gmail.com>
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

-module(test_avm_pubsub).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_sub_pub(),
    ok = test_sub_other_pid(),
    ok = test_sub_twice(),
    ok = test_unsub(),
    ok = test_unsub_other_pid(),
    ok = test_wildcard_plus(),
    ok = test_wildcard_hash(),
    ok = test_unsub_unknown_topic(),
    ok = test_dead_subscriber_removed(),
    ok = test_killed_subscriber_removed(),
    ok = test_dead_subscriber_removed_from_all_topics(),
    ok = test_resubscribe_after_unsub(),
    ok = test_sub_remote_pid(),
    ok.

test_sub_pub() ->
    {ok, PubSub} = avm_pubsub:start(),
    ok = avm_pubsub:sub(PubSub, [a, b]),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a, b], hello), {ok, 1}),
    Self = self(),
    ok = expect_pub([a, b], Self, hello),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a, c], hello), {ok, 0}),
    ok = expect_no_message(),
    stop(PubSub).

test_sub_other_pid() ->
    {ok, PubSub} = avm_pubsub:start(),
    Subscriber = spawn_forwarder(self()),
    ok = avm_pubsub:sub(PubSub, [a], Subscriber),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 1}),
    Self = self(),
    ok = expect_forwarded(Subscriber, {pub, [a], Self, hello}),
    stop_process(Subscriber),
    stop(PubSub).

test_sub_twice() ->
    {ok, PubSub} = avm_pubsub:start(),
    ok = avm_pubsub:sub(PubSub, [a]),
    ok = avm_pubsub:sub(PubSub, [a]),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 1}),
    Self = self(),
    ok = expect_pub([a], Self, hello),
    ok = expect_no_message(),
    stop(PubSub).

test_unsub() ->
    {ok, PubSub} = avm_pubsub:start(),
    ok = avm_pubsub:sub(PubSub, [a]),
    ok = avm_pubsub:sub(PubSub, [b]),
    ok = avm_pubsub:unsub(PubSub, [a]),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 0}),
    ok = expect_no_message(),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [b], hello), {ok, 1}),
    Self = self(),
    ok = expect_pub([b], Self, hello),
    stop(PubSub).

test_unsub_other_pid() ->
    {ok, PubSub} = avm_pubsub:start(),
    Subscriber = spawn_forwarder(self()),
    ok = avm_pubsub:sub(PubSub, [a], Subscriber),
    ok = avm_pubsub:unsub(PubSub, [a], Subscriber),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 0}),
    stop_process(Subscriber),
    stop(PubSub).

test_sub_remote_pid() ->
    {ok, PubSub} = avm_pubsub:start(),
    RemotePid = binary_to_term(<<131, 88, 119, 10, "other@node", 1:32, 0:32, 42:32>>),
    ok = avm_pubsub:sub(PubSub, [a], RemotePid),
    ok = avm_pubsub:unsub(PubSub, [a], RemotePid),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 0}),
    stop(PubSub).

test_wildcard_plus() ->
    {ok, PubSub} = avm_pubsub:start(),
    ok = avm_pubsub:sub(PubSub, [a, '+', c]),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a, b, c], hello), {ok, 1}),
    Self = self(),
    ok = expect_pub([a, b, c], Self, hello),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a, c], hello), {ok, 0}),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a, b, d], hello), {ok, 0}),
    ok = expect_no_message(),
    stop(PubSub).

test_wildcard_hash() ->
    {ok, PubSub} = avm_pubsub:start(),
    ok = avm_pubsub:sub(PubSub, [a, '#']),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a, b], hello), {ok, 1}),
    Self = self(),
    ok = expect_pub([a, b], Self, hello),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a, b, c], hello), {ok, 1}),
    ok = expect_pub([a, b, c], Self, hello),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 0}),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [b, c], hello), {ok, 0}),
    ok = expect_no_message(),
    stop(PubSub).

%% unsub on a topic that was never subscribed must not create a subscription
test_unsub_unknown_topic() ->
    {ok, PubSub} = avm_pubsub:start(),
    ok = avm_pubsub:unsub(PubSub, [a]),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 0}),
    ok = expect_no_message(),
    stop(PubSub).

%% https://github.com/atomvm/AtomVM/issues/711
test_dead_subscriber_removed() ->
    {ok, PubSub} = avm_pubsub:start(),
    Parent = self(),
    {Subscriber, Ref} = spawn_monitor(fun() ->
        ok = avm_pubsub:sub(PubSub, [a]),
        Parent ! {subscribed, self()}
    end),
    ok =
        receive
            {subscribed, Subscriber} -> ok
        after 5000 -> timeout
        end,
    ok = wait_down(Ref),
    ok = wait_subscriber_count(PubSub, [a], 0),
    stop(PubSub).

test_killed_subscriber_removed() ->
    {ok, PubSub} = avm_pubsub:start(),
    Subscriber = spawn_forwarder(self()),
    ok = avm_pubsub:sub(PubSub, [a], Subscriber),
    Ref = monitor(process, Subscriber),
    exit(Subscriber, kill),
    ok = wait_down(Ref),
    ok = wait_subscriber_count(PubSub, [a], 0),
    stop(PubSub).

test_dead_subscriber_removed_from_all_topics() ->
    {ok, PubSub} = avm_pubsub:start(),
    Subscriber = spawn_forwarder(self()),
    ok = avm_pubsub:sub(PubSub, [a], Subscriber),
    ok = avm_pubsub:sub(PubSub, [b, '#'], Subscriber),
    ok = avm_pubsub:sub(PubSub, [b]),
    Ref = monitor(process, Subscriber),
    exit(Subscriber, kill),
    ok = wait_down(Ref),
    ok = wait_subscriber_count(PubSub, [a], 0),
    ok = wait_subscriber_count(PubSub, [b, c], 0),
    % live subscribers are kept
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [b], hello), {ok, 1}),
    Self = self(),
    ok = expect_pub([b], Self, hello),
    stop(PubSub).

%% unsubscribing must not prevent a later subscription from being cleaned up
test_resubscribe_after_unsub() ->
    {ok, PubSub} = avm_pubsub:start(),
    Subscriber = spawn_forwarder(self()),
    ok = avm_pubsub:sub(PubSub, [a], Subscriber),
    ok = avm_pubsub:unsub(PubSub, [a], Subscriber),
    ok = avm_pubsub:sub(PubSub, [a], Subscriber),
    ?ASSERT_MATCH(avm_pubsub:pub(PubSub, [a], hello), {ok, 1}),
    Self = self(),
    ok = expect_forwarded(Subscriber, {pub, [a], Self, hello}),
    Ref = monitor(process, Subscriber),
    exit(Subscriber, kill),
    ok = wait_down(Ref),
    ok = wait_subscriber_count(PubSub, [a], 0),
    stop(PubSub).

%% helpers

spawn_forwarder(Parent) ->
    spawn(fun() -> forward_loop(Parent) end).

forward_loop(Parent) ->
    receive
        stop ->
            ok;
        Msg ->
            Parent ! {forwarded, self(), Msg},
            forward_loop(Parent)
    end.

stop_process(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    ok = wait_down(Ref).

expect_pub(Topic, From, Term) ->
    receive
        {pub, Topic, From, Term} -> ok
    after 5000 -> timeout
    end.

expect_forwarded(Pid, Msg) ->
    receive
        {forwarded, Pid, Msg} -> ok
    after 5000 -> timeout
    end.

expect_no_message() ->
    receive
        Msg -> {unexpected, Msg}
    after 100 -> ok
    end.

wait_down(Ref) ->
    receive
        {'DOWN', Ref, process, _Pid, _Reason} -> ok
    after 5000 -> timeout
    end.

%% The 'DOWN' message seen by the test and the one seen by avm_pubsub are
%% not ordered with respect to each other, so poll until the count settles.
wait_subscriber_count(PubSub, Topic, Expected) ->
    wait_subscriber_count(PubSub, Topic, Expected, 50).

wait_subscriber_count(PubSub, Topic, Expected, 0) ->
    {ok, Actual} = avm_pubsub:pub(PubSub, Topic, probe),
    {subscriber_count, Topic, Expected, Actual};
wait_subscriber_count(PubSub, Topic, Expected, Retries) ->
    case avm_pubsub:pub(PubSub, Topic, probe) of
        {ok, Expected} ->
            ok;
        {ok, _} ->
            receive
            after 20 -> ok
            end,
            wait_subscriber_count(PubSub, Topic, Expected, Retries - 1)
    end.

stop(PubSub) ->
    ok = gen_server:stop(PubSub),
    ok.
