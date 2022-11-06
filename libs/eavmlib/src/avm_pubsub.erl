%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(avm_pubsub).

-export([start/0, start/1, pub/3, sub/2, sub/3, unsub/2, unsub/3]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

start() ->
    gen_server:start(?MODULE, [], []).

start(LocalName) ->
    gen_server:start({local, LocalName}, ?MODULE, [], []).

pub(PubSub, Topic, Term) ->
    gen_server:call(PubSub, {pub, Topic, Term}).

sub(PubSub, Topic) ->
    gen_server:call(PubSub, {sub, Topic}).

sub(PubSub, Topic, Pid) ->
    gen_server:call(PubSub, {sub, Topic, Pid}).

unsub(PubSub, Topic) ->
    gen_server:call(PubSub, {unsub, Topic}).

unsub(PubSub, Topic, Pid) ->
    gen_server:call(PubSub, {unsub, Topic, Pid}).

init(_) ->
    {ok, []}.

handle_call({pub, Topic, Term}, {FromPid, _Ref}, Table) ->
    Sub = broadcast_subscribers(Table, Topic, FromPid, Term),
    {reply, {ok, Sub}, Table};
handle_call({sub, Topic}, {FromPid, _Ref}, Table) ->
    NewTable = add_subscriber(Table, FromPid, Topic),
    {reply, ok, NewTable};
handle_call({sub, Topic, Subscriber}, _From, Table) ->
    NewTable = add_subscriber(Table, Subscriber, Topic),
    {reply, ok, NewTable};
handle_call({unsub, Topic}, {FromPid, _Ref}, Table) ->
    NewTable = remove_subscriber(Table, FromPid, Topic),
    {reply, ok, NewTable};
handle_call({unsub, Topic, Subscriber}, _From, Table) ->
    NewTable = remove_subscriber(Table, Subscriber, Topic),
    {reply, ok, NewTable}.

handle_info(_Info, Table) ->
    {noreply, Table}.

terminate(_Reason, _State) ->
    ok.

broadcast_subscribers(Table, Topic, From, Term) ->
    Subs = find_subscribers(Table, Topic),
    Message = {pub, Topic, From, Term},
    lists:foreach(fun(Subscriber) -> Subscriber ! Message end, Subs),
    length(Subs).

add_subscriber(Table, Subscriber, Pattern) ->
    case lists:keyfind(Pattern, 2, Table) of
        {Subscribers, Pattern} ->
            case lists:member(Subscriber, Subscribers) of
                false -> lists:keyreplace(Pattern, 2, Table, {[Subscriber | Subscribers], Pattern});
                true -> Table
            end;
        false ->
            [{[Subscriber], Pattern} | Table]
    end.

remove_subscriber(Table, Subscriber, Pattern) ->
    case lists:keyfind(Pattern, 2, Table) of
        {Subscribers, Pattern} ->
            NewSubs = lists:delete(Subscriber, Subscribers),
            lists:keyreplace(Pattern, 2, Table, {NewSubs, Pattern});
        false ->
            [{[Subscriber], Pattern} | Table]
    end.

find_subscribers(Table, Topic) ->
    find_subscribers(Table, Topic, []).

find_subscribers([], _Topic, Acc) ->
    Acc;
find_subscribers([{Subscribers, SubscribePattern} | T], Topic, Acc) ->
    case match_topic(SubscribePattern, Topic) of
        true -> find_subscribers(T, Topic, Subscribers ++ Acc);
        false -> find_subscribers(T, Topic, Acc)
    end.

match_topic([H | PatternT], [H | TopicT]) ->
    match_topic(PatternT, TopicT);
match_topic(['+' | PatternT], [_H | TopicT]) ->
    match_topic(PatternT, TopicT);
match_topic(['#'], [_H | _T]) ->
    true;
match_topic([], []) ->
    true;
match_topic(_, _) ->
    false.
