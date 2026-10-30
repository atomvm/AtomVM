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
    {ok, {[], #{}}}.

handle_call({pub, Topic, Term}, {FromPid, _Ref}, {Table, _Monitors} = State) ->
    Sub = broadcast_subscribers(Table, Topic, FromPid, Term),
    {reply, {ok, Sub}, State};
handle_call({sub, Topic}, {FromPid, _Ref}, State) ->
    NewState = add_subscriber(State, FromPid, Topic),
    {reply, ok, NewState};
handle_call({sub, Topic, Subscriber}, _From, State) ->
    NewState = add_subscriber(State, Subscriber, Topic),
    {reply, ok, NewState};
handle_call({unsub, Topic}, {FromPid, _Ref}, State) ->
    NewState = remove_subscriber(State, FromPid, Topic),
    {reply, ok, NewState};
handle_call({unsub, Topic, Subscriber}, _From, State) ->
    NewState = remove_subscriber(State, Subscriber, Topic),
    {reply, ok, NewState}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, {Table, Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, Ref} ->
            {noreply, {remove_from_all(Table, Pid), maps:remove(Pid, Monitors)}};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

broadcast_subscribers(Table, Topic, From, Term) ->
    Subs = find_subscribers(Table, Topic),
    Message = {pub, Topic, From, Term},
    lists:foreach(fun(Subscriber) -> Subscriber ! Message end, Subs),
    length(Subs).

add_subscriber({Table, Monitors}, Subscriber, Pattern) ->
    NewTable =
        case lists:keyfind(Pattern, 2, Table) of
            {Subscribers, Pattern} ->
                case lists:member(Subscriber, Subscribers) of
                    false ->
                        lists:keyreplace(Pattern, 2, Table, {[Subscriber | Subscribers], Pattern});
                    true ->
                        Table
                end;
            false ->
                [{[Subscriber], Pattern} | Table]
        end,
    {NewTable, ensure_monitor(Monitors, Subscriber)}.

remove_subscriber({Table, Monitors} = State, Subscriber, Pattern) ->
    case lists:keyfind(Pattern, 2, Table) of
        {Subscribers, Pattern} ->
            NewTable =
                case lists:delete(Subscriber, Subscribers) of
                    [] -> lists:keydelete(Pattern, 2, Table);
                    NewSubs -> lists:keyreplace(Pattern, 2, Table, {NewSubs, Pattern})
                end,
            {NewTable, maybe_demonitor(NewTable, Monitors, Subscriber)};
        false ->
            State
    end.

remove_from_all(Table, Subscriber) ->
    lists:foldr(
        fun({Subscribers, Pattern}, Acc) ->
            case lists:delete(Subscriber, Subscribers) of
                [] -> Acc;
                NewSubs -> [{NewSubs, Pattern} | Acc]
            end
        end,
        [],
        Table
    ).

%% TODO: monitor remote subscribers too once erlang:monitor/2 supports pids from other
%% nodes; until then they stay in the table without being cleaned up, as before this fix.
ensure_monitor(Monitors, Subscriber) when is_pid(Subscriber), node(Subscriber) =:= node() ->
    case maps:is_key(Subscriber, Monitors) of
        true -> Monitors;
        false -> Monitors#{Subscriber => monitor(process, Subscriber)}
    end;
ensure_monitor(Monitors, _Subscriber) ->
    Monitors.

maybe_demonitor(Table, Monitors, Subscriber) ->
    case is_subscribed(Table, Subscriber) of
        true ->
            Monitors;
        false ->
            case maps:find(Subscriber, Monitors) of
                {ok, Ref} ->
                    demonitor(Ref, [flush]),
                    maps:remove(Subscriber, Monitors);
                error ->
                    Monitors
            end
    end.

is_subscribed(Table, Subscriber) ->
    lists:any(fun({Subscribers, _Pattern}) -> lists:member(Subscriber, Subscribers) end, Table).

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
