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

-module(gen_server_like_test).

-behaviour(gen_server).

-export([
    start/0,
    start_link/0,
    handle_call/3,
    handle_cast/2,
    init/1,
    loop/1,
    push/2,
    pop/1,
    initial_state/0
]).

start() ->
    Pid = fake_start(),
    push(Pid, 0),
    0 = pop(Pid),
    empty_stack = pop(Pid),
    empty_stack = pop(Pid),
    push(Pid, 1),
    push(Pid, 1),
    push(Pid, 2),
    push(Pid, 3),
    push(Pid, 5),
    5 = pop(Pid),
    Val = pop(Pid),
    2 = pop(Pid),
    1 = pop(Pid),
    1 = pop(Pid),
    empty_stack = pop(Pid),
    stop(Pid),
    Val.

push(Pid, Value) ->
    Pid ! {'$gen_cast', {push, Value}},
    ok.

pop(Pid) ->
    Reference = make_ref(),
    Pid ! {'$gen_call', {self(), Reference}, pop},
    receive
        {Reference, ReplyVal} -> ReplyVal
    end.

stop(Pid) ->
    Pid ! terminate.

initial_state() ->
    [].

start_link() ->
    gen_server:start_link(?MODULE, [], []).

fake_start() ->
    {ok, InitialState} = init(initial_state()),
    spawn_opt(?MODULE, loop, [InitialState], []).

init(Initial) ->
    {ok, Initial}.

handle_call(pop, _From, []) ->
    {reply, empty_stack, initial_state()};
handle_call(pop, _From, State) ->
    [Head | NextState] = State,
    {reply, Head, NextState}.

handle_cast({push, Value}, State) ->
    {noreply, [Value | State]}.

loop(State) ->
    receive
        {'$gen_cast', Request} ->
            {noreply, NextState} = handle_cast(Request, State),
            loop(NextState);
        {'$gen_call', {Pid, Reference}, Request} ->
            {reply, TheReply, NextState} = handle_call(Request, Pid, State),
            Pid ! {Reference, TheReply},
            loop(NextState);
        terminate ->
            ok
    end.
