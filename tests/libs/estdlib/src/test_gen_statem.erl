%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(test_gen_statem).

-export([test/0]).
-export([init/1, initial/3, jail/3, free/3, terminate/3, callback_mode/0]).

-record(data, {
    num_casts = 0,
    num_infos = 0,
    num_spurious_timeouts = 0
}).

test() ->
    ok = test_call(),
    ok = test_cast(),
    ok = test_info(),
    ok = test_timeout(),
    ok = test_start_link(),
    ok.

test_call() ->
    {ok, Pid} = gen_statem:start(?MODULE, [], []),
    pong = gen_statem:call(Pid, ping),
    gen_statem:stop(Pid),
    ok.

test_cast() ->
    {ok, Pid} = gen_statem:start(?MODULE, [], []),

    ok = gen_statem:cast(Pid, ping),
    ok = gen_statem:cast(Pid, ping),
    ok = gen_statem:cast(Pid, ping),
    ok = gen_statem:cast(Pid, ping),
    ok = gen_statem:cast(Pid, ping),

    5 = gen_statem:call(Pid, get_num_casts),
    0 = gen_statem:call(Pid, get_num_casts),

    gen_statem:stop(Pid),
    ok.

test_info() ->
    {ok, Pid} = gen_statem:start(?MODULE, [], []),

    Pid ! ping,
    Pid ! ping,
    Pid ! ping,

    3 = gen_statem:call(Pid, get_num_infos),
    0 = gen_statem:call(Pid, get_num_infos),

    gen_statem:stop(Pid),
    ok.

test_timeout() ->
    {ok, Pid} = gen_statem:start(?MODULE, [], []),
    ok = gen_statem:call(Pid, {go_to_jail, 250}),
    no = gen_statem:call(Pid, are_you_free),
    timer:sleep(500),
    yes = gen_statem:call(Pid, are_you_free),

    ok = gen_statem:call(Pid, {go_to_jail, 250}),
    no = gen_statem:call(Pid, are_you_free),
    ok = gen_statem:cast(Pid, escape),
    yes = gen_statem:call(Pid, are_you_free),
    timer:sleep(500),
    0 = gen_statem:call(Pid, get_num_spurious_timeouts),

    gen_statem:stop(Pid),
    ok.

test_start_link() ->
    {ok, Pid} = gen_statem:start_link(?MODULE, [], []),

    pong = gen_statem:call(Pid, ping),

    false = erlang:process_flag(trap_exit, true),
    ok = gen_statem:cast(Pid, crash),
    ok =
        receive
            {'EXIT', Pid, _Reason} -> ok
        after 30000 -> timeout
        end,
    true = erlang:process_flag(trap_exit, false),
    ok.

%%
%% callbacks
%%

callback_mode() -> state_functions.

init(_) ->
    {ok, initial, #data{}}.

initial({call, From}, ping, Data) ->
    {next_state, initial, Data, [{reply, From, pong}]};
initial(cast, ping, #data{num_casts = NumCasts} = Data) ->
    {next_state, initial, Data#data{num_casts = NumCasts + 1}};
initial(info, ping, #data{num_infos = NumInfos} = Data) ->
    {next_state, initial, Data#data{num_infos = NumInfos + 1}};
initial({call, From}, get_num_casts, #data{num_casts = NumCasts} = Data) ->
    {next_state, initial, Data#data{num_casts = 0}, [{reply, From, NumCasts}]};
initial({call, From}, get_num_infos, #data{num_infos = NumInfos} = Data) ->
    {next_state, initial, Data#data{num_infos = 0}, [{reply, From, NumInfos}]};
initial({call, From}, {go_to_jail, Ms}, Data) ->
    {next_state, jail, Data, [{reply, From, ok}, {state_timeout, Ms, you_are_free}]};
initial(cast, crash, _Data) ->
    throw(test_crash).

jail({call, From}, are_you_free, Data) ->
    {next_state, jail, Data, [{reply, From, no}]};
jail(cast, escape, Data) ->
    {next_state, free, Data};
jail(state_timeout, you_are_free, Data) ->
    {next_state, free, Data}.

free({call, From}, {go_to_jail, Ms}, Data) ->
    {next_state, jail, Data, [{reply, From, ok}, {state_timeout, Ms, you_are_free}]};
free({call, From}, are_you_free, Data) ->
    {next_state, free, Data, [{reply, From, yes}]};
%% we should never receive a timeout message, because the timer set in jail should be cancelled as the result of the escape
free(state_timeout, _Msg, #data{num_spurious_timeouts = NumSpuriousTimeouts} = Data) ->
    {next_state, free, Data#data{num_spurious_timeouts = NumSpuriousTimeouts + 1}};
free(
    {call, From},
    get_num_spurious_timeouts,
    #data{num_spurious_timeouts = NumSpuriousTimeouts} = Data
) ->
    {next_state, free, Data, [{reply, From, NumSpuriousTimeouts}]}.

terminate(_Reason, _StateName, _Data) ->
    ok.
