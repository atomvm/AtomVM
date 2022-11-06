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

%% Adapted from Erlang/OTP gen_statem documentation
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start/0, start/1]).
-export([button/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([locked/3, open/3]).

-record(state, {
    code, length, buttons
}).

start() ->
    code_lock:start([1, 2, 3, 4]),
    code_lock:button(1),
    code_lock:button(2),
    code_lock:button(3),
    code_lock:button(4),
    timer:sleep(30000),
    ok.

start(Code) ->
    gen_statem:start({local, ?NAME}, ?MODULE, Code, []).

button(Button) ->
    gen_statem:cast(?NAME, {button, Button}).

init(Code) ->
    do_lock(),
    Data = #state{code = Code, length = length(Code), buttons = []},
    {ok, locked, Data}.

callback_mode() ->
    state_functions.

locked(
    cast,
    {button, Button},
    #state{code = Code, length = Length, buttons = Buttons} = Data
) ->
    NewButtons =
        if
            length(Buttons) < Length ->
                Buttons;
            true ->
                tl(Buttons)
        end ++ [Button],
    if
        % Correct
        NewButtons =:= Code ->
            do_unlock(),
            {next_state, open, Data#state{buttons = []},
                % Time in milliseconds
                [{state_timeout, 10000, lock}]};
        % Incomplete | Incorrect
        true ->
            {next_state, locked, Data#state{buttons = NewButtons}}
    end.

open(state_timeout, lock, Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button, _}, Data) ->
    {next_state, open, Data}.

do_lock() ->
    erlang:display(lock).
do_unlock() ->
    erlang:display(unlock).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
