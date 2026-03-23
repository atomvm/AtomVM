%
% This file is part of AtomVM.
%
% Copyright 2018-2021 Davide Bettio <davide@uninstall.it>
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

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP timer interface.
%%
%% This module implements a strict subset of the Erlang/OTP timer
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(timer).

-export([sleep/1, send_after/2, send_after/3, apply_after/4]).

%%-----------------------------------------------------------------------------
%% @param Timeout number of milliseconds to sleep or `infinity'
%% @returns `ok'
%%
%% @doc Pauses the execution of the current process for a given number of
%%      milliseconds, or forever, using `infinity' as the parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec sleep(Timeout :: timeout()) -> ok.
sleep(Timeout) ->
    receive
    after Timeout ->
        ok
    end.

%%-----------------------------------------------------------------------------
%% @param Time time in milliseconds after which to send the message.
%% @param Message the message to send to the calling process.
%% @returns `{ok, TRef}' where `TRef' is a reference to the timer.
%%
%% @doc Sends `Message' to the calling process after `Time' milliseconds.
%% @end
%%-----------------------------------------------------------------------------
-spec send_after(Time :: non_neg_integer(), Message :: term()) -> {ok, reference()}.
send_after(Time, Message) ->
    TRef = timer_manager:send_after(Time, self(), Message),
    {ok, TRef}.

%%-----------------------------------------------------------------------------
%% @param Time time in milliseconds after which to send the message.
%% @param Pid the process to which to send the message.
%% @param Message the message to send.
%% @returns `{ok, TRef}' where `TRef' is a reference to the timer.
%%
%% @doc Sends `Message' to `Pid' after `Time' milliseconds.
%% @end
%%-----------------------------------------------------------------------------
-spec send_after(Time :: non_neg_integer(), Pid :: pid() | atom(), Message :: term()) ->
    {ok, reference()}.
send_after(Time, Pid, Message) ->
    TRef = timer_manager:send_after(Time, Pid, Message),
    {ok, TRef}.

%%-----------------------------------------------------------------------------
%% @param Time time in milliseconds after which to apply the function.
%% @param Module the module of the function to apply.
%% @param Function the function to apply.
%% @param Arguments the arguments to pass to the function.
%% @returns `{ok, TRef}' where `TRef' is a reference to the timer.
%%
%% @doc Applies `Module:Function(Arguments)' after `Time' milliseconds.
%% @end
%%-----------------------------------------------------------------------------
-spec apply_after(
    Time :: non_neg_integer(), Module :: module(), Function :: atom(), Arguments :: [term()]
) -> {ok, reference()}.
apply_after(Time, Module, Function, Arguments) ->
    Pid = spawn(fun() ->
        receive
            apply_now -> apply(Module, Function, Arguments)
        end
    end),
    TRef = timer_manager:send_after(Time, Pid, apply_now),
    {ok, TRef}.
