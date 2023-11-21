%
% This file is part of AtomVM.
%
% Copyright 2021 Fred Dushin <fred@dushin.net>
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

-module(test_raise).

-export([start/0]).

start() ->
    Pid = spawn_opt(fun() -> loop(0) end, []),
    Tick = fun() -> Pid ! tick end,
    foo = tryit(
        fun() -> foo end,
        foo,
        fun() -> bar end,
        Tick
    ),
    bar = tryit(
        fun() -> throw(foo) end,
        foo,
        fun() -> bar end,
        Tick
    ),
    foo =
        try
            tryit(
                fun() -> throw(foo) end,
                bar,
                fun() -> bar end,
                Tick
            )
        catch
            _:E -> E
        end,
    caughtit = tryit(
        fun() ->
            tryit(
                fun() -> throw(passit) end,
                letitpass,
                fun() -> bar end,
                Tick
            )
        end,
        passit,
        fun() -> caughtit end,
        Tick
    ),
    caughtit = tryit(
        fun() ->
            tryit(
                fun() -> throw(passit) end,
                passit,
                fun() -> throw(throwfromcatch) end,
                Tick
            )
        end,
        throwfromcatch,
        fun() -> caughtit end,
        Tick
    ),
    Pid ! {self(), stop},
    receive
        X -> X
    end.

tryit(DoTry, CatchTerm, DoCatch, DoAfter) ->
    try
        DoTry()
    catch
        _:CatchTerm ->
            DoCatch()
    after
        DoAfter()
    end.

loop(Afters) ->
    receive
        tick ->
            loop(Afters + 1);
        {Pid, stop} ->
            Pid ! Afters
    end.
