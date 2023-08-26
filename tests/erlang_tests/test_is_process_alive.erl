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

-module(test_is_process_alive).

-export([start/0, fact/1]).

start() ->
    {Pid, Monitor} = spawn_opt(fun no_loop/0, [monitor]),
    A = g(is_process_alive(Pid)),
    Pid ! {self(), 5},
    Fact =
        receive
            {Pid, Result} ->
                Result
        end,
    ok =
        receive
            {'DOWN', Monitor, process, Pid, normal} -> ok
        after 500 -> timeout
        end,
    case is_process_alive(Pid) of
        false ->
            Fact + A;
        true ->
            A
    end.

no_loop() ->
    receive
        {Pid, AnyInteger} when is_integer(AnyInteger) ->
            Pid ! {self(), fact(AnyInteger)};
        {Pid, _AnyVal} ->
            Pid ! {self(), error}
    end.

fact(0) ->
    1;
fact(N) ->
    N * fact(N - 1).

g(true) ->
    1;
g(false) ->
    0.
