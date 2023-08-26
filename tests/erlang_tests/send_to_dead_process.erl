%
% This file is part of AtomVM.
%
% Copyright 2019 Riccardo Binetti <rbino@gmx.com>
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

-module(send_to_dead_process).

-export([start/0]).

start() ->
    Pid = spawn_opt(fun double_and_terminate/0, []),
    Pid ! {self(), 10},
    Result =
        receive
            Any -> Any
        end,
    case Pid ! ping of
        ping ->
            Result;
        _Other ->
            0
    end.

double_and_terminate() ->
    receive
        {Pid, N} -> Pid ! N * 2
    end.
