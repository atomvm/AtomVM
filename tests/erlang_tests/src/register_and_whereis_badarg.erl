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

-module(register_and_whereis_badarg).

-export([start/0, f/1, do_register/2, do_whereis/1]).

start() ->
    do_register(fail, fail) + do_register(fail, self()) * 10 + do_whereis(fail) * 100.

f(Pid) when is_pid(Pid) ->
    Pid;
f(fail) ->
    5;
f(good) ->
    good.

do_register(A, B) ->
    try register(f(A), f(B)) of
        true -> 1;
        _Any -> 2
    catch
        error:badarg -> 3;
        _:_ -> 4
    end.

do_whereis(A) ->
    try whereis(f(A)) of
        APid when is_pid(APid) -> 1;
        _Any -> 2
    catch
        error:badarg -> 3;
        _:_ -> 4
    end.
