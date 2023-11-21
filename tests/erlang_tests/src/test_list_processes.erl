%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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

-module(test_list_processes).

-export([start/0, loop/1]).

start() ->
    Self = self(),
    IdList = [1, 2, 3],
    Pids = [spawn_opt(?MODULE, loop, [Self], [monitor]) || _ <- IdList],
    [
        receive
            ok -> ok
        end
     || _ <- IdList
    ],
    test_list_processes(Pids, 0, Self).

test_list_processes([], Accum, _Self) ->
    Accum;
test_list_processes([{Pid, Ref} | T] = _PidList, Accum, Self) ->
    assert(contains(erlang:processes(), Pid)),
    Pid ! {Self, stop},
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    end,
    assert(not contains(erlang:processes(), Pid)),
    test_list_processes(T, Accum + 1, Self).

loop(undefined) ->
    receive
        {_Pid, stop} ->
            ok;
        _ ->
            loop(undefined)
    end;
loop(Pid) ->
    Pid ! ok,
    loop(undefined).

contains([], _E) ->
    false;
contains([E | _T], E) ->
    true;
contains([_ | T], E) ->
    contains(T, E).

assert(true) -> ok.
