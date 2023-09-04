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

-module(copy_terms16).

-export([start/0, find/2]).

start() ->
    Pid = spawn_opt(fun loop/0, []),
    Ref = make_ref(),
    Pid ! {find, Ref, self(), [<<"Hello">>, <<"Ciao">>, <<"Hola">>, <<"Hi">>, <<"Bonjur">>]},
    Res =
        receive
            {reply, Ref, Longest} -> Longest
        end,
    Pid ! terminate,
    Res.

loop() ->
    case handle_request() of
        terminate ->
            terminate;
        ok ->
            loop()
    end.

handle_request() ->
    receive
        {find, Ref, Pid, List} ->
            Pid ! {reply, Ref, find(List, 0)},
            ok;
        terminate ->
            terminate
    end.

find([], Max) ->
    Max;
find([H | T], Max) when byte_size(H) > Max ->
    find(T, byte_size(H));
find([_H | T], Max) ->
    find(T, Max).
