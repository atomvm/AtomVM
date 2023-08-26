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

-module(copy_terms17).

-export([start/0, sort/1, insert/2, loop/0]).

start() ->
    Pid = spawn_opt(fun loop/0, []),
    Ref = make_ref(),
    Ref2 = make_ref(),
    Pid !
        {sort, Ref, self(), Ref2, [
            <<"Hello">>,
            <<"Ciao">>,
            <<"Hola">>,
            <<"Hi">>,
            <<"Hello World">>,
            <<"Bonjur">>
        ]},
    Res =
        receive
            {reply, Ref, Sorted, Ref2} -> Sorted
        end,
    Pid ! terminate,
    [_, _, _, _, _, Last] = Res,
    byte_size(Last).

loop() ->
    case handle_request() of
        terminate ->
            terminate;
        ok ->
            loop()
    end.

handle_request() ->
    receive
        {sort, Ref, Pid, Ref2, L} ->
            Pid ! {reply, Ref, sort(L, []), Ref2},
            ok;
        terminate ->
            terminate
    end.

sort(L) ->
    sort(L, []).

sort([], Sorted) ->
    Sorted;
sort([H | Unsorted], Sorted) ->
    NextSorted = insert(Sorted, H),
    sort(Unsorted, NextSorted).

insert(L, I) ->
    insert(L, [], I).

insert([], HL, I) ->
    HL ++ [I];
insert([H | T], HL, I) when byte_size(I) < byte_size(H) ->
    HL ++ [I, H | T];
insert([H | T], HL, I) ->
    insert(T, HL ++ [H], I).
