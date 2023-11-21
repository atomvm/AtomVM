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

-module(test_send).

-export([start/0]).

start() ->
    {Dead, DeadMonitor} = spawn_opt(fun() -> ok end, [monitor]),
    ok =
        receive
            {'DOWN', DeadMonitor, process, Dead, normal} -> ok
        after 500 -> timeout
        end,
    Sent = send(self(), 32),
    receive
        Sent ->
            Recv = Sent;
        _Any ->
            Recv = 64
    after 5000 ->
        Recv = 128
    end,
    T1 = Sent - Recv,
    T2 = send_mal(5, 3),
    T3 = send_bad_atom(bogus, 4),
    T4 = send_dead(Dead, 6),
    T5 = send_registered(8),
    T1 + T2 + T3 + T4 + T5.

send(A, B) ->
    try erlang:send(A, B) of
        B -> B;
        _Any -> -1
    catch
        error:badarg -> B - 2;
        _:_ -> -4
    end.

send_mal(A, B) ->
    try erlang:send(A, B) of
        B -> B;
        _Any -> -1
    catch
        error:badarg -> B - 3;
        _:_ -> -4
    end.

send_bad_atom(A, B) ->
    try erlang:send(A, B) of
        B -> B;
        _Any -> -1
    catch
        error:badarg -> B - 4;
        _:_ -> -4
    end.

send_dead(A, B) ->
    try erlang:send(A, B) of
        B -> B - 6;
        _Any -> -1
    catch
        error:badarg -> -2;
        _:_ -> -4
    end.

send_registered(B) ->
    erlang:register(listen, self()),
    try erlang:send(listen, B) of
        B ->
            receive
                B -> B - 8;
                _Any -> -1
            after 5000 ->
                512
            end;
        _Any ->
            -1
    catch
        error:badarg -> -2;
        _:_ -> -4
    end.
