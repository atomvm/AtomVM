%
% This file is part of AtomVM.
%
% Copyright 2017 Davide Bettio <davide@uninstall.it>
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

-module(send_receive).

-export([start/0, send_test/2]).

start() ->
    Test0 = test_send2(),
    Test1 = test_send(),
    Test2 = test_send_to_int(),
    Test3 = test_non_resitered_name(),
    Test0 + Test1 + Test2 + Test3.

send_test(V, P) ->
    P ! V.

%priv

%% test send to pid
test_send2() ->
    send_test(9, self()),
    receive
        Value -> Value
    end.

%% test send to registered process
test_send() ->
    erlang:register(tester, self()),
    send_test(6, tester),
    receive
        RegisteredVal -> RegisteredVal
    end.

%% test send to integer
test_send_to_int() ->
    try 1 ! 1 of
        _Any -> 0
    catch
        error:badarg -> 2
    end.

%% test send to non-registered atom
test_non_resitered_name() ->
    try nobody ! test of
        _Any -> 0
    catch
        error:badarg -> 1
    end.
