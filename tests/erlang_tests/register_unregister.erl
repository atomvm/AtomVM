%
% This file is part of AtomVM.
%
% Copyright 2017-2022 Davide Bettio <davide@uninstall.it>
% Copyright 2022 Winford (Uncle Grumpy) <dwinford@pm.me>
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

-module(register_unregister).

-export([start/0]).

start() ->
    %% test register result
    T1 = do_reg(test, self()),
    %% confirm that registered atom is retrievable
    T2 = test_reg(test),
    %% test register with atom already registered
    T3 = test_already_registered(test),
    %% test unregister result
    T4 = do_unreg(test),
    %% confirm name was unregistered
    T5 = test_unreg(test),
    %% test register with invalid PID
    T6 = reg_bad_arg(test, 1000),
    %% register as atom "undefined"
    T7 = reg_bad_arg(undefined, self()),
    %% test unregister an atom that has not been registered
    T8 = unreg_not_registered(something),
    T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8.

do_reg(Pid, Name) ->
    Res = register(Pid, Name),
    case Res of
        true ->
            1;
        _ ->
            0
    end.

test_reg(Name) ->
    Pid = whereis(Name),
    Res = is_process_alive(Pid),
    case Res of
        true ->
            1;
        _ ->
            0
    end.

do_unreg(Name) ->
    Res = unregister(Name),
    case Res of
        true ->
            1;
        _ ->
            0
    end.

test_unreg(Name) ->
    Res = whereis(Name),
    case Res of
        undefined ->
            1;
        _ ->
            0
    end.

reg_bad_arg(Name, Pid) ->
    try register(Name, Pid) of
        true ->
            0
    catch
        error:badarg ->
            1
    end.

test_already_registered(Name) ->
    try register(Name, self()) of
        true ->
            0
    catch
        error:badarg ->
            1
    end.

unreg_not_registered(Name) ->
    try unregister(Name) of
        true ->
            0
    catch
        error:badarg ->
            1
    end.
