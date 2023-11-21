%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(esp32init).

-export([start/0]).

start() ->
    console:print(<<"AtomVM init.\n">>),
    boot().

is_dev_mode_enabled(SystemStatus) ->
    case {SystemStatus, esp:nvs_get_binary(atomvm, dev_mode)} of
        {_, <<"always">>} ->
            true;
        {_, <<"never">>} ->
            false;
        {ok, undefined} ->
            false;
        {app_exit, undefined} ->
            false;
        {app_fail, undefined} ->
            true
    end.

maybe_start_dev_mode(SystemStatus) ->
    case is_dev_mode_enabled(SystemStatus) of
        true -> esp32devmode:start_dev_mode();
        false -> not_started
    end.

% TODO: add support for multiple apps
% /dev/partition/by-name/app1.avm
% /dev/partition/by-name/app2.avm

boot() ->
    BootPath = get_boot_path(),
    case atomvm:add_avm_pack_file(BootPath, [{name, app}]) of
        ok ->
            StartModule = get_start_module(),
            DevOnExit = is_dev_mode_enabled(app_exit),
            StartedDevMode = maybe_start_dev_mode(ok),

            io:format("Starting application...~n"),
            case DevOnExit of
                true ->
                    try StartModule:start() of
                        Result -> io:format("Exited: ~p.~n", [Result])
                    catch
                        Error -> io:format("Crashed: ~p.~n", [Error])
                    end,
                    case StartedDevMode of
                        started -> ok;
                        _NotStarted -> maybe_start_dev_mode(app_exit)
                    end,
                    timer:sleep(infinity);
                false ->
                    StartModule:start()
            end;
        {error, Reason} ->
            io:format("Failed app start: ~p.~n", [Reason]),
            maybe_start_dev_mode(app_fail),
            timer:sleep(infinity)
    end.

get_boot_path() ->
    case esp:nvs_get_binary(atomvm, boot_path) of
        undefined ->
            "/dev/partition/by-name/main.avm";
        Path ->
            Path
    end.

get_start_module() ->
    case esp:nvs_get_binary(atomvm, start_module) of
        undefined ->
            case atomvm:get_start_beam(app) of
                {error, not_found} ->
                    main;
                {ok, ModuleNameWithExt} when is_binary(ModuleNameWithExt) ->
                    Len = byte_size(ModuleNameWithExt) - byte_size(<<".beam">>),
                    ModuleName = binary:part(ModuleNameWithExt, 0, Len),
                    erlang:binary_to_atom(ModuleName, latin1)
            end;
        Module ->
            erlang:binary_to_atom(Module, latin1)
    end.
