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

    io:format("Starting application...~n"),

    Exit =
        try boot() of
            Result -> {exit, Result}
        catch
            Error -> {crash, Error}
        end,
    erlang:display(Exit),

    io:format("Looping...~n"),

    loop().

loop() ->
    receive
        Msg ->
            erlang:display({received_message, Msg}),
            loop()
    end.

maybe_start_dev_mode(SystemStatus) ->
    case {SystemStatus, esp:nvs_get_binary(atomvm, dev_mode)} of
        {_, <<"always">>} ->
            ep32devmode:start_dev_mode();
        {_, <<"never">>} ->
            not_started;
        {ok, undefined} ->
            not_started;
        {failed_app_start, undefined} ->
            esp32devmode:start_dev_mode()
    end.

% TODO: add support for multiple apps
% /dev/partition/by-name/app1.avm
% /dev/partition/by-name/app2.avm

boot() ->
    BootPath = get_boot_path(),
    case atomvm:add_avm_pack_file(BootPath, [{name, app}]) of
        ok ->
            StartModule = get_start_module(),
            maybe_start_dev_mode(ok),
            StartModule:start();
        {error, Reason} ->
            io:format("Failed app start: ~p.~n", [Reason]),
            maybe_start_dev_mode(failed_app_start)
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
                error ->
                    main;
                {ok, ModuleNameWithExt} when is_binary(ModuleNameWithExt) ->
                    Len = byte_size(ModuleNameWithExt) - byte_size(<<".beam">>),
                    ModuleName = binary:part(ModuleNameWithExt, 0, Len),
                    erlang:binary_to_atom(ModuleName, latin1)
            end;
        Module ->
            erlang:binary_to_atom(Module, latin1)
    end.
