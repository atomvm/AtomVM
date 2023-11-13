%!/usr/bin/env escript
%
% This file is part of AtomVM.
%
% Copyright 2023 Winford <winford@object.stream>
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

-module(atomvm_stm32_config_query).

-include("device_config.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    case length(Args) of
        2 ->
            ok;
        _ ->
            error()
    end,
    [Device | Parameter] = Args,
    Config = get_dev_config(Device),
    case Parameter of
        ["nucleo_com"] ->
            io:put_chars(get_nucleo_com(Device));
        _ ->
            Key = get_param_key(Parameter),
            io:put_chars(maps:get(Key, Config, ""))
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

error() ->
    io:format("~n    Usage: ~s <Device> <Parameter>~n", [?MODULE_STRING]),
    io:format("~n        Where <Parameter> is one of: clock | rom |  ram | ccm~n"),
    erlang:halt(255).

get_dev_config(Device) ->
    Core = string:left(Device, 7),
    Lookup = string:slice(Device, 7),
    case Core of
        "stm32f4" ->
            Config = get_f4dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32f7" ->
            Config = get_f7dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        _ ->
            io:format("Error! Unsupported device ~s.~n", [Device]),
            erlang:halt(255)
    end.

get_f4dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "01" ->
            case Flash of
                "e" ->
                    ?STM32F401_E;
                _ ->
                    unsupported
            end;
        "05" ->
            case Flash of
                "e" ->
                    ?STM32F4_01_57_E;
                "g" ->
                    ?STM32F4_01_57_G;
                _ ->
                    unsupported
            end;
        "07" ->
            case Flash of
                "e" ->
                    ?STM32F4_01_57_E;
                "g" ->
                    ?STM32F4_01_57_G;
                _ ->
                    unsupported
            end;
        "11" ->
            case Flash of
                "e" ->
                    ?STM32F411_E;
                _ ->
                    unsupported
            end;
        "12" ->
            case Flash of
                "e" ->
                    ?STM32F412_E;
                "g" ->
                    ?STM32F412_G;
                _ ->
                    unsupported
            end;
        "13" ->
            case Flash of
                "g" ->
                    ?STM32F4_12_3_G;
                "h" ->
                    ?STM32F4_12_3_H;
                _ ->
                    unsupported
            end;
        "15" ->
            case Flash of
                "e" ->
                    ?STM32F4_01_57_E;
                "g" ->
                    ?STM32F4_01_57_G;
                _ ->
                    unsupported
            end;
        "17" ->
            case Flash of
                "e" ->
                    ?STM32F4_01_57_E;
                "g" ->
                    ?STM32F4_01_57_G;
                _ ->
                    unsupported
            end;
        "23" ->
            case Flash of
                "g" ->
                    ?STM32F4_12_3_G;
                "h" ->
                    ?STM32F4_12_3_H;
                _ ->
                    unsupported
            end;
        "27" ->
            case Flash of
                "e" ->
                    ?STM32F4_23_79_E;
                "g" ->
                    ?STM32F4_23_79_G;
                "i" ->
                    ?STM32F4_23_79_I;
                _ ->
                    unsupported
            end;
        "29" ->
            case Flash of
                "e" ->
                    ?STM32F4_23_79_E;
                "g" ->
                    ?STM32F4_23_79_G;
                "i" ->
                    ?STM32F4_23_79_I;
                _ ->
                    unsupported
            end;
        "37" ->
            case Flash of
                "e" ->
                    ?STM32F4_23_79_E;
                "g" ->
                    ?STM32F4_23_79_G;
                "i" ->
                    ?STM32F4_23_79_I;
                _ ->
                    unsupported
            end;
        "39" ->
            case Flash of
                "e" ->
                    ?STM32F4_23_79_E;
                "g" ->
                    ?STM32F4_23_79_G;
                "i" ->
                    ?STM32F4_23_79_I;
                _ ->
                    unsupported
            end;
        "46" ->
            case Flash of
                "e" ->
                    ?STM32F446_E;
                _ ->
                    unsupported
            end;
        "69" ->
            case Flash of
                "e" ->
                    ?STM32F4_67_9_E;
                "g" ->
                    ?STM32F4_67_9_G;
                "i" ->
                    ?STM32F4_67_9_I;
                _ ->
                    unsupported
            end;
        "79" ->
            case Flash of
                "e" ->
                    ?STM32F4_67_9_E;
                "g" ->
                    ?STM32F4_67_9_G;
                "i" ->
                    ?STM32F4_67_9_I;
                _ ->
                    unsupported
            end;
        _ ->
            unsupported
    end.

get_f7dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "22" ->
            case Flash of
                "e" ->
                    ?STM32F7_23_23_E;
                _ ->
                    unsupported
            end;
        "23" ->
            case Flash of
                "e" ->
                    ?STM32F7_23_23_E;
                _ ->
                    unsupported
            end;
        "32" ->
            case Flash of
                "e" ->
                    ?STM32F7_23_23_E;
                _ ->
                    unsupported
            end;
        "33" ->
            case Flash of
                "e" ->
                    ?STM32F7_23_23_E;
                _ ->
                    unsupported
            end;
        "45" ->
            case Flash of
                "e" ->
                    ?STM32F745_E;
                "g" ->
                    ?STM32F745_G;
                _ ->
                    unsupported
            end;
        "46" ->
            case Flash of
                "e" ->
                    ?STM32F7_45_6_E;
                "g" ->
                    ?STM32F7_45_6_G;
                _ ->
                    unsupported
            end;
        "56" ->
            case Flash of
                "e" ->
                    ?STM32F7_45_6_E;
                "g" ->
                    ?STM32F7_45_6_G;
                _ ->
                    unsupported
            end;
        "65" ->
            case Flash of
                "g" ->
                    ?STM32F765_G;
                "i" ->
                    ?STM32F765_I;
                _ ->
                    unsupported
            end;
        "67" ->
            case Flash of
                "g" ->
                    ?STM32F7_67_7_G;
                "i" ->
                    ?STM32F7_67_7_I;
                _ ->
                    unsupported
            end;
        "68" ->
            case Flash of
                "i" ->
                    ?STM32F7_67_89_I;
                _ ->
                    unsupported
            end;
        "69" ->
            case Flash of
                "g" ->
                    ?STM32F769_G;
                "i" ->
                    ?STM32F7_67_89_I;
                _ ->
                    unsupported
            end;
        "77" ->
            case Flash of
                "g" ->
                    ?STM32F7_67_7_G;
                "i" ->
                    ?STM32F7_67_7_I;
                _ ->
                    unsupported
            end;
        "78" ->
            case Flash of
                "i" ->
                    ?STM32F7_67_89_I;
                _ ->
                    unsupported
            end;
        "79" ->
            case Flash of
                "i" ->
                    ?STM32F7_67_89_I;
                _ ->
                    unsupported
            end;
        _ ->
            unsupported
    end.

get_nucleo_com(Device) ->
    PinVariant = string:slice(Device, 9, 1),
    case PinVariant of
        "r" ->
            "CONSOLE_2";
        "z" ->
            "CONSOLE_3";
        _ ->
            io:format("Error: ~p is not a valid Nucleo device.~n", [Device]),
            erlang:halt(255)
    end.

get_param_key(Parameter) ->
    case Parameter of
        ["rom"] ->
            rom;
        ["ram"] ->
            ram;
        ["ccm"] ->
            ccm;
        ["clock"] ->
            clock;
        _ ->
            io:format("Error: ~p is not a valid configuration parameter.~n", [Parameter]),
            erlang:halt(255)
    end.
