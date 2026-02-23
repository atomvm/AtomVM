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
    io:format(
        "~n        Where <Parameter> is one of: clock | rom | ram | ccm | flash_size_kb | ram_size_kb~n"
    ),
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
        "stm32h7" ->
            Config = get_h7dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32u5" ->
            Config = get_u5dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32wb" ->
            Config = get_wbdev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32h5" ->
            Config = get_h5dev_config(Lookup),
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
        "stm32g0" ->
            Config = get_g0dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32g4" ->
            Config = get_g4dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32l4" ->
            Config = get_l4dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32l5" ->
            Config = get_l5dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32f2" ->
            Config = get_f2dev_config(Lookup),
            case Config of
                unsupported ->
                    io:format("Error! Unsupported device ~s.~n", [Device]),
                    erlang:halt(255);
                _ ->
                    Config
            end;
        "stm32u3" ->
            Config = get_u3dev_config(Lookup),
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

get_h7dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "43" ->
            case Flash of
                "i" ->
                    ?STM32H743_I;
                "g" ->
                    ?STM32H743_G;
                _ ->
                    unsupported
            end;
        _ ->
            unsupported
    end.

get_u5dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "85" ->
            case Flash of
                "i" ->
                    ?STM32U585_I;
                _ ->
                    unsupported
            end;
        _ ->
            unsupported
    end.

get_wbdev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "55" ->
            case Flash of
                "g" ->
                    ?STM32WB55_G;
                "e" ->
                    ?STM32WB55_E;
                _ ->
                    unsupported
            end;
        _ ->
            unsupported
    end.

get_h5dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "62" ->
            case Flash of
                "g" ->
                    ?STM32H562_G;
                "i" ->
                    ?STM32H562_I;
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
        "22" -> f7_72_73(Flash);
        "23" -> f7_72_73(Flash);
        "32" -> f7_72_73(Flash);
        "33" -> f7_72_73(Flash);
        "30" -> f7_72_73(Flash);
        "50" -> f7_72_73(Flash);
        "45" -> f7_74_75(Flash);
        "46" -> f7_74_75(Flash);
        "56" -> f7_74_75(Flash);
        "65" -> f7_76_77(Flash);
        "67" -> f7_76_77(Flash);
        "69" -> f7_76_77(Flash);
        "77" -> f7_76_77(Flash);
        "79" -> f7_76_77(Flash);
        _ -> unsupported
    end.

f7_72_73(Flash) ->
    case Flash of
        "e" -> ?STM32F72_73_E;
        _ -> unsupported
    end.

f7_74_75(Flash) ->
    case Flash of
        "e" -> ?STM32F74_75_E;
        "g" -> ?STM32F74_75_G;
        _ -> unsupported
    end.

f7_76_77(Flash) ->
    case Flash of
        "e" -> ?STM32F76_77_E;
        "g" -> ?STM32F76_77_G;
        "i" -> ?STM32F76_77_I;
        _ -> unsupported
    end.

get_g0dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "b1" ->
            case Flash of
                "e" -> ?STM32G0B1_E;
                _ -> unsupported
            end;
        "71" ->
            case Flash of
                "b" -> ?STM32G071_B;
                _ -> unsupported
            end;
        _ ->
            unsupported
    end.

get_g4dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "73" ->
            case Flash of
                "e" -> ?STM32G47_48_E;
                _ -> unsupported
            end;
        "74" ->
            case Flash of
                "e" -> ?STM32G47_48_E;
                _ -> unsupported
            end;
        "83" ->
            case Flash of
                "e" -> ?STM32G47_48_E;
                _ -> unsupported
            end;
        "84" ->
            case Flash of
                "e" -> ?STM32G47_48_E;
                _ -> unsupported
            end;
        "91" ->
            case Flash of
                "e" -> ?STM32G49_4A_E;
                _ -> unsupported
            end;
        "a1" ->
            case Flash of
                "e" -> ?STM32G49_4A_E;
                _ -> unsupported
            end;
        _ ->
            unsupported
    end.

get_l4dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "51" ->
            case Flash of
                "e" -> ?STM32L45_46_E;
                _ -> unsupported
            end;
        "52" ->
            case Flash of
                "e" -> ?STM32L45_46_E;
                _ -> unsupported
            end;
        "62" ->
            case Flash of
                "e" -> ?STM32L45_46_E;
                _ -> unsupported
            end;
        "71" ->
            case Flash of
                "e" -> ?STM32L47_E;
                "g" -> ?STM32L47_48_G;
                _ -> unsupported
            end;
        "75" ->
            case Flash of
                "e" -> ?STM32L47_E;
                "g" -> ?STM32L47_48_G;
                _ -> unsupported
            end;
        "76" ->
            case Flash of
                "e" -> ?STM32L47_E;
                "g" -> ?STM32L47_48_G;
                _ -> unsupported
            end;
        "85" ->
            case Flash of
                "e" -> ?STM32L47_E;
                "g" -> ?STM32L47_48_G;
                _ -> unsupported
            end;
        "86" ->
            case Flash of
                "e" -> ?STM32L47_E;
                "g" -> ?STM32L47_48_G;
                _ -> unsupported
            end;
        "96" ->
            case Flash of
                "g" -> ?STM32L49_4A_G;
                _ -> unsupported
            end;
        "a6" ->
            case Flash of
                "g" -> ?STM32L49_4A_G;
                _ -> unsupported
            end;
        "r5" ->
            case Flash of
                "g" -> ?STM32L4R_4S_G;
                "i" -> ?STM32L4R_4S_I;
                _ -> unsupported
            end;
        "r7" ->
            case Flash of
                "g" -> ?STM32L4R_4S_G;
                "i" -> ?STM32L4R_4S_I;
                _ -> unsupported
            end;
        "r9" ->
            case Flash of
                "g" -> ?STM32L4R_4S_G;
                "i" -> ?STM32L4R_4S_I;
                _ -> unsupported
            end;
        "s5" ->
            case Flash of
                "g" -> ?STM32L4R_4S_G;
                "i" -> ?STM32L4R_4S_I;
                _ -> unsupported
            end;
        "s7" ->
            case Flash of
                "g" -> ?STM32L4R_4S_G;
                "i" -> ?STM32L4R_4S_I;
                _ -> unsupported
            end;
        "s9" ->
            case Flash of
                "g" -> ?STM32L4R_4S_G;
                "i" -> ?STM32L4R_4S_I;
                _ -> unsupported
            end;
        _ ->
            unsupported
    end.

get_l5dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "52" ->
            case Flash of
                "e" -> ?STM32L55_56_E;
                _ -> unsupported
            end;
        "62" ->
            case Flash of
                "e" -> ?STM32L55_56_E;
                _ -> unsupported
            end;
        _ ->
            unsupported
    end.

get_f2dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "05" -> f2_flash(Flash);
        "07" -> f2_flash(Flash);
        "15" -> f2_flash(Flash);
        "17" -> f2_flash(Flash);
        _ -> unsupported
    end.

f2_flash(Flash) ->
    case Flash of
        "e" -> ?STM32F2_E;
        "f" -> ?STM32F2_F;
        "g" -> ?STM32F2_G;
        _ -> unsupported
    end.

get_u3dev_config(Lookup) ->
    Line = string:slice(Lookup, 0, 2),
    Flash = string:slice(Lookup, 3, 1),
    case Line of
        "75" ->
            case Flash of
                "e" -> ?STM32U37_38_E;
                "g" -> ?STM32U37_38_G;
                _ -> unsupported
            end;
        "85" ->
            case Flash of
                "e" -> ?STM32U37_38_E;
                "g" -> ?STM32U37_38_G;
                _ -> unsupported
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
        ["flash_size_kb"] ->
            flash_size_kb;
        ["ram_size_kb"] ->
            ram_size_kb;
        _ ->
            io:format("Error: ~p is not a valid configuration parameter.~n", [Parameter]),
            erlang:halt(255)
    end.
