%
% This file is part of AtomVM.
%
% Copyright 2025 Winford <winford@object.stream>
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

-module(test_gpio).

-export([start/0, test_nifs/2, test_ports/2]).

%% Bad argument and error reason raised if it is accepted
-define(BADPINS, [
    {-1, accepted_neg_pin_number},
    {<<0>>, accepted_binary_pin},
    {"GPIO_NUM_0", accepted_pin_string},
    {button, accepted_pin_atom},
    {{a, 0}, accepted_pin_tuple},
    {[0, 1, 2], accepted_pin_list},
    {2048, accepted_too_large},
    {2.0, accepted_float},
    {#{}, accepted_map}
]).
-define(BADMODES, [
    {up, accepted_badarg},
    {1, accepted_int_mode},
    {<<0>>, accepted_binary_mode},
    {<<"input">>, accepted_binary_string_mode},
    {"input", accepted_mode_string},
    {[input, up], accepted_mode_list},
    {{input, up}, accepted_mode_tuple},
    {2.0, accepted_float},
    {#{}, accepted_map}
]).
-define(BADPULL, [
    {1, accepted_int_pull},
    {<<1>>, accepted_binary_pull},
    {<<"up">>, accepted_binary_string_pull},
    {"up", accepted_pull_string},
    {{up, hold}, accepted_pull_tuple},
    {[up, foo, bar], accepted_pull_list},
    {sideways, accepted_invalid_atom},
    {2.0, accepted_float},
    {#{}, accepted_map}
]).
-define(BADLEVEL, [
    {medium, accepted_bad_atom},
    {-1, accepted_neg_level_number},
    {10, accepted_badarg_number},
    {<<1>>, accepted_binary_level},
    {<<"high">>, accepted_binary_level},
    {"high", accepted_level_string},
    {{0, high}, accepted_level_tuple},
    {[1], accepted_level_list},
    {1.0, accepted_float},
    {#{}, accepted_map}
]).

start() ->
    {Pin0, Pin1} = get_board_pins(maps:get(model, erlang:system_info(esp32_chip_info))),
    io:format(
        "Starting GPIO test, this board should have a jumper wire between pins ~p and ~p.~n", [
            Pin0, Pin1
        ]
    ),
    ok = test_nifs(Pin0, Pin1),
    %% test ports with the pins reversed so we be sure to test reconfiguring an input to an output and vice versa
    test_ports(Pin1, Pin0).

test_nifs(Input, Output) ->
    io:format("Testing nifs raise errors for badargs... "),
    ok = test_nif_badargs(Input),
    io:format("passed.~n"),
    io:format("Testing set_pin_mode/2, set_pin_pull/2, digital_write/2 & digital_read/1... "),
    ok = gpio:set_pin_mode(Input, input),
    ok = gpio:set_pin_pull(Input, up),
    ok = gpio:set_pin_mode(Output, output),
    ok = gpio:set_pin_pull(Output, floating),
    ok = gpio:digital_write(Output, high),
    high = gpio:digital_read(Input),
    ok = gpio:digital_write(Output, low),
    low = gpio:digital_read(Input),
    ok = gpio:digital_write(Output, 1),
    high = gpio:digital_read(Input),
    ok = gpio:digital_write(Output, 0),
    low = gpio:digital_read(Input),
    io:format("passed.~n").

test_ports(Input, Output) ->
    io:format("Testing ports return {error, Reason} for badargs... "),
    GPIO = gpio:start(),
    ok = test_port_bardargs(GPIO, Input),
    io:format("passed.~n"),
    io:format("Testing set_direction/3, set_level/3 & read/2... "),
    ok = gpio:set_direction(GPIO, Input, input),
    ok = gpio:set_pin_pull(Input, up),
    ok = gpio:set_direction(GPIO, Output, output),
    ok = gpio:set_pin_pull(Output, floating),
    ok = gpio:set_level(GPIO, Output, low),
    low = gpio:read(GPIO, Input),
    ok = gpio:set_level(GPIO, Output, high),
    high = gpio:read(GPIO, Input),
    ok = gpio:set_level(GPIO, Output, 0),
    low = gpio:read(GPIO, Input),
    ok = gpio:set_level(GPIO, Output, 1),
    io:format("passed.~n"),
    io:format("Testing GPIO interrupt... "),
    Self = self(),
    Listener = erlang:spawn(fun() -> interrupt_listener(Input, Self) end),
    erlang:spawn(fun() -> interrupt_after(1000, GPIO, Output, 0) end),
    ok = gpio:set_int(GPIO, Input, falling, Listener),
    receive
        {ok, interrupt} -> ok;
        Error -> throw(Error)
    after 5000 ->
        io:format("No interrupt after 5000 ms giving up"),
        throw(timeout_no_interrupt)
    end,
    erlang:spawn(fun() -> interrupt_after(1000, GPIO, Output, 0) end),
    ok = gpio:set_int(GPIO, Input, falling),
    receive
        {gpio_interrupt, Input} -> ok;
        Other -> throw(Other)
    after 5000 ->
        io:format("No interrupt after 5000 ms giving up"),
        throw(timeout_no_interrupt)
    end,
    io:format("passed.~n").

test_nif_badargs(Pin) ->
    Badpin_funs1 = [digital_read, hold_en, hold_dis],
    Badpin_funs2 = [{set_pin_mode, output}, {set_pin_pull, floating}, {digital_write, low}],
    Fun_args = [{set_pin_mode, ?BADMODES}, {set_pin_pull, ?BADPULL}, {digital_write, ?BADLEVEL}],

    lists:foreach(
        fun(TestFun) ->
            lists:foreach(
                fun({Badpin, Err}) -> ok = want_catch_throw(TestFun, Badpin, badarg, Err) end,
                ?BADPINS
            )
        end,
        Badpin_funs1
    ),

    lists:foreach(
        fun({TestFun, Arg}) ->
            lists:foreach(
                fun({Badpin, Err}) -> ok = want_catch_throw(TestFun, Badpin, Arg, badarg, Err) end,
                ?BADPINS
            )
        end,
        Badpin_funs2
    ),

    lists:foreach(
        fun({TestFun, BadArgs}) ->
            lists:foreach(
                fun({Badarg, Err}) -> ok = want_catch_throw(TestFun, Pin, Badarg, badarg, Err) end,
                BadArgs
            )
        end,
        Fun_args
    ),
    ok.

test_port_bardargs(GPIO, Pin) ->
    Badpin_funs2 = [read, remove_int],
    Badpin_funs3 = [{set_direction, input}, {set_level, low}, {set_int, low}],
    Fun_args = [{set_direction, ?BADMODES}, {set_level, ?BADLEVEL}, {set_int, ?BADLEVEL}],

    lists:foreach(
        fun(TestFun) ->
            lists:foreach(
                fun({Badpin, Err}) -> ok = want_error_tuple(TestFun, GPIO, Badpin, badarg, Err) end,
                ?BADPINS
            )
        end,
        Badpin_funs2
    ),

    lists:foreach(
        fun({TestFun, Arg}) ->
            lists:foreach(
                fun({Badpin, Err}) ->
                    ok = want_error_tuple(TestFun, GPIO, Badpin, Arg, badarg, Err)
                end,
                ?BADPINS
            )
        end,
        Badpin_funs3
    ),

    lists:foreach(
        fun({TestFun, TestArgs}) ->
            lists:foreach(
                fun({Badarg, Err}) ->
                    ok = want_error_tuple(TestFun, GPIO, Pin, Badarg, badarg, Err)
                end,
                TestArgs
            )
        end,
        Fun_args
    ),
    ok.

want_catch_throw(Fun, Pin, Catch, ErrorAtom) ->
    try gpio:Fun(Pin) of
        ok ->
            throw({Fun, ErrorAtom});
        Any ->
            throw({Fun, Any})
    catch
        _:Catch:_ ->
            ok
    end.

want_catch_throw(Fun, Pin, Arg, Catch, ErrorAtom) ->
    try gpio:Fun(Pin, Arg) of
        ok ->
            throw({Fun, ErrorAtom});
        Any ->
            throw({Fun, Any})
    catch
        _:Catch:_ ->
            ok
    end.

want_error_tuple(Fun, GPIO, Pin, Reason, ErrorAtom) ->
    try gpio:Fun(GPIO, Pin) of
        ok ->
            throw({Fun, ErrorAtom});
        {error, Reason} ->
            ok
    catch
        Error ->
            throw({Fun, {caught, Error}})
    end.

want_error_tuple(Fun, GPIO, Pin, Arg, Reason, ErrorAtom) ->
    try gpio:Fun(GPIO, Pin, Arg) of
        ok ->
            throw({Fun, ErrorAtom});
        {error, Reason} ->
            ok
    catch
        Error ->
            throw({Fun, {caught, Error}})
    end.

interrupt_after(Delay, GPIO, Pin, Level) ->
    timer:sleep(Delay),
    ok = gpio:set_level(GPIO, Pin, Level),
    ok = gpio:set_level(GPIO, Pin, 1 - Level).

interrupt_listener(Pin, ReplyTo) ->
    receive
        {gpio_interrupt, Pin} ->
            ok = gpio:remove_int(whereis(gpio), Pin),
            ReplyTo ! {ok, interrupt};
        Any ->
            throw({interrupt_listener, {received, Any}})
    end,
    interrupt_listener(Pin, ReplyTo).

get_board_pins(Chipset) ->
    case (Chipset) of
        esp32 -> {25, 26};
        esp32_c3 -> {4, 5};
        esp32_c6 -> {6, 7};
        esp32_h2 -> {0, 1};
        esp32_p4 -> {4, 5};
        esp32_s2 -> {3, 4};
        esp32_s3 -> {4, 5};
        _ -> throw(unsupported_chipset)
    end.
