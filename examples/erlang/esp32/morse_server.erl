%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Davide Bettio <davide@uninstall.it>
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

-module(morse_server).

-export([start/0, handle_req/3]).

start() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, esp:nvs_get_binary(atomvm, sta_ssid, <<"myssid">>)},
            {psk, esp:nvs_get_binary(atomvm, sta_psk, <<"mypsk">>)},
            {connected, fun() -> Self ! connected end},
            {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
            {disconnected, fun() -> Self ! disconnected end}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            wait_for_message();
        Error ->
            erlang:display(Error)
    end.

handle_req("GET", [], Conn) ->
    Body =
        <<
            "<html>\n"
            "                <body>\n"
            "                    <h1>Morse Encoder</h1>\n"
            "                    <form method=\"post\">\n"
            "                        <p>Text: <input type=\"text\" name=\"text\"></p>\n"
            "                        <p>GPIO: <input type=\"text\" name=\"gpio\"></p>\n"
            "                        <input type=\"submit\" value=\"Submit\">\n"
            "                    </form>\n"
            "                </body>\n"
            "             </html>"
        >>,
    http_server:reply(200, Body, Conn);
handle_req("POST", [], Conn) ->
    ParamsBody = proplists:get_value(body_chunk, Conn),
    Params = http_server:parse_query_string(ParamsBody),

    GPIOString = proplists:get_value("gpio", Params),
    GPIONum = safe_list_to_integer(GPIOString),

    Text = proplists:get_value("text", Params, "off"),
    MorseText = morse_encode(Text),

    spawn(fun() -> blink_led(GPIONum, MorseText) end),

    Body = [
        <<
            "<html>\n"
            "                <body>\n"
            "                    <h1>Text Encoded</h1>"
        >>,
        <<"<p>">>,
        MorseText,
        <<
            "</p1>\n"
            "                </body>\n"
            "             </html>"
        >>
    ],
    http_server:reply(200, Body, Conn);
handle_req(Method, Path, Conn) ->
    erlang:display(Conn),
    erlang:display({Method, Path}),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).

safe_list_to_integer(L) ->
    try erlang:list_to_integer(L) of
        Res -> Res
    catch
        _:_ -> undefined
    end.

wait_for_message() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    receive
        connected ->
            erlang:display(connected);
        {ok, IpInfo} ->
            erlang:display(IpInfo),
            http_server:start_server(8080, Router);
        disconnected ->
            erlang:display(disconnected)
    after 15000 ->
        ok
    end,
    wait_for_message().

get_gpio() ->
    case whereis(gpio) of
        undefined ->
            GPIO = gpio:open(),
            GPIO;
        GPIO ->
            GPIO
    end.

blink_led(undefined, _L) ->
    ok;
blink_led(GPIONum, L) ->
    GPIO = get_gpio(),
    gpio:set_direction(GPIO, GPIONum, output),
    blink_led(GPIO, GPIONum, L).

blink_led(_GPIO, _GPIONum, []) ->
    ok;
blink_led(GPIO, GPIONum, [H | T]) ->
    case H of
        $\s ->
            gpio:set_level(GPIO, GPIONum, 0),
            timer:sleep(120);
        $. ->
            gpio:set_level(GPIO, GPIONum, 1),
            timer:sleep(120),
            gpio:set_level(GPIO, GPIONum, 0),
            timer:sleep(120);
        $- ->
            gpio:set_level(GPIO, GPIONum, 1),
            timer:sleep(120 * 3),
            gpio:set_level(GPIO, GPIONum, 0),
            timer:sleep(120)
    end,
    blink_led(GPIO, GPIONum, T).

morse_encode(L) ->
    morse_encode(L, []).

morse_encode([], Acc) ->
    Acc;
morse_encode([H | L], Acc) ->
    M = to_morse(string:to_upper(H)),
    morse_encode(L, Acc ++ M).

to_morse(C) ->
    case C of
        $\s -> "       ";
        $0 -> "-----   ";
        $1 -> ".----   ";
        $2 -> "..---   ";
        $3 -> "...--   ";
        $4 -> "....-   ";
        $5 -> ".....   ";
        $6 -> "-....   ";
        $7 -> "--...   ";
        $8 -> "---..   ";
        $9 -> "----.   ";
        $A -> ".-";
        $B -> "-...   ";
        $C -> "-.-.   ";
        $D -> "-..   ";
        $E -> ".   ";
        $F -> "..-.   ";
        $G -> "--.   ";
        $H -> "....   ";
        $I -> "..   ";
        $J -> ".---   ";
        $K -> "-.-   ";
        $L -> ".-..   ";
        $M -> "--   ";
        $N -> "-.   ";
        $O -> "---   ";
        $P -> ".--.   ";
        $Q -> "--.-   ";
        $R -> ".-.   ";
        $S -> "...   ";
        $T -> "-   ";
        $U -> "..-   ";
        $V -> "...-   ";
        $W -> ".--   ";
        $X -> "-..-    ";
        $Y -> "-.--   ";
        $Z -> "--..   "
    end.
