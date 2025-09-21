%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_websockets).
-export([start/0]).

-define(DEFAULT_ECHO_WEBSOCKET_URL, <<"wss://echo.websocket.org">>).

get_echo_websocket_url() ->
    case os:getenv("ECHO_WEBSOCKET_URL") of
        false -> ?DEFAULT_ECHO_WEBSOCKET_URL;
        Url -> list_to_binary(Url)
    end.

start() ->
    try
        true = websocket:is_supported(),
        EchoUrl = get_echo_websocket_url(),
        Websocket = websocket:new(EchoUrl),
        connecting = websocket:ready_state(Websocket),
        ok =
            receive
                {websocket_open, Websocket} -> ok;
                Other0 -> {unexpected, ?LINE, Other0}
            after 2500 ->
                {timeout, ?LINE}
            end,
        open = websocket:ready_state(Websocket),
        ok =
            receive
                {websocket, Websocket, <<"Request served by ", _/binary>>} -> ok;
                Other1 -> {unexpected, ?LINE, Other1}
            after 2500 ->
                {timeout, ?LINE}
            end,
        ok = websocket:send_utf8(Websocket, <<"Hello">>),
        ok =
            receive
                {websocket, Websocket, <<"Hello">>} -> ok;
                Other2 -> {unexpected, ?LINE, Other2}
            after 2500 ->
                {timeout, ?LINE}
            end,
        ok = websocket:send_binary(Websocket, <<"World">>),
        ok =
            receive
                {websocket, Websocket, <<"World">>} -> ok;
                Other3 -> {unexpected, ?LINE, Other3}
            after 2500 ->
                {timeout, ?LINE}
            end,
        ok = websocket:close(Websocket),
        closing = websocket:ready_state(Websocket),
        ok =
            receive
                {websocket_close, Websocket, {true, 1000, <<>>}} -> ok;
                Other4 -> {unexpected, ?LINE, Other4}
            after 2500 ->
                {timeout, ?LINE}
            end,
        closed = websocket:ready_state(Websocket),
        emscripten:run_script(
            [
                <<"window.document.getElementById('result').innerHTML = 'Test success';">>
            ],
            [main_thread, async]
        ),
        io:format("OK\n")
    catch
        T:V:S ->
            emscripten:run_script(
                [
                    <<"window.document.getElementById('result').innerHTML = \"Failure: ">>,
                    escape_js_str(lists:flatten(io_lib:format("~p\n~p\n~p", [T, V, S]))),
                    <<"\";">>
                ],
                [main_thread, async]
            )
    end.

escape_js_str(Str) ->
    escape_js_str(Str, []).

escape_js_str([$" | Tail], Acc) ->
    escape_js_str(Tail, ["\\\"" | Acc]);
escape_js_str([$\n | Tail], Acc) ->
    escape_js_str(Tail, ["<br />" | Acc]);
escape_js_str([C | Tail], Acc) ->
    escape_js_str(Tail, [C | Acc]);
escape_js_str([], Acc) ->
    lists:reverse(Acc).
