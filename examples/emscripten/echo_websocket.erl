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

-module(echo_websocket).
-export([start/0]).

-define(ECHO_WEBSOCKET_URL, <<"wss://echo.websocket.org">>).

start() ->
    register(main, self()),
    Supported = websocket:is_supported(),
    if
        Supported ->
            emscripten:run_script(
                [
                    <<"window.document.getElementById('supported').innerHTML = 'yes';">>,
                    <<"window.document.getElementById('send-button').onclick = () => { Module.cast('main', window.document.getElementById('send-text').value); };">>
                ],
                [main_thread, async]
            ),
            websocket_loop();
        true ->
            emscripten:run_script(
                [
                    <<"window.document.getElementById('supported').innerHTML = 'no';">>
                ],
                [main_thread, async]
            )
    end.

websocket_loop() ->
    Websocket = websocket:new(?ECHO_WEBSOCKET_URL),
    websocket_loop(Websocket).

websocket_loop(Websocket) ->
    ReadyState = websocket:ready_state(Websocket),
    URL = websocket:url(Websocket),
    Protocol = websocket:protocol(Websocket),
    Extensions = websocket:extensions(Websocket),
    emscripten:run_script(
        [
            <<"window.document.getElementById('ready-state').innerHTML = '">>,
            atom_to_list(ReadyState),
            <<"';">>,
            <<"window.document.getElementById('url').innerHTML = '">>,
            URL,
            <<"';">>,
            <<"window.document.getElementById('protocol').innerHTML = '">>,
            Protocol,
            <<"';">>,
            <<"window.document.getElementById('extensions').innerHTML = '">>,
            Extensions,
            <<"';">>
        ],
        [main_thread, async]
    ),
    receive
        {emscripten, {cast, Message}} ->
            emscripten:run_script(
                [
                    <<"const e = window.document.createElement('div');">>,
                    <<"e.classList.add('client-msg');">>,
                    <<"e.append(\"">>,
                    escape_js_str(binary_to_list(Message)),
                    <<"\");">>,
                    <<"window.document.getElementById('transcript').appendChild(e);">>
                ],
                [main_thread, async]
            ),
            ok = websocket:send_binary(Websocket, Message),
            websocket_loop(Websocket);
        {websocket_open, Websocket} ->
            emscripten:run_script(
                [
                    <<"window.document.getElementById('send-text').disabled = false;">>,
                    <<"window.document.getElementById('send-button').disabled = false;">>
                ],
                [main_thread, async]
            ),
            websocket_loop(Websocket);
        {websocket, Websocket, Data} ->
            emscripten:run_script(
                [
                    <<"const e = window.document.createElement('div');">>,
                    <<"e.classList.add('server-msg');">>,
                    <<"e.append(\"">>,
                    escape_js_str(binary_to_list(Data)),
                    <<"\");">>,
                    <<"window.document.getElementById('transcript').appendChild(e);">>
                ],
                [main_thread, async]
            ),
            websocket_loop(Websocket);
        {websocket_error, Websocket} ->
            emscripten:run_script(
                [
                    <<"window.document.getElementById('send-text').disabled = false;">>,
                    <<"window.document.getElementById('send-button').disabled = false;">>,
                    <<"const e = window.document.createElement('div');">>,
                    <<"e.classList.add('error-msg');">>,
                    <<"e.append('Error');">>,
                    <<"window.document.getElementById('transcript').appendChild(e);">>
                ],
                [main_thread, async]
            );
        {websocket_closed, Websocket, {WasClean, Code, Reason}} ->
            emscripten:run_script(
                [
                    <<"window.document.getElementById('send-text').disabled = true;">>,
                    <<"window.document.getElementById('send-button').disabled = true;">>,
                    <<"const e = window.document.createElement('div');">>,
                    <<"e.classList.add('close-msg');">>,
                    <<"const wasClean = window.document.createElement('p');">>,
                    <<"wasClean.append(\"">>,
                    atom_to_list(WasClean),
                    <<"\");">>,
                    <<"e.appendChild(wasClean);">>,
                    <<"const code = window.document.createElement('p');">>,
                    <<"code.append(\"">>,
                    integer_to_list(Code),
                    <<"\");">>,
                    <<"e.appendChild(code);">>,
                    <<"const reason = window.document.createElement('p');">>,
                    <<"reason.append(\"">>,
                    escape_js_str(binary_to_list(Reason)),
                    <<"\");">>,
                    <<"e.appendChild(reason);">>,
                    <<"window.document.getElementById('transcript').appendChild(e);">>
                ],
                [main_thread, async]
            );
        Other ->
            io:format("Unexpected message ~p\n", [Other]),
            websocket_loop(Websocket)
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
