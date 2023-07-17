%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_html5).
-export([start/0]).

start() ->
    register(main, self()),
    emscripten:run_script(
        [
            <<"window.document.getElementById('memory-binary').onclick = () => { Module.cast('main', 'memory_binary'); };">>,
            <<"window.document.getElementById('memory-binary').disabled = false;">>
        ],
        [main_thread, async]
    ),
    spawn(fun() -> register_once_and_exit() end),
    spawn(fun() -> register_and_loop() end),
    loop().

loop() ->
    % make sure refc promise is garbage collected
    erlang:garbage_collect(),
    receive
        {emscripten, {cast, <<"memory_binary">>}} ->
            memory_binary(),
            loop()
    end.

append_result_span(Class, Text) ->
    emscripten:run_script(
        [
            <<"var spanEl = document.createElement('span');">>,
            <<"spanEl.classList.add('">>,
            Class,
            <<"');">>,
            <<"spanEl.append('">>,
            Text,
            <<"');">>,
            <<"document.querySelector('#result').appendChild(spanEl);">>
        ],
        [main_thread, async]
    ).

memory_binary() ->
    Value = erlang:memory(binary),
    append_result_span(<<"binary">>, integer_to_list(Value)).

register_once_and_exit() ->
    {ok, _Resource} = emscripten:register_click_callback(<<"#register-once-and-exit">>),
    emscripten:run_script(<<"document.querySelector('#register-once-and-exit').hidden = false">>, [
        main_thread, async
    ]),
    receive
        {emscripten, {click, #{timestamp := Timestamp}}} ->
            append_result_span(<<"timestamp">>, io_lib:format("~.3f", [Timestamp]))
    end.

register_and_loop() ->
    {ok, _Resource} = emscripten:register_click_callback(<<"#register-and-loop">>),
    emscripten:run_script(<<"document.querySelector('#register-and-loop').hidden = false">>, [
        main_thread, async
    ]),
    register_and_loop0().

register_and_loop0() ->
    receive
        {emscripten, {click, #{timestamp := Timestamp}}} ->
            append_result_span(<<"timestamp">>, io_lib:format("~.3f", [Timestamp]))
    end,
    register_and_loop0().
