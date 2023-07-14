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

-module(test_call).
-export([start/0]).

start() ->
    register(main, self()),
    emscripten:run_script(
        [
            <<
                "window.document.getElementById('call-button').onclick = async () => {"
                "const target = window.document.getElementById('target-input').value;"
                "const message = window.document.getElementById('message-input').value;"
                "try {"
                "const result = await Module.call(target, message);"
                "window.document.getElementById('result').innerHTML = result;"
                "} catch (error) {"
                "window.document.getElementById('error').innerHTML = error;"
                "}"
                "};"
            >>,
            <<"window.document.getElementById('call-button').disabled = false;">>
        ],
        [main_thread, async]
    ),
    loop().

loop() ->
    % make sure refc promise is garbage collected
    erlang:garbage_collect(),
    receive
        {emscripten, {call, _Promise, <<"ignore">>}} ->
            loop();
        {emscripten, {call, Promise, <<"resolve-42">>}} ->
            emscripten:promise_resolve(Promise, 42),
            loop();
        {emscripten, {call, Promise, <<"resolve-ok">>}} ->
            emscripten:promise_resolve(Promise, <<"ok">>),
            loop();
        {emscripten, {call, Promise, <<"reject-42">>}} ->
            emscripten:promise_reject(Promise, 42),
            loop();
        {emscripten, {call, Promise, <<"reject-ok">>}} ->
            emscripten:promise_reject(Promise, <<"ok">>),
            loop()
    end.
