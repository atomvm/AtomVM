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
-module(call_cast).
-export([start/0]).

start() ->
    % Register as main
    register(main, self()),
    % Add a handler to send a message to self.
    % For cypress testing, buttons are disabled until the handler is installed
    % Nevertheless, there seems to be a race condition issue where Cypress
    % clicks but the handler isn't called. It doesn't happen with the
    % console.log call in the click handler.
    emscripten:run_script(
        [
            <<"window.document.getElementById('call-button').onclick = async () => { console.log('clicked call'); const result = await Module.call('main', 'click'); window.document.getElementById('call-counter').innerHTML = result; };">>,
            <<"window.document.getElementById('call-button').disabled = false;">>,
            <<"window.document.getElementById('cast-button').onclick = () => { console.log('clicked cast'); Module.cast('main', 'click'); };">>,
            <<"window.document.getElementById('cast-button').disabled = false;">>
        ],
        [main_thread, async]
    ),
    loop({0, 0}).

loop({CastCounter, CallCounter}) ->
    NewState =
        receive
            {emscripten, {cast, <<"click">>}} ->
                emscripten:run_script(
                    [
                        <<"window.document.getElementById('cast-counter').innerHTML = '">>,
                        integer_to_list(CastCounter + 1),
                        <<"';">>
                    ],
                    [main_thread, async]
                ),
                {CastCounter + 1, CallCounter};
            {emscripten, {call, Promise, <<"click">>}} ->
                emscripten:promise_resolve(Promise, CallCounter + 1),
                {CastCounter, CallCounter + 1}
        end,
    loop(NewState).
