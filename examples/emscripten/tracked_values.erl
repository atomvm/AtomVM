%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(tracked_values).
-export([start/0]).

start() ->
    % A DOM node cannot be sent to Erlang, but a handle to it can: the script
    % evaluates to an array, so this tracks one value and gets one handle.
    {ok, [Box]} = emscripten:run_script_tracked(
        <<"[window.document.getElementById('demo-box')]">>
    ),

    % The handle addresses the node in later scripts through the key it
    % carries, without ever serializing the node itself.
    ok = paint(Box, <<"lightgreen">>),
    ok = show(<<"tracked the box and painted it green">>),

    % Values come back as strings, so read a property rather than the node.
    % Trimmed, because it goes back into a script as a string literal.
    {ok, [Text]} = emscripten:run_script_tracked(
        <<"[window.document.getElementById('demo-box').textContent.trim()]">>
    ),
    [{ok, Content}] = emscripten:get_tracked([Text], value),
    ok = show([<<"the box says: ">>, Content]),

    % The JavaScript value lives exactly as long as the handle: dropping this
    % one and collecting leaves nothing behind on the JavaScript side.
    ok = drop_a_handle(),
    erlang:garbage_collect(),
    ok = show(<<"a dropped handle took its JavaScript value with it">>),

    % Box is still reachable here, so its node is still tracked.
    ok = paint(Box, <<"lightblue">>),
    ok = show(<<"the box is still tracked, and now blue">>),
    loop(Box).

paint(Handle, Color) ->
    [Key] = emscripten:get_tracked([Handle], key),
    ok = emscripten:run_script(
        [
            <<"window.Module.trackedObjectsMap.get(">>,
            integer_to_list(Key),
            <<").style.backgroundColor = '">>,
            Color,
            <<"';">>
        ],
        [main_thread]
    ),
    ok.

% The handle must go out of scope before the collection, so it is made in a
% frame of its own and dropped on return.
drop_a_handle() ->
    {ok, [_]} = emscripten:run_script_tracked(<<"['collected soon']">>),
    ok.

show(Message) ->
    ok = emscripten:run_script(
        [
            <<"window.document.getElementById('demo-log').innerHTML += '<li>">>,
            Message,
            <<"</li>';">>
        ],
        [main_thread]
    ),
    ok.

% Returning from start/0 would tear the runtime down, and with it every
% tracked value.
loop(Handle) ->
    receive
        _Any -> loop(Handle)
    end.
