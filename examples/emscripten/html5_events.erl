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
-module(html5_events).
-export([start/0]).

start() ->
    % Register as main
    register(main, self()),

    Script = [
        [
            <<"const ">>,
            Event,
            <<"Checkbox = document.querySelector('#">>,
            Event,
            <<"-checkbox');">>,
            Event,
            <<"Checkbox.addEventListener('change', (e) => { Module.cast('main', e.target.checked + '-">>,
            Event,
            <<"'); });">>,
            Event,
            <<"Checkbox.disabled = false;">>
        ]
     || Event <- [
            % key
            <<"keypress">>,
            <<"keyup">>,
            <<"keydown">>,
            % mouse
            <<"click">>,
            <<"mousedown">>,
            <<"mouseup">>,
            <<"dblclick">>,
            <<"mousemove">>,
            <<"mouseenter">>,
            <<"mouseleave">>,
            <<"mouseover">>,
            <<"mouseout">>,
            % wheel
            <<"wheel">>,
            % ui
            <<"resize">>,
            <<"scroll">>,
            % focus
            <<"blur">>,
            <<"focus">>,
            <<"focusin">>,
            <<"focusout">>,
            % touch
            <<"touchstart">>,
            <<"touchend">>,
            <<"touchmove">>,
            <<"touchcancel">>
        ]
    ],
    emscripten:run_script(
        Script,
        [main_thread, async]
    ),

    loop(1, []).

escape_js_str(Str) ->
    escape_js_str(Str, []).

escape_js_str([$" | Tail], Acc) ->
    escape_js_str(Tail, ["\\\"" | Acc]);
escape_js_str([C | Tail], Acc) ->
    escape_js_str(Tail, [C | Acc]);
escape_js_str([], Acc) ->
    lists:reverse(Acc).

timestamp_to_iodata(#{timestamp := Timestamp}) ->
    io_lib:format("~.3f", [Timestamp]);
timestamp_to_iodata(_) ->
    "undefined".

insert_event_to_table(EventIndex, Event, EventMap, WithUserData, UserData) ->
    Script0 =
        [
            <<"var tbody = window.document.getElementById('events');">>,
            <<"var row = tbody.insertRow(0);">>,
            <<"row.id = 'event-">>,
            integer_to_list(EventIndex),
            <<"';">>,
            <<"var eventIndexCell = row.insertCell();">>,
            <<"var eventTypeCell = row.insertCell();">>,
            <<"eventTypeCell.classList.add('event-type');">>,
            <<"var eventTimestampCell = row.insertCell();">>,
            <<"var eventMapCell = row.insertCell();">>,
            <<"eventMapCell.classList.add('event-map');">>,
            <<"var eventUserDataCell = row.insertCell();">>,
            <<"eventUserDataCell.classList.add('event-user-data');">>,
            <<"eventIndexCell.append('">>,
            integer_to_list(EventIndex),
            <<"');">>,
            <<"eventTypeCell.append('">>,
            atom_to_list(Event),
            <<"');">>,
            <<"eventTimestampCell.append('">>,
            timestamp_to_iodata(EventMap),
            <<"');">>,
            <<"eventMapCell.append(\"">>,
            escape_js_str(lists:flatten(io_lib:format("~p", [EventMap]))),
            <<"\");">>
        ],
    Script =
        case WithUserData of
            false ->
                Script0;
            true ->
                Script0 ++
                    [
                        <<"eventUserDataCell.append(\"">>,
                        escape_js_str(lists:flatten(io_lib:format("~p", [UserData]))),
                        <<"\");">>
                    ]
        end,
    emscripten:run_script(Script, [main_thread, async]).

register_event(EventBin, Handlers) ->
    FunctionStr = "register_" ++ binary_to_list(EventBin) ++ "_callback",
    FunctionAtom = list_to_atom(FunctionStr),
    {ok, WindowResource} = apply(emscripten, FunctionAtom, [window]),
    {ok, DocumentResource} = apply(emscripten, FunctionAtom, [document, false, document]),
    {ok, InputResource} = apply(emscripten, FunctionAtom, [
        "#input-field",
        [{use_capture, true}, {prevent_default, true}],
        list_to_binary([EventBin, <<" -- input field">>])
    ]),
    [{EventBin, [WindowResource, DocumentResource, InputResource]} | Handlers].

unregister_event(EventBin, Handlers) ->
    FunctionStr = "unregister_" ++ binary_to_list(EventBin) ++ "_callback",
    FunctionAtom = list_to_atom(FunctionStr),
    [
        ok = apply(emscripten, FunctionAtom, [Handler])
     || {Event, EventHandlers} <- Handlers, Event =:= EventBin, Handler <- EventHandlers
    ],
    [Tuple || {Event, _EventHandlers} = Tuple <- Handlers, Event =/= EventBin].

loop(EventIndex, Handlers) ->
    {NewEventIndex, NewHandlers} =
        receive
            {emscripten, {cast, <<"true-", EventBin/binary>>}} ->
                NewHandlers0 = register_event(EventBin, Handlers),
                {EventIndex, NewHandlers0};
            {emscripten, {cast, <<"false-", EventBin/binary>>}} ->
                NewHandlers0 = unregister_event(EventBin, Handlers),
                {EventIndex, NewHandlers0};
            {emscripten, {Event, EventMap}} ->
                insert_event_to_table(EventIndex, Event, EventMap, false, undefined),
                {EventIndex + 1, Handlers};
            {emscripten, {Event, EventMap}, UserData} ->
                insert_event_to_table(EventIndex, Event, EventMap, true, UserData),
                {EventIndex + 1, Handlers}
        end,
    loop(NewEventIndex, NewHandlers).
