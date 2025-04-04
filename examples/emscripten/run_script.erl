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

-module(run_script).
-export([start/0]).

start() ->
    {CounterPid, CounterMonitor} = spawn_opt(fun() -> counter_loop() end, [monitor]),
    emscripten:run_script(
        <<
            "console.log('hello from Erlang in main thread');"
            "alert('hello from Erlang in main thread')"
        >>,
        [main_thread]
    ),
    CounterPid ! {self(), terminate},
    receive
        {CounterPid, CounterValue, DeltaMS} ->
            emscripten:run_script(
                [
                    <<"window.document.getElementById('demo-counter').innerHTML = '">>,
                    integer_to_list(CounterValue),
                    <<" and it ran for ">>,
                    integer_to_list(DeltaMS),
                    <<" ms.'">>
                ],
                [
                    main_thread, async
                ]
            )
    end,
    % Ensure process terminated
    receive
        {'DOWN', CounterMonitor, process, CounterPid, normal} -> ok
    end,
    emscripten:run_script(
        <<
            "console.log('hello from Erlang in main thread async');"
            "alert('hello from Erlang in main thread async')"
        >>,
        [
            main_thread, async
        ]
    ),
    emscripten:run_script(<<"console.log('hello from Erlang in worker thread')">>).

counter_loop() ->
    StartTimeMS = erlang:system_time(millisecond),
    counter_loop(0, StartTimeMS).

counter_loop(Counter, StartTimeMS) ->
    receive
        {Caller, terminate} ->
            EndTimeMS = erlang:system_time(millisecond),
            DeltaMS = EndTimeMS - StartTimeMS,
            Caller ! {self(), Counter, DeltaMS}
    after 0 ->
        counter_loop(Counter + 1, StartTimeMS)
    end.
