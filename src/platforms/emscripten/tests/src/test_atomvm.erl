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

-module(test_atomvm).
-export([start/0]).

start() ->
    erlang:display({?MODULE, ?LINE}),
    Platform = atomvm:platform(),
    erlang:display({?MODULE, ?LINE}),
    Random = atomvm:random(),
    erlang:display({?MODULE, ?LINE}),
    PI = pi_by_random(),
    PIError = abs((PI - 3.141592653589793) / 3.141592653589793),
    PIErrorThreshold = PIError < 0.01,
    erlang:display({?MODULE, ?LINE}),
    emscripten:run_script(
        [
            <<"document.querySelector('#platform').append('">>,
            atom_to_list(Platform),
            <<"');">>,
            <<"document.querySelector('#random').append('">>,
            integer_to_list(Random),
            <<"');">>,
            <<"document.querySelector('#pi').append('">>,
            float_to_list(PI),
            <<"');">>,
            <<"document.querySelector('#pierror').append('">>,
            atom_to_list(PIErrorThreshold),
            <<"');">>
        ],
        [main_thread, async]
    ),
    loop().

loop() ->
    receive
    after infinity -> ok
    end.

pi_by_random() ->
    pi_by_random(100000, 0, 0).

pi_by_random(0, AccHit, AccTotal) ->
    4 * AccHit / AccTotal;
pi_by_random(N, AccHit, AccTotal) ->
    X = atomvm:random(),
    Y = atomvm:random(),
    SqDistance = (X - 16#80000000) * (X - 16#80000000) + (Y - 16#80000000) * (Y - 16#80000000),
    NewAccHit =
        case SqDistance > (16#80000000 * 16#80000000) of
            true -> AccHit;
            false -> AccHit + 1
        end,
    pi_by_random(N - 1, NewAccHit, AccTotal + 1).
