%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(copy_terms3).

-export([start/0, count_nestings/1]).

start() ->
    Pid = spawn_opt(fun loop/0, []),
    Pid ! {self(), {{{{{{}}}}}}},
    Count =
        receive
            Any -> Any
        end,
    Pid ! terminate,
    Count.

loop() ->
    case handle_request() of
        terminate ->
            terminate;
        ok ->
            loop()
    end.

handle_request() ->
    receive
        {Pid, ATuple} when is_tuple(ATuple) ->
            Pid ! count_nestings(ATuple),
            ok;
        terminate ->
            terminate
    end.

count_nestings(T) ->
    count_nestings(T, 0).

count_nestings({}, Acc) ->
    Acc;
count_nestings({T}, Acc) ->
    count_nestings(T, Acc + 1).
