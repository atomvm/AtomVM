%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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

-module(test_timestamp).

-export([start/0]).

start() ->
    {M1, S1, Mic1} = erlang:timestamp(),
    receive
    after 100 -> ok
    end,
    {M2, S2, Mic2} = erlang:timestamp(),
    case leq({M1, S1, Mic1}, {M2, S2, Mic2}) of
        true ->
            1;
        false ->
            0
    end.

leq({M1, S1, Mic1}, {M1, S1, Mic1}) ->
    true;
leq({M1, S1, Mic1}, {M1, S1, Mic2}) when Mic1 < Mic2 ->
    true;
leq({M1, S1, _}, {M1, S2, _}) when S1 < S2 ->
    true;
leq({M1, _, _}, {M2, _, _}) when M1 < M2 ->
    true;
leq(_, _) ->
    false.
