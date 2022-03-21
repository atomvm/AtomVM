%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(try_error2_nif).

-export([start/0, f/1, g/1, h/1, factorial/1]).

start() ->
    f(g(factorial(3))).

f({_V1, V2, V3}) ->
    try h(V2) of
        AnyVal -> AnyVal * V3
    catch
        error:N when is_integer(N) ->
            N + 1;
        error:_ ->
            0;
        _:_ ->
            -1
    end.

g(I) ->
    {I, I * 2, I * 4}.

h(I) when I > 4 ->
    error(I, [a, b, c]);
h(I) ->
    I.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).
