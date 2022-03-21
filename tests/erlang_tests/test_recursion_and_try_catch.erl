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

-module(test_recursion_and_try_catch).

-export([start/0, id/1, f/1, h/1, g/1]).

start() ->
    try f(10) of
        AnyNum -> AnyNum
    catch
        _:_ -> 0
    end.

id(X) ->
    X.

f(A) when A < 1 ->
    10 div A;
f(A) when A > 3 andalso A < 6 ->
    f(A - 1) * A;
f(A) ->
    Tmp = g(A),
    try id(f(id(A - 1))) * A + h(Tmp) of
        AnyNum -> AnyNum
    catch
        error:badarith -> Tmp;
        _:_ -> -1024
    end.

g(A) ->
    A rem 4.

h(A) when A < 4 ->
    0;
h(A) when A > 4 ->
    A.
