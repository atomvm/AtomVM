%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(test_funs8).

-export([start/0, sumeach/3, g/1, id/1]).

start() ->
    C = g(four),
    sumeach([1, 2, 3, 4], 22, fun(V) -> V * C end).

sumeach([H | T], F, Acc) ->
    try F(H, g(zero)) of
        Result -> sumeach(F, T, Result + Acc)
    catch
        error:{badarity, _f} ->
            1000;
        error:badarity ->
            1000;
        error:{badfun, N} ->
            N * 1000;
        _:_ ->
            3000
    end;
sumeach([], _F, Acc) ->
    Acc.

g(zero) ->
    0;
g(four) ->
    4;
g(five) ->
    5.

id(I) ->
    I.
