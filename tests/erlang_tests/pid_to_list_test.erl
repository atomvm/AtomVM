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

-module(pid_to_list_test).

-export([start/0, check/1, g/1]).

start() ->
    [A | T] = g(self()),
    A + check(T).

check([$>]) ->
    3;
check([$. | T]) ->
    check(T);
check([H | T]) when H >= $0 andalso H =< $9 ->
    check(T).

g(X) ->
    try erlang:pid_to_list(X) of
        Res -> Res
    catch
        error:badarg -> 0;
        _:_ -> 10
    end.
