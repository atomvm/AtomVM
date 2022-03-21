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

-module(long_atoms).

-export([start/0, make_long_list/1, f/1]).

start() ->
    A = "aaaaaaaaaaaaaaaaa",
    L = make_long_list(A),
    f(L ++ A) + f(L).

make_long_list(A) ->
    A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A.

f(L) ->
    try erlang:list_to_atom(L) of
        At when is_atom(At) -> 1;
        _At -> 2
    catch
        error:system_limit -> 3;
        _:_ -> 4
    end.
