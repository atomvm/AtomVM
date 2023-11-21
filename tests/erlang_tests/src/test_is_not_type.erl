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

-module(test_is_not_type).

-export([start/0, bool_to_integer/2, eval/6]).

start() ->
    eval({}, make_ref(), [], 10, <<"hello">>, hello).

bool_to_integer(true, _I) ->
    0;
bool_to_integer(false, I) ->
    I.

eval(A, B, C, D, E, F) ->
    bool_to_integer(is_atom(A), 1) + bool_to_integer(is_binary(B), 2) +
        bool_to_integer(is_integer(C), 4) +
        bool_to_integer(is_list(D), 8) + bool_to_integer(is_reference(E), 16) +
        bool_to_integer(is_tuple(F), 32) +
        bool_to_integer(is_number(F), 64) + bool_to_integer(is_pid(F), 128).
