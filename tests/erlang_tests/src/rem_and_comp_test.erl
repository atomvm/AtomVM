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

-module(rem_and_comp_test).

-export([start/0, rem_func/2]).

start() ->
    one_if_true(rem_func(5, 3) =:= 2) +
        one_if_true(rem_func(0, 1) =:= 0) +
        one_if_true(rem_func(0, 1) =:= 0) +
        one_if_true(rem_func(5, 5) =:= 7) +
        one_if_true(rem_func(0, 1) =/= 1) +
        one_if_true(rem_func(0, 1) =/= 0) +
        one_if_true(rem_func(0, 2) =/= 0).

rem_func(A, B) ->
    A rem B.

one_if_true(true) ->
    1;
one_if_true(false) ->
    0.
