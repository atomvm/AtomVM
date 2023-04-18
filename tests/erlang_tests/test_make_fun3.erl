%
% This file is part of AtomVM.
%
% Copyright 2023 Illya Petrov <ilya.muromec@gmail.com>
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
-module(test_make_fun3).

-export([start/0, maketuple/3, sumtuple/1]).

start() ->
    erlang:garbage_collect(self()),
    X = make_config(1, 2, 3),
    6 = sumtuple(X),
    0.

make_config(A, B, C) ->
    erlang:garbage_collect(self()),

    % With OTP24+, this is implemented with make_fun3 which optimizes allocation.
    X = [
        {fun() -> 0 end, 1, 2, 3, 4},
        % env = 1
        {fun() -> A end, 1, 2, 3, 4},
        % env = 2
        {fun() -> A + B end, 1, 2, 3, 4, 5}
    ],
    maketuple(A, B, C, X).

maketuple(A, B, C) -> {A, B, C}.

maketuple(A, B, C, _D) -> {A, B, C}.

sumtuple({A, B, C}) -> A + B + C.
