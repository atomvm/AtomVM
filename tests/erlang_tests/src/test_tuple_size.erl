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

-module(test_tuple_size).

-export([start/0]).

start() ->
    tuple_size(test_t(0, a)) + tuple_size(test_t(1, b)) + tuple_size(test_t(2, c)) +
        tuple_size(test_t(3, d)).

test_t(0, _V) ->
    {};
test_t(1, V) ->
    {V};
test_t(2, V) ->
    {V, V};
test_t(3, V) ->
    {V, V, V}.
