%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(floatext).

-export([start/0]).

start() ->
    true = test_reverse(3.14159265, <<131, 70, 64, 9, 33, 251, 83, 200, 212, 241>>),
    true = test_reverse(0.0, <<131, 70, 0, 0, 0, 0, 0, 0, 0, 0>>),
    true = test_reverse(negate(0.0), <<131, 70, 128, 0, 0, 0, 0, 0, 0, 0>>),
    true = test_reverse(negate(3.14159265), <<131, 70, 192, 9, 33, 251, 83, 200, 212, 241>>),
    0.

test_reverse(T, Interop) ->
    Bin = erlang:term_to_binary(T),
    Bin = Interop,
    {X, Used} = erlang:binary_to_term(Bin, [used]),
    Used = erlang:byte_size(Bin),
    X =:= T.

negate(X) ->
    -1 * X.
