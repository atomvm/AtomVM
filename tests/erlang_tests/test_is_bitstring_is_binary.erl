%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_is_bitstring_is_binary).

-export([start/0, id/1]).

start() ->
    test_is_bitstring(),
    test_is_binary(),
    0.

id(X) -> X.

test_is_bitstring() ->
    true = is_bitstring(id(<<"hello">>)),
    % bitstrings are currently unsupported
    %   true = is_bitstring(id(<<1:1>>)),
    true = is_bitstring(id(<<>>)),
    false = is_bitstring(id(binary)),
    false = is_bitstring(id("hello")),
    false = is_bitstring(id(42)),
    ok.

test_is_binary() ->
    true = is_binary(id(<<"hello">>)),
    % bitstrings are currently unsupported
    %   false = is_binary(id(<<1:1>>)),
    true = is_binary(id(<<>>)),
    false = is_binary(id(binary)),
    false = is_binary(id("hello")),
    false = is_binary(id(42)),
    ok.
