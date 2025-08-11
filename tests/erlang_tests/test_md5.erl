%
% This file is part of AtomVM.
%
% Copyright 2025 Franciszek Kubis <franciszek.kubis@swmansion.com>
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

-module(test_md5).
-define(MD5_BIN, <<62, 37, 150, 10, 121, 219, 198, 155, 103, 76, 212, 236, 103, 167, 44, 98>>).

-export([start/0]).

start() ->
    ok = test_binary(),
    ok = test_iolist(),
    0.

test_binary() ->
     ?MD5_BIN = erlang:md5(<<"Hello world">>),
    ok.

test_iolist() ->
    ?MD5_BIN = erlang:md5("Hello world"),
    ?MD5_BIN = erlang:md5([[[[72, 101, 108, 108], 111], 32], "world"]),
    ok.
