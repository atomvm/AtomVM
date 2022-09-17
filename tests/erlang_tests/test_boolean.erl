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

-module(test_boolean).

-export([start/0]).

start() ->
    ok = test_boolean_call(),
    ok = test_boolean_guard(),
    0.

test_boolean_call() ->
    true = is_boolean(true),
    true = is_boolean(false),
    false = is_boolean(0),
    false = is_boolean(1),
    false = is_boolean(1342),
    false = is_boolean("true"),
    false = is_boolean(<<"true">>),
    false = is_boolean([]),
    false = is_boolean([foo, bar]),
    false = is_boolean({}),
    false = is_boolean({foo, bar}),
    false = is_boolean(#{}),
    false = is_boolean(#{foo => bar}),
    ok.

test_boolean_guard() ->
    true = check_boolean(true),
    true = check_boolean(false),
    false = check_boolean(0),
    false = check_boolean(1),
    false = check_boolean(1342),
    false = check_boolean("true"),
    false = check_boolean(<<"true">>),
    false = check_boolean([]),
    false = check_boolean([foo, bar]),
    false = check_boolean({}),
    false = check_boolean({foo, bar}),
    false = check_boolean(#{}),
    false = check_boolean(#{foo => bar}),
    ok.

check_boolean(B) when is_boolean(B) ->
    true;
check_boolean(_) ->
    false.
