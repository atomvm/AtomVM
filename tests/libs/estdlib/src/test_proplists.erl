%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(test_proplists).

-export([test/0]).

test() ->
    ok = test_get_value(),
    ok.

test_get_value() ->
    ok = etest:assert_match(proplists:get_value(a, []), undefined),
    ok = etest:assert_match(proplists:get_value(a, [a]), true),
    ok = etest:assert_match(proplists:get_value(a, [{a, foo}]), foo),

    ok = etest:assert_match(proplists:get_value(a, [], gnu), gnu),
    ok = etest:assert_match(proplists:get_value(a, [a], gnu), true),
    ok = etest:assert_match(proplists:get_value(a, [{a, foo}], gnu), foo),
    ok = etest:assert_match(proplists:get_value(b, [{a, foo}], gnu), gnu),
    ok.
