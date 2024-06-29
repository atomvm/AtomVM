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

-module(test_eunit).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

assert_true_test() ->
    ?assert(true).

generator_test_() ->
    ?_assert(true).

generator_list_test_() ->
    [
        ?_assert(true),
        ?_assert(true)
    ].

simple_spawn_test_() ->
    {
        spawn,
        ?_assert(true)
    }.

spawn_common_process_test_() ->
    {
        spawn,
        {inorder, [
            ?_test(begin
                put(val, 42)
            end),
            ?_assertEqual(42, get(val))
        ]}
    }.

setup_test_() ->
    {
        setup,
        fun() ->
            register(setup_process, self())
        end,
        ?_test(begin
            ?assertNotEqual(undefined, whereis(setup_process))
        end)
    }.
