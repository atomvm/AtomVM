%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_init).
-export([start/0, test/0]).

start() ->
    test().

test() ->
    %% On AtomVM no emulator flags exist; the test harness may pass its own
    %% plain arguments on BEAM, so only the shape of the results is checked.
    Arguments = init:get_arguments(),
    true = is_list(Arguments),
    ok = check_flags(Arguments),
    PlainArguments = init:get_plain_arguments(),
    true = lists:all(fun(A) -> is_list(A) end, PlainArguments),
    error = init:get_argument(no_such_flag_atomvm_test),
    ok.

check_flags([]) ->
    ok;
check_flags([{Flag, Values} | T]) when is_atom(Flag) ->
    true = lists:all(fun(V) -> is_list(V) end, Values),
    check_flags(T).
