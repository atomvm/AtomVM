%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(ref_to_list_test).

-export([start/0]).

start() ->
    ok = test_loop(10),
    0.

test_loop(0) ->
    ok;
test_loop(N) when N > 0 ->
    RefStr = ref_to_list(make_ref()),
    [0 | T] = parse_ref(RefStr),
    ok = check_words(T),
    test_loop(N - 1).

parse_ref("#Ref<" ++ T) ->
    parse_ref0(T, 0, []).

parse_ref0(">", N, Acc) ->
    Acc ++ [N];
parse_ref0([$. | T], N, Acc) ->
    parse_ref0(T, 0, Acc ++ [N]);
parse_ref0([H | T], N, Acc) when H >= $0 andalso H =< $9 ->
    parse_ref0(T, N * 10 + (H - $0), Acc).

check_words([]) ->
    ok;
check_words([W | T]) when is_integer(W) andalso W >= 0 andalso W < 1 bsl 32 ->
    check_words(T).
