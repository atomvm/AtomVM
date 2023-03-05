%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
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

-module(small_big_ext).

-export([start/0]).

-define(INT64_MAX, 9223372036854775807).
-define(INT64_MIN, -9223372036854775808).

start() ->
    true = test_reverse(pow(32), <<131, 110, 5, 0, 0, 0, 0, 0, 1>>),
    true = test_reverse(pow(32) + 1, <<131, 110, 5, 0, 1, 0, 0, 0, 1>>),
    true = test_reverse(pow(60) + 1, <<131, 110, 8, 0, 1, 0, 0, 0, 0, 0, 0, 16>>),
    true = test_reverse(-1 * (pow(60) + 1), <<131, 110, 8, 1, 1, 0, 0, 0, 0, 0, 0, 16>>),
    true = test_reverse(?INT64_MAX, <<131, 110, 8, 0, 255, 255, 255, 255, 255, 255, 255, 127>>),
    true = test_reverse(?INT64_MIN, <<131, 110, 8, 1, 0, 0, 0, 0, 0, 0, 0, 128>>),

    %% we can still decode really small encodings
    1 = erlang:binary_to_term(<<131, 110, 1, 0, 1>>),
    -1 = erlang:binary_to_term(<<131, 110, 1, 1, 1>>),
    -1 = erlang:binary_to_term(<<131, 110, 1, 3, 1>>),
    -1 = erlang:binary_to_term(<<131, 110, 1, -1, 1>>),

    %% 0-length encodings are legal
    0 = erlang:binary_to_term(<<131, 110, 0, 0>>),
    0 = erlang:binary_to_term(<<131, 110, 0, 1>>),

    %% edge cases around INT32 min/max boundaries
    true = test_reverse(pow(31) - 1, <<131, 98, 127, 255, 255, 255>>),
    true = test_reverse(pow(31), <<131, 110, 4, 0, 0, 0, 0, 128>>),
    true = test_reverse(-pow(31), <<131, 98, 128, 0, 0, 0>>),
    true = test_reverse(-pow(31) - 1, <<131, 110, 4, 1, 1, 0, 0, 128>>),

    %% missing sign
    ok = assert_badarg(
        fun() ->
            erlang:binary_to_term(<<131, 110, 0>>)
        end
    ),

    %% we currently only support up to 64 bit (signed) integers
    case erlang:system_info(machine) of
        "BEAM" ->
            test_reverse(
                pow(63) + 1, <<131, 110, 8, 0, 1, 0, 0, 0, 0, 0, 0, 128>>
            ),
            test_reverse(
                -(pow(63) + 2), <<131, 110, 8, 1, 2, 0, 0, 0, 0, 0, 0, 128>>
            ),
            test_reverse(
                pow(128), <<131, 110, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>
            );
        _ ->
            ok = assert_badarg(
                fun() ->
                    erlang:binary_to_term(
                        <<131, 110, 8, 0, 1, 0, 0, 0, 0, 0, 0, 128>>
                    )
                end
            ),
            ok = assert_badarg(
                fun() ->
                    erlang:binary_to_term(
                        <<131, 110, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>
                    )
                end
            ),
            ok = assert_badarg(
                fun() ->
                    erlang:binary_to_term(
                        <<131, 110, 8, 1, 2, 0, 0, 0, 0, 0, 0, 128>>
                    )
                end
            )
    end,
    0.

test_reverse(T, Interop) ->
    Bin = erlang:term_to_binary(T),
    Bin = Interop,
    {X, Used} = erlang:binary_to_term(Bin, [used]),
    Used = erlang:byte_size(Bin),
    X =:= T.

assert_badarg(F) ->
    try
        R = F(),
        {fail_no_ex, R}
    catch
        error:badarg -> ok
    end.

pow(0) ->
    1;
pow(X) ->
    Y = pow(X - 1),
    Y bsl 1.
