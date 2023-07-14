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
-module(test_crypto).

-export([start/0]).

start() ->
    ok = test_hash(),
    0.

test_hash() ->
    test_hash(
        <<56, 88, 246, 34, 48, 172, 60, 145, 95, 48, 12, 102, 67, 18, 198, 63>>,
        md5,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    test_hash(
        <<136, 67, 215, 249, 36, 22, 33, 29, 233, 235, 185, 99, 255, 76, 226, 129, 37, 147, 40,
            120>>,
        sha,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    % test_hash(
    %     <<222, 118, 195, 229, 103, 252, 169, 210, 70, 245, 248, 211, 178, 231, 4, 163, 140, 60, 94,
    %         37, 137, 136, 171, 82, 95, 148, 29, 184>>,
    %     sha224,
    %     [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    % ),
    test_hash(
        <<195, 171, 143, 241, 55, 32, 232, 173, 144, 71, 221, 57, 70, 107, 60, 137, 116, 229, 146,
            194, 250, 56, 61, 74, 57, 96, 113, 76, 174, 240, 196, 242>>,
        sha256,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    % test_hash(
    %     <<60, 156, 48, 217, 246, 101, 231, 77, 81, 92, 132, 41, 96, 212, 164, 81, 200, 58, 1, 37,
    %         253, 61, 231, 57, 45, 123, 55, 35, 26, 241, 12, 114, 234, 88, 174, 223, 205, 248, 154,
    %         87, 101, 191, 144, 42, 249, 62, 207, 6>>,
    %     sha384,
    %     [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    % ),
    test_hash(
        <<10, 80, 38, 30, 189, 26, 57, 15, 237, 43, 243, 38, 242, 103, 60, 20, 85, 130, 166, 52, 45,
            82, 50, 4, 151, 61, 2, 25, 51, 127, 129, 97, 106, 128, 105, 176, 18, 88, 124, 245, 99,
            95, 105, 37, 241, 181, 108, 54, 2, 48, 193, 155, 39, 53, 0, 238, 1, 62, 3, 6, 1, 191,
            36, 37>>,
        sha512,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),

    ok = expect(badarg, fun() -> crypto:hash(not_a_type, <<"foobar">>) end),
    ok = expect(badarg, fun() -> crypto:hash(sha, not_a_binary) end),
    ok = expect(badarg, fun() -> crypto:hash(sha, ["not", "an", [iolist]]) end),

    ok.

test_hash(Expect, Type, DataList) ->
    [Expect = crypto:hash(Type, Data) || Data <- DataList].

expect(Error, F) ->
    try
        F(),
        fail
    catch
        _:E ->
            %% NB. Error types vary across OTP implementations
            case erlang:system_info(machine) of
                "BEAM" ->
                    ok;
                _ when Error == E ->
                    ok
            end
    end.
