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
    ok = test_crypto_one_time(),
    ok.

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
    test_hash(
        <<222, 118, 195, 229, 103, 252, 169, 210, 70, 245, 248, 211, 178, 231, 4, 163, 140, 60, 94,
            37, 137, 136, 171, 82, 95, 148, 29, 184>>,
        sha224,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    test_hash(
        <<195, 171, 143, 241, 55, 32, 232, 173, 144, 71, 221, 57, 70, 107, 60, 137, 116, 229, 146,
            194, 250, 56, 61, 74, 57, 96, 113, 76, 174, 240, 196, 242>>,
        sha256,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    test_hash(
        <<60, 156, 48, 217, 246, 101, 231, 77, 81, 92, 132, 41, 96, 212, 164, 81, 200, 58, 1, 37,
            253, 61, 231, 57, 45, 123, 55, 35, 26, 241, 12, 114, 234, 88, 174, 223, 205, 248, 154,
            87, 101, 191, 144, 42, 249, 62, 207, 6>>,
        sha384,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
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
    ok = expect(badarg, fun() -> crypto:hash(sha, ["not", "a", "byte", -1]) end),
    ok = expect(badarg, fun() -> crypto:hash(sha, ["not", "a", "byte", 32768]) end),
    ok = expect(badarg, fun() -> crypto:hash(sha, ["not", "a", "byte", 34359738367]) end),

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

test_crypto_one_time() ->
    <<50, 136, 204, 108, 55>> = crypto:crypto_one_time(
        aes_128_ctr,
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
        <<1:120, 5:8>>,
        <<"Hello">>,
        true
    ),

    % No padding is used so output will be truncated
    <<231, 108, 83, 104, 188, 131, 182, 65, 2, 128, 78, 162, 210, 149, 128, 248>> = crypto:crypto_one_time(
        aes_128_cbc, <<1:128>>, <<0:128>>, <<"First bytesSecond bytes">>, true
    ),

    % Previous test, but with iolist
    <<231, 108, 83, 104, 188, 131, 182, 65, 2, 128, 78, 162, 210, 149, 128, 248>> = crypto:crypto_one_time(
        aes_128_cbc,
        <<1:128>>,
        <<0:128>>,
        [<<"First ">>, $b, $y, <<"tes">>, [<<"Second ">>, <<"bytes">>]],
        true
    ),

    <<117, 16, 152, 235, 154, 151, 58, 120, 64, 65, 33, 201, 242, 240, 41, 177>> = crypto:crypto_one_time(
        aes_256_cbc, <<5, 1:240, 7>>, <<1:120, 7:8>>, <<"Test">>, [
            {encrypt, true}, {padding, pkcs_padding}
        ]
    ),

    <<218, 189, 18, 174, 31, 123, 37, 254, 119, 34, 71, 35, 219, 0, 185, 153>> = crypto:crypto_one_time(
        aes_256_ecb, <<5, 1:240, 7>>, <<"Test1234567890ab">>, [{encrypt, true}]
    ),

    {badarg, {File, Line}, Message} = get_error(fun() ->
        crypto:crypto_one_time(bad, <<1:128>>, <<0:128>>, <<"Test">>, true)
    end),
    true = is_list(File),
    true = is_integer(Line),
    true = is_list(Message),

    % Invalid key
    {badarg, {File1, Line1}, Message1} = get_error(fun() ->
        crypto:crypto_one_time(
            aes_128_ctr,
            <<>>,
            <<1:120, 5:8>>,
            <<"Hello">>,
            true
        )
    end),
    true = is_list(File1),
    true = is_integer(Line1),
    true = is_list(Message1),

    % Invalid IV
    {badarg, {File2, Line2}, Message2} = get_error(fun() ->
        crypto:crypto_one_time(
            aes_128_ctr,
            <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            <<5>>,
            <<"Hello">>,
            true
        )
    end),
    true = is_list(File2),
    true = is_integer(Line2),
    true = is_list(Message2),

    % Invalid opts
    badarg = get_error(fun() ->
        crypto:crypto_one_time(
            aes_128_ctr,
            <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            <<1:120, 5:8>>,
            <<"Hello">>,
            {bad}
        )
    end),

    ok.

get_error(F) ->
    try
        F(),
        fail
    catch
        _:E -> E
    end.
