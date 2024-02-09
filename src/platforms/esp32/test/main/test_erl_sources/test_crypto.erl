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
    ok = test_available_ciphers(),
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

test_available_ciphers() ->
    <<171, 29, 253, 3, 110, 255, 225, 168, 40, 2, 92, 101, 18, 22, 104, 89>> =
        crypto:crypto_one_time(aes_128_cbc, <<1:128>>, <<2:128>>, <<3:128>>, false),
    %error:
    % <<172, 173, 71, 170, 66, 92, 132, 117, 22, 33, 191, 18, 17, 207, 171, 238>> =
    %     crypto:crypto_one_time(aes_192_cbc, <<1:192>>, <<2:128>>, <<3:128>>, false),
    <<33, 51, 81, 23, 26, 72, 178, 26, 115, 82, 208, 26, 225, 24, 76, 245>> =
        crypto:crypto_one_time(aes_256_cbc, <<1:256>>, <<2:128>>, <<3:128>>, false),
    <<149, 146, 215, 117, 124, 68, 24, 44, 51, 164, 46, 233, 81, 71, 162, 220>> =
        crypto:crypto_one_time(aes_128_ctr, <<1:128>>, <<2:128>>, <<3:128>>, false),
    %error:
    % <<220, 113, 165, 81, 21, 142, 16, 189, 39, 210, 3, 12, 128, 110, 174, 43>> =
    %     crypto:crypto_one_time(aes_192_ctr, <<1:192>>, <<2:128>>, <<3:128>>, false),

    <<89, 151, 109, 175, 200, 98, 75, 207, 80, 33, 65, 131, 194, 29, 141, 242>> =
        crypto:crypto_one_time(aes_256_ctr, <<1:256>>, <<2:128>>, <<3:128>>, false),
    <<149, 146, 215, 117, 124, 68, 24, 44, 51, 164, 46, 233, 81, 71, 162, 220>> =
        crypto:crypto_one_time(aes_128_cfb128, <<1:128>>, <<2:128>>, <<3:128>>, false),
    %error:
    % <<220, 113, 165, 81, 21, 142, 16, 189, 39, 210, 3, 12, 128, 110, 174, 43>> =
    %     crypto:crypto_one_time(aes_192_cfb128, <<1:192>>, <<2:128>>, <<3:128>>, false),
    <<89, 151, 109, 175, 200, 98, 75, 207, 80, 33, 65, 131, 194, 29, 141, 242>> =
        crypto:crypto_one_time(aes_256_cfb128, <<1:256>>, <<2:128>>, <<3:128>>, false),
    <<51, 126, 5, 238, 121, 110, 153, 245, 229, 187, 6, 58, 119, 97, 242, 197>> =
        crypto:crypto_one_time(aes_128_ecb, <<1:128>>, <<2:128>>, false),
    %error:
    % <<209, 55, 221, 80, 157, 38, 71, 63, 77, 135, 199, 107, 73, 45, 41, 120>> =
    %     crypto:crypto_one_time(aes_192_ecb, <<1:192>>, <<2:128>>, false),
    % <<209, 55, 221, 80, 157, 38, 71, 63, 77, 135, 199, 107, 73, 45, 41, 120>> =
    %     crypto:crypto_one_time(aes_192_ecb, <<1:192>>, <<2:128>>, false),
    <<9, 134, 59, 77, 138, 44, 15, 97, 69, 171, 187, 23, 29, 143, 25, 227>> =
        crypto:crypto_one_time(aes_256_ecb, <<1:256>>, <<2:128>>, false),
    % Erlang/OTP also allows to call aes_*_ecb with an iv
    <<171, 29, 253, 3, 110, 255, 225, 168, 40, 2, 92, 101, 18, 22, 104, 91>> =
        crypto:crypto_one_time(aes_128_ecb, <<1:128>>, <<2:128>>, <<3:128>>, false),
    %error:
    % <<172, 173, 71, 170, 66, 92, 132, 117, 22, 33, 191, 18, 17, 207, 171, 236>> =
    %     crypto:crypto_one_time(aes_192_ecb, <<1:192>>, <<2:128>>, <<3:128>>, false),
    <<33, 51, 81, 23, 26, 72, 178, 26, 115, 82, 208, 26, 225, 24, 76, 247>> =
        crypto:crypto_one_time(aes_256_ecb, <<1:256>>, <<2:128>>, <<3:128>>, false),
    ok.

get_error(F) ->
    try
        F(),
        fail
    catch
        _:E -> E
    end.
