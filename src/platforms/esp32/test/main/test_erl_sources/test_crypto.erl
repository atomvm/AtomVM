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
    Sysinfo = erlang:system_info(esp32_chip_info),
    Model =
        if
            is_map(Sysinfo) -> maps:get(model, Sysinfo);
            true -> undefined
        end,
    io:format("test_crypto: starting tests, chip model=~p~n", [Model]),
    io:format("test_crypto: running test_hash~n"),
    ok = test_hash(),
    io:format("test_crypto: test_hash passed~n"),
    io:format("test_crypto: running test_crypto_one_time~n"),
    ok = test_crypto_one_time(),
    io:format("test_crypto: test_crypto_one_time passed~n"),
    io:format("test_crypto: running test_available_ciphers~n"),
    ok = test_available_ciphers(Model),
    io:format("test_crypto: all tests passed~n"),
    ok.

test_hash() ->
    %% NOTE: MD5 may fail on mbedtls 4.x if PSA_WANT_ALG_MD5 is not enabled
    io:format("  test_hash: testing md5~n"),
    test_hash(
        <<56, 88, 246, 34, 48, 172, 60, 145, 95, 48, 12, 102, 67, 18, 198, 63>>,
        md5,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    io:format("  test_hash: testing sha~n"),
    test_hash(
        <<136, 67, 215, 249, 36, 22, 33, 29, 233, 235, 185, 99, 255, 76, 226, 129, 37, 147, 40,
            120>>,
        sha,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    % io:format("  test_hash: testing sha224~n"),
    % test_hash(
    %     <<222, 118, 195, 229, 103, 252, 169, 210, 70, 245, 248, 211, 178, 231, 4, 163, 140, 60, 94,
    %         37, 137, 136, 171, 82, 95, 148, 29, 184>>,
    %     sha224,
    %     [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    % ),
    io:format("  test_hash: testing sha256~n"),
    test_hash(
        <<195, 171, 143, 241, 55, 32, 232, 173, 144, 71, 221, 57, 70, 107, 60, 137, 116, 229, 146,
            194, 250, 56, 61, 74, 57, 96, 113, 76, 174, 240, 196, 242>>,
        sha256,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    io:format("  test_hash: testing sha384~n"),
    test_hash(
        <<60, 156, 48, 217, 246, 101, 231, 77, 81, 92, 132, 41, 96, 212, 164, 81, 200, 58, 1, 37,
            253, 61, 231, 57, 45, 123, 55, 35, 26, 241, 12, 114, 234, 88, 174, 223, 205, 248, 154,
            87, 101, 191, 144, 42, 249, 62, 207, 6>>,
        sha384,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),
    io:format("  test_hash: testing sha512~n"),
    test_hash(
        <<10, 80, 38, 30, 189, 26, 57, 15, 237, 43, 243, 38, 242, 103, 60, 20, 85, 130, 166, 52, 45,
            82, 50, 4, 151, 61, 2, 25, 51, 127, 129, 97, 106, 128, 105, 176, 18, 88, 124, 245, 99,
            95, 105, 37, 241, 181, 108, 54, 2, 48, 193, 155, 39, 53, 0, 238, 1, 62, 3, 6, 1, 191,
            36, 37>>,
        sha512,
        [<<"foobar">>, "foobar", [<<"foo">>, <<"bar">>]]
    ),

    io:format("  test_hash: testing error cases~n"),
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
    io:format("  crypto_one_time: testing aes_128_ctr~n"),
    <<50, 136, 204, 108, 55>> = crypto:crypto_one_time(
        aes_128_ctr,
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
        <<1:120, 5:8>>,
        <<"Hello">>,
        true
    ),

    %% NOTE: CBC mode requires PSA_WANT_ALG_CBC_NO_PADDING on mbedtls 4.x
    io:format("  crypto_one_time: testing aes_128_cbc (no padding)~n"),
    <<231, 108, 83, 104, 188, 131, 182, 65, 2, 128, 78, 162, 210, 149, 128, 248>> = crypto:crypto_one_time(
        aes_128_cbc, <<1:128>>, <<0:128>>, <<"First bytesSecond bytes">>, true
    ),

    io:format("  crypto_one_time: testing aes_128_cbc with iolist~n"),
    <<231, 108, 83, 104, 188, 131, 182, 65, 2, 128, 78, 162, 210, 149, 128, 248>> = crypto:crypto_one_time(
        aes_128_cbc,
        <<1:128>>,
        <<0:128>>,
        [<<"First ">>, $b, $y, <<"tes">>, [<<"Second ">>, <<"bytes">>]],
        true
    ),

    %% NOTE: PKCS padding (CBC_PKCS7) may not be available on all mbedtls 4.x configs
    io:format("  crypto_one_time: testing aes_256_cbc with pkcs_padding~n"),
    <<117, 16, 152, 235, 154, 151, 58, 120, 64, 65, 33, 201, 242, 240, 41, 177>> = crypto:crypto_one_time(
        aes_256_cbc, <<5, 1:240, 7>>, <<1:120, 7:8>>, <<"Test">>, [
            {encrypt, true}, {padding, pkcs_padding}
        ]
    ),

    %% NOTE: ECB mode requires PSA_WANT_ALG_ECB_NO_PADDING on mbedtls 4.x
    io:format("  crypto_one_time: testing aes_256_ecb~n"),
    <<218, 189, 18, 174, 31, 123, 37, 254, 119, 34, 71, 35, 219, 0, 185, 153>> = crypto:crypto_one_time(
        aes_256_ecb, <<5, 1:240, 7>>, <<"Test1234567890ab">>, [{encrypt, true}]
    ),

    io:format("  crypto_one_time: testing error cases~n"),
    {badarg, {File, Line}, Message} = get_error(fun() ->
        crypto:crypto_one_time(bad, <<1:128>>, <<0:128>>, <<"Test">>, true)
    end),
    true = is_list(File),
    true = is_integer(Line),
    true = is_list(Message),

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

    {badarg, {File3, Line3}, Message3} = get_error(fun() ->
        crypto:crypto_one_time(
            aes_128_ctr,
            <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            <<1:120, 5:8>>,
            <<"Hello">>,
            {bad}
        )
    end),
    true = is_list(File3),
    true = is_integer(Line3),
    true = is_list(Message3),

    ok.

test_available_ciphers(esp32) ->
    io:format("  available_ciphers: ALL ciphers (esp32)~n"),
    io:format("  available_ciphers: aes_128_cbc~n"),
    <<171, 29, 253, 3, 110, 255, 225, 168, 40, 2, 92, 101, 18, 22, 104, 89>> =
        crypto:crypto_one_time(aes_128_cbc, <<1:128>>, <<2:128>>, <<3:128>>, false),
    %% NOTE: AES-192 may not be available on mbedtls 4.x unless PSA_WANT_AES_KEY_SIZE_192 is set
    io:format("  available_ciphers: aes_192_cbc~n"),
    <<172, 173, 71, 170, 66, 92, 132, 117, 22, 33, 191, 18, 17, 207, 171, 238>> =
        crypto:crypto_one_time(aes_192_cbc, <<1:192>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_256_cbc~n"),
    <<33, 51, 81, 23, 26, 72, 178, 26, 115, 82, 208, 26, 225, 24, 76, 245>> =
        crypto:crypto_one_time(aes_256_cbc, <<1:256>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_128_ctr~n"),
    <<149, 146, 215, 117, 124, 68, 24, 44, 51, 164, 46, 233, 81, 71, 162, 220>> =
        crypto:crypto_one_time(aes_128_ctr, <<1:128>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_192_ctr~n"),
    <<220, 113, 165, 81, 21, 142, 16, 189, 39, 210, 3, 12, 128, 110, 174, 43>> =
        crypto:crypto_one_time(aes_192_ctr, <<1:192>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_256_ctr~n"),
    <<89, 151, 109, 175, 200, 98, 75, 207, 80, 33, 65, 131, 194, 29, 141, 242>> =
        crypto:crypto_one_time(aes_256_ctr, <<1:256>>, <<2:128>>, <<3:128>>, false),
    %% NOTE: CFB mode (PSA_ALG_CFB) may not be available on all mbedtls 4.x configs
    io:format("  available_ciphers: aes_128_cfb128~n"),
    <<149, 146, 215, 117, 124, 68, 24, 44, 51, 164, 46, 233, 81, 71, 162, 220>> =
        crypto:crypto_one_time(aes_128_cfb128, <<1:128>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_192_cfb128~n"),
    <<220, 113, 165, 81, 21, 142, 16, 189, 39, 210, 3, 12, 128, 110, 174, 43>> =
        crypto:crypto_one_time(aes_192_cfb128, <<1:192>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_256_cfb128~n"),
    <<89, 151, 109, 175, 200, 98, 75, 207, 80, 33, 65, 131, 194, 29, 141, 242>> =
        crypto:crypto_one_time(aes_256_cfb128, <<1:256>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_128_ecb~n"),
    <<51, 126, 5, 238, 121, 110, 153, 245, 229, 187, 6, 58, 119, 97, 242, 197>> =
        crypto:crypto_one_time(aes_128_ecb, <<1:128>>, <<2:128>>, false),
    io:format("  available_ciphers: aes_192_ecb~n"),
    <<209, 55, 221, 80, 157, 38, 71, 63, 77, 135, 199, 107, 73, 45, 41, 120>> =
        crypto:crypto_one_time(aes_192_ecb, <<1:192>>, <<2:128>>, false),
    <<209, 55, 221, 80, 157, 38, 71, 63, 77, 135, 199, 107, 73, 45, 41, 120>> =
        crypto:crypto_one_time(aes_192_ecb, <<1:192>>, <<2:128>>, false),
    io:format("  available_ciphers: aes_256_ecb~n"),
    <<9, 134, 59, 77, 138, 44, 15, 97, 69, 171, 187, 23, 29, 143, 25, 227>> =
        crypto:crypto_one_time(aes_256_ecb, <<1:256>>, <<2:128>>, false),
    io:format("  available_ciphers: aes_*_ecb with iv~n"),
    <<171, 29, 253, 3, 110, 255, 225, 168, 40, 2, 92, 101, 18, 22, 104, 91>> =
        crypto:crypto_one_time(aes_128_ecb, <<1:128>>, <<2:128>>, <<3:128>>, false),
    <<172, 173, 71, 170, 66, 92, 132, 117, 22, 33, 191, 18, 17, 207, 171, 236>> =
        crypto:crypto_one_time(aes_192_ecb, <<1:192>>, <<2:128>>, <<3:128>>, false),
    <<33, 51, 81, 23, 26, 72, 178, 26, 115, 82, 208, 26, 225, 24, 76, 247>> =
        crypto:crypto_one_time(aes_256_ecb, <<1:256>>, <<2:128>>, <<3:128>>, false),
    ok;
test_available_ciphers(_Model) ->
    %% Leave out AES 192 ciphers for non-esp32 platforms
    io:format("  available_ciphers: WITHOUT AES_192 ciphers~n"),
    io:format("  available_ciphers: aes_128_cbc~n"),
    <<171, 29, 253, 3, 110, 255, 225, 168, 40, 2, 92, 101, 18, 22, 104, 89>> =
        crypto:crypto_one_time(aes_128_cbc, <<1:128>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_256_cbc~n"),
    <<33, 51, 81, 23, 26, 72, 178, 26, 115, 82, 208, 26, 225, 24, 76, 245>> =
        crypto:crypto_one_time(aes_256_cbc, <<1:256>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_128_ctr~n"),
    <<149, 146, 215, 117, 124, 68, 24, 44, 51, 164, 46, 233, 81, 71, 162, 220>> =
        crypto:crypto_one_time(aes_128_ctr, <<1:128>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_256_ctr~n"),
    <<89, 151, 109, 175, 200, 98, 75, 207, 80, 33, 65, 131, 194, 29, 141, 242>> =
        crypto:crypto_one_time(aes_256_ctr, <<1:256>>, <<2:128>>, <<3:128>>, false),
    %% NOTE: CFB mode (PSA_ALG_CFB) may not be available on all mbedtls 4.x configs
    io:format("  available_ciphers: aes_128_cfb128~n"),
    <<149, 146, 215, 117, 124, 68, 24, 44, 51, 164, 46, 233, 81, 71, 162, 220>> =
        crypto:crypto_one_time(aes_128_cfb128, <<1:128>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_256_cfb128~n"),
    <<89, 151, 109, 175, 200, 98, 75, 207, 80, 33, 65, 131, 194, 29, 141, 242>> =
        crypto:crypto_one_time(aes_256_cfb128, <<1:256>>, <<2:128>>, <<3:128>>, false),
    io:format("  available_ciphers: aes_128_ecb~n"),
    <<51, 126, 5, 238, 121, 110, 153, 245, 229, 187, 6, 58, 119, 97, 242, 197>> =
        crypto:crypto_one_time(aes_128_ecb, <<1:128>>, <<2:128>>, false),
    io:format("  available_ciphers: aes_256_ecb~n"),
    <<9, 134, 59, 77, 138, 44, 15, 97, 69, 171, 187, 23, 29, 143, 25, 227>> =
        crypto:crypto_one_time(aes_256_ecb, <<1:256>>, <<2:128>>, false),
    io:format("  available_ciphers: aes_*_ecb with iv~n"),
    <<171, 29, 253, 3, 110, 255, 225, 168, 40, 2, 92, 101, 18, 22, 104, 91>> =
        crypto:crypto_one_time(aes_128_ecb, <<1:128>>, <<2:128>>, <<3:128>>, false),
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
