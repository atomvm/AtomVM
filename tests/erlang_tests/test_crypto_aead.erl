%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_crypto_aead).
-export([
    start/0,
    test_aes_128_gcm/0,
    test_aes_128_gcm_custom_tag/0,
    test_aes_192_gcm/0,
    test_aes_256_gcm/0,
    test_chacha20_poly1305/0,
    test_aes_128_ccm/0,
    test_aes_128_ccm_short_tag/0,
    test_aes_256_ccm/0,
    test_gcm_empty_aad/0,
    test_gcm_empty_plaintext/0,
    test_gcm_single_byte/0,
    test_gcm_multi_block/0,
    test_gcm_iolist_input/0,
    test_gcm_wrong_tag/0,
    test_gcm_wrong_aad/0,
    test_ccm_wrong_tag/0,
    test_bad_cipher/0,
    test_bad_key_size/0,
    test_bad_key/0,
    test_bad_iv/0,
    test_bad_text/0,
    test_bad_aad/0,
    test_bad_enc_flag/0,
    get_bad/0,
    get_list/0
]).

start() ->
    ok = mbedtls_conditional_run(test_aes_128_gcm, 16#03000000),
    ok = mbedtls_conditional_run(test_aes_128_gcm_custom_tag, 16#03000000),
    ok = mbedtls_conditional_run(test_aes_192_gcm, 16#03000000),
    ok = mbedtls_conditional_run(test_aes_256_gcm, 16#03000000),
    ok = mbedtls_conditional_run(test_chacha20_poly1305, 16#03000000),
    ok = mbedtls_conditional_run(test_aes_128_ccm, 16#03000000),
    ok = mbedtls_conditional_run(test_aes_128_ccm_short_tag, 16#03000000),
    ok = mbedtls_conditional_run(test_aes_256_ccm, 16#03000000),
    ok = mbedtls_conditional_run(test_gcm_empty_aad, 16#03000000),
    ok = mbedtls_conditional_run(test_gcm_empty_plaintext, 16#03000000),
    ok = mbedtls_conditional_run(test_gcm_single_byte, 16#03000000),
    ok = mbedtls_conditional_run(test_gcm_multi_block, 16#03000000),
    ok = mbedtls_conditional_run(test_gcm_iolist_input, 16#03000000),
    ok = mbedtls_conditional_run(test_gcm_wrong_tag, 16#03000000),
    ok = mbedtls_conditional_run(test_gcm_wrong_aad, 16#03000000),
    ok = mbedtls_conditional_run(test_ccm_wrong_tag, 16#03000000),
    ok = mbedtls_conditional_run(test_bad_cipher, 16#03000000),
    ok = mbedtls_conditional_run(test_bad_key_size, 16#03000000),
    ok = mbedtls_conditional_run(test_bad_key, 16#03000000),
    ok = mbedtls_conditional_run(test_bad_iv, 16#03000000),
    ok = mbedtls_conditional_run(test_bad_text, 16#03000000),
    ok = mbedtls_conditional_run(test_bad_aad, 16#03000000),
    ok = mbedtls_conditional_run(test_bad_enc_flag, 16#03000000),
    0.

mbedtls_conditional_run(F, RVer) ->
    Info = crypto:info_lib(),
    case find_openssl_or_mbedtls_ver(Info, RVer) of
        true ->
            ?MODULE:F();
        false ->
            ok
    end.

find_openssl_or_mbedtls_ver([], _RVer) ->
    false;
find_openssl_or_mbedtls_ver([{<<"OpenSSL">>, _, _} | _T], _RVer) ->
    true;
find_openssl_or_mbedtls_ver([{<<"mbedtls">>, Ver, _} | _T], RVer) when Ver >= RVer ->
    true;
find_openssl_or_mbedtls_ver([_ | T], RVer) ->
    find_openssl_or_mbedtls_ver(T, RVer).

test_aes_128_gcm() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<219, 9, 203, 162, 9, 59, 160, 59, 57, 190, 5, 171>>,
    ExpTag = <<217, 60, 70, 220, 33, 186, 23, 31, 40, 99, 108, 203, 171, 45, 198, 199>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, Text, AAD, true),
    Text = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, ExpCT, AAD, ExpTag, false),
    ok.

test_aes_128_gcm_custom_tag() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<219, 9, 203, 162, 9, 59, 160, 59, 57, 190, 5, 171>>,
    ExpTag12 = <<217, 60, 70, 220, 33, 186, 23, 31, 40, 99, 108, 203>>,

    {ExpCT, ExpTag12} = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, Text, AAD, 12, true),
    Text = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, ExpCT, AAD, ExpTag12, false),
    ok.

test_aes_192_gcm() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<174, 156, 78, 247, 246, 153, 158, 97, 162, 95, 184, 163>>,
    ExpTag = <<200, 59, 103, 157, 222, 78, 200, 253, 99, 56, 127, 114, 136, 106, 166, 184>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_192_gcm, Key, IV, Text, AAD, true),
    Text = crypto:crypto_one_time_aead(aes_192_gcm, Key, IV, ExpCT, AAD, ExpTag, false),
    ok.

test_aes_256_gcm() ->
    Key =
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
            25, 26, 27, 28, 29, 30, 31>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<15, 103, 186, 119, 170, 197, 149, 116, 255, 45, 243, 170>>,
    ExpTag = <<197, 30, 63, 146, 222, 109, 48, 198, 91, 203, 80, 4, 171, 113, 230, 145>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Text, AAD, true),
    Text = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, ExpCT, AAD, ExpTag, false),
    ok.

test_chacha20_poly1305() ->
    Key =
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
            25, 26, 27, 28, 29, 30, 31>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<193, 158, 100, 108, 70, 55, 242, 47, 197, 239, 91, 210>>,
    ExpTag = <<97, 69, 212, 254, 35, 192, 149, 166, 45, 215, 96, 33, 97, 153, 201, 7>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(chacha20_poly1305, Key, IV, Text, AAD, true),
    Text = crypto:crypto_one_time_aead(chacha20_poly1305, Key, IV, ExpCT, AAD, ExpTag, false),
    ok.

test_aes_128_ccm() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<123, 112, 159, 11, 179, 160, 147, 222, 3, 127, 173, 193>>,
    ExpTag = <<185, 41, 226, 20, 214, 206, 126, 253, 73, 241, 14, 245, 208, 205, 103, 221>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_128_ccm, Key, IV, Text, AAD, 16, true),
    Text = crypto:crypto_one_time_aead(aes_128_ccm, Key, IV, ExpCT, AAD, ExpTag, false),
    ok.

test_aes_128_ccm_short_tag() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<123, 112, 159, 11, 179, 160, 147, 222, 3, 127, 173, 193>>,
    ExpTag8 = <<24, 105, 116, 10, 159, 68, 212, 36>>,

    {ExpCT, ExpTag8} = crypto:crypto_one_time_aead(aes_128_ccm, Key, IV, Text, AAD, 8, true),
    Text = crypto:crypto_one_time_aead(aes_128_ccm, Key, IV, ExpCT, AAD, ExpTag8, false),
    ok.

test_aes_256_ccm() ->
    Key =
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
            25, 26, 27, 28, 29, 30, 31>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    AAD = <<"additional data">>,
    ExpCT = <<194, 176, 212, 122, 81, 15, 158, 248, 222, 165, 213, 2>>,
    ExpTag = <<8, 177, 253, 246, 77, 128, 145, 236, 35, 24, 242, 144, 95, 121, 102, 135>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_256_ccm, Key, IV, Text, AAD, 16, true),
    Text = crypto:crypto_one_time_aead(aes_256_ccm, Key, IV, ExpCT, AAD, ExpTag, false),
    ok.

test_gcm_empty_aad() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Text = <<"Hello World!">>,
    ExpCT = <<219, 9, 203, 162, 9, 59, 160, 59, 57, 190, 5, 171>>,
    ExpTag = <<75, 143, 238, 84, 229, 251, 14, 134, 50, 98, 23, 182, 171, 214, 143, 119>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, Text, <<>>, true),
    Text = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, ExpCT, <<>>, ExpTag, false),
    ok.

test_gcm_empty_plaintext() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    AAD = <<"additional data">>,
    ExpTag = <<255, 87, 83, 41, 97, 2, 247, 149, 52, 155, 82, 96, 180, 3, 95, 179>>,

    {<<>>, ExpTag} = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, <<>>, AAD, true),
    <<>> = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, <<>>, AAD, ExpTag, false),
    ok.

test_gcm_single_byte() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    ExpCT = <<185>>,
    ExpTag = <<223, 155, 197, 124, 191, 43, 44, 98, 180, 217, 233, 184, 221, 58, 44, 192>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, <<42>>, <<>>, true),
    <<42>> = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, ExpCT, <<>>, ExpTag, false),
    ok.

test_gcm_multi_block() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    AAD = <<"additional data">>,
    Text =
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
            25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
            47>>,
    ExpCT =
        <<147, 109, 165, 205, 98, 30, 241, 83, 67, 219, 107, 129, 58, 174, 126, 7, 163, 55, 8, 245,
            71, 248, 235, 225, 254, 56, 235, 54, 8, 89, 188, 115, 165, 133, 249, 212, 208, 165, 145,
            196, 104, 221, 35, 204, 236, 164, 249, 189>>,
    ExpTag = <<50, 121, 28, 32, 128, 57, 158, 87, 158, 161, 22, 187, 55, 32, 98, 230>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, Text, AAD, true),
    Text = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, ExpCT, AAD, ExpTag, false),
    ok.

test_gcm_iolist_input() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    ExpCT = <<219, 9, 203, 162, 9, 59, 160, 59, 57, 190, 5, 171>>,
    ExpTag = <<217, 60, 70, 220, 33, 186, 23, 31, 40, 99, 108, 203, 171, 45, 198, 199>>,

    {ExpCT, ExpTag} = crypto:crypto_one_time_aead(
        aes_128_gcm,
        Key,
        IV,
        [<<"Hello">>, <<" World!">>],
        [<<"additional">>, <<" data">>],
        true
    ),
    ok.

test_gcm_wrong_tag() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    CT = <<218, 9, 203, 162, 9, 59, 160, 59, 57, 190, 5, 171>>,
    AAD = <<"additional data">>,
    WrongTag = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,

    error = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, CT, AAD, WrongTag, false),
    ok.

test_gcm_wrong_aad() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    CT = <<219, 9, 203, 162, 9, 59, 160, 59, 57, 190, 5, 171>>,
    Tag = <<217, 60, 70, 220, 33, 186, 23, 31, 40, 99, 108, 203, 171, 45, 198, 199>>,

    error = crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, CT, <<"wrong aad">>, Tag, false),
    ok.

test_ccm_wrong_tag() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    CT = <<123, 112, 159, 11, 179, 160, 147, 222, 3, 127, 173, 193>>,
    AAD = <<"additional data">>,
    WrongTag = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,

    error = crypto:crypto_one_time_aead(aes_128_ccm, Key, IV, CT, AAD, WrongTag, false),
    ok.

test_bad_cipher() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,

    exp_err =
        try
            crypto:crypto_one_time_aead(?MODULE:get_bad(), Key, IV, <<"test">>, <<>>, true)
        catch
            error:{badarg, _Desc} ->
                exp_err
        end,
    ok.

test_bad_key_size() ->
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,
    Key15 = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14>>,
    Key17 = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>,

    exp_err =
        try
            crypto:crypto_one_time_aead(aes_128_gcm, Key15, IV, <<"test">>, <<>>, true)
        catch
            error:{badarg, {File1, Line1}, _Desc1} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,

    exp_err =
        try
            crypto:crypto_one_time_aead(aes_128_gcm, Key17, IV, <<"test">>, <<>>, true)
        catch
            error:{badarg, {File2, Line2}, _Desc2} when
                is_list(File2) andalso is_integer(Line2)
            ->
                exp_err
        end,
    ok.

test_bad_key() ->
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,

    exp_err =
        try
            crypto:crypto_one_time_aead(aes_128_gcm, ?MODULE:get_bad(), IV, <<"test">>, <<>>, true)
        catch
            error:{badarg, {File1, Line1}, _Desc1} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,
    ok.

test_bad_iv() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,

    exp_err =
        try
            crypto:crypto_one_time_aead(aes_128_gcm, Key, ?MODULE:get_bad(), <<"test">>, <<>>, true)
        catch
            error:{badarg, {File1, Line1}, _Desc1} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,
    ok.

test_bad_text() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,

    exp_err =
        try
            crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, ?MODULE:get_bad(), <<>>, true)
        catch
            error:{badarg, {File1, Line1}, _Desc1} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,
    ok.

test_bad_aad() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,

    exp_err =
        try
            crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, <<"test">>, ?MODULE:get_bad(), true)
        catch
            error:{badarg, {File1, Line1}, _Desc1} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,
    ok.

test_bad_enc_flag() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>,

    exp_err =
        try
            crypto:crypto_one_time_aead(
                aes_128_gcm, Key, IV, <<"test">>, <<>>, 16, ?MODULE:get_bad()
            )
        catch
            error:{badarg, {File1, Line1}, _Desc1} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,
    ok.

get_bad() -> foo.

get_list() -> [<<"0123456789">>, <<"ABCDEF">>].
