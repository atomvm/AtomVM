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

-module(test_crypto_crypto).
-export([
    start/0,
    test_encrypt_aes128_ctr/0,
    test_encrypt_aes128_ctr_list/0,
    test_decrypt_aes128_cbc/0,
    test_encrypt_aes128_cbc_bool/0,
    test_use_after_final/0,
    test_use_after_final_cbc/0,
    test_ecb_less_than_one_block/0,
    test_cbc_final_padding/0,
    test_ecb_final_no_padding/0,
    test_encrypt_aes256_ecb/0,
    test_bad_key_size/0,
    test_bad_iv_size/0,
    test_bad_algo/0,
    test_bad_key/0,
    test_bad_iv/0,
    test_bad_opt/0,
    test_bad_padding_ecb/0,
    test_bad_state/0,
    test_bad_data/0,
    get_bad/0,
    get_list/0
]).

start() ->
    case at_least(24) of
        true ->
            ok = mbedtls_conditional_run(test_encrypt_aes128_ctr, 16#03000000),
            ok = mbedtls_conditional_run(test_encrypt_aes128_ctr_list, 16#03000000),
            ok = mbedtls_conditional_run(test_decrypt_aes128_cbc, 16#03000000),
            ok = mbedtls_conditional_run(test_encrypt_aes128_cbc_bool, 16#03000000),
            ok = mbedtls_conditional_run(test_use_after_final, 16#03000000),
            ok = mbedtls_conditional_run(test_use_after_final_cbc, 16#03000000),
            ok = mbedtls_conditional_run(test_ecb_less_than_one_block, 16#03000000),
            ok = mbedtls_conditional_run(test_cbc_final_padding, 16#03000000),
            ok = mbedtls_conditional_run(test_ecb_final_no_padding, 16#03000000),
            ok = mbedtls_conditional_run(test_encrypt_aes256_ecb, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_key_size, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_iv_size, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_algo, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_key, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_iv, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_opt, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_padding_ecb, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_state, 16#03000000),
            ok = mbedtls_conditional_run(test_bad_data, 16#03000000);
        false ->
            ok
    end,
    0.

otp_version() ->
    case erlang:system_info(machine) of
        "ATOM" -> atomvm;
        _ -> list_to_integer(erlang:system_info(otp_release))
    end.

at_least(Min) ->
    V = otp_version(),
    V =:= atomvm orelse V >= Min.

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

test_encrypt_aes128_ctr() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    EncryptState = crypto:crypto_init(aes_128_ctr, Key, IV, [{encrypt, true}]),
    true = is_reference(EncryptState),
    <<66, 241, 103, 217, 46, 78, 167, 42, 131, 175, 240, 120, 231, 114, 203, 123>> = crypto:crypto_update(
        EncryptState, <<"Hello World !!!!">>
    ),
    <<74, 6, 128, 248, 9, 56, 37, 249, 232, 182, 153, 47, 106, 133, 46, 253>> = crypto:crypto_update(
        EncryptState, <<"Hello World !!!!">>
    ),
    <<"">> = crypto:crypto_final(EncryptState),
    ok.

test_encrypt_aes128_ctr_list() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    EncryptState = crypto:crypto_init(aes_128_ctr, Key, IV, [{encrypt, true}]),
    true = is_reference(EncryptState),
    <<66, 241, 103, 217, 46, 78, 167, 42, 131, 175, 240, 120, 231, 114, 203, 123>> = crypto:crypto_update(
        EncryptState, [$H, $e, <<"llo ">>, <<"World !!!!">>]
    ),
    <<74, 6, 128, 248, 9, 56, 37, 249, 232, 182, 153, 47, 106, 133, 46, 253>> = crypto:crypto_update(
        EncryptState, [<<"Hello World ">>, <<"!!!!">>]
    ),
    <<"">> = crypto:crypto_final(EncryptState),
    ok.

test_decrypt_aes128_cbc() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    DecryptState = crypto:crypto_init(aes_128_ctr, Key, IV, [{encrypt, false}]),
    true = is_reference(DecryptState),
    <<"0123456789ABCDEF0123456789ABCDEF">> = crypto:crypto_update(
        DecryptState,
        <<58, 165, 57, 134, 117, 91, 198, 114, 201, 250, 213, 26, 133, 23, 175, 28, 50, 82, 222,
            167, 82, 45, 68, 161, 162, 227, 188, 77, 8, 224, 74, 154>>
    ),
    <<"">> = crypto:crypto_final(DecryptState),
    ok.

test_encrypt_aes128_cbc_bool() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    EncryptState = crypto:crypto_init(aes_128_ctr, Key, IV, true),
    true = is_reference(EncryptState),
    <<58, 165, 57, 134, 117, 91, 198, 114, 201, 250, 213, 26, 133, 23, 175, 28, 50, 82, 222, 167,
        82, 45, 68, 161, 162, 227, 188, 77, 8, 224, 74, 154>> = crypto:crypto_update(
        EncryptState, <<"0123456789ABCDEF0123456789ABCDEF">>
    ),
    <<"">> = crypto:crypto_final(EncryptState),

    DecryptState = crypto:crypto_init(aes_128_ctr, Key, IV, false),
    true = is_reference(DecryptState),
    <<"0123456789ABCDEF0123456789ABCDEF">> = crypto:crypto_update(
        DecryptState,
        <<58, 165, 57, 134, 117, 91, 198, 114, 201, 250, 213, 26, 133, 23, 175, 28, 50, 82, 222,
            167, 82, 45, 68, 161, 162, 227, 188, 77, 8, 224, 74, 154>>
    ),
    <<"">> = crypto:crypto_final(DecryptState),

    ok.

%% AtomVM does not allow reuse of a crypto state after crypto_final/1.
%% This test verifies that both crypto_update/2 and crypto_final/1 raise a
%% badarg error when called on a finalized state.
%%
%% On the BEAM, reuse after final is allowed, so this test is skipped there.
test_use_after_final() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            ok;
        "ATOM" ->
            Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            State = crypto:crypto_init(aes_128_ctr, Key, IV, [{encrypt, true}]),
            <<64, 225, 120, 193, 97, 38, 149, 41, 157, 172>> = crypto:crypto_update(
                State, <<"Just Hello">>
            ),
            <<"">> = crypto:crypto_final(State),

            exp_err =
                try
                    crypto:crypto_update(State, <<"Hello World !!!!">>)
                catch
                    error:{badarg, {File1, Line1},
                        "Bad state: AtomVM does not allow operations after crypto_final"} when
                        is_list(File1) andalso is_integer(Line1)
                    ->
                        exp_err
                end,

            exp_err =
                try
                    crypto:crypto_final(State)
                catch
                    error:{badarg, {File2, Line2},
                        "Bad state: AtomVM does not allow calling crypto_final more than once"} when
                        is_list(File2) andalso is_integer(Line2)
                    ->
                        exp_err
                end,

            ok
    end.

%% Like test_use_after_final but with CBC + PKCS7 padding, where crypto_final
%% produces real output (the padded last block).  Verifies that calling
%% crypto_final a second time or crypto_update after crypto_final raises badarg.
%%
%% On the BEAM, reuse after final is allowed, so this test is skipped there.
test_use_after_final_cbc() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            ok;
        "ATOM" ->
            Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            State = crypto:crypto_init(aes_128_cbc, Key, IV, [
                {padding, pkcs_padding}, {encrypt, true}
            ]),
            <<"">> = crypto:crypto_update(State, <<"1234567">>),
            <<"">> = crypto:crypto_update(State, <<"89ABCDE">>),
            <<45, 86, 231, 103, 240, 183, 169, 144, 1, 145, 121, 129, 65, 23, 60, 94>> =
                crypto:crypto_final(State),

            %% crypto_final called twice must fail
            exp_err =
                try
                    crypto:crypto_final(State)
                catch
                    error:{badarg, {File1, Line1},
                        "Bad state: AtomVM does not allow calling crypto_final more than once"} when
                        is_list(File1) andalso is_integer(Line1)
                    ->
                        exp_err
                end,

            %% crypto_update after crypto_final must fail
            exp_err =
                try
                    crypto:crypto_update(State, <<"more data">>)
                catch
                    error:{badarg, {File2, Line2},
                        "Bad state: AtomVM does not allow operations after crypto_final"} when
                        is_list(File2) andalso is_integer(Line2)
                    ->
                        exp_err
                end,

            ok
    end.

test_ecb_less_than_one_block() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    EncryptState = crypto:crypto_init(aes_128_ecb, Key, IV, [{encrypt, true}]),
    true = is_reference(EncryptState),
    <<"">> = crypto:crypto_update(EncryptState, <<"01234567">>),
    <<165, 186, 43, 98, 128, 212, 51, 188, 207, 230, 160, 70, 28, 184, 140, 78>> = crypto:crypto_update(
        EncryptState, <<"89ABCDEF">>
    ),
    <<"">> = crypto:crypto_final(EncryptState),
    ok.

%% PKCS7 padding is supported only with CBC ciphers via the PSA_ALG_CBC_PKCS7
%% algorithm.  Feed 14 bytes in two updates (7 + 7); PSA buffers the incomplete
%% block and on crypto_final emits the PKCS7-padded ciphertext (one 16-byte
%% block with 2 bytes of padding value 0x02).
%%
%% Expected output was computed with OTP crypto on the BEAM.
test_cbc_final_padding() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    EncryptState = crypto:crypto_init(aes_128_cbc, Key, IV, [
        {padding, pkcs_padding}, {encrypt, true}
    ]),
    true = is_reference(EncryptState),
    <<"">> = crypto:crypto_update(EncryptState, <<"1234567">>),
    <<"">> = crypto:crypto_update(EncryptState, <<"89ABCDE">>),
    <<45, 86, 231, 103, 240, 183, 169, 144, 1, 145, 121, 129, 65, 23, 60, 94>> =
        crypto:crypto_final(EncryptState),
    ok.

test_ecb_final_no_padding() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    EncryptState = crypto:crypto_init(aes_128_ecb, Key, IV, [{padding, none}, {encrypt, true}]),
    true = is_reference(EncryptState),
    <<"">> = crypto:crypto_update(EncryptState, <<"1234567">>),
    <<"">> = crypto:crypto_update(EncryptState, <<"89ABCDE">>),
    exp_err =
        try
            crypto:crypto_final(EncryptState)
        catch
            error:{error, {File1, Line1}, "Padding 'none' but unfilled last block"} when
                is_list(File1) and is_integer(Line1)
            ->
                exp_err
        end,
    ok.

test_encrypt_aes256_ecb() ->
    Key =
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15>>,
    IV = <<>>,
    EncryptState = crypto:crypto_init(aes_256_ecb, Key, IV, [{encrypt, true}]),
    true = is_reference(EncryptState),
    <<35, 71, 82, 126, 155, 68, 42, 51, 62, 71, 70, 111, 54, 5, 236, 105>> = crypto:crypto_update(
        EncryptState, <<"Hello World 1234">>
    ),
    <<100, 132, 105, 177, 37, 34, 214, 229, 166, 252, 187, 188, 14, 140, 71, 253>> = crypto:crypto_update(
        EncryptState, <<"Hello World ????">>
    ),
    <<"">> = crypto:crypto_final(EncryptState),
    ok.

test_bad_key_size() ->
    Key15Bytes = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    exp_err =
        try
            crypto:crypto_init(aes_128_ctr, Key15Bytes, IV, [{encrypt, true}])
        catch
            error:{badarg, {File1, Line1}, "Bad key size"} when
                is_list(File1) and is_integer(Line1)
            ->
                exp_err
        end,

    Key17Bytes = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>,
    exp_err =
        try
            crypto:crypto_init(aes_128_ctr, Key17Bytes, IV, [{encrypt, true}])
        catch
            error:{badarg, {File2, Line2}, "Bad key size"} when
                is_list(File2) and is_integer(Line2)
            ->
                exp_err
        end,

    ok.

test_bad_iv_size() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV15Bytes = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14>>,
    try
        crypto:crypto_init(aes_128_ctr, Key, IV15Bytes, [{encrypt, false}])
    catch
        error:{badarg, {File1, Line1}, "Bad iv size"} when
            is_list(File1) andalso is_integer(Line1)
        ->
            exp_err
    end,

    IV17Bytes = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>,
    try
        crypto:crypto_init(aes_128_ctr, Key, IV17Bytes, [{encrypt, false}])
    catch
        error:{badarg, {File2, Line2}, "Bad iv size"} when
            is_list(File2) andalso is_integer(Line2)
        ->
            exp_err
    end,

    ok.

test_bad_algo() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    try
        crypto:crypto_init(?MODULE:get_bad(), Key, IV, [{encrypt, true}])
    catch
        error:{badarg, {File, Line}, "Unknown cipher"} when
            is_list(File) andalso is_integer(Line)
        ->
            exp_err
    end,

    ok.

test_bad_key() ->
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    try
        crypto:crypto_init(aes_128_ctr, ?MODULE:get_bad(), IV, [{encrypt, true}])
    catch
        error:{badarg, {File1, Line1}, "Bad key"} when is_list(File1) andalso is_integer(Line1) ->
            exp_err
    end,

    try
        crypto:crypto_init(aes_128_ctr, ?MODULE:get_list(), IV, [{encrypt, true}])
    catch
        error:{badarg, {File2, Line2}, "Bad key"} when is_list(File2) andalso is_integer(Line2) ->
            exp_err
    end,

    ok.

test_bad_iv() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    try
        crypto:crypto_init(aes_128_ctr, Key, ?MODULE:get_bad(), [{encrypt, true}])
    catch
        error:{badarg, {File1, Line1}, "Bad iv type"} when
            is_list(File1) andalso is_integer(Line1)
        ->
            exp_err
    end,

    try
        crypto:crypto_init(aes_128_ctr, Key, ?MODULE:get_list(), [{encrypt, true}])
    catch
        error:{badarg, {File2, Line2}, "Bad iv type"} when
            is_list(File2) andalso is_integer(Line2)
        ->
            exp_err
    end,

    ok.

test_bad_opt() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,

    % We don't care for the following error messages, OTP is very specific, but we don't have to

    try
        crypto:crypto_init(?MODULE:get_bad(), Key, IV, [{encrypt, true}, ?MODULE:get_bad()])
    catch
        error:{badarg, {File1, Line1}, _DontCare1} when is_list(File1) andalso is_integer(Line1) ->
            exp_err
    end,

    try
        crypto:crypto_init(?MODULE:get_bad(), Key, IV, [{encrypt, true}, {?MODULE:get_bad(), true}])
    catch
        error:{badarg, {File2, Line2}, _DontCare2} when is_list(File2) andalso is_integer(Line2) ->
            exp_err
    end,

    try
        crypto:crypto_init(?MODULE:get_bad(), Key, IV, [{encrypt, ?MODULE:get_bad()}])
    catch
        error:{badarg, {File3, Line3}, _DontCare3} when is_list(File3) andalso is_integer(Line3) ->
            exp_err
    end,

    ok.

%% AtomVM supports PKCS7 padding only with CBC ciphers.  Requesting
%% pkcs_padding with ECB (or any non-CBC cipher) raises a badarg error.
%% This differs from OTP which silently accepts the option for ECB.
test_bad_padding_ecb() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            ok;
        "ATOM" ->
            Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
            IV = <<>>,
            exp_err =
                try
                    crypto:crypto_init(aes_128_ecb, Key, IV, [
                        {padding, pkcs_padding}, {encrypt, true}
                    ])
                catch
                    error:{badarg, {File, Line},
                        "PKCS padding is supported only with CBC ciphers"} when
                        is_list(File) andalso is_integer(Line)
                    ->
                        exp_err
                end,
            ok
    end.

test_bad_state() ->
    try
        crypto:crypto_update(?MODULE:get_bad(), <<"Test">>)
    catch
        error:{badarg, {File1, Line1}, "Bad State"} when is_list(File1) andalso is_integer(Line1) ->
            exp_err
    end,

    try
        crypto:crypto_final(?MODULE:get_bad())
    catch
        error:{badarg, {File2, Line2}, "Bad State"} when is_list(File2) andalso is_integer(Line2) ->
            exp_err
    end,

    ok.

test_bad_data() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    EncryptState = crypto:crypto_init(aes_128_ctr, Key, IV, [{encrypt, true}]),
    try
        crypto:crypto_update(EncryptState, ?MODULE:get_bad())
    catch
        error:{badarg, {File, Line}, "expected binary"} when
            is_list(File) andalso is_integer(Line)
        ->
            exp_err
    end,

    ok.

get_bad() -> foo.

get_list() -> [<<"0123456789">>, <<"ABCDEF">>].
