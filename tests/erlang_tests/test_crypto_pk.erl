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

-module(test_crypto_pk).
-export([
    start/0,
    test_generate_and_compute_key/0,
    test_sign_and_verify/0,
    test_sign_bad_algorithm/0,
    test_sign_malformed_key/0,
    test_verify_bad_algorithm/0,
    test_verify_malformed_key/0,
    test_ed25519_generate_key/0,
    test_ed25519_sign_and_verify/0,
    test_ed25519_verify_bad_sig/0,
    test_ed25519_sign_malformed_key/0,
    test_ed25519_verify_malformed_key/0,
    test_ed25519_sign_bad_digest/0,
    test_ed25519_verify_bad_digest/0,
    test_x25519_mutual_key_agreement/0
]).

start() ->
    case at_least(24) of
        true ->
            ok = mbedtls_conditional_run(test_generate_and_compute_key, 16#03000000),
            ok = mbedtls_conditional_run(test_sign_and_verify, 16#03060100);
        false ->
            ok
    end,
    case at_least(27) of
        true ->
            ok = mbedtls_conditional_run(test_sign_bad_algorithm, 16#03060100),
            ok = mbedtls_conditional_run(test_sign_malformed_key, 16#03060100),
            ok = mbedtls_conditional_run(test_verify_bad_algorithm, 16#03060100),
            ok = mbedtls_conditional_run(test_verify_malformed_key, 16#03060100);
        false ->
            ok
    end,
    case at_least(25) of
        true ->
            ok = libsodium_conditional_run(test_ed25519_generate_key),
            ok = libsodium_conditional_run(test_ed25519_sign_and_verify),
            ok = libsodium_conditional_run(test_ed25519_verify_bad_sig),
            ok = libsodium_conditional_run(test_ed25519_sign_malformed_key),
            ok = libsodium_conditional_run(test_ed25519_verify_malformed_key),
            ok = libsodium_conditional_run(test_ed25519_sign_bad_digest),
            ok = libsodium_conditional_run(test_ed25519_verify_bad_digest),
            ok = libsodium_conditional_run(test_x25519_mutual_key_agreement);
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

libsodium_conditional_run(F) ->
    Info = crypto:info_lib(),
    case has_libsodium_or_openssl(Info) of
        true ->
            ?MODULE:F();
        false ->
            ok
    end.

has_libsodium_or_openssl([]) ->
    false;
has_libsodium_or_openssl([{<<"OpenSSL">>, _, _} | _T]) ->
    true;
has_libsodium_or_openssl([{<<"libsodium">>, _, _} | _T]) ->
    true;
has_libsodium_or_openssl([_ | T]) ->
    has_libsodium_or_openssl(T).

test_generate_and_compute_key() ->
    {Pub, Priv} = crypto:generate_key(eddh, x25519),
    true = is_binary(Pub),
    32 = byte_size(Pub),
    true = is_binary(Priv),
    32 = byte_size(Priv),

    ComputedKey = crypto:compute_key(eddh, Pub, Priv, x25519),
    true = is_binary(ComputedKey),
    32 = byte_size(ComputedKey),

    {Pub2, Priv2} = crypto:generate_key(eddh, x25519),
    true = is_binary(Pub2),
    32 = byte_size(Pub2),
    true = is_binary(Priv2),
    32 = byte_size(Priv2),

    ComputedKey2 = crypto:compute_key(eddh, Pub2, Priv2, x25519),
    true = is_binary(ComputedKey2),
    32 = byte_size(ComputedKey2),

    ok.

test_sign_and_verify() ->
    Data = <<"Hello">>,

    {SECPPub, SECPPriv} = crypto:generate_key(ecdh, secp256r1),
    Sig = crypto:sign(ecdsa, sha256, Data, [SECPPriv, secp256r1]),
    Sig2 = crypto:sign(ecdsa, sha256, [<<"">>, Data, <<"">>], [SECPPriv, secp256r1]),

    false = crypto:verify(ecdsa, sha256, <<"Invalid">>, Sig, [SECPPub, secp256r1]),
    false = crypto:verify(ecdsa, sha256, Data, <<"InvalidSig">>, [SECPPub, secp256r1]),
    true = crypto:verify(ecdsa, sha256, Data, Sig, [SECPPub, secp256r1]),
    true = crypto:verify(ecdsa, sha256, Data, Sig2, [SECPPub, secp256r1]),
    true = crypto:verify(ecdsa, sha256, [<<"">>, Data, <<"">>], Sig, [SECPPub, secp256r1]),

    ok.

expect_crypto_error_one_of(Fun, Expected) ->
    try
        Fun(),
        unexpected
    catch
        error:{Tag, {File, Line}, Desc} when
            is_atom(Tag), is_list(File), is_integer(Line), is_list(Desc)
        ->
            case lists:member({Tag, Desc}, Expected) of
                true ->
                    ok;
                false ->
                    unexpected
            end
    end.

test_sign_bad_algorithm() ->
    {_Pub, Priv} = crypto:generate_key(ecdh, secp256r1),
    Data = <<"Hello">>,
    ok = expect_crypto_error_one_of(
        fun() ->
            crypto:sign(bad_algo, sha256, Data, [Priv, secp256r1])
        end,
        [
            {badarg, "Bad algorithm"},
            {error, "Can't create PKEY_CTX from name"},
            {error, "Invalid public key"}
        ]
    ),
    ok.

test_sign_malformed_key() ->
    Data = <<"Hello">>,

    exp_err =
        try
            crypto:sign(ecdsa, sha256, Data, <<"bad_key">>)
        catch
            error:{badarg, {File1, Line1}, "Couldn't get ECDSA private key"} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,

    exp_err =
        try
            crypto:sign(ecdsa, sha256, Data, [])
        catch
            error:{badarg, {File2, Line2}, "Couldn't get ECDSA private key"} when
                is_list(File2) andalso is_integer(Line2)
            ->
                exp_err
        end,

    ok.

test_verify_bad_algorithm() ->
    {Pub, Priv} = crypto:generate_key(ecdh, secp256r1),
    Data = <<"Hello">>,
    Sig = crypto:sign(ecdsa, sha256, Data, [Priv, secp256r1]),
    ok = expect_crypto_error_one_of(
        fun() ->
            crypto:verify(bad_algo, sha256, Data, Sig, [Pub, secp256r1])
        end,
        [
            {badarg, "Bad algorithm"},
            {error, "Can't create PKEY_CTX from name"},
            {error, "Invalid public key"}
        ]
    ),
    ok.

test_verify_malformed_key() ->
    {_Pub, Priv} = crypto:generate_key(ecdh, secp256r1),
    Data = <<"Hello">>,
    Sig = crypto:sign(ecdsa, sha256, Data, [Priv, secp256r1]),

    exp_err =
        try
            crypto:verify(ecdsa, sha256, Data, Sig, <<"bad_key">>)
        catch
            error:{badarg, {File1, Line1}, "Couldn't get ECDSA public key"} when
                is_list(File1) andalso is_integer(Line1)
            ->
                exp_err
        end,

    exp_err =
        try
            crypto:verify(ecdsa, sha256, Data, Sig, [])
        catch
            error:{badarg, {File2, Line2}, "Couldn't get ECDSA public key"} when
                is_list(File2) andalso is_integer(Line2)
            ->
                exp_err
        end,

    ok.

test_ed25519_generate_key() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    true = is_binary(Pub),
    32 = byte_size(Pub),
    true = is_binary(Priv),
    32 = byte_size(Priv),

    {Pub2, Priv2} = crypto:generate_key(eddsa, ed25519),
    true = Pub =/= Pub2,
    true = Priv =/= Priv2,

    ok.

test_ed25519_sign_and_verify() ->
    Data = <<"Hello, Ed25519!">>,

    % Keys and expected signature generated with Erlang/OTP 28 for interop testing.
    % Ed25519 signatures are deterministic (RFC 8032), so we can match the exact value.
    Pub =
        <<72, 77, 139, 244, 181, 199, 46, 12, 113, 181, 146, 30, 254, 48, 112, 55, 255, 27, 106,
            181, 172, 201, 127, 251, 30, 104, 26, 71, 210, 113, 74, 85>>,
    Priv =
        <<244, 240, 61, 112, 113, 48, 75, 102, 161, 198, 170, 106, 163, 9, 232, 213, 33, 139, 66,
            65, 101, 130, 174, 239, 125, 124, 1, 98, 83, 253, 45, 15>>,
    ExpectedSig =
        <<235, 18, 241, 255, 117, 146, 90, 166, 47, 219, 223, 90, 128, 243, 129, 216, 178, 176, 72,
            96, 150, 83, 249, 80, 45, 155, 45, 179, 151, 91, 3, 179, 76, 12, 110, 251, 245, 226,
            148, 161, 39, 114, 42, 167, 247, 20, 192, 99, 124, 41, 101, 128, 54, 204, 93, 65, 95,
            180, 111, 154, 56, 189, 161, 9>>,

    Sig = crypto:sign(eddsa, none, Data, [Priv, ed25519]),
    ExpectedSig = Sig,

    true = crypto:verify(eddsa, none, Data, Sig, [Pub, ed25519]),

    Sig2 = crypto:sign(eddsa, none, [<<"Hello,">>, <<" Ed25519!">>], [Priv, ed25519]),
    ExpectedSig = Sig2,
    true = crypto:verify(eddsa, none, Data, Sig2, [Pub, ed25519]),

    false = crypto:verify(eddsa, none, <<"Wrong data">>, Sig, [Pub, ed25519]),

    {Pub2, _Priv2} = crypto:generate_key(eddsa, ed25519),
    false = crypto:verify(eddsa, none, Data, Sig, [Pub2, ed25519]),

    ok.

test_ed25519_verify_bad_sig() ->
    Data = <<"Hello, Ed25519!">>,

    {Pub, _Priv} = crypto:generate_key(eddsa, ed25519),

    false = crypto:verify(eddsa, none, Data, <<"not_64_bytes">>, [Pub, ed25519]),

    ZeroSig = binary:copy(<<0>>, 64),
    false = crypto:verify(eddsa, none, Data, ZeroSig, [Pub, ed25519]),

    ok.

test_ed25519_sign_malformed_key() ->
    Data = <<"Hello">>,

    ok = expect_crypto_error_one_of(
        fun() -> crypto:sign(eddsa, none, Data, [<<"short">>, ed25519]) end,
        [{badarg, "Couldn't get EDDSA private key"}]
    ),

    LongKey = binary:copy(<<0>>, 64),
    ok = expect_crypto_error_one_of(
        fun() -> crypto:sign(eddsa, none, Data, [LongKey, ed25519]) end,
        [{badarg, "Couldn't get EDDSA private key"}]
    ),

    ok = expect_crypto_error_one_of(
        fun() -> crypto:sign(eddsa, none, Data, <<"not_a_list">>) end,
        [{badarg, "Couldn't get EDDSA private key"}]
    ),

    ok = expect_crypto_error_one_of(
        fun() -> crypto:sign(eddsa, none, Data, []) end,
        [{badarg, "Couldn't get EDDSA private key"}]
    ),

    ok.

test_ed25519_verify_malformed_key() ->
    Data = <<"Hello">>,
    {_Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    Sig = crypto:sign(eddsa, none, Data, [Priv, ed25519]),

    ok = expect_crypto_error_one_of(
        fun() -> crypto:verify(eddsa, none, Data, Sig, [<<"short">>, ed25519]) end,
        [{badarg, "Couldn't get EDDSA public key"}]
    ),

    LongKey = binary:copy(<<0>>, 64),
    ok = expect_crypto_error_one_of(
        fun() -> crypto:verify(eddsa, none, Data, Sig, [LongKey, ed25519]) end,
        [{badarg, "Couldn't get EDDSA public key"}]
    ),

    ok = expect_crypto_error_one_of(
        fun() -> crypto:verify(eddsa, none, Data, Sig, <<"not_a_list">>) end,
        [{badarg, "Couldn't get EDDSA public key"}]
    ),

    ok = expect_crypto_error_one_of(
        fun() -> crypto:verify(eddsa, none, Data, Sig, []) end,
        [{badarg, "Couldn't get EDDSA public key"}]
    ),

    ok.

test_ed25519_sign_bad_digest() ->
    %% Ed25519 is PureEdDSA: the digest parameter is ignored.
    %% All digest types must produce the same deterministic signature as 'none'.
    Data = <<"Hello">>,
    Priv =
        <<244, 240, 61, 112, 113, 48, 75, 102, 161, 198, 170, 106, 163, 9, 232, 213, 33, 139, 66,
            65, 101, 130, 174, 239, 125, 124, 1, 98, 83, 253, 45, 15>>,
    ExpectedSig =
        <<36, 155, 137, 168, 56, 15, 250, 205, 0, 211, 105, 9, 144, 98, 52, 20, 180, 122, 207, 177,
            172, 28, 6, 153, 237, 37, 174, 180, 21, 185, 91, 154, 5, 42, 208, 28, 106, 118, 243,
            229, 74, 52, 219, 3, 113, 89, 255, 198, 215, 87, 152, 27, 178, 109, 31, 160, 60, 73,
            138, 192, 25, 84, 105, 14>>,

    SigNone = crypto:sign(eddsa, none, Data, [Priv, ed25519]),
    ExpectedSig = SigNone,
    SigSha256 = crypto:sign(eddsa, sha256, Data, [Priv, ed25519]),
    ExpectedSig = SigSha256,
    SigSha384 = crypto:sign(eddsa, sha384, Data, [Priv, ed25519]),
    ExpectedSig = SigSha384,
    SigSha512 = crypto:sign(eddsa, sha512, Data, [Priv, ed25519]),
    ExpectedSig = SigSha512,

    ok.

test_ed25519_verify_bad_digest() ->
    %% Verify must also succeed regardless of the digest type parameter.
    Data = <<"Hello">>,
    Pub =
        <<72, 77, 139, 244, 181, 199, 46, 12, 113, 181, 146, 30, 254, 48, 112, 55, 255, 27, 106,
            181, 172, 201, 127, 251, 30, 104, 26, 71, 210, 113, 74, 85>>,
    Priv =
        <<244, 240, 61, 112, 113, 48, 75, 102, 161, 198, 170, 106, 163, 9, 232, 213, 33, 139, 66,
            65, 101, 130, 174, 239, 125, 124, 1, 98, 83, 253, 45, 15>>,
    Sig = crypto:sign(eddsa, none, Data, [Priv, ed25519]),

    true = crypto:verify(eddsa, none, Data, Sig, [Pub, ed25519]),
    true = crypto:verify(eddsa, sha256, Data, Sig, [Pub, ed25519]),
    true = crypto:verify(eddsa, sha384, Data, Sig, [Pub, ed25519]),
    true = crypto:verify(eddsa, sha512, Data, Sig, [Pub, ed25519]),

    ok.

test_x25519_mutual_key_agreement() ->
    AlicePub =
        <<126, 202, 61, 75, 59, 222, 126, 116, 223, 26, 30, 46, 224, 75, 223, 157, 155, 110, 150,
            114, 59, 158, 161, 67, 178, 168, 61, 126, 140, 24, 214, 87>>,
    AlicePriv =
        <<104, 202, 170, 101, 51, 84, 248, 100, 7, 205, 175, 170, 130, 118, 38, 132, 234, 193, 224,
            215, 64, 97, 187, 157, 255, 240, 35, 10, 158, 243, 28, 82>>,
    BobPub =
        <<118, 230, 132, 89, 1, 115, 195, 35, 64, 195, 9, 207, 96, 78, 56, 157, 236, 43, 202, 25,
            228, 252, 55, 187, 225, 132, 182, 198, 255, 197, 194, 21>>,
    BobPriv =
        <<160, 28, 57, 71, 222, 241, 20, 233, 68, 53, 166, 6, 239, 253, 179, 138, 19, 183, 125, 133,
            13, 160, 103, 13, 172, 142, 224, 6, 115, 184, 141, 88>>,
    ExpectedShared =
        <<0, 58, 164, 159, 221, 139, 255, 82, 15, 161, 198, 226, 187, 61, 239, 5, 194, 210, 82, 166,
            254, 50, 118, 210, 170, 81, 207, 128, 191, 140, 53, 4>>,

    AliceShared = crypto:compute_key(eddh, BobPub, AlicePriv, x25519),
    ExpectedShared = AliceShared,

    BobShared = crypto:compute_key(eddh, AlicePub, BobPriv, x25519),
    ExpectedShared = BobShared,

    {ThirdPub, _ThirdPriv} = crypto:generate_key(eddh, x25519),
    ThirdShared = crypto:compute_key(eddh, ThirdPub, AlicePriv, x25519),
    true = (ThirdShared =/= AliceShared),
    32 = byte_size(ThirdShared),

    ok.
