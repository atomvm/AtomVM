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
    test_verify_malformed_key/0
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
