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

-module(test_crypto_hash_update).
-export([
    start/0,
    test_sha256/0,
    test_sha256_list/0,
    test_cloning_sha256/0,
    test_sha256_empty/0,
    test_md5/0,
    test_badarg/0,
    get_bad/0
]).

start() ->
    ok = mbedtls_conditional_run(test_sha256, 16#03000000),
    ok = mbedtls_conditional_run(test_sha256_list, 16#03000000),
    ok = mbedtls_conditional_run(test_cloning_sha256, 16#03000000),
    ok = mbedtls_conditional_run(test_sha256_empty, 16#03000000),
    ok = mbedtls_conditional_run(test_md5, 16#03000000),
    case at_least(27) of
        true ->
            ok = mbedtls_conditional_run(test_badarg, 16#03000000);
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

test_sha256() ->
    HashInitialState = crypto:hash_init(sha256),
    HashUpdatedState = crypto:hash_update(HashInitialState, <<"Hello">>),
    <<24, 95, 141, 179, 34, 113, 254, 37, 245, 97, 166, 252, 147, 139, 46, 38, 67, 6, 236, 48, 78,
        218, 81, 128, 7, 209, 118, 72, 38, 56, 25, 105>> = crypto:hash_final(HashUpdatedState),
    ok.

test_sha256_list() ->
    HashInitialState = crypto:hash_init(sha256),
    HashUpdatedState = crypto:hash_update(HashInitialState, [$H, $e, <<"llo">>]),
    <<24, 95, 141, 179, 34, 113, 254, 37, 245, 97, 166, 252, 147, 139, 46, 38, 67, 6, 236, 48, 78,
        218, 81, 128, 7, 209, 118, 72, 38, 56, 25, 105>> = crypto:hash_final(HashUpdatedState),
    ok.

test_cloning_sha256() ->
    HashInitialState = crypto:hash_init(sha256),
    HashUpdatedState = crypto:hash_update(HashInitialState, <<"hello: ">>),

    HashUpdatedStateA = crypto:hash_update(HashInitialState, <<"world">>),
    HashUpdatedStateB = crypto:hash_update(HashInitialState, <<"everyone">>),
    HashUpdatedStateC = crypto:hash_update(HashInitialState, <<"folks">>),

    <<72, 110, 164, 98, 36, 209, 187, 79, 182, 128, 243, 79, 124, 154, 217, 106, 143, 36, 236, 136,
        190, 115, 234, 142, 90, 108, 101, 38, 14, 156, 184, 167>> = crypto:hash_final(
        HashUpdatedStateA
    ),
    <<93, 103, 153, 26, 233, 103, 153, 76, 148, 178, 226, 26, 77, 99, 156, 101, 157, 53, 122, 111,
        189, 63, 167, 67, 228, 246, 99, 138, 68, 13, 53, 199>> = crypto:hash_final(
        HashUpdatedStateB
    ),
    <<95, 145, 188, 155, 205, 195, 48, 130, 185, 45, 86, 227, 186, 245, 8, 122, 110, 121, 122, 98,
        231, 149, 15, 149, 193, 222, 49, 69, 247, 55, 31, 112>> = crypto:hash_final(
        HashUpdatedStateC
    ),
    <<154, 32, 219, 114, 31, 158, 11, 194, 61, 234, 117, 20, 94, 185, 94, 95, 170, 196, 55, 150,
        138, 129, 254, 106, 97, 237, 152, 248, 19, 101, 242, 235>> = crypto:hash_final(
        HashUpdatedState
    ),

    HashUpdatedStateB2 = crypto:hash_update(HashUpdatedStateB, <<"!!!">>),

    <<215, 154, 156, 137, 108, 16, 178, 36, 142, 177, 122, 249, 164, 173, 10, 35, 85, 104, 6, 237,
        194, 215, 157, 126, 198, 167, 31, 206, 247, 159, 70, 41>> = crypto:hash_final(
        HashUpdatedStateB2
    ),
    <<215, 154, 156, 137, 108, 16, 178, 36, 142, 177, 122, 249, 164, 173, 10, 35, 85, 104, 6, 237,
        194, 215, 157, 126, 198, 167, 31, 206, 247, 159, 70, 41>> = crypto:hash_final(
        HashUpdatedStateB2
    ),

    ok.

test_sha256_empty() ->
    <<227, 176, 196, 66, 152, 252, 28, 20, 154, 251, 244, 200, 153, 111, 185, 36, 39, 174, 65, 228,
        100, 155, 147, 76, 164, 149, 153, 27, 120, 82, 184, 85>> = crypto:hash_final(
        crypto:hash_init(sha256)
    ),

    <<227, 176, 196, 66, 152, 252, 28, 20, 154, 251, 244, 200, 153, 111, 185, 36, 39, 174, 65, 228,
        100, 155, 147, 76, 164, 149, 153, 27, 120, 82, 184, 85>> = crypto:hash_final(
        crypto:hash_update(crypto:hash_init(sha256), <<"">>)
    ),

    ok.

test_md5() ->
    HashInitialState = crypto:hash_init(md5),
    HashUpdatedState = crypto:hash_update(HashInitialState, <<0, 1, 2, 3, 4>>),
    <<208, 83, 116, 220, 56, 29, 155, 82, 128, 100, 70, 167, 28, 142, 121, 177>> = crypto:hash_final(
        HashUpdatedState
    ),
    ok.

test_badarg() ->
    exp_err =
        try
            crypto:hash_init(?MODULE:get_bad())
        catch
            error:{badarg, {File1, Line1}, "Bad digest type"} when
                is_list(File1) and is_integer(Line1)
            ->
                exp_err
        end,

    HashInitialState = crypto:hash_init(sha256),
    exp_err =
        try
            crypto:hash_update(HashInitialState, ?MODULE:get_bad())
        catch
            error:badarg -> exp_err
        end,
    exp_err =
        try
            crypto:hash_update(?MODULE:get_bad(), <<"Hello">>)
        catch
            error:{badarg, {File2, Line2}, "Bad state"} when is_list(File2) and is_integer(Line2) ->
                exp_err
        end,

    exp_err =
        try
            crypto:hash_final(?MODULE:get_bad())
        catch
            error:{badarg, {File3, Line3}, "Bad state"} when is_list(File3) and is_integer(Line3) ->
                exp_err
        end,

    ok.

get_bad() -> foo.
