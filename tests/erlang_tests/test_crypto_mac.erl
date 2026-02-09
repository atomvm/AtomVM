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

-module(test_crypto_mac).
-export([start/0, test_hmac/0, test_cmac/0]).

start() ->
    ok = mbedtls_conditional_run(test_hmac, 16#03000000),
    ok = mbedtls_conditional_run(test_cmac, 16#03000000),
    0.

mbedtls_conditional_run(F, RVer) ->
    Info = crypto:info_lib(),
    case find_openssl_or_mbedtls_ver(Info, RVer) of
        true ->
            ?MODULE:F();
        false ->
            erlang:display({skipped, ?MODULE, F}),
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

test_hmac() ->
    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122, 176>> = crypto:mac(
        hmac, sha256, <<"Hello">>, <<"Data">>
    ),

    <<153, 146, 251, 20, 217, 139, 50, 190, 240, 28, 191, 144, 120, 206, 138, 44, 47, 139, 14, 233,
        146, 3, 76, 170, 214, 207, 208, 7, 109, 0, 155, 23>> = crypto:mac(
        hmac, sha256, <<"Hello">>, <<"">>
    ),

    {error, {badarg, {File, Line}, "Bad digest algorithm for HMAC"}} = expect_error(fun() ->
        crypto:mac(hmac, sha89, <<"Key">>, <<"Data">>)
    end),
    true = is_list(File),
    true = is_integer(Line) and (Line >= 0),

    ok.

test_cmac() ->
    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac(
        cmac, aes_128_cbc, <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>, <<"Data">>
    ),

    <<19, 247, 81, 225, 123, 105, 6, 29, 226, 176, 251, 80, 224, 17, 174, 122>> = crypto:mac(
        cmac, aes_128_cbc, <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>, <<"More Data">>
    ),

    <<151, 221, 110, 90, 136, 44, 189, 86, 76, 57, 174, 125, 28, 90, 49, 170>> = crypto:mac(
        cmac, aes_128_cbc, <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>, <<"">>
    ),

    {error, {badarg, {File, Line}, "Bad key size"}} = expect_error(fun() ->
        crypto:mac(
            cmac, aes_128_cbc, <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14>>, <<"Data">>
        )
    end),
    true = is_list(File),
    true = is_integer(Line) and (Line >= 0),

    {error, {badarg, {File2, Line2}, "Unknown cipher"}} = expect_error(fun() ->
        crypto:mac(cmac, none, <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14>>, <<"Data">>)
    end),
    true = is_list(File2),
    true = is_integer(Line2) and (Line2 >= 0),

    ok.

expect_error(Fun) ->
    try Fun() of
        Res -> {unexpected, Res}
    catch
        C:R -> {C, R}
    end.
