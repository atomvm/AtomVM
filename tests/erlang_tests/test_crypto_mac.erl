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
-export([
    start/0,
    test_hmac/0,
    test_cmac/0,
    test_hmac_iolist/0,
    test_cmac_iolist/0,
    test_hmac_update/0,
    test_hmac_update_iolist/0,
    test_cmac_update/0,
    test_cmac_update_iolist/0,
    test_mac_finalN/0,
    test_mac_update_badarg/0,
    get_bad/0
]).

start() ->
    case at_least(27) of
        true ->
            ok = mbedtls_conditional_run(test_hmac, 16#03000000),
            ok = mbedtls_conditional_run(test_cmac, 16#03000000),
            ok = mbedtls_conditional_run(test_hmac_iolist, 16#03000000),
            ok = mbedtls_conditional_run(test_cmac_iolist, 16#03000000),
            ok = mbedtls_conditional_run(test_hmac_update, 16#03000000),
            ok = mbedtls_conditional_run(test_hmac_update_iolist, 16#03000000),
            ok = mbedtls_conditional_run(test_cmac_update, 16#03000000),
            ok = mbedtls_conditional_run(test_cmac_update_iolist, 16#03000000),
            ok = mbedtls_conditional_run(test_mac_finalN, 16#03000000),
            ok = mbedtls_conditional_run(test_mac_update_badarg, 16#03000000);
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

test_hmac_iolist() ->
    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122, 176>> = crypto:mac(
        hmac, sha256, [$H, <<"ell">>, <<"o">>], <<"Data">>
    ),

    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122,
        176>> = crypto:mac(
        hmac, sha256, <<"Hello">>, [$D, <<"at">>, <<"a">>]
    ),

    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122,
        176>> = crypto:mac(
        hmac, sha256, [<<"He">>, $l, $l, <<"o">>], [<<"Da">>, <<"ta">>]
    ),

    <<153, 146, 251, 20, 217, 139, 50, 190, 240, 28, 191, 144, 120, 206, 138, 44, 47, 139, 14, 233,
        146, 3, 76, 170, 214, 207, 208, 7, 109, 0, 155,
        23>> = crypto:mac(
        hmac, sha256, <<"Hello">>, [<<"">>]
    ),

    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122,
        176>> = crypto:mac(
        hmac, sha256, <<"Hello">>, [[[$D], <<"a">>], <<"ta">>]
    ),

    ok.

test_cmac_iolist() ->
    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac(
        cmac,
        aes_128_cbc,
        [<<0, 1, 2, 3>>, <<4, 5, 6, 7>>, <<8, 9, 10, 11, 12, 13, 14, 15>>],
        <<"Data">>
    ),

    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac(
        cmac, aes_128_cbc, <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>, [$D, <<"ata">>]
    ),

    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac(
        cmac,
        aes_128_cbc,
        [<<0, 1, 2, 3, 4, 5, 6, 7>>, <<8, 9, 10, 11, 12, 13, 14, 15>>],
        [<<"Da">>, <<"ta">>]
    ),

    <<19, 247, 81, 225, 123, 105, 6, 29, 226, 176, 251, 80, 224, 17, 174, 122>> = crypto:mac(
        cmac,
        aes_128_cbc,
        <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
        [<<"More">>, <<" ">>, <<"Data">>]
    ),

    <<151, 221, 110, 90, 136, 44, 189, 86, 76, 57, 174, 125, 28, 90, 49, 170>> = crypto:mac(
        cmac, aes_128_cbc, <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>, [<<"">>]
    ),

    ok.

test_hmac_update() ->
    MacInitState0 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    MacUpdatedState0 = crypto:mac_update(MacInitState0, <<"Data">>),
    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122, 176>> = crypto:mac_final(MacUpdatedState0),

    MacInitState1 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    MacUpdatedState1 = crypto:mac_update(MacInitState1, <<"Da">>),
    MacUpdatedState2 = crypto:mac_update(MacUpdatedState1, <<"ta">>),
    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122, 176>> = crypto:mac_final(MacUpdatedState2),

    MacInitState2 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    MacUpdatedState3 = crypto:mac_update(MacInitState2, <<"He">>),
    MacUpdatedState4 = crypto:mac_update(MacUpdatedState3, <<"llo">>),
    MacUpdatedState5 = crypto:mac_update(MacUpdatedState4, <<" World">>),
    <<99, 67, 211, 45, 120, 45, 165, 143, 115, 74, 108, 211, 235, 13, 148, 227, 194, 92, 12, 108,
        123, 27, 186, 81, 200, 116, 209, 8, 129, 63, 62, 109>> = crypto:mac_final(MacUpdatedState5),

    MacInitState3 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    MacUpdatedState6 = crypto:mac_update(MacInitState3, <<"">>),
    <<153, 146, 251, 20, 217, 139, 50, 190, 240, 28, 191, 144, 120, 206, 138, 44, 47, 139, 14, 233,
        146, 3, 76, 170, 214, 207, 208, 7, 109, 0, 155, 23>> = crypto:mac_final(MacUpdatedState6),

    MacInitState4 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    <<153, 146, 251, 20, 217, 139, 50, 190, 240, 28, 191, 144, 120, 206, 138, 44, 47, 139, 14, 233,
        146, 3, 76, 170, 214, 207, 208, 7, 109, 0, 155, 23>> = crypto:mac_final(MacInitState4),

    ok.

test_hmac_update_iolist() ->
    MacInitState0 = crypto:mac_init(hmac, sha256, [$H, <<"ell">>, <<"o">>]),
    MacUpdatedState0 = crypto:mac_update(MacInitState0, <<"Data">>),
    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122, 176>> = crypto:mac_final(MacUpdatedState0),

    MacInitState1 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    MacUpdatedState1 = crypto:mac_update(MacInitState1, [$D, <<"a">>, <<"ta">>]),
    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27, 68, 220, 32, 133,
        108, 193, 194, 77, 15, 26, 51, 8, 197, 95, 122, 176>> = crypto:mac_final(MacUpdatedState1),

    MacInitState2 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    MacUpdatedState2 = crypto:mac_update(MacInitState2, <<"He">>),
    MacUpdatedState3 = crypto:mac_update(MacUpdatedState2, [<<"llo">>, " World"]),
    <<99, 67, 211, 45, 120, 45, 165, 143, 115, 74, 108, 211, 235, 13, 148, 227, 194, 92, 12, 108,
        123, 27, 186, 81, 200, 116, 209, 8, 129, 63, 62, 109>> = crypto:mac_final(MacUpdatedState3),

    ok.

test_cmac_update() ->
    Key16 = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,

    MacInitState0 = crypto:mac_init(cmac, aes_128_cbc, Key16),
    MacUpdatedState0 = crypto:mac_update(MacInitState0, <<"Data">>),
    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac_final(
        MacUpdatedState0
    ),

    MacInitState1 = crypto:mac_init(cmac, aes_128_cbc, Key16),
    MacUpdatedState1 = crypto:mac_update(MacInitState1, <<"Da">>),
    MacUpdatedState2 = crypto:mac_update(MacUpdatedState1, <<"ta">>),
    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac_final(
        MacUpdatedState2
    ),

    MacInitState2 = crypto:mac_init(cmac, aes_128_cbc, Key16),
    MacUpdatedState3 = crypto:mac_update(MacInitState2, <<"More">>),
    MacUpdatedState4 = crypto:mac_update(MacUpdatedState3, <<" ">>),
    MacUpdatedState5 = crypto:mac_update(MacUpdatedState4, <<"Data">>),
    <<19, 247, 81, 225, 123, 105, 6, 29, 226, 176, 251, 80, 224, 17, 174, 122>> = crypto:mac_final(
        MacUpdatedState5
    ),

    MacInitState3 = crypto:mac_init(cmac, aes_128_cbc, Key16),
    MacUpdatedState6 = crypto:mac_update(MacInitState3, <<"">>),
    <<151, 221, 110, 90, 136, 44, 189, 86, 76, 57, 174, 125, 28, 90, 49, 170>> = crypto:mac_final(
        MacUpdatedState6
    ),

    ok.

test_cmac_update_iolist() ->
    MacInitState0 = crypto:mac_init(
        cmac, aes_128_cbc, [<<0, 1, 2, 3>>, <<4, 5, 6, 7>>, <<8, 9, 10, 11, 12, 13, 14, 15>>]
    ),
    MacUpdatedState0 = crypto:mac_update(MacInitState0, <<"Data">>),
    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac_final(
        MacUpdatedState0
    ),

    Key16 = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    MacInitState1 = crypto:mac_init(cmac, aes_128_cbc, Key16),
    MacUpdatedState1 = crypto:mac_update(MacInitState1, [$D, <<"ata">>]),
    <<227, 175, 5, 166, 7, 167, 180, 81, 30, 6, 147, 30, 211, 6, 207, 186>> = crypto:mac_final(
        MacUpdatedState1
    ),

    ok.

test_mac_finalN() ->
    MacInitState0 = crypto:mac_init(hmac, sha256, <<"Hello">>),
    MacUpdatedState0 = crypto:mac_update(MacInitState0, <<"Data">>),
    Mac16 = crypto:mac_finalN(MacUpdatedState0, 16),
    16 = byte_size(Mac16),
    <<211, 117, 58, 171, 240, 87, 74, 125, 159, 217, 148, 133, 209, 234, 203, 27>> = Mac16,

    Key16 = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    MacInitState1 = crypto:mac_init(cmac, aes_128_cbc, Key16),
    MacUpdatedState1 = crypto:mac_update(MacInitState1, <<"Data">>),
    Mac8 = crypto:mac_finalN(MacUpdatedState1, 8),
    8 = byte_size(Mac8),
    <<227, 175, 5, 166, 7, 167, 180, 81>> = Mac8,

    ok.

test_mac_update_badarg() ->
    exp_err =
        try
            crypto:mac_init(?MODULE:get_bad(), sha256, <<"key">>)
        catch
            error:{badarg, {File1, Line1}, "Unknown mac algorithm"} when
                is_list(File1) and is_integer(Line1)
            ->
                exp_err
        end,

    exp_err =
        try
            crypto:mac_init(hmac, ?MODULE:get_bad(), <<"key">>)
        catch
            error:{badarg, {File2, Line2}, "Bad digest algorithm for HMAC"} when
                is_list(File2) and is_integer(Line2)
            ->
                exp_err
        end,

    exp_err =
        try
            crypto:mac_update(?MODULE:get_bad(), <<"Data">>)
        catch
            error:{badarg, {File3, Line3}, "Bad ref"} when
                is_list(File3) and is_integer(Line3)
            ->
                exp_err
        end,

    MacInitState = crypto:mac_init(hmac, sha256, <<"Hello">>),
    exp_err =
        try
            crypto:mac_update(MacInitState, ?MODULE:get_bad())
        catch
            error:{badarg, {File4, Line4}, "Bad text"} when
                is_list(File4) and is_integer(Line4)
            ->
                exp_err
        end,

    exp_err =
        try
            crypto:mac_final(?MODULE:get_bad())
        catch
            error:{badarg, {File5, Line5}, "Bad ref"} when
                is_list(File5) and is_integer(Line5)
            ->
                exp_err
        end,

    exp_err =
        try
            crypto:mac_finalN(?MODULE:get_bad(), 16)
        catch
            error:{badarg, {File6, Line6}, "Bad ref"} when
                is_list(File6) and is_integer(Line6)
            ->
                exp_err
        end,

    ok.

get_bad() -> foo.

expect_error(Fun) ->
    try Fun() of
        Res -> {unexpected, Res}
    catch
        C:R -> {C, R}
    end.
