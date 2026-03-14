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

-module(test_crypto_pbkdf2_hmac).
-export([
    start/0,
    get_bad/0
]).

start() ->
    case otp_version() =:= atomvm orelse erlang:function_exported(crypto, pbkdf2_hmac, 5) of
        true ->
            ok = test_pbkdf2_hmac_sha1_rfc6070_1(),
            ok = test_pbkdf2_hmac_sha1_rfc6070_2(),
            ok = test_pbkdf2_hmac_sha1_rfc6070_3(),
            ok = test_pbkdf2_hmac_sha1_rfc6070_4(),
            ok = test_pbkdf2_hmac_sha256_1(),
            ok = test_pbkdf2_hmac_sha256_2(),
            ok = test_result_is_binary(),
            ok = test_keylen_truncation(),
            ok = test_bad_digest(),
            ok = test_bad_pass(),
            ok = test_bad_salt(),
            ok = test_bad_iter(),
            ok = test_bad_keylen();
        false ->
            ok
    end,
    0.

otp_version() ->
    case erlang:system_info(machine) of
        "ATOM" -> atomvm;
        _ -> list_to_integer(erlang:system_info(otp_release))
    end.

test_pbkdf2_hmac_sha1_rfc6070_1() ->
    DK = crypto:pbkdf2_hmac(sha, <<"password">>, <<"salt">>, 1, 20),
    <<16#0C, 16#60, 16#C8, 16#0F, 16#96, 16#1F, 16#0E, 16#71, 16#F3, 16#A9, 16#B5, 16#24, 16#AF,
        16#60, 16#12, 16#06, 16#2F, 16#E0, 16#37, 16#A6>> = DK,
    ok.

test_pbkdf2_hmac_sha1_rfc6070_2() ->
    DK = crypto:pbkdf2_hmac(sha, <<"password">>, <<"salt">>, 2, 20),
    <<16#EA, 16#6C, 16#01, 16#4D, 16#C7, 16#2D, 16#6F, 16#8C, 16#CD, 16#1E, 16#D9, 16#2A, 16#CE,
        16#1D, 16#41, 16#F0, 16#D8, 16#DE, 16#89, 16#57>> = DK,
    ok.

test_pbkdf2_hmac_sha1_rfc6070_3() ->
    DK = crypto:pbkdf2_hmac(sha, <<"password">>, <<"salt">>, 4096, 20),
    <<16#4B, 16#00, 16#79, 16#01, 16#B7, 16#65, 16#48, 16#9A, 16#BE, 16#AD, 16#49, 16#D9, 16#26,
        16#F7, 16#21, 16#D0, 16#65, 16#A4, 16#29, 16#C1>> = DK,
    ok.

test_pbkdf2_hmac_sha1_rfc6070_4() ->
    DK = crypto:pbkdf2_hmac(
        sha,
        <<"passwordPASSWORDpassword">>,
        <<"saltSALTsaltSALTsaltSALTsaltSALTsalt">>,
        4096,
        25
    ),
    <<16#3D, 16#2E, 16#EC, 16#4F, 16#E4, 16#1C, 16#84, 16#9B, 16#80, 16#C8, 16#D8, 16#36, 16#62,
        16#C0, 16#E4, 16#4A, 16#8B, 16#29, 16#1A, 16#96, 16#4C, 16#F2, 16#F0, 16#70, 16#38>> = DK,
    ok.

test_pbkdf2_hmac_sha256_1() ->
    DK = crypto:pbkdf2_hmac(sha256, <<"passwd">>, <<"salt">>, 1, 64),
    <<16#55, 16#AC, 16#04, 16#6E, 16#56, 16#E3, 16#08, 16#9F, 16#EC, 16#16, 16#91, 16#C2, 16#25,
        16#44, 16#B6, 16#05, 16#F9, 16#41, 16#85, 16#21, 16#6D, 16#DE, 16#04, 16#65, 16#E6, 16#8B,
        16#9D, 16#57, 16#C2, 16#0D, 16#AC, 16#BC, 16#49, 16#CA, 16#9C, 16#CC, 16#F1, 16#79, 16#B6,
        16#45, 16#99, 16#16, 16#64, 16#B3, 16#9D, 16#77, 16#EF, 16#31, 16#7C, 16#71, 16#B8, 16#45,
        16#B1, 16#E3, 16#0B, 16#D5, 16#09, 16#11, 16#20, 16#41, 16#D3, 16#A1, 16#97, 16#83>> = DK,
    ok.

test_pbkdf2_hmac_sha256_2() ->
    DK = crypto:pbkdf2_hmac(sha256, <<"Password">>, <<"NaCl">>, 80000, 64),
    <<16#4D, 16#DC, 16#D8, 16#F6, 16#0B, 16#98, 16#BE, 16#21, 16#83, 16#0C, 16#EE, 16#5E, 16#F2,
        16#27, 16#01, 16#F9, 16#64, 16#1A, 16#44, 16#18, 16#D0, 16#4C, 16#04, 16#14, 16#AE, 16#FF,
        16#08, 16#87, 16#6B, 16#34, 16#AB, 16#56, 16#A1, 16#D4, 16#25, 16#A1, 16#22, 16#58, 16#33,
        16#54, 16#9A, 16#DB, 16#84, 16#1B, 16#51, 16#C9, 16#B3, 16#17, 16#6A, 16#27, 16#2B, 16#DE,
        16#BB, 16#A1, 16#D0, 16#78, 16#47, 16#8F, 16#62, 16#B3, 16#97, 16#F3, 16#3C, 16#8D>> = DK,
    ok.

test_result_is_binary() ->
    DK = crypto:pbkdf2_hmac(sha256, <<"pass">>, <<"salt">>, 1, 32),
    true = is_binary(DK),
    32 = byte_size(DK),
    ok.

test_keylen_truncation() ->
    DK16 = crypto:pbkdf2_hmac(sha256, <<"pass">>, <<"salt">>, 1, 16),
    DK32 = crypto:pbkdf2_hmac(sha256, <<"pass">>, <<"salt">>, 1, 32),
    16 = byte_size(DK16),
    32 = byte_size(DK32),
    <<Prefix:16/binary, _/binary>> = DK32,
    DK16 = Prefix,
    ok.

test_bad_digest() ->
    try
        crypto:pbkdf2_hmac(?MODULE:get_bad(), <<"password">>, <<"salt">>, 1, 20)
    catch
        error:_ ->
            exp_err
    end,
    ok.

test_bad_pass() ->
    try
        crypto:pbkdf2_hmac(sha256, ?MODULE:get_bad(), <<"salt">>, 1, 20)
    catch
        error:_ ->
            exp_err
    end,
    ok.

test_bad_salt() ->
    try
        crypto:pbkdf2_hmac(sha256, <<"password">>, ?MODULE:get_bad(), 1, 20)
    catch
        error:_ ->
            exp_err
    end,
    ok.

test_bad_iter() ->
    try
        crypto:pbkdf2_hmac(sha256, <<"password">>, <<"salt">>, 0, 20)
    catch
        error:_ ->
            exp_err
    end,
    ok.

test_bad_keylen() ->
    try
        crypto:pbkdf2_hmac(sha256, <<"password">>, <<"salt">>, 1, 0)
    catch
        error:_ ->
            exp_err
    end,
    ok.

get_bad() -> foo.
