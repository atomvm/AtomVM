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
-export([start/0, test_generate_and_compute_key/0]).

start() ->
    ok = mbedtls_conditional_run(test_generate_and_compute_key, 16#03000000),
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
