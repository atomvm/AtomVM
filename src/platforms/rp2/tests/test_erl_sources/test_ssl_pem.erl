%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_ssl_pem).

-export([start/0]).

start() ->
    SSLConfig = ssl:nif_config_init(),
    ok = ssl:nif_config_defaults(SSLConfig, client, stream),
    ok = ssl:nif_conf_ca_chain(SSLConfig, [test_certificate()]),
    ok.

test_certificate() ->
    <<
        "-----BEGIN CERTIFICATE-----\n"
        "MIIBfTCCASSgAwIBAgIBATAKBggqhkjOPQQDAjAeMRwwGgYDVQQDDBNBdG9tVk0g\n"
        "UlAyIFBFTSB0ZXN0MB4XDTI2MDgzMDE5NTcxN1oXDTM2MDgyNzE5NTcxN1owHjEc\n"
        "MBoGA1UEAwwTQXRvbVZNIFJQMiBQRU0gdGVzdDBZMBMGByqGSM49AgEGCCqGSM49\n"
        "AwEHA0IABA0Ho+UApJ/P0dIVQ/U/3xoe3NOt9RtQygr/dkBbQRBtRsmfB1ZLD++m\n"
        "THcI37ZYmNcx32qi0Zflx3/94cxL1IijUzBRMB0GA1UdDgQWBBSaxKxJ6luhEOFC\n"
        "8RKaV9jB6O8l0DAfBgNVHSMEGDAWgBSaxKxJ6luhEOFC8RKaV9jB6O8l0DAPBgNV\n"
        "HRMBAf8EBTADAQH/MAoGCCqGSM49BAMCA0cAMEQCICds/6yiTQlL2j8P4sc2+aNi\n"
        "g4F0DLAC4cLExc9rOdLZAiBEXoZpIguvmOMs4iG5DwJMYBn9Tyvf20wD5InawCTs\n"
        "pw==\n"
        "-----END CERTIFICATE-----\n"
    >>.
