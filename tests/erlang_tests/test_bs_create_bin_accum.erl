%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_bs_create_bin_accum).

-export([start/0]).

start() ->
    HasBSCreateBin =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(otp_release) >= "25";
            "ATOM" ->
                ?OTP_RELEASE >= 25
        end,
    ok =
        if
            HasBSCreateBin ->
                ok = test_int_utf8(),
                ok = test_int_float();
            true ->
                ok
        end,
    0.

test_int_utf8() ->
    %% <<42:8, 65/utf8>> = <<42, 65>>
    %% 65 = 'A', 1 byte in UTF-8
    <<42, 65>> = test_bs_create_bin_accum_asm:create_int_utf8(id(42), id(65), id(1)),
    %% <<42:16, 12450/utf8>> = <<0, 42, 227, 130, 162>>
    %% 12450 is a 3-byte UTF-8 character
    <<0, 42, 227, 130, 162>> = test_bs_create_bin_accum_asm:create_int_utf8(
        id(42), id(12450), id(2)
    ),
    ok.

test_int_float() ->
    %% <<42:8, 3.14:64/float>>
    Bin = test_bs_create_bin_accum_asm:create_int_float(id(42), id(3.14), id(64), id(1)),
    9 = byte_size(Bin),
    <<42, F:64/float>> = Bin,
    true = (F > 3.13),
    true = (F < 3.15),
    ok.

id(X) -> X.
