%
% This file is part of AtomVM.
%
% Copyright 2024 Tomasz Sobkiewicz <tomasz.sobkiewicz@swmansion.com>
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

-module(test_zlib_compress).

-export([start/0]).

start() ->
    ok = compress_binary(),
    ok = compress_iolist(),
    ok = bad_inputs(),
    0.

compress_binary() ->
    ToCompress =
        <<"Actually, this sentence could be a bit shorter. We definitelly need to compress it.">>,
    % Data from original erlang implementation
    ProperlyCompressed =
        <<120, 156, 13, 203, 203, 13, 128, 32, 16, 4, 208, 86, 166, 0, 67, 15, 86, 225, 153, 207,
            24, 54, 65, 48, 236, 112, 176, 123, 185, 191, 119, 102, 173, 216, 218, 119, 64, 213, 28,
            206, 46, 246, 76, 228, 177, 90, 65, 34, 34, 146, 9, 94, 199, 20, 103, 192, 69, 20, 222,
            214, 77, 220, 11, 157, 44, 208, 216, 252, 121, 39, 221, 97, 10, 63, 241, 84, 30, 23>>,
    ProperlyCompressed = zlib:compress(ToCompress),
    ok.

compress_iolist() ->
    ToCompressList = [
        <<"Actually,">>,
        " this sentence ",
        <<"could be ">>,
        [[<<"a ">>, [<<"b">>, <<"i">>]], <<"t">>],
        <<" shorter. ">>,
        [<<"we ">>],
        <<"definitely need ">>,
        "to compress ",
        <<"it.">>
    ],
    % Data from original erlang implementation
    ProperlyCompressed =
        <<120, 156, 13, 202, 219, 13, 128, 32, 12, 5, 208, 85, 238, 0, 134, 29, 28, 133, 199, 53,
            52, 169, 197, 208, 18, 195, 246, 122, 190, 207, 89, 99, 101, 213, 125, 32, 186, 56, 156,
            22, 180, 74, 212, 177, 180, 161, 16, 25, 69, 2, 222, 199, 12, 206, 132, 151, 104, 188,
            196, 36, 168, 27, 70, 54, 196, 248, 247, 253, 76, 186, 67, 34, 125, 214, 36, 29, 203>>,
    ProperlyCompressed = zlib:compress(ToCompressList),
    
    true = is_binary(zlib:compress([])),
    true = is_binary(zlib:compress([[]])),
    true = is_binary(zlib:compress([[],[]])),
    true = is_binary(zlib:compress([[]|<<>>])),
    ok.

bad_inputs() ->
    ok = raises(badarg, fun() -> zlib:compress(1) end),
    ok = raises(badarg, fun() -> zlib:compress([{}]) end),
    ok = raises(badarg, fun() -> zlib:compress([1024]) end),
    ok.

raises(Error, F) ->
    try F() of
        V ->
            {unexpected, V}
    catch
        error:Error -> ok;
        C:E -> {unexpected, C, E}
    end.
