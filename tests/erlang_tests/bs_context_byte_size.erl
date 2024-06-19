%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(bs_context_byte_size).

-export([start/0, decode/2, decode_bit_size/2]).

start() ->
    {0, <<>>} = decode(0, undefined),
    {0, <<1>>} = decode(
        42, <<0, 0, 0, 0, 0, 0, 0, 0, 1>>
    ),
    {0, <<1, 2>>} = decode(
        42, <<0, 0, 0, 0, 0, 0, 0, 0, 1, 2>>
    ),
    {42, <<1, 2, 3>>} = decode(
        42, <<0, 0, 0, 0, 0, 0, 0, 42, 1, 2, 3>>
    ),
    {42, <<1, 2, 3, 4, 5, 6, 7, 8>>} = decode(
        42, <<0, 0, 0, 0, 0, 0, 0, 42, 1, 2, 3, 4, 5, 6, 7, 8>>
    ),
    {42, <<>>} = decode(
        42, <<0, 0, 0, 0, 0, 0, 0, 42, 1, 2, 3, 4, 5, 6, 7, 8, 9>>
    ),
    {42, <<1, 2, 3, 4, 5, 6, 7, 8>>} = decode_bit_size(
        42, <<0, 0, 0, 0, 0, 0, 0, 42, 1, 2, 3, 4, 5, 6, 7, 8>>
    ),
    {42, <<>>} = decode_bit_size(
        42, <<0, 0, 0, 0, 0, 0, 0, 42, 1, 2, 3, 4, 5, 6, 7, 8, 9>>
    ),
    {42, <<>>} = decode(
        42, <<0, 0, 0, 0, 0, 0, 0, 43, 1, 2, 3>>
    ),
    0.

decode(Now, <<BootTime:64, Bin/binary>>) when BootTime =< Now andalso byte_size(Bin) =< 8 ->
    {BootTime, Bin};
decode(Now, _) ->
    {Now, <<>>}.

decode_bit_size(Now, <<BootTime:64, Bin/binary>>) when BootTime =< Now andalso bit_size(Bin) < 65 ->
    {BootTime, Bin};
decode_bit_size(Now, _) ->
    {Now, <<>>}.
