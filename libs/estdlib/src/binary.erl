%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

%%-----------------------------------------------------------------------------
%% @doc An implementation of a subset of the Erlang/OTP binary interface.
%% @end
%%-----------------------------------------------------------------------------
-module(binary).

-export([at/2, part/3, split/2, split/3]).

%%-----------------------------------------------------------------------------
%% @param   Binary binary to get a byte from
%% @param   Index  0-based index of the byte to return
%% @returns value of the byte from the binary
%% @doc     Get a byte from a binary by index.
%% @end
%%-----------------------------------------------------------------------------
-spec at(Binary :: binary(), Index :: non_neg_integer()) -> byte().
at(_Binary, _Index) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Binary binary to extract a subbinary from
%% @param   Pos    0-based index of the subbinary to extract
%% @param   Len    length, in bytes, of the subbinary to extract.
%% @return a subbinary from Binary
%% @doc Get the part of a given binary.
%% A negative length can be passed to count bytes backwards.
%% @end
%%-----------------------------------------------------------------------------
-spec part(Binary :: binary(), Pos :: non_neg_integer(), Len :: integer()) -> binary().
part(_Binary, _Pos, _Len) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @equiv split(Binary, Pattern, [])
%% @param   Binary  binary to split
%% @param   Pattern pattern to perform the split
%% @return a list composed of one or two binaries
%% @doc Split a binary according to pattern.
%% If pattern is not found, returns a singleton list with the passed binary.
%% Unlike Erlang/OTP, pattern must be a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec split(Binary :: binary(), Pattern :: binary()) -> [binary()].
split(_Binary, _Pattern) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Binary  binary to split
%% @param   Pattern pattern to perform the split
%% @param   Options options for the split
%% @return a list composed of one or two binaries
%% @doc Split a binary according to pattern.
%% If pattern is not found, returns a singleton list with the passed binary.
%% Unlike Erlang/OTP, pattern must be a binary.
%% Only implemented option is `global'
%% @end
%%-----------------------------------------------------------------------------
-spec split(Binary :: binary(), Pattern :: binary(), Option :: [global]) -> [binary()].
split(_Binary, _Pattern, _Option) ->
    erlang:nif_error(undefined).
