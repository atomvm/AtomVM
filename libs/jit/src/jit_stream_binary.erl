%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(jit_stream_binary).

% Stream implementation using plain binaries, fine on desktop with plenty of RAM.

-export([
    new/1,
    offset/1,
    append/2,
    replace/3,
    map/4,
    flush/1
]).

-export_type([stream/0]).

-type stream() :: binary().

%%-----------------------------------------------------------------------------
%% @returns An empty binary
%% @doc     Create a new stream, i.e. return an empty binary
%% @end
%%-----------------------------------------------------------------------------
-spec new(pos_integer()) -> stream().
new(_MaxSize) ->
    <<>>.

%%-----------------------------------------------------------------------------
%% @param Stream    stream to get the offset from
%% @returns The current offset
%% @doc     Get the current offset in the stream
%% @end
%%-----------------------------------------------------------------------------
-spec offset(stream()) -> non_neg_integer().
offset(Stream) ->
    byte_size(Stream).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to append to
%% @param Binary    binary to append to the stream
%% @returns The updated stream
%% @doc     Append a binary to the stream
%% @end
%%-----------------------------------------------------------------------------
-spec append(stream(), binary()) -> stream().
append(Stream, Binary) ->
    <<Stream/binary, Binary/binary>>.

%%-----------------------------------------------------------------------------
%% @param Stream        stream to update
%% @param Offset        offset to update from
%% @param Replacement   binary to write at offset
%% @returns The updated stream
%% @doc     Replace bytes at a given offset
%% @end
%%-----------------------------------------------------------------------------
-spec replace(stream(), non_neg_integer(), binary()) -> stream().
replace(Stream, Offset, Replacement) ->
    {Prefix, <<_Previous:(byte_size(Replacement))/binary, Suffix/binary>>} = split_binary(
        Stream, Offset
    ),
    <<Prefix/binary, Replacement/binary, Suffix/binary>>.

%%-----------------------------------------------------------------------------
%% @param Stream        stream to update
%% @param Offset        offset to update from
%% @param Length        length of the section to update
%% @param MapFunction   function that updates the binary
%% @returns The updated stream
%% @doc     Replace bytes at a given offset calling a map function
%% @end
%%-----------------------------------------------------------------------------
-spec map(stream(), non_neg_integer(), pos_integer(), fun((binary()) -> binary())) -> stream().
map(Stream, Offset, Length, MapFunction) ->
    {Prefix, <<Previous:Length/binary, Suffix/binary>>} = split_binary(Stream, Offset),
    Replacement = MapFunction(Previous),
    <<Prefix/binary, Replacement/binary, Suffix/binary>>.

%%-----------------------------------------------------------------------------
%% @param Stream        stream to flush
%% @returns The stream flushed
%% @doc     Flush the stream. NOP with binaries.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec flush(stream()) -> stream().
flush(Stream) ->
    Stream.
