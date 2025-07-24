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
-module(jit_precompile).

-export([start/0, compile/3]).

-include_lib("jit.hrl").

%% @doc Precompile BEAM files on command line
start() ->
    [Target, Dir | Files] = init:get_plain_arguments(),
    lists:foreach(fun(File) -> compile(Target, Dir, File) end, Files).

compile(Target, Dir, Path) ->
    try
        {ok, InitialBinary} = file:read_file(Path),
        {ok, _Module, InitialChunks} = beam_lib:all_chunks(InitialBinary),
        FilteredChunks0 = lists:keydelete("avmN", 1, InitialChunks),
        FilteredChunks = lists:keydelete("Code", 1, FilteredChunks0),
        {"Code", CodeChunk} = lists:keyfind("Code", 1, InitialChunks),
        {"AtU8", AtomChunk} = lists:keyfind("AtU8", 1, InitialChunks),
        Atoms = parse_atom_chunk(AtomChunk),
        AtomResolver = fun(Index) -> lists:nth(Index, Atoms) end,
        LiteralsChunk =
            case lists:keyfind("LitU", 1, InitialChunks) of
                {"LitU", LiteralsChunk0} ->
                    LiteralsChunk0;
                false ->
                    case lists:keyfind("LitT", 1, InitialChunks) of
                        {"LitT", <<0:32, LiteralsChunk0/binary>>} ->
                            LiteralsChunk0;
                        {"LitT", <<_UncompressedSize:32, LiteralsChunk0Compressed/binary>>} ->
                            zlib:uncompress(LiteralsChunk0Compressed);
                        false ->
                            <<>>
                    end
            end,
        Literals = parse_literals_chunk(LiteralsChunk),
        LiteralResolver = fun(Index) -> lists:nth(Index + 1, Literals) end,

        Stream0 = jit_stream_binary:new(0),
        <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> =
            CodeChunk,

        Arch =
            case Target of
                "x86_64" -> ?JIT_ARCH_X86_64;
                "aarch64" -> ?JIT_ARCH_AARCH64
            end,

        Stream1 = jit_stream_binary:append(
            Stream0, jit:beam_chunk_header(LabelsCount, Arch, ?JIT_VARIANT_PIC)
        ),
        Backend = list_to_atom("jit_" ++ Target),
        Stream2 = Backend:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),
        {LabelsCount, Stream3} = jit:compile(
            CodeChunk, AtomResolver, LiteralResolver, Backend, Stream2
        ),
        NativeCode = Backend:stream(Stream3),
        UpdatedChunks = FilteredChunks ++ [{"avmN", NativeCode}],
        {ok, Binary} = beam_lib:build_module(UpdatedChunks),
        Basename = filename:basename(Path),
        UpdatedFile = filename:join(Dir, Basename),
        ok = file:write_file(UpdatedFile, Binary)
    catch
        error:function_clause:S ->
            [{jit, first_pass, [<<Opcode, _Rest/binary>> | _], _} | _] = S,
            io:format("Unimplemented opcode ~p (~s)\n", [Opcode, Path])
    end.

parse_atom_chunk(<<AtomCount:32/signed, Rest/binary>>) ->
    if
        AtomCount < 0 ->
            parse_atom_chunk_long_format(Rest, []);
        true ->
            parse_atom_chunk_old_format(Rest, [])
    end.

parse_atom_chunk_long_format(<<Size:4, 0:1, _:3, Atom:Size/binary, Tail/binary>>, Acc) ->
    parse_atom_chunk_long_format(Tail, [binary_to_atom(Atom, utf8) | Acc]);
parse_atom_chunk_long_format(
    <<SizeH:3, 0:1, _:4, SizeL, Atom:((SizeH bsl 8) bor SizeL)/binary, Tail/binary>>, Acc
) ->
    parse_atom_chunk_long_format(Tail, [binary_to_atom(Atom, utf8) | Acc]);
parse_atom_chunk_long_format(<<>>, Acc) ->
    lists:reverse(Acc).

parse_atom_chunk_old_format(<<Size, Atom:Size/binary, Tail/binary>>, Acc) ->
    parse_atom_chunk_old_format(Tail, [binary_to_atom(Atom, utf8) | Acc]);
parse_atom_chunk_old_format(<<>>, Acc) ->
    lists:reverse(Acc).

parse_literals_chunk(<<TermsCount:32, Rest/binary>>) ->
    parse_literals_chunk0(TermsCount, Rest, []);
parse_literals_chunk(<<>>) ->
    [].

parse_literals_chunk0(0, <<>>, Acc) ->
    lists:reverse(Acc);
parse_literals_chunk0(N, <<TermSize:32, TermBin:TermSize/binary, Rest/binary>>, Acc) ->
    Term = binary_to_term(TermBin),
    parse_literals_chunk0(N - 1, Rest, [Term | Acc]).
