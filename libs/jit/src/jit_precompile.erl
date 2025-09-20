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

-include("compact_term.hrl").

%% @doc Precompile BEAM files on command line
start() ->
    [Target, Dir | Files] = init:get_plain_arguments(),
    lists:foreach(fun(File) -> compile(Target, Dir, File) end, Files).

compile(Target, Dir, Path) ->
    try
        {ok, InitialBinary} = file:read_file(Path),
        {ok, Module, InitialChunks} = beam_lib:all_chunks(InitialBinary),
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

        % Parse line table (Line chunk) for DWARF line information
        LineResolver =
            case lists:keyfind("Line", 1, InitialChunks) of
                {"Line", LineTable} ->
                    fun(LineRef) -> resolve_line_info(Module, LineTable, LineRef) end;
                false ->
                    io:format("LineResolver -- Line chunk not found\n"),
                    % No line table - return false
                    fun(_LineRef) -> false end
            end,

        Backend = list_to_atom("jit_" ++ Target),

        Stream0 = jit_dwarf:new(Backend, Module, jit_stream_binary, 0, LineResolver),
        <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> =
            CodeChunk,

        Arch =
            case Target of
                "x86_64" -> ?JIT_ARCH_X86_64;
                "aarch64" -> ?JIT_ARCH_AARCH64;
                "armv6m" -> ?JIT_ARCH_ARMV6M
            end,

        Stream1 = Backend:new(?JIT_VARIANT_PIC, jit_dwarf, Stream0),
        {LabelsCount, Stream2} = jit:compile(
            CodeChunk, AtomResolver, LiteralResolver, Backend, Stream1
        ),
        DwarfStream = Backend:stream(Stream2),
        NativeCode = jit_dwarf:stream(DwarfStream),
        <<InfoSize:32, Info:InfoSize/binary>> = jit:beam_chunk_header(
            LabelsCount, Arch, ?JIT_VARIANT_PIC
        ),

        % Create chunks with embedded ELF when DWARF is enabled
        Basename = filename:basename(Path),
        NewChunks =
            case jit_dwarf:elf(DwarfStream, NativeCode) of
                false ->
                    % No debug info - just store native code
                    [{"avmN", <<InfoSize:32, Info:InfoSize/binary, NativeCode/binary>>}];
                {ok, TextSectionOffset, ELF} ->
                    % Update BEAM chunk header structure and combine with ELF.
                    EmbeddedElfChunk = update_avmn_chunk_with_elf(Info, ELF, TextSectionOffset),
                    [{"avmN", EmbeddedElfChunk}]
            end,
        UpdatedChunks = FilteredChunks ++ NewChunks,
        {ok, Binary} = beam_lib:build_module(UpdatedChunks),
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

%% @doc Update existing Info by updating offset
update_avmn_chunk_with_elf(Info, ElfBinary, TextSectionOffset) ->
    % Parse Info to update the offset: LabelsCount + Version + ArchCount + NativeCodeArch
    <<LabelsCount:32, Version:16, ArchCount:16, Arch:16, Variant:16, _OldOffset:32>> = Info,

    % Calculate new offset: from start of ELF to .text section
    NewOffset = TextSectionOffset,

    % Create updated Info with new offset
    UpdatedInfo = <<LabelsCount:32, Version:16, ArchCount:16, Arch:16, Variant:16, NewOffset:32>>,

    % Build updated chunk: InfoSize + UpdatedInfo + ELF
    <<(byte_size(UpdatedInfo)):32, UpdatedInfo/binary, ElfBinary/binary>>.

%% @doc Resolve a line reference to filename and line number
resolve_line_info(
    Module,
    <<Version:32, _Flags:32, _NumInstr:32, NumRefs:32, _NumFilenames:32, Rest/binary>>,
    LineRef
) when Version =:= 0, LineRef > 0, LineRef =< NumRefs ->
    resolve_line_info0(Module, 1, 0, LineRef, NumRefs, Rest, false);
resolve_line_info(_Module, <<Version:32, _/binary>>, _) when Version =/= 0 ->
    io:format("resolve_line_info -- unknown Line table version (~p)\n", [Version]),
    false;
resolve_line_info(
    _Module,
    <<_Version:32, _Flags:32, _NumInstr:32, _NumRefs:32, _NumFilenames:32, _Rest/binary>>,
    0
) ->
    false;
resolve_line_info(
    _Module,
    <<_Version:32, _Flags:32, _NumInstr:32, NumRefs:32, _NumFilenames:32, _Rest/binary>>,
    LineRef
) ->
    io:format("resolve_line_info -- invalid lineref (~p) (NumRefs = ~p)\n", [LineRef, NumRefs]),
    false.

resolve_line_info0(
    Module, CurrentLineRef, _CurrentLocationIx, _LineRef, NumRefs, LocationData, {Line, LocationIx}
) when CurrentLineRef > NumRefs ->
    resolve_line_info1(Module, LocationIx, LocationData, Line);
resolve_line_info0(
    Module,
    LineRef,
    CurrentLocationIx,
    LineRef,
    NumRefs,
    <<_:4, ?COMPACT_INTEGER:4, _/binary>> = Bin,
    false
) ->
    {Line, Rest} = jit:decode_value64(Bin),
    resolve_line_info0(
        Module, LineRef + 1, CurrentLocationIx, LineRef, NumRefs, Rest, {Line, CurrentLocationIx}
    );
resolve_line_info0(
    Module,
    CurrentLineRef,
    CurrentLocationIx,
    LineRef,
    NumRefs,
    <<_:4, ?COMPACT_INTEGER:4, _/binary>> = Bin,
    Acc
) ->
    {_Line, Rest} = jit:decode_value64(Bin),
    resolve_line_info0(Module, CurrentLineRef + 1, CurrentLocationIx, LineRef, NumRefs, Rest, Acc);
resolve_line_info0(
    Module,
    LineRef,
    CurrentLocationIx,
    LineRef,
    NumRefs,
    <<Val:3, ?COMPACT_LARGE_INTEGER_11BITS:5, NextByte, Rest/binary>>,
    false
) ->
    Line = (Val bsl 8) bor NextByte,
    resolve_line_info0(
        Module, LineRef + 1, CurrentLocationIx, LineRef, NumRefs, Rest, {Line, CurrentLocationIx}
    );
resolve_line_info0(
    Module,
    CurrentLineRef,
    CurrentLocationIx,
    LineRef,
    NumRefs,
    <<_Val:3, ?COMPACT_LARGE_INTEGER_11BITS:5, _NextByte, Rest/binary>>,
    Acc
) ->
    resolve_line_info0(Module, CurrentLineRef + 1, CurrentLocationIx, LineRef, NumRefs, Rest, Acc);
resolve_line_info0(
    Module,
    LineRef,
    CurrentLocationIx,
    LineRef,
    NumRefs,
    <<Size0:3, ?COMPACT_LARGE_INTEGER_NBITS:5, Line:(8 * (Size0 + 2))/signed, Rest/binary>>,
    false
) ->
    resolve_line_info0(
        Module, LineRef + 1, CurrentLocationIx, LineRef, NumRefs, Rest, {Line, CurrentLocationIx}
    );
resolve_line_info0(
    Module,
    CurrentLineRef,
    CurrentLocationIx,
    LineRef,
    NumRefs,
    <<Size0:3, ?COMPACT_LARGE_INTEGER_NBITS:5, _:(8 * (Size0 + 2))/signed, Rest/binary>>,
    Acc
) ->
    resolve_line_info0(Module, CurrentLineRef + 1, CurrentLocationIx, LineRef, NumRefs, Rest, Acc);
resolve_line_info0(
    Module,
    CurrentLineRef,
    _CurrentLocationIx,
    LineRef,
    NumRefs,
    <<_:4, AtomTag:4, _/binary>> = Bin,
    Acc
) when AtomTag =:= ?COMPACT_LARGE_ATOM; AtomTag =:= ?COMPACT_ATOM ->
    {NewLocationIx, Rest} = jit:decode_value64(Bin),
    resolve_line_info0(Module, CurrentLineRef, NewLocationIx, LineRef, NumRefs, Rest, Acc).

resolve_line_info1(Module, 0, _LocationData, Line) ->
    {ok, <<(atom_to_binary(Module, utf8))/binary, ".erl">>, Line};
resolve_line_info1(_Module, 1, <<Size:16, Filename:Size/binary, _/binary>>, Line) ->
    {ok, Filename, Line};
resolve_line_info1(Module, N, <<Size:16, _:Size/binary, Rest/binary>>, Line) ->
    resolve_line_info1(Module, N - 1, Rest, Line).
