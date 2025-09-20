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

-export([start/0, compile/3, atom_resolver/1, type_resolver/1]).

-include_lib("jit.hrl").

%% @doc Precompile BEAM files on command line
start() ->
    [Target, Dir | Files] = init:get_plain_arguments(),
    lists:foreach(fun(File) -> compile(Target, Dir, File) end, Files).

%% @doc Parse target string to extract base architecture and requested variant
%% Examples:
%%   "armv6m" -> {"armv6m", ?JIT_VARIANT_PIC}
%%   "armv6m+float32" -> {"armv6m", ?JIT_VARIANT_PIC + ?JIT_VARIANT_FLOAT32}
%%   "x86_64" -> {"x86_64", ?JIT_VARIANT_PIC}
parse_target(Target) ->
    case string:split(Target, "+", all) of
        [BaseTarget] ->
            {BaseTarget, ?JIT_VARIANT_PIC};
        [BaseTarget | Variants] ->
            RequestedVariant = lists:foldl(
                fun(Variant, Acc) ->
                    case Variant of
                        "float32" -> Acc + ?JIT_VARIANT_FLOAT32
                    end
                end,
                ?JIT_VARIANT_PIC,
                Variants
            ),
            {BaseTarget, RequestedVariant}
    end.

compile(Target, Dir, Path) ->
    try
        {ok, InitialBinary} = file:read_file(Path),
        {ok, _Module, InitialChunks} = beam_lib:all_chunks(InitialBinary),
        FilteredChunks0 = lists:keydelete("avmN", 1, InitialChunks),
        FilteredChunks = lists:keydelete("Code", 1, FilteredChunks0),
        {"Code", CodeChunk} = lists:keyfind("Code", 1, InitialChunks),
        {"AtU8", AtomChunk} = lists:keyfind("AtU8", 1, InitialChunks),
        AtomResolver = atom_resolver(AtomChunk),
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
        LiteralResolver = literal_resolver(LiteralsChunk),

        TypesChunk =
            case lists:keyfind("Type", 1, InitialChunks) of
                {"Type", TypesChunk0} ->
                    TypesChunk0;
                false ->
                    <<>>
            end,
        TypeResolver = type_resolver(TypesChunk),

        % Parse target to extract arch and variant
        {BaseTarget, RequestedVariant} = parse_target(Target),
        Backend = list_to_atom("jit_" ++ BaseTarget),

        Arch =
            case BaseTarget of
                "x86_64" -> ?JIT_ARCH_X86_64;
                "aarch64" -> ?JIT_ARCH_AARCH64;
                "armv6m" -> ?JIT_ARCH_ARMV6M;
                _ -> error({unsupported_target, Target})
            end,

        Stream0 = jit_stream_binary:new(0),
        <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> =
            CodeChunk,

        Stream1 = jit_stream_binary:append(
            Stream0, jit:beam_chunk_header(LabelsCount, Arch, RequestedVariant)
        ),

        Stream2 = Backend:new(RequestedVariant, jit_stream_binary, Stream1),
        {LabelsCount, Stream3} = jit:compile(
            CodeChunk, AtomResolver, LiteralResolver, TypeResolver, Backend, Stream2
        ),
        NativeCode = Backend:stream(Stream3),
        UpdatedChunks = FilteredChunks ++ [{"avmN", NativeCode}],

        {ok, Binary} = beam_lib:build_module(UpdatedChunks),
        Basename = filename:basename(Path),
        UpdatedFile = filename:join(Dir, Basename),
        ok = file:write_file(UpdatedFile, Binary)
    catch
        error:function_clause:S ->
            case S of
                [{jit, first_pass, [<<Opcode, _Rest/binary>> | _], _} | _] ->
                    io:format("Unimplemented opcode ~p (~s)\n", [Opcode, Path]);
                _ ->
                    io:format("Function clause error in ~s:~n", [Path]),
                    lists:foreach(
                        fun(Frame) -> io:format("  ~p~n", [Frame]) end, lists:sublist(S, 5)
                    ),
                    erlang:raise(error, function_clause, S)
            end;
        Class:Reason:Stack ->
            io:format("Error ~p:~p in ~s:~n", [Class, Reason, Path]),
            lists:foreach(fun(Frame) -> io:format("  ~p~n", [Frame]) end, lists:sublist(Stack, 5)),
            erlang:raise(Class, Reason, Stack)
    end.

atom_resolver(AtomChunk) ->
    Atoms = parse_atom_chunk(AtomChunk),
    fun(Index) -> lists:nth(Index, Atoms) end.

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

literal_resolver(LiteralsChunk) ->
    Literals = parse_literals_chunk(LiteralsChunk),
    fun(Index) -> lists:nth(Index + 1, Literals) end.

parse_literals_chunk(<<TermsCount:32, Rest/binary>>) ->
    parse_literals_chunk0(TermsCount, Rest, []);
parse_literals_chunk(<<>>) ->
    [].

parse_literals_chunk0(0, <<>>, Acc) ->
    lists:reverse(Acc);
parse_literals_chunk0(N, <<TermSize:32, TermBin:TermSize/binary, Rest/binary>>, Acc) ->
    Term = binary_to_term(TermBin),
    parse_literals_chunk0(N - 1, Rest, [Term | Acc]).

%% Version (from beam_types.hrl)
-define(BEAM_TYPES_VERSION, 3).

%% Type chunk constants (from beam_types.erl)
-define(BEAM_TYPE_ATOM, (1 bsl 0)).
-define(BEAM_TYPE_BITSTRING, (1 bsl 1)).
-define(BEAM_TYPE_CONS, (1 bsl 2)).
-define(BEAM_TYPE_FLOAT, (1 bsl 3)).
-define(BEAM_TYPE_FUN, (1 bsl 4)).
-define(BEAM_TYPE_INTEGER, (1 bsl 5)).
-define(BEAM_TYPE_MAP, (1 bsl 6)).
-define(BEAM_TYPE_NIL, (1 bsl 7)).
-define(BEAM_TYPE_PID, (1 bsl 8)).
-define(BEAM_TYPE_PORT, (1 bsl 9)).
-define(BEAM_TYPE_REFERENCE, (1 bsl 10)).
-define(BEAM_TYPE_TUPLE, (1 bsl 11)).

-define(BEAM_TYPE_HAS_LOWER_BOUND, (1 bsl 12)).
-define(BEAM_TYPE_HAS_UPPER_BOUND, (1 bsl 13)).
-define(BEAM_TYPE_HAS_UNIT, (1 bsl 14)).

type_resolver(<<Version:32, _Count:32, TypeData/binary>>) when Version =:= ?BEAM_TYPES_VERSION ->
    Types = parse_type_entries(TypeData, []),
    fun(Index) -> lists:nth(Index + 1, Types) end;
type_resolver(_) ->
    fun(_) -> any end.

parse_type_entries(<<>>, Acc) ->
    lists:reverse(Acc);
parse_type_entries(
    <<0:1, HasUnit:1, HasUpperBound:1, HasLowerBound:1, TypeBits:12, Rest0/binary>>, Acc
) ->
    {Rest, LowerBound, UpperBound, Unit} = parse_extra(
        HasLowerBound, HasUpperBound, HasUnit, Rest0, '-inf', '+inf', 1
    ),
    Type =
        case TypeBits of
            ?BEAM_TYPE_ATOM ->
                t_atom;
            ?BEAM_TYPE_BITSTRING ->
                {t_bs_matchable, Unit};
            ?BEAM_TYPE_CONS ->
                t_cons;
            ?BEAM_TYPE_FLOAT ->
                t_float;
            ?BEAM_TYPE_FUN ->
                t_fun;
            ?BEAM_TYPE_FLOAT bor ?BEAM_TYPE_INTEGER ->
                {t_number, {LowerBound, UpperBound}};
            ?BEAM_TYPE_INTEGER ->
                {t_integer, {LowerBound, UpperBound}};
            ?BEAM_TYPE_MAP ->
                t_map;
            ?BEAM_TYPE_NIL ->
                nil;
            ?BEAM_TYPE_NIL bor ?BEAM_TYPE_CONS ->
                t_list;
            ?BEAM_TYPE_PID ->
                pid;
            ?BEAM_TYPE_PORT ->
                port;
            ?BEAM_TYPE_REFERENCE ->
                reference;
            ?BEAM_TYPE_TUPLE ->
                t_tuple;
            _ ->
                any
        end,
    parse_type_entries(Rest, [Type | Acc]).

parse_extra(1, HasUpperBound, HasUnit, <<Value:64/signed, Rest/binary>>, '-inf', '+inf', 1) ->
    parse_extra(0, HasUpperBound, HasUnit, Rest, Value, '+inf', 1);
parse_extra(0, 1, HasUnit, <<Value:64/signed, Rest/binary>>, LowerBound, '+inf', 1) ->
    parse_extra(0, 0, HasUnit, Rest, LowerBound, Value, 1);
parse_extra(0, 0, 1, <<Value:8/unsigned, Rest/binary>>, LowerBound, UpperBound, 1) ->
    parse_extra(0, 0, 0, Rest, LowerBound, UpperBound, Value + 1);
parse_extra(0, 0, 0, Rest, LowerBound, UpperBound, Unit) ->
    {Rest, LowerBound, UpperBound, Unit}.
