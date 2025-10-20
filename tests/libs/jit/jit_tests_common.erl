%% This file is part of AtomVM.
%%
%% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% Shared helper functions for JIT assembler tests

-module(jit_tests_common).

-export([asm/3]).

-include_lib("eunit/include/eunit.hrl").

%% Architecture-specific assembler validation
-spec asm(atom(), binary(), string()) -> binary().
asm(Arch, Bin, Str) ->
    case erlang:system_info(machine) of
        "ATOM" ->
            Bin;
        "BEAM" ->
            case find_binutils(Arch) of
                false ->
                    Bin;
                {ok, AsCmd, ObjdumpCmd} ->
                    % Use unique temporary files to avoid conflicts
                    TempBase = "jit_test_" ++ integer_to_list(erlang:unique_integer([positive])),
                    AsmFile = TempBase ++ ".S",
                    ObjFile = TempBase ++ ".o",
                    try
                        ok = file:write_file(AsmFile, get_asm_header(Arch) ++ Str ++ "\n"),
                        Cmd = lists:flatten(
                            io_lib:format(
                                "~s ~s -c ~s -o ~s && ~s -j .text -D ~s",
                                [AsCmd, get_as_flags(Arch), AsmFile, ObjFile, ObjdumpCmd, ObjFile]
                            )
                        ),
                        Dump = os:cmd(Cmd),
                        DumpBin = list_to_binary(Dump),
                        DumpLines = binary:split(DumpBin, <<"\n">>, [global]),
                        AsmBin = asm_lines(DumpLines, <<>>, Arch),
                        if
                            AsmBin =:= Bin ->
                                ok;
                            true ->
                                io:format(
                                    "-------------------------------------------\n" ++
                                        "~s\n" ++
                                        "-------------------------------------------\n",
                                    [Dump]
                                )
                        end,
                        ?assertEqual(AsmBin, Bin),
                        Bin
                    after
                        % Clean up temporary files
                        file:delete(AsmFile),
                        file:delete(ObjFile)
                    end
            end
    end.

%% Helper function to find available binutils for a given architecture
-spec find_binutils(atom()) -> {ok, string(), string()} | false.
find_binutils(Arch) ->
    ArchStr = atom_to_list(Arch),
    BinutilsList = [
        {ArchStr ++ "-esp-elf-as", ArchStr ++ "-esp-elf-objdump"},
        {ArchStr ++ "-unknown-elf-as", ArchStr ++ "-unknown-elf-objdump"},
        {ArchStr ++ "-elf-as", ArchStr ++ "-elf-objdump"},
        {ArchStr ++ "-none-eabi-as", ArchStr ++ "-none-eabi-objdump"},
        {ArchStr ++ "-linux-gnu-as", ArchStr ++ "-linux-gnu-objdump"}
    ],
    find_binutils_from_list(BinutilsList).

%% Private functions

%% Generic helper function to find binutils from a list
-spec find_binutils_from_list([{string(), string()}]) -> {ok, string(), string()} | false.
find_binutils_from_list([]) ->
    false;
find_binutils_from_list([{AsCmd, ObjdumpCmd} | Rest]) ->
    case os:cmd("which " ++ AsCmd) of
        [] ->
            find_binutils_from_list(Rest);
        _ ->
            {ok, AsCmd, ObjdumpCmd}
    end.

%% Get architecture-specific assembly file header
-spec get_asm_header(atom()) -> string().
get_asm_header(arm) ->
    ".arch armv6-m\n.thumb\n.syntax unified\n";
get_asm_header(aarch64) ->
    ".text\n";
get_asm_header(x86_64) ->
    ".text\n";
get_asm_header(riscv32) ->
    ".text\n".

%% Get architecture-specific assembler flags
-spec get_as_flags(atom()) -> string().
get_as_flags(arm) ->
    "";
get_as_flags(aarch64) ->
    "";
get_as_flags(x86_64) ->
    "--64";
get_as_flags(riscv32) ->
    "-march=rv32imac".

%% Parse objdump output lines and extract binary data
-spec asm_lines([binary()], binary(), atom()) -> binary().
asm_lines([<<" ", Tail/binary>> | T], Acc, Arch) ->
    case binary:split(Tail, <<":\t">>) of
        [_Offset, HexAndRest] ->
            case binary:split(HexAndRest, <<"\t">>) of
                [HexStr, _] ->
                    AssembledBin = hex_to_bin(HexStr, <<>>, Arch),
                    asm_lines(T, <<Acc/binary, AssembledBin/binary>>, Arch);
                [HexStr] ->
                    % Handle continuation lines with no tab (just hex bytes)
                    AssembledBin = hex_to_bin(HexStr, <<>>, Arch),
                    asm_lines(T, <<Acc/binary, AssembledBin/binary>>, Arch)
            end;
        _ ->
            asm_lines(T, Acc, Arch)
    end;
asm_lines([_OtherLine | T], Acc, Arch) ->
    asm_lines(T, Acc, Arch);
asm_lines([], Acc, _Arch) ->
    Acc.

%% Convert hex string to binary with robust parsing (handles edge cases)
-spec hex_to_bin(binary(), binary(), atom()) -> binary().
hex_to_bin(<<>>, Acc, _Arch) ->
    Acc;
hex_to_bin(<<" ", Tail/binary>>, Acc, Arch) ->
    hex_to_bin(Tail, Acc, Arch);
hex_to_bin(HexStr, Acc, Arch) ->
    case binary:split(HexStr, <<" ">>) of
        [HexChunk, Rest] ->
            NumBits = byte_size(HexChunk) * 4,
            HexVal = binary_to_integer(HexChunk, 16),
            % All architectures use little-endian encoding
            hex_to_bin(Rest, <<Acc/binary, HexVal:NumBits/little>>, Arch);
        [HexChunk] when byte_size(HexChunk) > 0 ->
            NumBits = byte_size(HexChunk) * 4,
            HexVal = binary_to_integer(HexChunk, 16),
            % All architectures use little-endian encoding
            <<Acc/binary, HexVal:NumBits/little>>
    end.
