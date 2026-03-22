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

-export([asm/3, assert_stream/3, assert_stream/5]).

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
    case erlang:system_info(machine) of
        "ATOM" ->
            false;
        _ ->
            find_binutils_beam(Arch)
    end.

find_binutils_beam(Arch) ->
    Prefixes0 = toolchain_prefixes(Arch),
    Prefixes =
        case Arch of
            riscv32 ->
                Prefixes0 ++ toolchain_prefixes(riscv64);
            _ ->
                Prefixes0
        end,
    find_binutils_from_list([{P ++ "-as", P ++ "-objdump"} || P <- Prefixes]).

%% Private functions

-spec toolchain_prefixes(atom()) -> [string()].
toolchain_prefixes(Arch) ->
    ArchStr = atom_to_list(Arch),
    Variants = ["-esp-elf", "-unknown-elf", "-elf", "-none-eabi", "-linux-gnu"],
    [ArchStr ++ V || V <- Variants].

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
    ".text\n";
get_asm_header(riscv64) ->
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
    "-march=rv32imac";
get_as_flags(riscv64) ->
    "-march=rv64imac -mabi=lp64".

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

%% Assert that Stream matches the expected objdump output Dump.
%% On mismatch, run objdump on both expected and actual binaries and diff -u.
-spec assert_stream(atom(), binary(), binary()) -> ok.
assert_stream(Arch, Dump, Stream) ->
    Expected = dump_to_bin(Dump),
    case Expected =:= Stream of
        true ->
            ok;
        false ->
            diff_disasm(Arch, Expected, Stream),
            ?assertEqual(Expected, Stream)
    end.

%% Assert that Stream matches the expected objdump output Dump.
%% On BEAM, if UPDATE_JIT_TESTS environment variable is set, update the source
%% file with the correct objdump output instead of failing.
-spec assert_stream(atom(), binary(), binary(), string(), pos_integer()) -> ok.
assert_stream(Arch, Dump, Stream, File, _Line) ->
    Expected = dump_to_bin(Dump),
    case Expected =:= Stream of
        true ->
            ok;
        false ->
            case {erlang:system_info(machine), os:getenv("UPDATE_JIT_TESTS")} of
                {"BEAM", Value} when Value =/= false ->
                    update_test_source(Arch, Dump, Stream, File);
                _ ->
                    diff_disasm(Arch, Expected, Stream),
                    ?assertEqual(Expected, Stream)
            end
    end.

-define(IS_HEX_DIGIT(C),
    ((C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) orelse (C >= $A andalso C =< $F))
).

%% Parse an objdump text dump into binary.
%% Handles all architectures: hex chunks (bytes or words) are parsed as
%% little-endian values. Hex groups are separated by single spaces;
%% the instruction text starts after a tab or two consecutive spaces.
-spec dump_to_bin(binary()) -> binary().
dump_to_bin(Dump) ->
    dump_to_bin(Dump, addr, [], []).

dump_to_bin(<<$:, Tail/binary>>, addr, [], Acc) ->
    dump_to_bin(Tail, pre_hex, [], Acc);
dump_to_bin(<<$\n, Tail/binary>>, addr, [], Acc) ->
    dump_to_bin(Tail, addr, [], Acc);
dump_to_bin(<<_, Tail/binary>>, addr, [], Acc) ->
    dump_to_bin(Tail, addr, [], Acc);
dump_to_bin(<<$\s, Tail/binary>>, pre_hex, [], Acc) ->
    dump_to_bin(Tail, pre_hex, [], Acc);
dump_to_bin(<<$\t, Tail/binary>>, pre_hex, [], Acc) ->
    dump_to_bin(Tail, pre_hex, [], Acc);
dump_to_bin(<<$\n, Tail/binary>>, pre_hex, [], Acc) ->
    dump_to_bin(Tail, addr, [], Acc);
dump_to_bin(<<C, Tail/binary>>, pre_hex, [], Acc) when ?IS_HEX_DIGIT(C) ->
    dump_to_bin(Tail, hex, [C], Acc);
dump_to_bin(<<C, Tail/binary>>, hex, Chunk, Acc) when ?IS_HEX_DIGIT(C) ->
    dump_to_bin(Tail, hex, [C | Chunk], Acc);
dump_to_bin(<<$\s, Tail/binary>>, hex, Chunk, Acc) ->
    dump_to_bin(Tail, hex_space, Chunk, Acc);
dump_to_bin(<<$\t, Tail/binary>>, hex, Chunk, Acc) ->
    dump_to_bin(Tail, instr, [], [emit_chunk(Chunk) | Acc]);
dump_to_bin(<<$\n, Tail/binary>>, hex, Chunk, Acc) ->
    dump_to_bin(Tail, addr, [], [emit_chunk(Chunk) | Acc]);
dump_to_bin(<<C, Tail/binary>>, hex_space, Chunk, Acc) when ?IS_HEX_DIGIT(C) ->
    dump_to_bin(Tail, hex, [C], [emit_chunk(Chunk) | Acc]);
dump_to_bin(<<$\n, Tail/binary>>, hex_space, Chunk, Acc) ->
    dump_to_bin(Tail, addr, [], [emit_chunk(Chunk) | Acc]);
dump_to_bin(<<_, Tail/binary>>, hex_space, Chunk, Acc) ->
    dump_to_bin(Tail, instr, [], [emit_chunk(Chunk) | Acc]);
dump_to_bin(<<$\n, Tail/binary>>, instr, [], Acc) ->
    dump_to_bin(Tail, addr, [], Acc);
dump_to_bin(<<_, Tail/binary>>, instr, [], Acc) ->
    dump_to_bin(Tail, instr, [], Acc);
dump_to_bin(<<>>, _, Chunk, Acc) ->
    FinalAcc =
        case Chunk of
            [] -> Acc;
            _ -> [emit_chunk(Chunk) | Acc]
        end,
    list_to_binary(lists:reverse(FinalAcc)).

emit_chunk(RevChars) ->
    Chars = lists:reverse(RevChars),
    NumBits = length(Chars) * 4,
    Val = list_to_integer(Chars, 16),
    <<Val:NumBits/little>>.

%% Run objdump on both expected and actual binaries, then diff -u the outputs.
-spec diff_disasm(atom(), binary(), binary()) -> ok.
diff_disasm(Arch, Expected, Actual) ->
    case find_binutils(Arch) of
        false ->
            io:format("expected bytes: ~w~nactual bytes:   ~w~n", [Expected, Actual]);
        {ok, _AsCmd, ObjdumpCmd} ->
            TempBase = "jit_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            ExpFile = TempBase ++ "_expected.bin",
            ActFile = TempBase ++ "_actual.bin",
            ExpDis = TempBase ++ "_expected.dis",
            ActDis = TempBase ++ "_actual.dis",
            ObjdumpFlags = get_objdump_flags(Arch),
            try
                ok = file:write_file(ExpFile, Expected),
                ok = file:write_file(ActFile, Actual),
                Cleanup =
                    "grep '^ '"
                    " | sed -e 's/[[:space:]]*;.*$//' -e 's/[[:space:]]*\\/\\/.*$//'",
                ObjdumpCmdExp = lists:flatten(
                    io_lib:format(
                        "~s -b binary ~s -D -z ~s | ~s > ~s",
                        [ObjdumpCmd, ObjdumpFlags, ExpFile, Cleanup, ExpDis]
                    )
                ),
                ObjdumpCmdAct = lists:flatten(
                    io_lib:format(
                        "~s -b binary ~s -D -z ~s | ~s > ~s",
                        [ObjdumpCmd, ObjdumpFlags, ActFile, Cleanup, ActDis]
                    )
                ),
                os:cmd(ObjdumpCmdExp),
                os:cmd(ObjdumpCmdAct),
                DiffCmd = lists:flatten(
                    io_lib:format("diff -u -w ~s ~s | tail -n +3", [ExpDis, ActDis])
                ),
                DiffResult = os:cmd(DiffCmd),
                io:format("~s~n", [DiffResult])
            after
                file:delete(ExpFile),
                file:delete(ActFile),
                file:delete(ExpDis),
                file:delete(ActDis)
            end,
            ok
    end.

-spec get_objdump_flags(atom()) -> string().
get_objdump_flags(arm) ->
    "-marm --disassembler-options=force-thumb";
get_objdump_flags(aarch64) ->
    "-m aarch64";
get_objdump_flags(x86_64) ->
    "-m i386:x86-64";
get_objdump_flags(riscv32) ->
    "-m riscv:rv32";
get_objdump_flags(riscv64) ->
    "-m riscv:rv64".

%% Update the test source file when a stream assertion fails.
-spec update_test_source(atom(), binary(), binary(), string()) -> ok.
update_test_source(Arch, OldDump, ActualStream, File) ->
    case find_binutils(Arch) of
        false ->
            io:format("Cannot update: no binutils found for ~p~n", [Arch]);
        {ok, _AsCmd, ObjdumpCmd} ->
            NewDump = disassemble_stream(ObjdumpCmd, Arch, ActualStream),
            replace_dump_in_source(File, OldDump, NewDump)
    end.

%% Disassemble a binary stream into the objdump format used by test dumps.
-spec disassemble_stream(string(), atom(), binary()) -> string().
disassemble_stream(ObjdumpCmd, Arch, Stream) ->
    TmpFile = "jit_update_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".bin",
    try
        ok = file:write_file(TmpFile, Stream),
        ObjdumpFlags = get_objdump_flags(Arch),
        Cleanup =
            "grep '^ '"
            " | sed -e 's/[[:space:]]*;.*$//' -e 's/[[:space:]]*\\/\\/.*$//'",
        Cmd = lists:flatten(
            io_lib:format(
                "~s -b binary ~s -D -z ~s | ~s",
                [ObjdumpCmd, ObjdumpFlags, TmpFile, Cleanup]
            )
        ),
        os:cmd(Cmd)
    after
        file:delete(TmpFile)
    end.

%% Replace the Dump = <<...>> block in a source file by scanning the source
%% with erl_scan, finding the binary whose string content matches OldDump,
%% and replacing it with the new objdump output.
-spec replace_dump_in_source(string(), binary(), string()) -> ok.
replace_dump_in_source(File, OldDump, NewDumpRaw) ->
    {ok, Content} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), 1, [return]),
    %% Find the token range for the binary matching OldDump
    case find_dump_tokens(Tokens, OldDump) of
        {StartLine, EndLine} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            %% Get indent from the first string line (StartLine + 1 is first content)
            FirstContentLine = lists:nth(StartLine + 1, Lines),
            Indent = get_indent(FirstContentLine),
            NewDumpLines = format_dump_lines(NewDumpRaw, Indent),
            %% Replace: keep lines up to StartLine (the << line),
            %% insert new content, keep from EndLine (the >> line) onward
            Before = lists:sublist(Lines, 1, StartLine),
            After = lists:nthtail(EndLine - 1, Lines),
            NewLines = Before ++ NewDumpLines ++ After,
            NewContent = iolist_to_binary(lists:join(<<"\n">>, NewLines)),
            ok = file:write_file(File, NewContent),
            io:format("Updated ~s at line ~p~n", [File, StartLine]),
            ok;
        not_found ->
            io:format("WARNING: Could not find old dump in ~s~n", [File]),
            ok
    end.

%% Scan tokens to find a binary expression (between << and >>) whose
%% concatenated string content equals OldDump.
find_dump_tokens(Tokens, OldDump) ->
    find_dump_tokens(Tokens, OldDump, []).

find_dump_tokens([], _OldDump, _) ->
    not_found;
find_dump_tokens([{'<<', Anno} | Rest], OldDump, _) ->
    StartLine = erl_anno:line(Anno),
    case collect_bin_strings(Rest, []) of
        {Strings, [{'>>', EndAnno} | Tail]} ->
            EndLine = erl_anno:line(EndAnno),
            BinContent = list_to_binary(Strings),
            case BinContent =:= OldDump of
                true -> {StartLine, EndLine};
                false -> find_dump_tokens(Tail, OldDump, [])
            end;
        _ ->
            find_dump_tokens(Rest, OldDump, [])
    end;
find_dump_tokens([_ | Rest], OldDump, Acc) ->
    find_dump_tokens(Rest, OldDump, Acc).

%% Collect consecutive string tokens from inside a << >> expression,
%% skipping whitespace and comment tokens between strings.
collect_bin_strings([{string, _, S} | Rest], Acc) ->
    collect_bin_strings(Rest, [S | Acc]);
collect_bin_strings([{white_space, _, _} | Rest], Acc) ->
    collect_bin_strings(Rest, Acc);
collect_bin_strings([{comment, _, _} | Rest], Acc) ->
    collect_bin_strings(Rest, Acc);
collect_bin_strings(Rest, Acc) ->
    {lists:reverse(Acc), Rest}.

%% Get leading whitespace from a binary line
get_indent(Line) ->
    get_indent(Line, <<>>).

get_indent(<<C, Rest/binary>>, Acc) when C =:= $\s; C =:= $\t ->
    get_indent(Rest, <<Acc/binary, C>>);
get_indent(_, Acc) ->
    Acc.

%% Format objdump output into dump lines with proper indentation and quoting.
format_dump_lines(DumpRaw, Indent) ->
    RawLines0 = string:split(DumpRaw, "\n", all),
    % Remove empty trailing lines
    RawLines = lists:reverse(
        lists:dropwhile(
            fun(L) -> string:trim(L) =:= "" end,
            lists:reverse(RawLines0)
        )
    ),
    format_dump_lines(RawLines, Indent, []).

format_dump_lines([], _Indent, Acc) ->
    lists:reverse(Acc);
format_dump_lines([Line], Indent, Acc) ->
    %% Last line: no trailing \n
    Formatted = iolist_to_binary([Indent, $", Line, $"]),
    lists:reverse([Formatted | Acc]);
format_dump_lines([Line | Rest], Indent, Acc) ->
    Formatted = iolist_to_binary([Indent, $", Line, "\\n", $"]),
    format_dump_lines(Rest, Indent, [Formatted | Acc]).
