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
asm(wasm32, Bin, Str) ->
    case erlang:system_info(machine) of
        "ATOM" ->
            Bin;
        "BEAM" ->
            case find_wat2wasm() of
                false ->
                    Bin;
                {ok, AsCmd} ->
                    TempBase = "jit_test_" ++ integer_to_list(erlang:unique_integer([positive])),
                    WatFile = TempBase ++ ".wat",
                    WasmFile = TempBase ++ ".wasm",
                    try
                        ok = file:write_file(
                            WatFile,
                            get_asm_header(wasm32) ++ Str ++ "\n" ++ get_asm_footer(wasm32)
                        ),
                        os:cmd(AsCmd ++ " " ++ WatFile ++ " -o " ++ WasmFile),
                        case file:read_file(WasmFile) of
                            {ok, WasmBin} ->
                                AllCodeBytes = wasm_code_bytes(WasmBin),
                                %% Strip trailing end (0x0b) added implicitly by wat2wasm
                                AsmBin = binary:part(AllCodeBytes, 0, byte_size(AllCodeBytes) - 1),
                                if
                                    AsmBin =:= Bin ->
                                        ok;
                                    true ->
                                        io:format(
                                            "expected: ~w~nactual:   ~w~n",
                                            [Bin, AsmBin]
                                        )
                                end,
                                ?assertEqual(AsmBin, Bin),
                                Bin;
                            {error, _} ->
                                io:format("wat2wasm failed for: ~s~n", [Str]),
                                Bin
                        end
                    after
                        file:delete(WatFile),
                        file:delete(WasmFile)
                    end
            end
    end;
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
                    {AsmFile, ObjFile} = asm_file_exts(Arch, TempBase),
                    try
                        ok = file:write_file(
                            AsmFile,
                            get_asm_header(Arch) ++ Str ++ "\n" ++ get_asm_footer(Arch)
                        ),
                        Cmd = asm_cmd(Arch, AsCmd, AsmFile, ObjdumpCmd, ObjFile),
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

find_binutils_beam(wasm32) ->
    %% diff_disasm and update_test_source both need wasm-objdump; check for it explicitly.
    case {os:cmd("which wat2wasm"), os:cmd("which wasm-objdump")} of
        {[], _} -> false;
        {_, []} -> false;
        _ -> {ok, "wat2wasm", "wasm-objdump"}
    end;
find_binutils_beam(Arch) ->
    Prefixes0 = toolchain_prefixes(Arch),
    Prefixes =
        case Arch of
            riscv32 ->
                Prefixes0 ++ toolchain_prefixes(riscv64);
            arm_thumb2 ->
                Prefixes0 ++ toolchain_prefixes(arm);
            _ ->
                Prefixes0
        end,
    find_binutils_from_list(Arch, [{P ++ "-as", P ++ "-objdump"} || P <- Prefixes]).

%% Private functions

-spec toolchain_prefixes(atom()) -> [string()].
toolchain_prefixes(arm32) ->
    toolchain_prefixes(arm);
toolchain_prefixes(xtensa) ->
    %% Prefer the ESP32-specific LE toolchain over the generic BE one.
    %% The generic xtensa-esp-elf toolchain defaults to big-endian ELF,
    %% whose objdump displays raw bytes rather than instruction words,
    %% breaking hex_to_bin/3's little-endian conversion.
    ["xtensa-esp32-elf", "xtensa-esp32s2-elf", "xtensa-esp32s3-elf"] ++
        ["xtensa" ++ V || V <- ["-unknown-elf", "-elf", "-linux-gnu"]] ++
        ["xtensa-lx6-linux-gnu"];
toolchain_prefixes(Arch) ->
    ArchStr = atom_to_list(Arch),
    Variants = [
        "-esp-elf",
        "-unknown-elf",
        "-elf",
        "-none-eabi",
        "-linux",
        "-linux-gnu",
        "-linux-gnueabihf",
        "-buildroot-linux-uclibc"
    ],
    [ArchStr ++ V || V <- Variants].

%% Find wat2wasm for wasm32 asm() cross-validation (does not need wasm-objdump).
-spec find_wat2wasm() -> {ok, string()} | false.
find_wat2wasm() ->
    case os:cmd("which wat2wasm") of
        [] -> false;
        _ -> {ok, "wat2wasm"}
    end.

%% Generic helper function to find binutils from a list
-spec find_binutils_from_list(atom(), [{string(), string()}]) -> {ok, string(), string()} | false.
find_binutils_from_list(_Arch, []) ->
    false;
find_binutils_from_list(Arch, [{AsCmd, ObjdumpCmd} | Rest]) ->
    case os:cmd("which " ++ AsCmd) of
        [] ->
            find_binutils_from_list(Arch, Rest);
        _ ->
            case is_as_version_buggy(Arch, AsCmd) of
                false -> {ok, AsCmd, ObjdumpCmd};
                true -> find_binutils_from_list(Arch, Rest)
            end
    end.

%% Check if assembler has a buggy version that doesn't encode certain instructions correctly.
%% For riscv32-as: binutils version 2.40 and lower have a bug encoding jal instruction.
-spec is_as_version_buggy(atom(), string()) -> boolean().
is_as_version_buggy(riscv32, AsCmd) ->
    VersionOutput = os:cmd(AsCmd ++ " --version"),
    case parse_binutils_version(VersionOutput) of
        Version when Version =< {2, 40} ->
            io:format("Skipping ~s version ~p (buggy for riscv32 jal encoding)~n", [
                AsCmd, Version
            ]),
            true;
        _ ->
            false
    end;
is_as_version_buggy(_, _AsCmd) ->
    false.

%% Parse binutils version from the first line of --version output.
%% Expected format: "GNU assembler (GNU Binutils) 2.40.0"
%% Returns a {Major, Minor} tuple for comparison.
-spec parse_binutils_version(string()) -> {non_neg_integer(), non_neg_integer()}.
parse_binutils_version(VersionOutput) ->
    case binary:split(list_to_binary(VersionOutput), <<"\n">>) of
        [FirstLine | _] ->
            %% Look for version pattern like "2.40" in "GNU assembler (GNU Binutils) 2.40.0"
            %% Match digits.digits after a space
            case
                re:run(
                    FirstLine,
                    <<" ([0-9]+)\\.([0-9]+)">>,
                    [{capture, all_but_first, binary}]
                )
            of
                {match, [Major, Minor]} ->
                    {
                        binary_to_integer(Major),
                        binary_to_integer(Minor)
                    };
                _ ->
                    %% If we can't parse, assume a high version (not buggy)
                    {infinity, infinity}
            end;
        _ ->
            {infinity, infinity}
    end.

%% Get architecture-specific assembly file header
-spec get_asm_header(atom()) -> string().
get_asm_header(arm) ->
    ".arch armv6-m\n.thumb\n.syntax unified\n";
get_asm_header(arm32) ->
    ".arch armv6\n.arm\n.syntax unified\n";
get_asm_header(arm_thumb2) ->
    ".arch armv7-m\n.thumb\n.syntax unified\n";
get_asm_header(aarch64) ->
    ".text\n";
get_asm_header(x86_64) ->
    ".text\n";
get_asm_header(riscv32) ->
    ".text\n";
get_asm_header(riscv64) ->
    ".text\n";
get_asm_header(wasm32) ->
    %% Include a memory so that memory instruction tests don't need extra module context.
    "(module\n  (memory 1)\n  (func\n";
get_asm_header(xtensa) ->
    ".text\n".

-spec get_asm_footer(atom()) -> string().
get_asm_footer(wasm32) ->
    "  ))\n";
get_asm_footer(_Arch) ->
    "".

%% Get architecture-specific assembler flags
-spec get_as_flags(atom()) -> string().
get_as_flags(arm) ->
    "";
get_as_flags(arm32) ->
    "";
get_as_flags(arm_thumb2) ->
    "";
get_as_flags(aarch64) ->
    "";
get_as_flags(x86_64) ->
    "--64";
get_as_flags(riscv32) ->
    "-march=rv32imac";
get_as_flags(riscv64) ->
    "-march=rv64imac -mabi=lp64";
get_as_flags(xtensa) ->
    "--no-transform".

%% File extensions for assembler input/output
-spec asm_file_exts(atom(), string()) -> {string(), string()}.
asm_file_exts(wasm32, TempBase) ->
    {TempBase ++ ".wat", TempBase ++ ".wasm"};
asm_file_exts(_Arch, TempBase) ->
    {TempBase ++ ".S", TempBase ++ ".o"}.

%% Build the assemble + disassemble command.
-spec asm_cmd(atom(), string(), string(), string(), string()) -> string().
asm_cmd(wasm32, AsCmd, AsmFile, ObjdumpCmd, ObjFile) ->
    lists:flatten(
        io_lib:format(
            "~s ~s -o ~s && ~s -d ~s"
            " | grep '^ '"
            " | sed -e 's/ | /\\t/' -e 's/: /:\\t/'",
            [AsCmd, AsmFile, ObjFile, ObjdumpCmd, ObjFile]
        )
    );
asm_cmd(Arch, AsCmd, AsmFile, ObjdumpCmd, ObjFile) ->
    lists:flatten(
        io_lib:format(
            "~s ~s -c ~s -o ~s && ~s -j .text -D ~s",
            [AsCmd, get_as_flags(Arch), AsmFile, ObjFile, ObjdumpCmd, ObjFile]
        )
    ).

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

%% Get the disassemblable portion of a stream.
%% For wasm32, extracts the code instruction bytes from the WASM module
%% embedded in the stream.
-spec stream_code(atom(), binary()) -> binary().
stream_code(wasm32, Stream) ->
    {WasmModule, _LinesData} = wasm_stream_extract(Stream),
    wasm_code_bytes(WasmModule);
stream_code(_Arch, Stream) ->
    Stream.

-define(WASM_CODE_SECTION, 10).

%% Extract the instruction bytes from a WASM module that wasm-objdump -d would show.
%% Skips the module header, finds the code section (ID 10), then for each function body
%% skips the body_size uleb128 and local_count uleb128, returning the rest.
-spec wasm_code_bytes(binary()) -> binary().
wasm_code_bytes(<<_Magic:4/binary, _Version:4/binary, Sections/binary>>) ->
    wasm_code_bytes_sections(Sections).

wasm_code_bytes_sections(<<>>) ->
    <<>>;
wasm_code_bytes_sections(<<SectionId:8, Rest0/binary>>) ->
    {SectionSize, Rest1} = jit_wasm32_asm:decode_uleb128(Rest0),
    <<SectionContent:SectionSize/binary, Rest2/binary>> = Rest1,
    case SectionId of
        ?WASM_CODE_SECTION ->
            {_FuncCount, BodiesData} = jit_wasm32_asm:decode_uleb128(SectionContent),
            wasm_code_bytes_bodies(BodiesData, <<>>);
        _ ->
            wasm_code_bytes_sections(Rest2)
    end.

wasm_code_bytes_bodies(<<>>, Acc) ->
    Acc;
wasm_code_bytes_bodies(Data, Acc) ->
    {BodySize, Rest0} = jit_wasm32_asm:decode_uleb128(Data),
    <<FuncBody:BodySize/binary, Rest1/binary>> = Rest0,
    %% Skip local_count uleb128; the rest is local declarations + instructions
    {_LocalCount, FuncBodyRest} = jit_wasm32_asm:decode_uleb128(FuncBody),
    wasm_code_bytes_bodies(Rest1, <<Acc/binary, FuncBodyRest/binary>>).

%% Assert that Stream matches the expected objdump output Dump.
%% On mismatch, run objdump on both expected and actual binaries and diff -u.
-spec assert_stream(atom(), binary(), binary()) -> ok.
assert_stream(Arch, Dump, Stream) ->
    Expected = dump_to_bin(Dump),
    Actual = stream_code(Arch, Stream),
    case Expected =:= Actual of
        true ->
            ok;
        false ->
            diff_disasm(Arch, Expected, Actual),
            ?assertEqual(Expected, Actual)
    end.

%% Assert that Stream matches the expected objdump output Dump.
%% On BEAM, if UPDATE_JIT_TESTS environment variable is set, update the source
%% file with the correct objdump, and also fail.
-spec assert_stream(atom(), binary(), binary(), string(), pos_integer()) -> ok.
assert_stream(Arch, Dump, Stream, File, _Line) ->
    Expected = dump_to_bin(Dump),
    Actual = stream_code(Arch, Stream),
    case Expected =:= Actual of
        true ->
            ok;
        false ->
            case {erlang:system_info(machine), os:getenv("UPDATE_JIT_TESTS")} of
                {"BEAM", Value} when Value =/= false ->
                    %% For wasm32, wasm-objdump needs the full WASM module binary,
                    %% not just the extracted code bytes.
                    DisasmInput =
                        case Arch of
                            wasm32 ->
                                {WasmModule, _} = wasm_stream_extract(Stream),
                                WasmModule;
                            _ ->
                                Actual
                        end,
                    update_test_source(Arch, Dump, DisasmInput, File);
                _ ->
                    diff_disasm(Arch, Expected, Actual)
            end,
            ?assertEqual(Expected, Actual)
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

%% Extract the WASM module from a wasm32 stream.
%% Stream layout: num_entries(4) + wasm_offset(4) + lines_offset(4)
%%                + entries(num_entries*4) + wasm_module + lines_data
%% Returns {WasmModule, LinesData} where WasmModule is a complete .wasm binary.
-spec wasm_stream_extract(binary()) -> {binary(), binary()}.
wasm_stream_extract(
    <<_NumEntries:32/little, WasmOffset:32/little, LinesOffset:32/little, _/binary>> = Stream
) ->
    %% Offsets are relative to the start of the stream (byte 0)
    WasmModule = binary:part(Stream, WasmOffset, LinesOffset - WasmOffset),
    LinesData = binary:part(Stream, LinesOffset, byte_size(Stream) - LinesOffset),
    {WasmModule, LinesData}.

%% Run objdump on both expected and actual binaries, then diff -u the outputs.
-spec diff_disasm(atom(), binary(), binary()) -> ok.
diff_disasm(wasm32, Expected, Actual) ->
    %% For wasm32 the comparison is against code bytes, not a full WASM module, so
    %% wasm-objdump cannot be used here. Print raw bytes for diagnostics.
    io:format("expected bytes: ~w~nactual bytes:   ~w~n", [Expected, Actual]);
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
            try
                ok = write_disasm_input(Arch, ExpFile, Expected),
                ok = write_disasm_input(Arch, ActFile, Actual),
                ObjdumpCmdExp = disasm_cmd(Arch, ObjdumpCmd, ExpFile) ++ " > " ++ ExpDis,
                ObjdumpCmdAct = disasm_cmd(Arch, ObjdumpCmd, ActFile) ++ " > " ++ ActDis,
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
get_objdump_flags(arm32) ->
    "-marm";
get_objdump_flags(aarch64) ->
    "-m aarch64";
get_objdump_flags(x86_64) ->
    "-m i386:x86-64";
get_objdump_flags(riscv32) ->
    "-m riscv:rv32";
get_objdump_flags(arm_thumb2) ->
    "-marm --disassembler-options=force-thumb";
get_objdump_flags(riscv64) ->
    "-m riscv:rv64";
get_objdump_flags(wasm32) ->
    "";
get_objdump_flags(xtensa) ->
    "-m xtensa -EL".

%% Write binary data to a file suitable for disassembly.
%% For wasm32, wraps raw bytes in a WASM module; for others, writes raw bytes.
-spec write_disasm_input(atom(), string(), binary()) -> ok.
write_disasm_input(_Arch, File, Bin) ->
    file:write_file(File, Bin).

%% Build the full disassembly command for a given file.
-spec disasm_cmd(atom(), string(), string()) -> string().
disasm_cmd(wasm32, ObjdumpCmd, File) ->
    lists:flatten(
        io_lib:format(
            "~s -d ~s 2>/dev/null"
            " | grep '^ '"
            " | sed -e 's/ | /\\t/' -e 's/: /:\\t/'",
            [ObjdumpCmd, File]
        )
    );
disasm_cmd(Arch, ObjdumpCmd, File) ->
    ObjdumpFlags = get_objdump_flags(Arch),
    Cleanup =
        "grep '^ '"
        " | sed -e 's/[[:space:]]*;.*$//' -e 's/[[:space:]]*\\/\\/.*$//'",
    lists:flatten(
        io_lib:format(
            "~s -b binary ~s -D -z ~s | ~s",
            [ObjdumpCmd, ObjdumpFlags, File, Cleanup]
        )
    ).

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
        ok = write_disasm_input(Arch, TmpFile, Stream),
        os:cmd(disasm_cmd(Arch, ObjdumpCmd, TmpFile))
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
