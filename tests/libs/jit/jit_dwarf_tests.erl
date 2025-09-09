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

-module(jit_dwarf_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../../libs/jit/src/opcodes.hrl").

basic_dwarf_state_test() ->
    % Create a basic DWARF state
    State = jit_dwarf:new(jit_armv6m, test_module, jit_stream_binary, 1024),

    % Add some test data
    State1 = jit_dwarf:opcode(State, ?OP_FUNC_INFO),
    State2 = jit_dwarf:function(State1, get_value, 2),
    State3 = jit_dwarf:line(State2, 42),

    % Verify state contains our data
    ?assert(is_tuple(State3)),

    % Test stream interface
    Stream = jit_dwarf:stream(State3),
    ?assert(is_binary(Stream)).

elf_generation_test() ->
    % Create state with some debug info
    State = jit_dwarf:new(jit_armv6m, test_module, jit_stream_binary, 1024),
    % Some opcode
    State1 = jit_dwarf:opcode(State, <<"test_opcode/2">>),
    State2 = jit_dwarf:function(State1, test_func, 1),
    State3 = jit_dwarf:line(State2, 100),

    % Generate ELF
    case jit_dwarf:elf(State3, <<>>) of
        false ->
            ok;
        {ok, ElfBinary, _ElfWithText} ->
            % Verify ELF magic
            <<127, $E, $L, $F, _Rest/binary>> = ElfBinary,

            % Verify ELF header structure
            ?assert(byte_size(ElfBinary) >= 52),

            % Extract and verify key header fields
            <<_Magic:4/binary, Class, Endian, _Version, _OSABI, _ABIVersion:8/binary,
                Type:16/little, Machine:16/little, _ElfVersion:32/little, _Entry:32/little,
                _PHOff:32/little, SHOff:32/little, _Flags:32/little, EHSize:16/little,
                _PHEntSize:16/little, _PHNum:16/little, _SHEntSize:16/little, SHNum:16/little,
                _SHStrNdx:16/little, _/binary>> = ElfBinary,

            % Verify basic ELF structure

            % ELFCLASS32
            ?assertEqual(1, Class),
            % ELFDATA2LSB
            ?assertEqual(1, Endian),
            % ET_REL
            ?assertEqual(1, Type),
            % EM_ARM
            ?assertEqual(40, Machine),
            % ELF header size
            ?assertEqual(52, EHSize),

            % Verify we have the expected sections

            % null + 4 debug sections + shstrtab
            ?assert(SHNum >= 6),
            % Section headers after ELF header
            ?assert(SHOff > 52),

            % Verify the ELF is complete (section headers exist)

            % Headers should exist
            ExpectedMinSize = SHOff + (SHNum * 40),
            ?assert(byte_size(ElfBinary) >= ExpectedMinSize)
    end.

section_header_test() ->
    State = jit_dwarf:new(jit_armv6m, test_module, jit_stream_binary, 1024),
    State1 = jit_dwarf:function(State, main, 0),

    case jit_dwarf:elf(State1, <<>>) of
        false ->
            ok;
        {ok, ElfBinary, _ElfWithText} ->
            % Extract section header info from ELF header (parse full header)
            <<_ElfMagic:16/binary, _Type:16/little, _Machine:16/little, _Version:32/little,
                _Entry:32/little, _PHOff:32/little, SHOff:32/little, _Flags:32/little,
                _EHSize:16/little, _PHEntSize:16/little, _PHNum:16/little, _SHEntSize:16/little,
                SHNum:16/little, _SHStrNdx:16/little, _Rest/binary>> = ElfBinary,

            % Verify we can read section headers
            SectionHeadersSize = SHNum * 40,
            SectionHeadersStart = SHOff,

            ?assert(byte_size(ElfBinary) >= SectionHeadersStart + SectionHeadersSize),

            % Extract first section header (should be null)
            <<_:SectionHeadersStart/binary, NullHeader:40/binary, _/binary>> = ElfBinary,
            % All zeros
            ?assertEqual(<<0:320>>, NullHeader)
    end.

string_table_test() ->
    State = jit_dwarf:new(jit_armv6m, string_test, jit_stream_binary, 1024),

    case jit_dwarf:elf(State, <<>>) of
        false ->
            ok;
        {ok, ElfBinary, _ElfWithText} ->
            % Find string table section - parse ELF header
            <<_ElfMagic2:16/binary, _Type2:16/little, _Machine2:16/little, _Version2:32/little,
                _Entry2:32/little, _PHOff2:32/little, SHOff:32/little, _Flags2:32/little,
                _EHSize2:16/little, _PHEntSize2:16/little, _PHNum2:16/little, _SHEntSize2:16/little,
                _SHNum:16/little, SHStrNdx:16/little, _Rest2/binary>> = ElfBinary,

            % Extract string table section header
            StrTabHeaderOffset = SHOff + (SHStrNdx * 40),
            <<_:StrTabHeaderOffset/binary, _StrName:32/little, _StrType:32/little,
                _StrFlags:32/little, _StrAddr:32/little, StrOffset:32/little, StrSize:32/little,
                _/binary>> = ElfBinary,

            % Extract string table data
            <<_:StrOffset/binary, StringTable:StrSize/binary, _/binary>> = ElfBinary,

            % Verify string table contains expected section names
            StrTabStrings = binary:split(StringTable, <<0>>, [global]),

            ?assert(lists:member(<<".debug_info">>, StrTabStrings)),
            ?assert(lists:member(<<".shstrtab">>, StrTabStrings))
    end.

elf_with_text_test() ->
    % Test the new elf_with_text/2 function that creates complete ELF with .text section
    State = jit_dwarf:new(jit_x86_64, test_module, jit_stream_binary, 1024),

    % Some dummy x86_64 native code (mov rax, 1; ret)
    NativeCode = <<16#48, 16#c7, 16#c0, 16#01, 16#00, 16#00, 16#00, 16#c3>>,

    % Generate complete ELF with debug info and .text section
    case jit_dwarf:elf(State, NativeCode) of
        false ->
            ok;
        {ok, _DebugOnlyELF, CombinedELF} ->
            % Verify ELF magic
            <<127, $E, $L, $F, _Rest/binary>> = CombinedELF,

            % Parse ELF header to check section count (should be 9: null + 6 debug sections + .text + shstrtab)
            <<_ElfMagic:16/binary, _Type:16/little, _Machine:16/little, _Version:32/little,
                _Entry:32/little, _PHOff:32/little, _SHOff:32/little, _Flags:32/little,
                _EHSize:16/little, _PHEntSize:16/little, _PHNum:16/little, _SHEntSize:16/little,
                SHNum:16/little, _SHStrNdx:16/little, _/binary>> = CombinedELF,

            % Should have 9 sections total
            ?assertEqual(9, SHNum),

            % Verify the native code is present in the binary
            ?assert(binary:match(CombinedELF, NativeCode) =/= nomatch),

            % Verify ELF is larger due to added .text section
            {ok, DebugOnlyELF, _} = jit_dwarf:elf(State, <<>>),
            ?assert(byte_size(CombinedELF) > byte_size(DebugOnlyELF))
    end.

text_section_properties_test() ->
    % Test that the .text section has proper properties
    State = jit_dwarf:new(jit_aarch64, test_module, jit_stream_binary, 1024),

    % AArch64 native code (mov x0, #42; ret)
    NativeCode = <<16#d2800540, 16#d65f03c0>>,

    case jit_dwarf:elf(State, NativeCode) of
        false ->
            ok;
        {ok, _DebugOnlyELF, CombinedELF} ->
            % Parse ELF to find .text section
            <<_ElfMagic:16/binary, _Type:16/little, _Machine:16/little, _Version:32/little,
                _Entry:32/little, _PHOff:32/little, SHOff:32/little, _Flags:32/little,
                _EHSize:16/little, _PHEntSize:16/little, _PHNum:16/little, _SHEntSize:16/little,
                SHNum:16/little, SHStrNdx:16/little, RestOfFile/binary>> = CombinedELF,

            % Extract section headers
            SectionHeadersStart = SHOff - 52,
            <<_SectionData:SectionHeadersStart/binary, SectionHeaders/binary>> = RestOfFile,

            % Extract string table to find .text section by name
            StringTableHeaderOffset = SHStrNdx * 40,
            <<_:StringTableHeaderOffset/binary, _StrName:32/little, _StrType:32/little,
                _StrFlags:32/little, _StrAddr:32/little, StrOffset:32/little, StrSize:32/little,
                _/binary>> = SectionHeaders,

            % Extract string table content
            StrTableFileOffset = StrOffset - 52,
            <<_:StrTableFileOffset/binary, StringTable:StrSize/binary, _/binary>> = _SectionData,

            % Find .text section by scanning all section headers
            TextSectionFound = find_text_section(SectionHeaders, StringTable, SHNum, 0),
            ?assert(TextSectionFound =/= not_found),

            {TextType, TextFlags, TextSize, TextAddr} = TextSectionFound,

            % Verify .text section properties
            SHT_PROGBITS = 1,
            SHF_ALLOC = 2,
            SHF_EXECINSTR = 4,
            ExpectedFlags = SHF_ALLOC bor SHF_EXECINSTR,

            ?assertEqual(SHT_PROGBITS, TextType),
            ?assertEqual(ExpectedFlags, TextFlags),
            ?assertEqual(byte_size(NativeCode), TextSize),
            % Should be 0 for relocatable
            ?assertEqual(0, TextAddr)
    end.

different_architectures_test() ->
    % Test elf_with_text with different JIT backends
    Backends = [jit_x86_64, jit_aarch64, jit_armv6m],
    % Simple nop instruction
    NativeCode = <<16#90>>,

    lists:foreach(
        fun(Backend) ->
            State = jit_dwarf:new(Backend, test_module, jit_stream_binary, 1024),
            case jit_dwarf:elf(State, NativeCode) of
                false ->
                    ok;
                {ok, _DebugOnlyELF, CombinedELF} ->
                    % Verify ELF magic and basic structure
                    <<127, $E, $L, $F, _Rest/binary>> = CombinedELF,
                    % Verify native code is present
                    ?assert(binary:match(CombinedELF, NativeCode) =/= nomatch)
            end
        end,
        Backends
    ).

% Helper function to find .text section in ELF
find_text_section(_Headers, _StringTable, 0, _Index) ->
    not_found;
find_text_section(Headers, StringTable, Remaining, Index) ->
    HeaderOffset = Index * 40,
    <<_:HeaderOffset/binary, NameOffset:32/little, Type:32/little, Flags:32/little, Addr:32/little,
        _Offset:32/little, Size:32/little, _/binary>> = Headers,

    % Extract section name from string table
    SectionName = extract_string_at_offset(StringTable, NameOffset),

    case SectionName of
        <<".text">> ->
            {Type, Flags, Size, Addr};
        _ ->
            find_text_section(Headers, StringTable, Remaining - 1, Index + 1)
    end.

% Helper function to extract null-terminated string at given offset
extract_string_at_offset(StringTable, Offset) ->
    <<_:Offset/binary, Rest/binary>> = StringTable,
    [String | _] = binary:split(Rest, <<0>>, []),
    String.
