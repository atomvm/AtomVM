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

-module(jit_dwarf).

-record(state, {
    % Backend module (jit_armv6m, etc.)
    backend :: module(),
    % Current module being compiled
    module_name :: module(),
    opcodes = [] :: [{Offset :: non_neg_integer(), Opcode :: atom()}],
    labels = [] :: [{Offset :: non_neg_integer(), Label :: non_neg_integer()}],
    functions = [] :: [
        {Offset :: non_neg_integer(), FunctionName :: atom(), Arity :: non_neg_integer()}
    ],
    lines = [] :: [{Offset :: non_neg_integer(), LineNumber :: pos_integer()}],
    stream_module :: module(),
    stream :: any()
}).

-type state() :: #state{}.

-export([
    new/4,
    opcode/2,
    label/2,
    function/3,
    line/2,
    stream/1,
    elf/2
]).

% jit_stream interface
-export([
    offset/1,
    append/2,
    replace/3,
    map/4
]).

%%-----------------------------------------------------------------------------
%% @returns A new state
%% @doc     Create a new state with the proxied stream.
%% @end
%%-----------------------------------------------------------------------------
-spec new(module(), module(), module(), pos_integer()) -> state().
new(Backend, ModuleName, StreamModule, MaxSize) ->
    Stream = StreamModule:new(MaxSize),
    #state{
        backend = Backend, module_name = ModuleName, stream_module = StreamModule, stream = Stream
    }.

%%-----------------------------------------------------------------------------
%% @param Stream    stream to get the offset from
%% @returns The current offset
%% @doc     Get the current offset in the stream
%% @end
%%-----------------------------------------------------------------------------
-spec offset(state()) -> non_neg_integer().
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to append to
%% @param Binary    binary to append to the stream
%% @returns The updated stream
%% @doc     Append a binary to the stream
%% @end
%%-----------------------------------------------------------------------------
-spec append(state(), binary()) -> state().
append(#state{stream_module = StreamModule, stream = Stream0} = State, Binary) ->
    Stream1 = StreamModule:append(Stream0, Binary),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param Stream        stream to update
%% @param Offset        offset to update from
%% @param Replacement   binary to write at offset
%% @returns The updated stream
%% @doc     Replace bytes at a given offset
%% @end
%%-----------------------------------------------------------------------------
-spec replace(state(), non_neg_integer(), binary()) -> state().
replace(#state{stream_module = StreamModule, stream = Stream0} = State, Offset, Replacement) ->
    Stream1 = StreamModule:replace(Stream0, Offset, Replacement),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param Stream        stream to update
%% @param Offset        offset to update from
%% @param Length        length of the section to update
%% @param MapFunction   function that updates the binary
%% @returns The updated stream
%% @doc     Replace bytes at a given offset calling a map function
%% @end
%%-----------------------------------------------------------------------------
-spec map(state(), non_neg_integer(), pos_integer(), fun((binary()) -> binary())) -> state().
map(#state{stream_module = StreamModule, stream = Stream0} = State, Offset, Length, MapFunction) ->
    Stream1 = StreamModule:map(Stream0, Offset, Length, MapFunction),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param State    current state
%% @param Opcode   the opcode atom to record
%% @returns The updated state with opcode recorded at current offset
%% @doc     Record an opcode at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec opcode(state(), binary()) -> state().
opcode(#state{stream_module = StreamModule, stream = Stream, opcodes = Opcodes0} = State, Opcode) ->
    Offset = StreamModule:offset(Stream),
    Opcodes1 = [{Offset, Opcode} | Opcodes0],
    State#state{opcodes = Opcodes1}.

%%-----------------------------------------------------------------------------
%% @param State    current state
%% @param Label    the label number to record
%% @returns The updated state with label recorded at current offset
%% @doc     Record a label at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec label(state(), non_neg_integer()) -> state().
label(#state{stream_module = StreamModule, stream = Stream, labels = Labels0} = State, Label) ->
    Offset = StreamModule:offset(Stream),
    Labels1 = [{Offset, Label} | Labels0],
    State#state{labels = Labels1}.

%%-----------------------------------------------------------------------------
%% @param State         current state
%% @param FunctionName  the function name atom to record
%% @param Arity         the function arity
%% @returns The updated state with function recorded at current offset
%% @doc     Record a function at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec function(state(), atom(), non_neg_integer()) -> state().
function(
    #state{stream_module = StreamModule, stream = Stream, functions = Functions0} = State,
    FunctionName,
    Arity
) ->
    Offset = StreamModule:offset(Stream),
    Functions1 = [{Offset, FunctionName, Arity} | Functions0],
    State#state{functions = Functions1}.

%%-----------------------------------------------------------------------------
%% @param State    current state
%% @param Line     the line number to record
%% @returns The updated state with line recorded at current offset
%% @doc     Record a line number at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec line(state(), pos_integer()) -> state().
line(#state{stream_module = StreamModule, stream = Stream, lines = Lines0} = State, Line) ->
    Offset = StreamModule:offset(Stream),
    Lines1 = [{Offset, Line} | Lines0],
    State#state{lines = Lines1}.

-spec stream(state()) -> any().
stream(#state{stream = Stream}) ->
    Stream.

%%-----------------------------------------------------------------------------
%% @param State    DWARF state containing debug information
%% @returns {ok, binary(), binary()} with ELF structure containing DWARF info,
%%          (without and with native code in .text) or false if not compiled
%%          with JIT_DWARF
%% @doc     Generate ELF binaries with DWARF debug sections
%% @end
%%-----------------------------------------------------------------------------
-spec elf(state(), binary()) -> {ok, binary(), binary()} | false.
-ifdef(JIT_DWARF).
elf(#state{module_name = ModuleName, backend = Backend} = State, NativeCode) ->
    SourceFile = <<(atom_to_binary(ModuleName, utf8))/binary, ".erl">>,

    % Generate DWARF sections
    DebugInfoSection = generate_debug_info_section_with_opcodes(State, SourceFile),
    DebugLineSection = generate_debug_line_section(State, SourceFile),
    DebugAbbrevSection = generate_debug_abbrev_section_with_opcodes(),
    DebugStrSection = generate_debug_str_section(State, SourceFile),

    % Generate symbol table sections for function names
    {SymtabSection, StrtabSection} = generate_symbol_table(State, Backend),

    % Create base sections list
    BaseSections = [
        {<<".debug_info">>, DebugInfoSection},
        {<<".debug_line">>, DebugLineSection},
        {<<".debug_abbrev">>, DebugAbbrevSection},
        {<<".debug_str">>, DebugStrSection},
        {<<".symtab">>, SymtabSection},
        {<<".strtab">>, StrtabSection}
    ],

    % Add ARM attributes section for armv6m backend
    Sections =
        case Backend of
            jit_armv6m ->
                ArmAttributesSection = generate_arm_attributes_section(),
                BaseSections ++ [{<<".ARM.attributes">>, ArmAttributesSection}];
            _ ->
                BaseSections
        end,

    % Create ELF with debug sections and symbol table
    ElfBinary = create_elf_header_and_sections(Backend, Sections),

    CombinedELF = add_text_section_to_elf(ElfBinary, NativeCode),
    {ok, ElfBinary, CombinedELF}.
-else.
elf(_State, _NativeCode) ->
    false.
-endif.

-ifdef(JIT_DWARF).

%% DWARF constants
-define(DW_TAG_compile_unit, 16#11).
-define(DW_TAG_subprogram, 16#2e).
-define(DW_TAG_lexical_block, 16#0b).
-define(DW_TAG_label, 16#0a).
-define(DW_AT_name, 16#03).
-define(DW_AT_comp_dir, 16#1b).
-define(DW_AT_producer, 16#25).
-define(DW_AT_language, 16#13).
-define(DW_AT_low_pc, 16#11).
-define(DW_AT_high_pc, 16#12).
-define(DW_AT_stmt_list, 16#10).
-define(DW_FORM_string, 16#08).
-define(DW_FORM_addr, 16#01).
-define(DW_FORM_data4, 16#06).
-define(DW_FORM_data1, 16#0b).
-define(DW_FORM_udata, 16#0f).
-define(DW_LANG_C, 16#02).

%% ELF constants
-define(EI_MAG0, 16#7f).
-define(EI_MAG1, $E).
-define(EI_MAG2, $L).
-define(EI_MAG3, $F).
-define(ELFCLASS32, 1).
-define(ELFDATA2LSB, 1).
-define(EV_CURRENT, 1).
-define(ET_REL, 1).
-define(EM_ARM, 40).
-define(EM_X86_64, 62).
-define(EM_AARCH64, 183).
-define(SHT_PROGBITS, 1).
-define(SHT_SYMTAB, 2).
-define(SHT_STRTAB, 3).
-define(SHT_ARM_ATTRIBUTES, 16#70000003).
-define(SHF_ALLOC, 2).
-define(SHF_EXECINSTR, 4).

%% ARM EABI flags

% EABI version 5
-define(EF_ARM_EABI_VER5, 16#05000000).
% Soft float ABI
-define(EF_ARM_ABI_FLOAT_SOFT, 16#00000200).
% ARM architecture v6-M (Thumb-only)
-define(EF_ARM_ARCH_V6M, 16#00000009).

%% Map JIT backend to ELF machine type
backend_to_machine_type(jit_x86_64) -> ?EM_X86_64;
backend_to_machine_type(jit_aarch64) -> ?EM_AARCH64;
backend_to_machine_type(jit_armv6m) -> ?EM_ARM.

%% Map JIT backend to ELF flags
backend_to_elf_flags(jit_armv6m) ->
    ?EF_ARM_EABI_VER5 bor ?EF_ARM_ABI_FLOAT_SOFT bor ?EF_ARM_ARCH_V6M;
backend_to_elf_flags(_) ->
    0.

%% Find section index by name
find_section_index(SectionName, SectionNames) ->
    find_section_index_helper(SectionName, SectionNames, 0).

find_section_index_helper(_, [], _) ->
    error({section_not_found});
find_section_index_helper(SectionName, [SectionName | _], Index) ->
    Index;
find_section_index_helper(SectionName, [_ | Rest], Index) ->
    find_section_index_helper(SectionName, Rest, Index + 1).

%% Find .symtab section index in section headers
find_symtab_section_index(SectionHeaders) ->
    find_symtab_section_index_helper(SectionHeaders, 0, 40).

find_symtab_section_index_helper(<<>>, _, _) ->
    error({symtab_not_found});
find_symtab_section_index_helper(
    <<_NameOffset:32/little, SectionType:32/little, _Rest:32/binary, Remaining/binary>>,
    Index,
    SectionHeaderSize
) ->
    case SectionType of
        ?SHT_SYMTAB -> Index;
        _ -> find_symtab_section_index_helper(Remaining, Index + 1, SectionHeaderSize)
    end.

%% Generate ARM attributes section for ARMv6-M
generate_arm_attributes_section() ->
    % ARM EABI attributes format according to ARM IHI 0045E

    % Build the tag-value pairs for file attributes
    TagValuePairs = <<
        % CPU_arch attribute: ARMv6S-M (value 11)
        6,
        11,
        % CPU_arch_profile attribute: 'M' profile (value 77 = 'M')
        7,
        77,
        % ARM_ISA_use attribute: No ARM ISA (value 0)
        8,
        0,
        % THUMB_ISA_use attribute: Thumb-1 only (value 1)
        9,
        1,
        % FP_arch attribute: No FP (value 0)
        10,
        0,
        % ABI_PCS_wchar_t attribute: 4 bytes (value 2)
        18,
        2,
        % ABI_enum_size attribute: int-sized (value 2)
        26,
        2,
        % ABI_align_needed attribute: 8-byte alignment (value 1)
        24,
        1,
        % ABI_align_preserved attribute: 8-byte alignment (value 1)
        25,
        1
    >>,

    % Calculate file attributes subsection length (tag + length field + tag-value pairs)
    FileAttributesLength = 1 + 4 + byte_size(TagValuePairs),

    % Build file attributes subsection
    FileAttributes = <<
        % File attributes tag
        1,
        % Length of this file attributes subsection
        FileAttributesLength:32/little,
        % The tag-value pairs
        TagValuePairs/binary
    >>,

    % Build vendor subsection ("aeabi" + null + file attributes)
    VendorContent = <<"aeabi", 0, FileAttributes/binary>>,
    VendorLength = byte_size(VendorContent),

    % Calculate total section length (format version + vendor length + vendor content)
    TotalLength = 1 + 4 + VendorLength,

    % Build final section according to ARM EABI spec
    <<
        % Format version 'A'
        $A,
        % Total section length (4 bytes, little-endian)
        TotalLength:32/little,
        % Vendor subsection content
        VendorContent/binary
    >>.

generate_debug_str_section(#state{module_name = ModuleName}, SourceFile) ->
    % String table: null-terminated strings
    Strings = [
        % Index 0: empty string
        <<0>>,
        % Index 1: source file name
        SourceFile,
        <<0>>,
        % Index 2: producer
        <<"AtomVM JIT Compiler v0.7.0">>,
        <<0>>,
        % Index 3: comp_dir
        <<"/tmp">>,
        <<0>>,
        % Index 4: module name
        atom_to_binary(ModuleName, utf8),
        <<0>>
    ],
    iolist_to_binary(Strings).

generate_debug_abbrev_section_with_opcodes() ->
    % Abbreviation table
    <<
        % Abbrev 1: DW_TAG_compile_unit

        % Abbreviation code
        1,
        % Tag
        ?DW_TAG_compile_unit,
        % Has children (DW_CHILDREN_yes)
        1,
        % Name attribute
        ?DW_AT_name,
        ?DW_FORM_string,
        % Compilation directory
        ?DW_AT_comp_dir,
        ?DW_FORM_string,
        % Producer
        ?DW_AT_producer,
        ?DW_FORM_string,
        % Language
        ?DW_AT_language,
        ?DW_FORM_data4,
        % Low PC
        ?DW_AT_low_pc,
        ?DW_FORM_addr,
        % High PC
        ?DW_AT_high_pc,
        ?DW_FORM_addr,
        % Statement list
        ?DW_AT_stmt_list,
        ?DW_FORM_data4,
        % End of attributes
        0,
        0,

        % Abbrev 2: DW_TAG_lexical_block (for opcodes)
        % Abbreviation code
        2,
        % Tag
        ?DW_TAG_lexical_block,
        % Has no children
        0,
        % Name attribute (opcode name)
        ?DW_AT_name,
        ?DW_FORM_string,
        % Low PC
        ?DW_AT_low_pc,
        ?DW_FORM_addr,
        % End of attributes
        0,
        0,

        % Abbrev 3: DW_TAG_label (for labels)
        % Abbreviation code
        3,
        % Tag
        ?DW_TAG_label,
        % Has no children
        0,
        % Name attribute (label name)
        ?DW_AT_name,
        ?DW_FORM_string,
        % Low PC
        ?DW_AT_low_pc,
        ?DW_FORM_addr,
        % End of attributes
        0,
        0,

        % Abbrev 4: DW_TAG_subprogram (for functions)
        % Abbreviation code
        4,
        % Tag
        ?DW_TAG_subprogram,
        % Has no children
        0,
        % Name attribute (module:function/arity)
        ?DW_AT_name,
        ?DW_FORM_string,
        % Low PC
        ?DW_AT_low_pc,
        ?DW_FORM_addr,
        % High PC
        ?DW_AT_high_pc,
        ?DW_FORM_addr,
        % End of attributes
        0,
        0,

        % End of abbreviations
        0
    >>.

generate_debug_info_section_with_opcodes(
    #state{functions = Functions, opcodes = Opcodes, labels = Labels, module_name = ModuleName} =
        State,
    SourceFile
) ->
    % Calculate address ranges
    {LowPC, HighPC} = calculate_address_range(State),

    % Build content first to calculate actual length
    CompileUnitContent = <<
        % DWARF version
        4:16/little,
        % Abbreviation offset
        0:32/little,
        % Address size
        4,
        % Compilation unit DIE (abbreviation 1)
        1,
        % DW_AT_name
        SourceFile/binary,
        0,
        % DW_AT_comp_dir
        "/tmp",
        0,
        % DW_AT_producer
        "AtomVM JIT Compiler v0.7.0",
        0,
        % DW_AT_language
        ?DW_LANG_C:32/little,
        % DW_AT_low_pc
        LowPC:32/little,
        % DW_AT_high_pc
        HighPC:32/little,
        % DW_AT_stmt_list (offset into .debug_line)
        0:32/little
    >>,

    % Generate DIEs for functions, opcodes and labels
    FunctionDIEs = generate_function_dies_with_module(Functions, ModuleName),
    OpcodeDIEs = generate_opcode_dies(Opcodes),
    LabelDIEs = generate_label_dies(Labels),

    % End of children marker
    EndMarker = <<0>>,

    % Calculate actual unit length (everything after the length field)
    Content =
        <<CompileUnitContent/binary, FunctionDIEs/binary, OpcodeDIEs/binary, LabelDIEs/binary,
            EndMarker/binary>>,
    UnitLength = byte_size(Content),

    % Build final section with correct length
    <<UnitLength:32/little, Content/binary>>.

generate_debug_line_section(#state{lines = _Lines, opcodes = _Opcodes}, SourceFile) ->
    % Build header content first to calculate actual lengths
    HeaderContent = <<
        % DWARF version
        4:16/little,
        % Header length (placeholder, calculated below)
        0:32/little,
        % Minimum instruction length (Thumb)
        2,
        % Maximum operations per instruction
        1,
        % Default is_stmt
        1,
        % Line base
        (-5):8/signed,
        % Line range
        14,
        % Opcode base
        13
    >>,

    % Standard opcode lengths (for opcodes 1-12)
    StdOpcodeLengths = <<0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1>>,

    % File names table - include directory table and multiple files
    FileTable = <<
        % Directory table (empty)
        0,
        % File table
        % File 1: Original source
        SourceFile/binary,
        0,
        % Directory index
        0,
        % Last modified
        0,
        % File size
        0,
        % End of file table
        0
    >>,

    % Line number program - simplified to avoid malformed opcodes
    Program = generate_simple_line_program(),

    % Calculate actual header length (everything from version to end of file table)
    HeaderPlusTablesContent = <<StdOpcodeLengths/binary, FileTable/binary>>,
    % -4 for header_length field itself
    HeaderLength = byte_size(HeaderContent) - 4 + byte_size(HeaderPlusTablesContent),

    % Build corrected header with actual length
    CorrectedHeader = <<
        % DWARF version
        4:16/little,
        % Header length (actual)
        HeaderLength:32/little,
        % Minimum instruction length (Thumb)
        2,
        % Maximum operations per instruction
        1,
        % Default is_stmt
        1,
        % Line base
        (-5):8/signed,
        % Line range
        14,
        % Opcode base
        13
    >>,

    % Calculate total unit length (everything after unit length field)
    ContentAfterLength =
        <<CorrectedHeader/binary, StdOpcodeLengths/binary, FileTable/binary, Program/binary>>,
    UnitLength = byte_size(ContentAfterLength),

    <<UnitLength:32/little, ContentAfterLength/binary>>.

create_elf_header_and_sections(Backend, Sections) ->
    % Create section name string table (dynamic based on sections)
    SectionNames =
        [<<>>] ++ [SectionName || {SectionName, _Section} <- Sections] ++ [<<".shstrtab">>],
    ShStrTab = create_string_table(SectionNames),

    % Calculate offsets
    ElfHeaderSize = 52,
    % null + debug sections + shstrtab
    SectionCount = length(SectionNames),
    SectionHeaderSize = 40,

    % String table index is the last section
    ShStrTabIndex = SectionCount - 1,

    % Section data layout: debug sections + string table
    {SectionData, SectionOffsets} = layout_sections(Sections, ShStrTab, ElfHeaderSize),

    % Section headers start after all section data
    SectionHeaderOffset = ElfHeaderSize + byte_size(SectionData),

    % Get machine type and flags for this backend
    MachineType = backend_to_machine_type(Backend),
    ElfFlags = backend_to_elf_flags(Backend),

    % ELF header
    ElfHeader = <<
        % Magic
        ?EI_MAG0,
        ?EI_MAG1,
        ?EI_MAG2,
        ?EI_MAG3,
        % 32-bit
        ?ELFCLASS32,
        % Little endian
        ?ELFDATA2LSB,
        % ELF version
        ?EV_CURRENT,
        % OS ABI
        0,
        % ABI version
        0,
        % Padding
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        % Relocatable file
        ?ET_REL:16/little,
        % Architecture from backend
        MachineType:16/little,
        % Version
        1:32/little,
        % Entry point
        0:32/little,
        % Program header offset
        0:32/little,
        % Section header offset
        SectionHeaderOffset:32/little,
        % Flags
        ElfFlags:32/little,
        % ELF header size
        ElfHeaderSize:16/little,
        % Program header entry size
        0:16/little,
        % Program header count
        0:16/little,
        % Section header entry size
        SectionHeaderSize:16/little,
        % Section count
        SectionCount:16/little,
        % String table index (.shstrtab)
        ShStrTabIndex:16/little
    >>,

    % Generate section headers
    SectionHeaders = create_section_headers_proper(
        SectionNames, Sections, SectionOffsets, ShStrTab, Backend
    ),

    <<ElfHeader/binary, SectionData/binary, SectionHeaders/binary>>.

%% Helper functions
calculate_address_range(#state{opcodes = Opcodes}) ->
    case Opcodes of
        [] ->
            {0, 0};
        _ ->
            Offsets = [Offset || {Offset, _} <- Opcodes],
            % Assume 4-byte instruction
            {lists:min(Offsets), lists:max(Offsets) + 4}
    end.

generate_simple_line_program() ->
    % Very simple line program that just ends the sequence
    <<
        % Set file to 1
        1,
        1,
        % End sequence: extended opcode

        % Extended opcode prefix
        0,
        % Length of extended opcode
        1,
        % DW_LNE_end_sequence
        1
    >>.

%% Generate DIEs for functions as DW_TAG_subprogram with module:func/arity naming
generate_function_dies_with_module(Functions, ModuleName) ->
    % Filter and sort functions by address
    ValidFunctions = lists:sort([
        {Offset, FunctionName, Arity}
     || {Offset, FunctionName, Arity} <- Functions, Offset >= 0
    ]),

    % Generate DIE for each function
    FunctionDIEsList = [
        generate_function_die_with_module(Offset, FunctionName, Arity, ModuleName)
     || {Offset, FunctionName, Arity} <- ValidFunctions
    ],
    iolist_to_binary(FunctionDIEsList).

%% Generate DIE for a single function with module name
generate_function_die_with_module(Offset, FunctionName, Arity, ModuleName) ->
    % Create module:function/arity format
    FunctionString = list_to_binary(io_lib:format("~s:~s/~B", [ModuleName, FunctionName, Arity])),
    % Estimate function size (can be improved later)
    FunctionSize = 100,
    <<
        % Abbreviation code (4 = DW_TAG_subprogram)
        4,
        % DW_AT_name
        FunctionString/binary,
        0,
        % DW_AT_low_pc
        Offset:32/little,
        % DW_AT_high_pc (low_pc + size)
        (Offset + FunctionSize):32/little
    >>.

%% Generate DIEs for opcodes as DW_TAG_lexical_block
generate_opcode_dies(Opcodes) ->
    % Filter and sort opcodes by address
    ValidOpcodes = lists:sort([{Offset, Opcode} || {Offset, Opcode} <- Opcodes, Offset >= 0]),

    % Generate DIE for each opcode
    OpcodeDIEsList = [generate_opcode_die(Offset, Opcode) || {Offset, Opcode} <- ValidOpcodes],
    iolist_to_binary(OpcodeDIEsList).

%% Generate DIE for a single opcode
generate_opcode_die(Offset, Opcode) ->
    OpcodeString = list_to_binary(io_lib:format("~s@~B", [Opcode, Offset])),
    <<
        % Abbreviation code (2 = DW_TAG_lexical_block)
        2,
        % DW_AT_name
        OpcodeString/binary,
        0,
        % DW_AT_low_pc
        Offset:32/little
    >>.

%% Generate DIEs for labels as DW_TAG_label
generate_label_dies(Labels) ->
    % Filter and sort labels by address
    ValidLabels = lists:sort([{Offset, Label} || {Offset, Label} <- Labels, Offset >= 0]),

    % Generate DIE for each label
    LabelDIEsList = [generate_label_die(Offset, Label) || {Offset, Label} <- ValidLabels],
    iolist_to_binary(LabelDIEsList).

%% Generate DIE for a single label
generate_label_die(Offset, Label) ->
    LabelString = list_to_binary(io_lib:format("label_~B", [Label])),
    <<
        % Abbreviation code (3 = DW_TAG_label)
        3,
        % DW_AT_name
        LabelString/binary,
        0,
        % DW_AT_low_pc
        Offset:32/little
    >>.

%% Generate symbol table for function names and opcode symbols
generate_symbol_table(
    #state{functions = Functions, opcodes = Opcodes, labels = Labels, module_name = ModuleName},
    Backend
) ->
    % Build string table for symbol names (functions) with module:function/arity format
    FunctionNames = [
        list_to_binary(io_lib:format("~s:~s/~B", [ModuleName, FunctionName, Arity]))
     || {_Offset, FunctionName, Arity} <- Functions
    ],
    % Build string table for opcode symbols with module#offset:opcode/arity format
    OpcodeNames = [
        list_to_binary(io_lib:format("~s#~w:~s", [ModuleName, Offset, Opcode]))
     || {Offset, Opcode} <- Opcodes
    ],
    % Build string table for label symbols with module#offset:label_X format
    LabelNames = [
        list_to_binary(io_lib:format("~s#~w:label_~w", [ModuleName, Offset, LabelNum]))
     || {Offset, LabelNum} <- Labels
    ],
    % Add ARM mapping symbol to indicate Thumb code (for armv6m backend)
    MappingSymbols =
        case Backend of
            % Thumb mapping symbol at start of .text section
            jit_armv6m -> [<<"$t">>];
            _ -> []
        end,
    SymbolNames = FunctionNames ++ OpcodeNames ++ LabelNames ++ MappingSymbols,

    % String table starts with null string
    StrtabContent = lists:foldl(
        fun(Name, Acc) ->
            <<Acc/binary, Name/binary, 0>>
        end,
        % Start with null string at offset 0
        <<0>>,
        SymbolNames
    ),

    % Calculate string offsets
    {_, StringOffsets} = lists:foldl(
        fun(Name, {CurrentOffset, Offsets}) ->
            % +1 for null terminator
            NextOffset = CurrentOffset + byte_size(Name) + 1,
            {NextOffset, [CurrentOffset | Offsets]}
        end,
        % Start after null string
        {1, []},
        SymbolNames
    ),
    ReversedOffsets = lists:reverse(StringOffsets),

    % Generate symbol table entries
    % First entry is always the null symbol

    % 16 bytes
    NullSymbol = <<0:32/little, 0:32/little, 0:32/little, 0, 0, 0:16/little>>,

    % Generate function symbols
    FunctionSymbols = lists:foldl(
        fun({{Offset, _FunctionName, _Arity}, StringOffset}, Acc) ->
            % Function name is now module:function/arity (already in FunctionNames)
            % Estimated function size
            FuncSize = 100,

            % Use raw offset for symbol address (no Thumb bit)
            FunctionAddress = Offset,

            % Symbol table entry (16 bytes for 32-bit ELF)
            Symbol = <<
                % st_name (offset in string table)
                StringOffset:32/little,
                % st_value (function address)
                FunctionAddress:32/little,
                % st_size (function size)
                FuncSize:32/little,
                % st_info (STB_GLOBAL << 4 | STT_FUNC)
                16#12,
                % st_other
                0,
                % st_shndx (section index - .text will be section 1)
                1:16/little
            >>,
            <<Acc/binary, Symbol/binary>>
        end,
        <<>>,
        lists:zip(Functions, lists:sublist(ReversedOffsets, length(Functions)))
    ),

    % Generate opcode symbols
    OpcodeStringOffsets = lists:sublist(ReversedOffsets, length(Functions) + 1, length(Opcodes)),
    OpcodeSymbols = lists:foldl(
        fun({{Offset, _Opcode}, StringOffset}, Acc) ->
            % Use raw offset for symbol address (no Thumb bit)
            OpcodeAddress = Offset,

            % Symbol table entry (16 bytes for 32-bit ELF)
            Symbol = <<
                % st_name (offset in string table)
                StringOffset:32/little,
                % st_value (opcode address)
                OpcodeAddress:32/little,
                % st_size (opcode size - small, typically 2-4 bytes)
                4:32/little,
                % st_info (STB_GLOBAL << 4 | STT_NOTYPE)
                16#10,
                % st_other
                0,
                % st_shndx (section index - .text will be section 1)
                1:16/little
            >>,
            <<Acc/binary, Symbol/binary>>
        end,
        <<>>,
        lists:zip(Opcodes, OpcodeStringOffsets)
    ),

    % Generate label symbols
    LabelStringOffsets = lists:sublist(
        ReversedOffsets, length(Functions) + length(Opcodes) + 1, length(Labels)
    ),
    LabelSymbols = lists:foldl(
        fun({{Offset, _LabelNum}, StringOffset}, Acc) ->
            % Use raw offset for symbol address
            LabelAddress = Offset,

            % Symbol table entry (16 bytes for 32-bit ELF)
            Symbol = <<
                % st_name (offset in string table)
                StringOffset:32/little,
                % st_value (label address)
                LabelAddress:32/little,
                % st_size (label size - 0 for point labels)
                0:32/little,
                % st_info (STB_GLOBAL << 4 | STT_NOTYPE)
                16#10,
                % st_other
                0,
                % st_shndx (section index - .text will be section 1)
                1:16/little
            >>,
            <<Acc/binary, Symbol/binary>>
        end,
        <<>>,
        lists:zip(Labels, LabelStringOffsets)
    ),

    % Generate mapping symbols for ARM (Thumb indicator)
    MappingSymbolOffsets =
        case Backend of
            jit_armv6m ->
                lists:sublist(
                    ReversedOffsets, length(Functions) + length(Opcodes) + length(Labels) + 1, 1
                );
            _ ->
                []
        end,
    MappingSymbolBinaries =
        case Backend of
            jit_armv6m ->
                [StringOffset] = MappingSymbolOffsets,
                % $t mapping symbol at address 0 (start of .text) to indicate Thumb code
                MappingSymbol = <<
                    % st_name (offset in string table for "$t")
                    StringOffset:32/little,
                    % st_value (address 0 - start of .text section)
                    0:32/little,
                    % st_size (0 for mapping symbols)
                    0:32/little,
                    % st_info (STB_LOCAL << 4 | STT_NOTYPE) - local symbol
                    16#00,
                    % st_other
                    0,
                    % st_shndx (section index - .text will be section 1)
                    1:16/little
                >>,
                <<MappingSymbol/binary>>;
            _ ->
                <<>>
        end,

    % Symbol table must have local symbols first, then global symbols
    SymtabContent =
        <<NullSymbol/binary, MappingSymbolBinaries/binary, FunctionSymbols/binary,
            OpcodeSymbols/binary, LabelSymbols/binary>>,

    {SymtabContent, StrtabContent}.

%% Create string table from list of binaries
create_string_table(Binaries) ->
    <<<<Binary/binary, 0>> || Binary <- Binaries>>.

%% Layout sections in memory and calculate offsets
layout_sections(Sections, ShStrTab, BaseOffset) ->
    {Data, Offsets} = lists:foldl(
        fun({_Name, SectionData}, {AccData, AccOffsets}) ->
            Offset = BaseOffset + byte_size(AccData),
            NewData = <<AccData/binary, SectionData/binary>>,
            NewOffsets = [Offset | AccOffsets],
            {NewData, NewOffsets}
        end,
        {<<>>, []},
        Sections
    ),

    % Add string table at the end
    ShStrTabOffset = BaseOffset + byte_size(Data),
    FinalData = <<Data/binary, ShStrTab/binary>>,
    FinalOffsets = [ShStrTabOffset | lists:reverse(Offsets)],

    {FinalData, FinalOffsets}.

%% Create properly formatted section headers
create_section_headers_proper(SectionNames, Sections, SectionOffsets, ShStrTab, Backend) ->
    % Create null section header (index 0)

    % 40 bytes of zeros
    NullHeader = <<0:320/little>>,

    % Create section headers for all sections (indices 1-6)
    % SectionOffsets from layout_sections: [ShStrTabOffset, ...SectionOffsets in order...]
    [_ShStrTabOffset | SectionOffsetsInOrder] = SectionOffsets,

    SectionHeaders = lists:foldl(
        fun({_Index, {{SectionName, SectionData}, FileOffset}}, Acc) ->
            % Calculate name offset in string table by finding the null-terminated section name
            SectionNameWithNull = <<SectionName/binary, 0>>,
            {NameOffset, _Length} = binary:match(ShStrTab, SectionNameWithNull),

            % Determine section type and properties
            {SectionType, Link, Info, EntrySize} =
                case SectionName of
                    <<".symtab">> ->
                        % Find .strtab index dynamically
                        StrtabIndex = find_section_index(<<".strtab">>, SectionNames),
                        % Local symbols: null symbol + mapping symbol (for armv6m)
                        NumLocalSymbols =
                            case Backend of
                                % null + $t mapping symbol
                                jit_armv6m -> 2;
                                % only null symbol
                                _ -> 1
                            end,
                        % SHT_SYMTAB, link to strtab, info = first non-local symbol, entsize = 16
                        {?SHT_SYMTAB, StrtabIndex, NumLocalSymbols, 16};
                    % SHT_STRTAB
                    <<".strtab">> ->
                        {3, 0, 0, 0};
                    % ARM attributes
                    <<".ARM.attributes">> ->
                        {?SHT_ARM_ATTRIBUTES, 0, 0, 0};
                    % Debug sections
                    _ ->
                        {?SHT_PROGBITS, 0, 0, 0}
                end,

            Header = <<
                % Name offset
                NameOffset:32/little,
                % Type
                SectionType:32/little,
                % Flags
                0:32/little,
                % Address
                0:32/little,
                % File offset
                FileOffset:32/little,
                % Size
                (byte_size(SectionData)):32/little,
                % Link
                Link:32/little,
                % Info
                Info:32/little,
                % Address align
                1:32/little,
                % Entry size
                EntrySize:32/little
            >>,
            <<Acc/binary, Header/binary>>
        end,
        <<>>,
        lists:zip(lists:seq(1, length(Sections)), lists:zip(Sections, SectionOffsetsInOrder))
    ),

    % Create string table section header (index 7, the last section)

    % Calculate offset for ".shstrtab"
    ShStrTabNameWithNull = <<".shstrtab", 0>>,
    {ShStrTabNameOffset, _Length} = binary:match(ShStrTab, ShStrTabNameWithNull),
    % First in offsets (ShStrTabOffset is added at the beginning)
    ShStrTabFileOffset = lists:nth(1, SectionOffsets),
    ShStrTabHeader = <<
        % Name offset
        ShStrTabNameOffset:32/little,
        % Type
        ?SHT_STRTAB:32/little,
        % Flags
        0:32/little,
        % Address
        0:32/little,
        % File offset
        ShStrTabFileOffset:32/little,
        % Size
        (byte_size(ShStrTab)):32/little,
        % Link
        0:32/little,
        % Info
        0:32/little,
        % Address align
        1:32/little,
        % Entry size
        0:32/little
    >>,

    <<NullHeader/binary, SectionHeaders/binary, ShStrTabHeader/binary>>.

%% @doc Add .text section containing native code to existing debug-only ELF
add_text_section_to_elf(DebugELF, NativeCode) ->
    % Parse full ELF header (52 bytes) to get section info
    <<_ElfMagic:16/binary, _Type:16/little, _Machine:16/little, _Version:32/little,
        _Entry:32/little, _PHOff:32/little, SHOff:32/little, _Flags:32/little, _EHSize:16/little,
        _PHEntSize:16/little, _PHNum:16/little, _SHEntSize:16/little, SHNum:16/little,
        SHStrNdx:16/little, RestOfFile/binary>> = DebugELF,

    % Parse section headers to find string table
    SectionHeadersStart = SHOff - 52,
    <<SectionData:SectionHeadersStart/binary, SectionHeaders/binary>> = RestOfFile,

    % Extract string table section header (SHStrNdx index)
    SectionHeaderSize = 40,
    StringTableHeaderOffset = SHStrNdx * SectionHeaderSize,
    <<_:StringTableHeaderOffset/binary, _StrName:32/little, _StrType:32/little, _StrFlags:32/little,
        _StrAddr:32/little, StrOffset:32/little, StrSize:32/little, _StrLink:32/little,
        _StrInfo:32/little, _StrAlign:32/little, _StrEntSize:32/little, _/binary>> = SectionHeaders,

    % Extract current string table
    % Relative to after ELF header
    StrTableFileOffset = StrOffset - 52,
    <<_:StrTableFileOffset/binary, CurrentStrTable:StrSize/binary, _/binary>> = SectionData,

    % Add ".text" to string table
    % Offset where ".text" will be added
    TextNameOffset = StrSize,
    NewStrTable = <<CurrentStrTable/binary, ".text", 0>>,
    NewStrTableSize = byte_size(NewStrTable),

    % Replace old string table in section data
    <<PreStrTable:StrTableFileOffset/binary, _:StrSize/binary, PostStrTable/binary>> = SectionData,
    UpdatedSectionData = <<PreStrTable/binary, NewStrTable/binary, PostStrTable/binary>>,

    % Update symbol table to point to .text section (which will be section index SHNum)
    TextSectionIndex = SHNum,
    UpdatedSectionDataWithSymbols = update_symbol_table_in_data(
        UpdatedSectionData, SectionHeaders, TextSectionIndex
    ),

    % Append .text section data
    TextSectionOffset = 52 + byte_size(UpdatedSectionDataWithSymbols),
    TextSectionSize = byte_size(NativeCode),
    FinalSectionData = <<UpdatedSectionDataWithSymbols/binary, NativeCode/binary>>,

    % Create .text section header
    TextSectionHeader = <<
        % Name offset in string table
        TextNameOffset:32/little,
        % Type
        ?SHT_PROGBITS:32/little,
        % Flags
        (?SHF_ALLOC bor ?SHF_EXECINSTR):32/little,
        % Address
        0:32/little,
        % File offset
        TextSectionOffset:32/little,
        % Size
        TextSectionSize:32/little,
        % Link
        0:32/little,
        % Info
        0:32/little,
        % Address alignment
        4:32/little,
        % Entry size
        0:32/little
    >>,

    % Update string table section header with new size
    UpdatedStrTableHeader = <<
        _StrName:32/little,
        _StrType:32/little,
        _StrFlags:32/little,
        _StrAddr:32/little,
        StrOffset:32/little,
        NewStrTableSize:32/little,
        _StrLink:32/little,
        _StrInfo:32/little,
        _StrAlign:32/little,
        _StrEntSize:32/little
    >>,

    % Update section headers - append text section header at the end
    <<PreStrHeader:StringTableHeaderOffset/binary, _:SectionHeaderSize/binary,
        PostStrHeader/binary>> = SectionHeaders,
    FinalSectionHeaders =
        <<PreStrHeader/binary, UpdatedStrTableHeader/binary, PostStrHeader/binary,
            TextSectionHeader/binary>>,

    % Update ELF header with new section count and header offset
    NewSHOff = TextSectionOffset + TextSectionSize,
    NewSHNum = SHNum + 1,
    UpdatedElfHeader =
        <<_ElfMagic:16/binary, _Type:16/little, _Machine:16/little, _Version:32/little,
            _Entry:32/little, _PHOff:32/little, NewSHOff:32/little, _Flags:32/little,
            _EHSize:16/little, _PHEntSize:16/little, _PHNum:16/little, _SHEntSize:16/little,
            NewSHNum:16/little, SHStrNdx:16/little>>,

    % Final ELF: header + section data + all section headers together
    <<UpdatedElfHeader/binary, FinalSectionData/binary, FinalSectionHeaders/binary>>.

%% @doc Update symbol table data to point function symbols to .text section
update_symbol_table_in_data(SectionData, SectionHeaders, TextSectionIndex) ->
    % Find .symtab section index dynamically
    % Need to parse section headers to find .symtab
    SymtabSectionIndex = find_symtab_section_index(SectionHeaders),
    SectionHeaderSize = 40,
    SymtabHeaderOffset = SymtabSectionIndex * SectionHeaderSize,

    <<_:SymtabHeaderOffset/binary, _SymtabName:32/little, _SymtabType:32/little,
        _SymtabFlags:32/little, _SymtabAddr:32/little, SymtabOffset:32/little, SymtabSize:32/little,
        _/binary>> = SectionHeaders,

    % Symbol table is at SymtabOffset - 52 (ELF header) in section data
    SymtabFileOffset = SymtabOffset - 52,

    % Extract symbol table data
    <<PreSymtab:SymtabFileOffset/binary, SymbolTable:SymtabSize/binary, PostSymtab/binary>> =
        SectionData,

    % Update symbol table entries - each entry is 16 bytes
    % Skip the null symbol (first 16 bytes) and update the rest
    <<NullSymbol:16/binary, FunctionSymbols/binary>> = SymbolTable,
    UpdatedFunctionSymbols = update_function_symbols(FunctionSymbols, TextSectionIndex, <<>>),
    UpdatedSymbolTable = <<NullSymbol/binary, UpdatedFunctionSymbols/binary>>,

    % Replace symbol table in section data
    <<PreSymtab/binary, UpdatedSymbolTable/binary, PostSymtab/binary>>.

%% @doc Update function symbol entries to point to .text section
update_function_symbols(<<>>, _TextSectionIndex, Acc) ->
    Acc;
update_function_symbols(
    <<NameOffset:32/little, Address:32/little, Size:32/little, Info, Other,
        _OldSectionIndex:16/little, Rest/binary>>,
    TextSectionIndex,
    Acc
) ->
    % Update section index to point to .text section
    UpdatedSymbol =
        <<NameOffset:32/little, Address:32/little, Size:32/little, Info, Other,
            TextSectionIndex:16/little>>,
    update_function_symbols(Rest, TextSectionIndex, <<Acc/binary, UpdatedSymbol/binary>>).

-endif.
