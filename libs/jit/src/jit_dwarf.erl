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

-include("jit_dwarf.hrl").

-record(dwarf, {
    % Backend module (jit_armv6m, etc.)
    backend :: module(),
    % Current module being compiled
    module_name :: module(),
    opcodes = [] :: [{Offset :: non_neg_integer(), Opcode :: atom(), Size :: non_neg_integer()}],
    labels = [] :: [{Offset :: non_neg_integer(), Label :: non_neg_integer()}],
    functions = [] :: [
        {Offset :: non_neg_integer(), FunctionName :: atom(), Arity :: non_neg_integer()}
    ],
    lines = [] :: [
        {Offset :: non_neg_integer(), Filename :: binary(), LineNumber :: pos_integer()}
    ],
    stream_module :: module(),
    stream :: any(),
    line_resolver :: fun((non_neg_integer()) -> false | {ok, binary(), pos_integer()})
}).

-type state() :: #dwarf{}.

-export([
    new/5,
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
-spec new(module(), module(), module(), pos_integer(), fun(
    (non_neg_integer()) -> false | {ok, binary(), pos_integer()}
)) -> state().
new(Backend, ModuleName, StreamModule, MaxSize, LineResolver) ->
    Stream = StreamModule:new(MaxSize),
    #dwarf{
        backend = Backend,
        module_name = ModuleName,
        stream_module = StreamModule,
        stream = Stream,
        line_resolver = LineResolver,
        % Add jump table symbol at offset 0, size will be calculated
        opcodes = [{0, jump_table, 0}]
    }.

%%-----------------------------------------------------------------------------
%% @param Stream    stream to get the offset from
%% @returns The current offset
%% @doc     Get the current offset in the stream
%% @end
%%-----------------------------------------------------------------------------
-spec offset(state()) -> non_neg_integer().
offset(#dwarf{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to append to
%% @param Binary    binary to append to the stream
%% @returns The updated stream
%% @doc     Append a binary to the stream
%% @end
%%-----------------------------------------------------------------------------
-spec append(state(), binary()) -> state().
append(#dwarf{stream_module = StreamModule, stream = Stream0} = State, Binary) ->
    Stream1 = StreamModule:append(Stream0, Binary),
    State#dwarf{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param Stream        stream to update
%% @param Offset        offset to update from
%% @param Replacement   binary to write at offset
%% @returns The updated stream
%% @doc     Replace bytes at a given offset
%% @end
%%-----------------------------------------------------------------------------
-spec replace(state(), non_neg_integer(), binary()) -> state().
replace(#dwarf{stream_module = StreamModule, stream = Stream0} = State, Offset, Replacement) ->
    Stream1 = StreamModule:replace(Stream0, Offset, Replacement),
    State#dwarf{stream = Stream1}.

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
map(#dwarf{stream_module = StreamModule, stream = Stream0} = State, Offset, Length, MapFunction) ->
    Stream1 = StreamModule:map(Stream0, Offset, Length, MapFunction),
    State#dwarf{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param State    current state
%% @param Opcode   the opcode atom to record
%% @returns The updated state with opcode recorded at current offset
%% @doc     Record an opcode at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec opcode(state(), binary()) -> state();
            (any(), binary()) -> any().
opcode(#dwarf{stream_module = StreamModule, stream = Stream, opcodes = Opcodes0} = State, Opcode) ->
    Offset = StreamModule:offset(Stream),
    % Update size of previous opcode and add new opcode
    Opcodes1 = update_previous_opcode_size(Opcodes0, Offset),
    % Size will be calculated later
    Opcodes2 = [{Offset, Opcode, 0} | Opcodes1],
    State#dwarf{opcodes = Opcodes2};
opcode(BackendStateDwarfDisabled, _Opcode) ->
    BackendStateDwarfDisabled.

%%-----------------------------------------------------------------------------
%% @param State    current state
%% @param Label    the label number to record
%% @returns The updated state with label recorded at current offset
%% @doc     Record a label at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec label(state(), non_neg_integer()) -> state();
           (any(), non_neg_integer()) -> any().
label(
    #dwarf{stream_module = StreamModule, stream = Stream, labels = Labels0, opcodes = Opcodes0} =
        State,
    Label
) ->
    Offset = StreamModule:offset(Stream),
    % Update size of previous opcode before adding label
    Opcodes1 = update_previous_opcode_size(Opcodes0, Offset),
    Labels1 = [{Offset, Label} | Labels0],
    State#dwarf{labels = Labels1, opcodes = Opcodes1};
label(BackendStateDwarfDisabled, _Label) ->
    BackendStateDwarfDisabled.

%%-----------------------------------------------------------------------------
%% @param State         current state
%% @param FunctionName  the function name atom to record
%% @param Arity         the function arity
%% @returns The updated state with function recorded at current offset
%% @doc     Record a function at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec function(state(), atom(), non_neg_integer()) -> state();
              (any(), atom(), non_neg_integer()) -> any().
function(
    #dwarf{stream_module = StreamModule, stream = Stream, functions = Functions0} = State,
    FunctionName,
    Arity
) ->
    Offset = StreamModule:offset(Stream),
    Functions1 = [{Offset, FunctionName, Arity} | Functions0],
    State#dwarf{functions = Functions1};
function(BackendStateDwarfDisabled, _FunctionName, _Arity) ->
    BackendStateDwarfDisabled.

%%-----------------------------------------------------------------------------
%% @param State    current state
%% @param Line     the line number to record
%% @returns The updated state with line recorded at current offset
%% @doc     Record a line number at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec line(state(), pos_integer()) -> state();
          (any(), pos_integer()) -> any().
line(
    #dwarf{
        stream_module = StreamModule,
        stream = Stream,
        lines = Lines0,
        line_resolver = LineResolver,
        module_name = ModuleName
    } = State,
    LineRef
) ->
    Offset = StreamModule:offset(Stream),
    case LineResolver(LineRef) of
        {ok, Filename, LineNumber} ->
            % Check if this is the first time we see the module file and add line 1 at offset 0
            Lines1 = maybe_add_initial_line(Lines0, ModuleName, Filename),
            Lines2 = [{Offset, Filename, LineNumber} | Lines1],
            State#dwarf{lines = Lines2};
        false ->
            % No line information available, skip storing this line
            State
    end;
line(BackendStateDwarfDisabled, _LineRef) ->
    BackendStateDwarfDisabled.

%% Helper function to add line 1 at offset 0 for the module file if not already present
maybe_add_initial_line(Lines, ModuleName, Filename) ->
    ExpectedBasename = <<(atom_to_binary(ModuleName, utf8))/binary, ".erl">>,
    Basename = lists:last(binary:split(Filename, <<"/">>, [global])),
    case Basename =:= ExpectedBasename of
        true ->
            % This is the module file, check if we already have an entry at offset 0
            case lists:any(fun({Offset, _, _}) -> Offset =:= 0 end, Lines) of
                false ->
                    % Add line 1 at offset 0 for the jump table
                    [{0, Filename, 1} | Lines];
                true ->
                    % Already have an entry at offset 0, don't duplicate
                    Lines
            end;
        false ->
            % Not the module file, no change needed
            Lines
    end.

%% Helper function to update the size of the most recent opcode
update_previous_opcode_size([], _NewOffset) ->
    % No previous opcode to update
    [];
update_previous_opcode_size([{Offset, Opcode, 0} | Rest], NewOffset) ->
    % Update the size of the most recent opcode
    Size = NewOffset - Offset,
    [{Offset, Opcode, Size} | Rest];
update_previous_opcode_size([{Offset, Opcode, Size} | Rest], _NewOffset) when Size > 0 ->
    % Previous opcode already has a calculated size, don't change it
    [{Offset, Opcode, Size} | Rest];
update_previous_opcode_size(Opcodes, _NewOffset) ->
    % Unexpected format, return unchanged
    Opcodes.

-spec stream(state()) -> any().
stream(#dwarf{stream = Stream}) ->
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
elf(#dwarf{module_name = ModuleName, backend = Backend} = State, NativeCode) ->
    SourceFile = <<(atom_to_binary(ModuleName, utf8))/binary, ".erl">>,

    % Generate DWARF sections
    DebugInfoSection = generate_debug_info_section_with_opcodes(State, SourceFile),
    DebugLineSection = generate_debug_line_section(State, SourceFile),
    DebugAbbrevSection = generate_debug_abbrev_section_with_opcodes(),
    DebugStrSection = generate_debug_str_section(State, SourceFile),
    DebugArangesSection = generate_debug_aranges_section(State),

    % Generate symbol table sections for function names
    {SymtabSection, StrtabSection} = generate_symbol_table(State, Backend),

    % Create base sections list
    BaseSections = [
        {<<".debug_info">>, DebugInfoSection},
        {<<".debug_line">>, DebugLineSection},
        {<<".debug_abbrev">>, DebugAbbrevSection},
        {<<".debug_str">>, DebugStrSection},
        {<<".debug_aranges">>, DebugArangesSection},
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

    % Create complete ELF with text section and debug sections
    {CombinedELF, TextSectionOffset} = create_elf_with_text_and_debug_sections(
        Backend, Sections, NativeCode
    ),
    {ok, TextSectionOffset, CombinedELF}.
-else.
elf(_State, _NativeCode) ->
    false.
-endif.

-ifdef(JIT_DWARF).

%% Map JIT backend to ELF machine type
backend_to_machine_type(jit_x86_64) -> ?EM_X86_64;
backend_to_machine_type(jit_aarch64) -> ?EM_AARCH64;
backend_to_machine_type(jit_armv6m) -> ?EM_ARM;
backend_to_machine_type(jit_riscv32) -> ?EM_RISCV.

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

generate_debug_str_section(#dwarf{module_name = ModuleName}, SourceFile) ->
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

generate_debug_aranges_section(#dwarf{backend = Backend} = State) ->
    % Get word size and calculate address range
    WordSize = Backend:word_size(),
    WordSizeInBits = WordSize * 8,
    {LowPC, HighPC} = calculate_address_range(State),
    Length = HighPC - LowPC,

    % Calculate padding needed to align descriptor to 2*address_size
    % Header so far: version(2) + debug_info_offset(4) + addr_size(1) + seg_size(1) = 8 bytes
    % Need to align to 2*WordSize boundary
    HeaderSize = 8,
    TupleAlignment = 2 * WordSize,
    PaddingSize = (TupleAlignment - (HeaderSize rem TupleAlignment)) rem TupleAlignment,
    Padding = <<0:(PaddingSize*8)/little>>,

    % Header
    Header = <<
        % DWARF version
        2:16/little,
        % Debug info offset (always 0 - first compile unit)
        0:32/little,
        % Address size
        WordSize,
        % Segment size (0 for flat address space)
        0
    >>,

    % Address descriptors
    Descriptors = <<
        % Address range descriptor
        LowPC:WordSizeInBits/little,  % Start address
        Length:WordSizeInBits/little,  % Length
        % Terminating entry (two zero addresses)
        0:WordSizeInBits/little,
        0:WordSizeInBits/little
    >>,

    % Combine all parts
    HeaderAndTable = <<Header/binary, Padding/binary, Descriptors/binary>>,

    % Calculate total length (header + table - 4 for the length field itself)
    TotalLength = byte_size(HeaderAndTable),

    % Build final section with length prefix
    <<TotalLength:32/little, HeaderAndTable/binary>>.

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
        ?DW_FORM_sec_offset,
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
        % Has children (ctx parameter)
        1,
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

        % Abbrev 5: DW_TAG_formal_parameter (for ctx parameter with type)
        % Abbreviation code
        5,
        % Tag
        ?DW_TAG_formal_parameter,
        % Has no children
        0,
        % Name attribute (parameter name)
        ?DW_AT_name,
        ?DW_FORM_string,
        % Type attribute (reference to type DIE)
        ?DW_AT_type,
        ?DW_FORM_ref4,
        % Location attribute (register location)
        ?DW_AT_location,
        ?DW_FORM_exprloc,
        % End of attributes
        0,
        0,

        % Abbrev 6: DW_TAG_base_type (for term/uintptr_t)
        % Abbreviation code
        6,
        % Tag
        ?DW_TAG_base_type,
        % Has no children
        0,
        % Name attribute
        ?DW_AT_name,
        ?DW_FORM_string,
        % Byte size
        ?DW_AT_byte_size,
        ?DW_FORM_data1,
        % Encoding
        ?DW_AT_encoding,
        ?DW_FORM_data1,
        % End of attributes
        0,
        0,

        % Abbrev 7: DW_TAG_pointer_type (for Context*)
        % Abbreviation code
        7,
        % Tag
        ?DW_TAG_pointer_type,
        % Has no children
        0,
        % Byte size
        ?DW_AT_byte_size,
        ?DW_FORM_data1,
        % Type attribute (points to Context structure)
        ?DW_AT_type,
        ?DW_FORM_ref4,
        % End of attributes
        0,
        0,

        % Abbrev 8: DW_TAG_structure_type (for Context)
        % Abbreviation code
        8,
        % Tag
        ?DW_TAG_structure_type,
        % Has children (members)
        1,
        % Name attribute
        ?DW_AT_name,
        ?DW_FORM_string,
        % Byte size
        ?DW_AT_byte_size,
        ?DW_FORM_data4,
        % End of attributes
        0,
        0,

        % Abbrev 9: DW_TAG_member (for structure members)
        % Abbreviation code
        9,
        % Tag
        ?DW_TAG_member,
        % Has no children
        0,
        % Name attribute
        ?DW_AT_name,
        ?DW_FORM_string,
        % Type attribute
        ?DW_AT_type,
        ?DW_FORM_ref4,
        % Data member location (offset from structure start)
        ?DW_AT_data_member_location,
        ?DW_FORM_data4,
        % End of attributes
        0,
        0,

        % Abbrev 10: DW_TAG_array_type (for term x[MAX_REG+1])
        % Abbreviation code
        10,
        % Tag
        ?DW_TAG_array_type,
        % Has children (subrange)
        1,
        % Type attribute (element type)
        ?DW_AT_type,
        ?DW_FORM_ref4,
        % End of attributes
        0,
        0,

        % Abbrev 11: DW_TAG_subrange_type (for array bounds)
        % Abbreviation code
        11,
        % Tag
        ?DW_TAG_subrange_type,
        % Has no children
        0,
        % Upper bound
        ?DW_AT_upper_bound,
        ?DW_FORM_data1,
        % End of attributes
        0,
        0,

        % End of abbreviations
        0
    >>.

generate_debug_info_section_with_opcodes(
    #dwarf{functions = Functions, opcodes = Opcodes, labels = Labels, module_name = ModuleName, backend = Backend} =
        State,
    SourceFile
) ->
    % Calculate address ranges
    {LowPC, HighPC} = calculate_address_range(State),

    % Get word size from backend and convert to bits
    WordSize = Backend:word_size(),
    WordSizeInBits = WordSize * 8,

    % Build content first to calculate actual length
    CompileUnitContent = <<
        % DWARF version
        4:16/little,
        % Abbreviation offset
        0:32/little,
        % Address size
        WordSize,
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
        ?DW_LANG_Erlang:32/little, % for now, we always say Erlang
        % DW_AT_low_pc
        LowPC:WordSizeInBits/little,
        % DW_AT_high_pc
        HighPC:WordSizeInBits/little,
        % DW_AT_stmt_list (offset into .debug_line)
        0:32/little
    >>,

    % Calculate base offset for type DIEs
    % DW_FORM_ref4 offsets are relative to start of compile unit (the length field itself)
    % So we need to add 4 bytes for the length field
    % CompileUnitContent already includes the header (version + abbrev_offset + addr_size)
    TypeDIEsBaseOffset = 4 + byte_size(CompileUnitContent),
    io:format("DEBUG CU: ContentSize=~p + 4 (length) = ~p~n",
              [byte_size(CompileUnitContent), TypeDIEsBaseOffset]),

    % Generate type DIEs and get the Context* type offset
    {TypeDIEs, ContextPtrTypeOffset} = generate_type_dies(State, TypeDIEsBaseOffset),

    % Generate DIEs for functions, opcodes and labels
    FunctionDIEs = generate_function_dies_with_module(Functions, ModuleName, State, ContextPtrTypeOffset, HighPC),
    OpcodeDIEs = generate_opcode_dies(Opcodes, Backend),
    LabelDIEs = generate_label_dies(Labels, Backend),

    % End of children marker
    EndMarker = <<0>>,

    % Calculate actual unit length (everything after the length field)
    Content =
        <<CompileUnitContent/binary, TypeDIEs/binary, FunctionDIEs/binary, OpcodeDIEs/binary, LabelDIEs/binary,
            EndMarker/binary>>,
    UnitLength = byte_size(Content),

    % Build final section with correct length
    <<UnitLength:32/little, Content/binary>>.

generate_debug_line_section(#dwarf{lines = Lines, opcodes = _Opcodes}, SourceFile) ->
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

    % Standard opcode lengths (for opcodes 1-12, opcode_base-1 entries)
    % DW_LNS_copy(1)=0, DW_LNS_advance_pc(2)=1, DW_LNS_advance_line(3)=1, etc.
    StdOpcodeLengths = <<0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1>>,

    % Build file table with actual filenames from line data
    UniqueFullPaths =
        case Lines of
            [] ->
                [SourceFile];
            _ ->
                % Extract unique filenames from Lines, don't add SourceFile as it may be a duplicate
                Filenames = [Filename || {_Offset, Filename, _LineNum} <- Lines],
                lists:usort(Filenames)
        end,

    % Split paths into directories and filenames, avoiding duplicates
    {Directories, FileEntries, _} = lists:foldl(
        fun(FullPath, {DirAcc, FileAcc, FileSet}) ->
            case filename:split(binary_to_list(FullPath)) of
                [Basename] ->
                    % Just a filename, no directory
                    FileKey = {Basename, 0},
                    case sets:is_element(FileKey, FileSet) of
                        % Skip duplicate
                        true -> {DirAcc, FileAcc, FileSet};
                        false -> {DirAcc, [FileKey | FileAcc], sets:add_element(FileKey, FileSet)}
                    end;
                PathParts ->
                    DirParts = lists:droplast(PathParts),
                    Dir = filename:join(DirParts),
                    Basename = lists:last(PathParts),
                    % Find or add directory to get proper index (1-based)
                    {NewDirAcc, DirIndex} =
                        case lists:search(fun(D) -> D =:= Dir end, DirAcc) of
                            {value, _} ->
                                % Find index of existing directory (1-based)
                                ExistingIndex =
                                    length(lists:takewhile(fun(D) -> D =/= Dir end, DirAcc)) + 1,
                                {DirAcc, ExistingIndex};
                            false ->
                                % Add new directory and return its 1-based index
                                NewIndex = length(DirAcc) + 1,
                                {DirAcc ++ [Dir], NewIndex}
                        end,
                    FileKey = {Basename, DirIndex},
                    case sets:is_element(FileKey, FileSet) of
                        % Skip duplicate
                        true ->
                            {NewDirAcc, FileAcc, FileSet};
                        false ->
                            {NewDirAcc, [FileKey | FileAcc], sets:add_element(FileKey, FileSet)}
                    end
            end
        end,
        {[], [], sets:new()},
        UniqueFullPaths
    ),

    % Build directory table
    DirectoryTable = lists:foldl(
        fun(Dir, Acc) ->
            DirBin = list_to_binary(Dir),
            <<Acc/binary, DirBin/binary, 0>>
        end,
        <<>>,
        Directories
    ),

    % Build file table entries with proper ULEB128 encoding for directory index
    FileTableEntries = lists:foldl(
        fun({Filename, DirIndex}, Acc) ->
            DirIndexEncoded = encode_uleb128(DirIndex),
            <<Acc/binary, (list_to_binary(Filename))/binary, 0, DirIndexEncoded/binary, 0, 0>>
        end,
        <<>>,
        lists:reverse(FileEntries)
    ),

    FileTable = <<
        % Directory table
        DirectoryTable/binary,
        % End of directory table
        0,
        % File table entries
        FileTableEntries/binary,
        % End of file table
        0
    >>,

    % Line number program - using actual line data with file mapping
    FileMapping = lists:zip(UniqueFullPaths, lists:seq(1, length(FileEntries))),
    Program = generate_line_program(Lines, FileMapping),

    % Calculate actual header length (everything after header_length field to end of file table)
    HeaderPlusTablesContent = <<StdOpcodeLengths/binary, FileTable/binary>>,
    % -6 to exclude version (2 bytes) and header_length field itself (4 bytes)
    DebugHeaderContentSize = byte_size(HeaderContent),
    DebugStdOpcodeSize = byte_size(StdOpcodeLengths),
    DebugFileTableSize = byte_size(FileTable),
    DebugHeaderPlusTablesSize = byte_size(HeaderPlusTablesContent),
    io:format("DEBUG: HeaderContent=~p StdOpcodes=~p FileTable=~p HeaderPlusTables=~p~n",
              [DebugHeaderContentSize, DebugStdOpcodeSize, DebugFileTableSize, DebugHeaderPlusTablesSize]),
    HeaderLength = byte_size(HeaderContent) - 6 + byte_size(HeaderPlusTablesContent),
    io:format("DEBUG: HeaderLength = ~p - 6 + ~p = ~p~n",
              [DebugHeaderContentSize, DebugHeaderPlusTablesSize, HeaderLength]),

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
    % Determine ELF format based on backend word size
    WordSize = Backend:word_size(),
    % 32 or 64 bits
    WordSizeInBits = WordSize * 8,
    ElfClass =
        case WordSize of
            8 -> ?ELFCLASS64;
            4 -> ?ELFCLASS32
        end,

    % ELF format dependent sizes
    {ElfHeaderSize, SectionHeaderSize} =
        case WordSize of
            % ELF64
            8 -> {64, 64};
            % ELF32
            4 -> {52, 40}
        end,

    % Create section name string table (dynamic based on sections)
    SectionNames =
        [<<>>] ++ [SectionName || {SectionName, _Section} <- Sections] ++ [<<".shstrtab">>],
    ShStrTab = create_string_table(SectionNames),

    % Calculate offsets
    % null + debug sections + shstrtab
    SectionCount = length(SectionNames),

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
        % ELF class (32-bit or 64-bit)
        ElfClass,
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
        % Entry point - 32 or 64 bit depending on word size
        0:WordSizeInBits/little,
        % Program header offset - 32 or 64 bit depending on word size
        0:WordSizeInBits/little,
        % Section header offset - 32 or 64 bit depending on word size
        SectionHeaderOffset:WordSizeInBits/little,
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
        SectionNames, Sections, SectionOffsets, ShStrTab, Backend, WordSizeInBits
    ),

    <<ElfHeader/binary, SectionData/binary, SectionHeaders/binary>>.

%% Helper functions
calculate_address_range(#dwarf{opcodes = Opcodes}) ->
    case Opcodes of
        [] ->
            {0, 0};
        _ ->
            % Use the new 3-tuple format {Offset, Opcode, Size}
            OffsetsAndSizes = [{Offset, Size} || {Offset, _, Size} <- Opcodes],
            Offsets = [Offset || {Offset, _} <- OffsetsAndSizes],
            MinOffset = lists:min(Offsets),
            % For max, use offset + size, or fallback to offset + 4 if size is 0
            MaxOffset = lists:max([
                case Size of
                    % Fallback for opcodes without calculated size
                    0 -> Offset + 4;
                    _ -> Offset + Size
                end
             || {Offset, Size} <- OffsetsAndSizes
            ]),
            {MinOffset, MaxOffset}
    end.

generate_line_program(Lines, FileMapping) ->
    case Lines of
        [] ->
            % No line data - generate simple program
            <<
                % Set file to 1 using DW_LNS_set_file (opcode 4) with file index 1
                4,
                1,
                % End sequence: extended opcode
                % Extended opcode prefix
                0,
                % Length of extended opcode
                1,
                % DW_LNE_end_sequence
                1
            >>;
        _ ->
            % Sort lines by offset
            SortedLines = lists:sort(
                fun({OffsetA, _, _}, {OffsetB, _, _}) ->
                    OffsetA =< OffsetB
                end,
                Lines
            ),
            generate_line_program_entries(SortedLines, FileMapping, 0, 1, 0)
    end.

generate_line_program_entries([], _FileMapping, _LastOffset, _LastLine, _LastFileIndex) ->
    % End the sequence
    <<
        % End sequence: extended opcode
        % Extended opcode prefix
        0,
        % Length of extended opcode
        1,
        % DW_LNE_end_sequence
        1
    >>;
generate_line_program_entries(
    [{Offset, Filename, LineNumber} | Rest], FileMapping, LastOffset, LastLine, LastFileIndex
) ->
    % Generate DWARF line program opcodes
    % For simplicity, we'll use DW_LNS_advance_pc and DW_LNS_advance_line

    % Find file index from mapping
    FileIndex =
        case lists:keyfind(Filename, 1, FileMapping) of
            {Filename, Index} -> Index;
            % Default to first file if not found
            false -> 1
        end,

    % Calculate address and line deltas
    AddressDelta = Offset - LastOffset,
    LineDelta = LineNumber - LastLine,

    % Build opcodes
    FileOpcodes =
        if
            FileIndex =/= LastFileIndex ->
                % DW_LNS_set_file (opcode 4) with file index
                <<4, FileIndex>>;
            true ->
                <<>>
        end,

    InitialOpcodes =
        if
            LastOffset == 0 ->
                % Set initial file index
                <<4, FileIndex>>;
            true ->
                FileOpcodes
        end,

    Opcodes = <<
        InitialOpcodes/binary,
        % DW_LNS_advance_pc (opcode 2) with ULEB128 delta
        2,
        (encode_uleb128(AddressDelta))/binary,
        % DW_LNS_advance_line (opcode 3) with SLEB128 delta
        3,
        (encode_sleb128(LineDelta))/binary,
        % DW_LNS_copy (opcode 1) - emit a new row
        1
    >>,

    RestOpcodes = generate_line_program_entries(Rest, FileMapping, Offset, LineNumber, FileIndex),
    <<Opcodes/binary, RestOpcodes/binary>>.

% Encode unsigned LEB128
encode_uleb128(Value) when Value < 128 ->
    <<Value>>;
encode_uleb128(Value) ->
    Byte = (Value band 16#7F) bor 16#80,
    Rest = encode_uleb128(Value bsr 7),
    <<Byte, Rest/binary>>.

% Encode signed LEB128
encode_sleb128(Value) when Value >= -64, Value < 64 ->
    ByteValue = Value band 16#7F,
    <<ByteValue>>;
encode_sleb128(Value) when Value >= 0 ->
    encode_uleb128(Value);
encode_sleb128(Value) ->
    encode_sleb128_negative(Value).

encode_sleb128_negative(Value) ->
    Byte = Value band 16#7F,
    NewValue = Value bsr 7,
    if
        NewValue == -1, (Byte band 16#40) =/= 0 ->
            <<Byte>>;
        true ->
            ByteWithCont = Byte bor 16#80,
            Rest = encode_sleb128_negative(NewValue),
            <<ByteWithCont, Rest/binary>>
    end.

%% Generate type DIEs for Context structure and return the Context* type offset
generate_type_dies(#dwarf{backend = Backend}, BaseOffset) ->
    % Get word size from backend
    WordSize = Backend:word_size(),

    % Abbrev 6: term base type (uintptr_t)
    TermTypeDIE = <<
        6,  % Abbreviation code
        "term", 0,  % Name
        WordSize,  % Byte size
        ?DW_ATE_unsigned  % Encoding (unsigned)
    >>,
    TermTypeOffset = BaseOffset,
    io:format("DEBUG TYPE OFFSETS: Base=~p Term=~p~n", [BaseOffset, TermTypeOffset]),

    % Abbrev 10: Array type for x[MAX_REG+1] (term x[17])
    % Abbrev 11: Subrange type
    XArraySubrangeDIE = <<
        11,  % Abbreviation code
        16  % Upper bound (MAX_REG = 16, so array is [0..16])
    >>,
    XArrayTypeDIE = <<
        10,  % Abbreviation code
        TermTypeOffset:32/little,  % Type (term)
        XArraySubrangeDIE/binary,
        0  % End of children
    >>,
    XArrayTypeOffset = BaseOffset + byte_size(TermTypeDIE),

    % Abbrev 8: Context structure type
    % Only include the x array member for now (most important for debugging)
    XOffset = case Backend of
        jit_x86_64 -> 16#30;
        jit_aarch64 -> 16#30;
        _ -> 16#18  % riscv32 and armv6m
    end,
    XMemberDIE = <<
        9,  % Abbreviation code
        "x", 0,  % Name
        XArrayTypeOffset:32/little,  % Type (term array)
        XOffset:32/little  % Data member location
    >>,
    % Estimate Context size (actual size varies, but this is good enough)
    ContextSize = 512,
    ContextStructDIE = <<
        8,  % Abbreviation code
        "Context", 0,  % Name
        ContextSize:32/little,  % Byte size
        XMemberDIE/binary,
        0  % End of children
    >>,
    ContextStructOffset = BaseOffset + byte_size(TermTypeDIE) + byte_size(XArrayTypeDIE),

    % Abbrev 7: Context* pointer type
    ContextPtrTypeDIE = <<
        7,  % Abbreviation code
        WordSize,  % Byte size
        ContextStructOffset:32/little  % Type (Context)
    >>,
    ContextPtrTypeOffset = BaseOffset + byte_size(TermTypeDIE) + byte_size(XArrayTypeDIE) + byte_size(ContextStructDIE),

    % Combine all type DIEs
    AllTypes = <<TermTypeDIE/binary, XArrayTypeDIE/binary, ContextStructDIE/binary, ContextPtrTypeDIE/binary>>,

    {AllTypes, ContextPtrTypeOffset}.

%% Generate DIEs for functions as DW_TAG_subprogram with module:func/arity naming
generate_function_dies_with_module(Functions, ModuleName, #dwarf{backend = Backend}, ContextPtrTypeOffset, CodeSize) ->
    % Filter and sort functions by address
    ValidFunctions = lists:sort([
        {Offset, FunctionName, Arity}
     || {Offset, FunctionName, Arity} <- Functions, Offset >= 0
    ]),

    % Calculate function sizes by finding the next function's offset
    % For the last function, use CodeSize to determine its end
    FunctionsWithSizes = case ValidFunctions of
        [] -> [];
        _ ->
            lists:zipwith(
                fun({Offset, Name, Arity}, NextFunc) ->
                    Size = case NextFunc of
                        {NextOffset, _, _} -> NextOffset - Offset;
                        end_of_code -> CodeSize - Offset  % Last function extends to end of code
                    end,
                    {Offset, Name, Arity, Size}
                end,
                ValidFunctions,
                tl(ValidFunctions) ++ [end_of_code]
            )
    end,

    % Generate DIE for each function
    FunctionDIEsList = [
        generate_function_die_with_module(Offset, FunctionName, Arity, Size, ModuleName, Backend, ContextPtrTypeOffset)
     || {Offset, FunctionName, Arity, Size} <- FunctionsWithSizes
    ],
    iolist_to_binary(FunctionDIEsList).

%% Generate DIE for a single function with module name
generate_function_die_with_module(Offset, FunctionName, Arity, FunctionSize, ModuleName, Backend, ContextPtrTypeOffset) ->
    % Create module:function/arity format
    FunctionString = list_to_binary(io_lib:format("~s:~s/~B", [ModuleName, FunctionName, Arity])),

    % Get the DWARF register number for ctx from the backend
    CtxRegNum = Backend:dwarf_ctx_register(),

    % Generate ctx parameter DIE
    CtxParamDIE = generate_ctx_parameter_die(CtxRegNum, ContextPtrTypeOffset),

    % Get word size for addresses and convert to bits
    WordSize = Backend:word_size(),
    WordSizeInBits = WordSize * 8,

    <<
        % Abbreviation code (4 = DW_TAG_subprogram)
        4,
        % DW_AT_name
        FunctionString/binary,
        0,
        % DW_AT_low_pc
        Offset:WordSizeInBits/little,
        % DW_AT_high_pc (low_pc + size)
        (Offset + FunctionSize):WordSizeInBits/little,
        % Child: ctx parameter
        CtxParamDIE/binary,
        % End of children marker
        0
    >>.

%% Generate DIE for ctx parameter
generate_ctx_parameter_die(CtxRegNum, ContextPtrTypeOffset) ->
    % DW_FORM_exprloc requires a ULEB128 length followed by the expression
    % Expression: DW_OP_reg0 + register_number (single byte)
    % DW_OP_regN means the value is in register N
    RegOpcode = ?DW_OP_reg0 + CtxRegNum,
    LocationExpr = <<RegOpcode>>,
    LocationExprLen = encode_uleb128(byte_size(LocationExpr)),

    <<
        % Abbreviation code (5 = DW_TAG_formal_parameter)
        5,
        % DW_AT_name
        "ctx",
        0,
        % DW_AT_type (reference to Context* type)
        ContextPtrTypeOffset:32/little,
        % DW_AT_location (exprloc: length + expression)
        LocationExprLen/binary,
        LocationExpr/binary
    >>.

%% Generate DIEs for opcodes as DW_TAG_lexical_block
generate_opcode_dies(Opcodes, Backend) ->
    % Filter and sort opcodes by address
    ValidOpcodes = lists:sort([{Offset, Opcode} || {Offset, Opcode} <- Opcodes, Offset >= 0]),

    % Generate DIE for each opcode
    OpcodeDIEsList = [generate_opcode_die(Offset, Opcode, Backend) || {Offset, Opcode} <- ValidOpcodes],
    iolist_to_binary(OpcodeDIEsList).

%% Generate DIE for a single opcode
generate_opcode_die(Offset, Opcode, Backend) ->
    OpcodeString = list_to_binary(io_lib:format("~s@~B", [Opcode, Offset])),
    WordSize = Backend:word_size(),
    WordSizeInBits = WordSize * 8,

    <<
        % Abbreviation code (2 = DW_TAG_lexical_block)
        2,
        % DW_AT_name
        OpcodeString/binary,
        0,
        % DW_AT_low_pc
        Offset:WordSizeInBits/little
    >>.

%% Generate DIEs for labels as DW_TAG_label
generate_label_dies(Labels, Backend) ->
    % Filter and sort labels by address
    ValidLabels = lists:sort([{Offset, Label} || {Offset, Label} <- Labels, Offset >= 0]),

    % Generate DIE for each label
    LabelDIEsList = [generate_label_die(Offset, Label, Backend) || {Offset, Label} <- ValidLabels],
    iolist_to_binary(LabelDIEsList).

%% Generate DIE for a single label
generate_label_die(Offset, Label, Backend) ->
    LabelString = list_to_binary(io_lib:format("label_~B", [Label])),
    WordSize = Backend:word_size(),
    WordSizeInBits = WordSize * 8,

    <<
        % Abbreviation code (3 = DW_TAG_label)
        3,
        % DW_AT_name
        LabelString/binary,
        0,
        % DW_AT_low_pc
        Offset:WordSizeInBits/little
    >>.

%% Generate symbol table for function names and opcode symbols
generate_symbol_table(
    #dwarf{functions = Functions, opcodes = Opcodes, labels = Labels, module_name = ModuleName},
    Backend
) ->
    % Determine ELF format based on backend word size
    WordSize = Backend:word_size(),
    % Build string table for symbol names (functions) with module:function/arity format
    FunctionNames = [
        list_to_binary(io_lib:format("~s:~s/~B", [ModuleName, FunctionName, Arity]))
     || {_Offset, FunctionName, Arity} <- Functions
    ],
    % Build string table for opcode symbols with module:op_opcode@offset format
    OpcodeNames = [
        list_to_binary(io_lib:format("~s:op_~s@~w", [ModuleName, Opcode, Offset]))
     || {Offset, Opcode, _Size} <- Opcodes
    ],
    % Build string table for label symbols with module:label_X@offset format
    LabelNames = [
        list_to_binary(io_lib:format("~s:label_~w@~w", [ModuleName, LabelNum, Offset]))
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
    NullSymbol =
        case WordSize of
            8 ->
                % ELF64: 24 bytes - st_name(4) + st_info(1) + st_other(1) + st_shndx(2) + st_value(8) + st_size(8)
                <<0:32/little, 0, 0, 0:16/little, 0:64/little, 0:64/little>>;
            4 ->
                % ELF32: 16 bytes - st_name(4) + st_value(4) + st_size(4) + st_info(1) + st_other(1) + st_shndx(2)
                <<0:32/little, 0:32/little, 0:32/little, 0, 0, 0:16/little>>
        end,

    % Generate function symbols
    FunctionSymbols = lists:foldl(
        fun({{Offset, _FunctionName, _Arity}, StringOffset}, Acc) ->
            % Function name is now module:function/arity (already in FunctionNames)
            % Estimated function size
            FuncSize = 100,

            % Use raw offset for symbol address (no Thumb bit)
            FunctionAddress = Offset,

            % Symbol table entry (format depends on word size)
            Symbol =
                case WordSize of
                    8 ->
                        % ELF64: 24 bytes - st_name(4) + st_info(1) + st_other(1) + st_shndx(2) + st_value(8) + st_size(8)
                        <<
                            StringOffset:32/little,
                            % st_info (STB_GLOBAL << 4 | STT_FUNC)
                            16#12,
                            % st_other
                            0,
                            % st_shndx (section index - .text will be section 1)
                            1:16/little,
                            % st_value (function address)
                            FunctionAddress:64/little,
                            % st_size (function size)
                            FuncSize:64/little
                        >>;
                    4 ->
                        % ELF32: 16 bytes - st_name(4) + st_value(4) + st_size(4) + st_info(1) + st_other(1) + st_shndx(2)
                        <<
                            StringOffset:32/little,
                            FunctionAddress:32/little,
                            FuncSize:32/little,
                            16#12,
                            0,
                            1:16/little
                        >>
                end,
            <<Acc/binary, Symbol/binary>>
        end,
        <<>>,
        lists:zip(Functions, lists:sublist(ReversedOffsets, length(Functions)))
    ),

    % Generate opcode symbols
    OpcodeStringOffsets = lists:sublist(ReversedOffsets, length(Functions) + 1, length(Opcodes)),
    OpcodeSymbols = lists:foldl(
        fun({{Offset, _Opcode, Size}, StringOffset}, Acc) ->
            % Use raw offset for symbol address (no Thumb bit)
            OpcodeAddress = Offset,

            % Symbol table entry (format depends on word size)
            Symbol =
                case WordSize of
                    8 ->
                        % ELF64: 24 bytes - st_name(4) + st_info(1) + st_other(1) + st_shndx(2) + st_value(8) + st_size(8)
                        <<
                            StringOffset:32/little,
                            % st_info (STB_GLOBAL << 4 | STT_NOTYPE)
                            16#10,
                            % st_other
                            0,
                            % st_shndx (section index - .text will be section 1)
                            1:16/little,
                            % st_value (opcode address)
                            OpcodeAddress:64/little,
                            % st_size (actual calculated opcode size)
                            Size:64/little
                        >>;
                    4 ->
                        % ELF32: 16 bytes - st_name(4) + st_value(4) + st_size(4) + st_info(1) + st_other(1) + st_shndx(2)
                        <<
                            StringOffset:32/little,
                            OpcodeAddress:32/little,
                            Size:32/little,
                            16#10,
                            0,
                            1:16/little
                        >>
                end,
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

            % Symbol table entry (format depends on word size)
            Symbol =
                case WordSize of
                    8 ->
                        % ELF64: 24 bytes - st_name(4) + st_info(1) + st_other(1) + st_shndx(2) + st_value(8) + st_size(8)
                        <<
                            StringOffset:32/little,
                            % st_info (STB_GLOBAL << 4 | STT_NOTYPE)
                            16#10,
                            % st_other
                            0,
                            % st_shndx (section index - .text will be section 1)
                            1:16/little,
                            % st_value (label address)
                            LabelAddress:64/little,
                            % st_size (label size - 0 for point labels)
                            0:64/little
                        >>;
                    4 ->
                        % ELF32: 16 bytes - st_name(4) + st_value(4) + st_size(4) + st_info(1) + st_other(1) + st_shndx(2)
                        <<
                            StringOffset:32/little,
                            LabelAddress:32/little,
                            0:32/little,
                            16#10,
                            0,
                            1:16/little
                        >>
                end,
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
create_section_headers_proper(
    SectionNames, Sections, SectionOffsets, ShStrTab, Backend, WordSizeInBits
) ->
    % Create null section header (index 0)
    % Size depends on ELF format: 40 bytes (ELF32) or 64 bytes (ELF64)
    SectionHeaderSizeBits =
        case WordSizeInBits of
            % 64 bytes * 8 bits
            64 -> 512;
            % 40 bytes * 8 bits
            32 -> 320
        end,
    NullHeader = <<0:SectionHeaderSizeBits/little>>,

    % Create section headers for all sections (indices 1-6)
    % SectionOffsets from layout_sections: [ShStrTabOffset, ...SectionOffsets in order...]
    [_ShStrTabOffset | SectionOffsetsInOrder] = SectionOffsets,

    SectionHeaders = lists:foldl(
        fun({_Index, {{SectionName, SectionData}, FileOffset}}, Acc) ->
            % Calculate name offset in string table by finding the null-terminated section name
            SectionNameWithNull = <<SectionName/binary, 0>>,
            {NameOffset, _Length} = binary:match(ShStrTab, SectionNameWithNull),

            % Determine section type, properties, and flags
            {SectionType, SectionFlags, Link, Info, EntrySize} =
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
                        SymTabEntrySize =
                            case WordSizeInBits of
                                32 -> 16;
                                64 -> 24
                            end,
                        {?SHT_SYMTAB, 0, StrtabIndex, NumLocalSymbols, SymTabEntrySize};
                    % SHT_STRTAB
                    <<".strtab">> ->
                        {3, 0, 0, 0, 0};
                    % ARM attributes
                    <<".ARM.attributes">> ->
                        {?SHT_ARM_ATTRIBUTES, 0, 0, 0, 0};
                    % .text section - executable code
                    <<".text">> ->
                        {?SHT_PROGBITS, ?SHF_ALLOC bor ?SHF_EXECINSTR, 0, 0, 0};
                    % Debug sections and other progbits
                    _ ->
                        {?SHT_PROGBITS, 0, 0, 0, 0}
                end,

            Header = <<
                % Name offset - always 32-bit
                NameOffset:32/little,
                % Type - always 32-bit
                SectionType:32/little,
                % Flags - 32/64 bit depending on word size
                SectionFlags:WordSizeInBits/little,
                % Address - 32/64 bit depending on word size
                0:WordSizeInBits/little,
                % File offset - 32/64 bit depending on word size
                FileOffset:WordSizeInBits/little,
                % Size - 32/64 bit depending on word size
                (byte_size(SectionData)):WordSizeInBits/little,
                % Link - always 32-bit
                Link:32/little,
                % Info - always 32-bit
                Info:32/little,
                % Address align - 32/64 bit depending on word size
                1:WordSizeInBits/little,
                % Entry size - 32/64 bit depending on word size
                EntrySize:WordSizeInBits/little
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
        % Name offset - always 32-bit
        ShStrTabNameOffset:32/little,
        % Type - always 32-bit
        ?SHT_STRTAB:32/little,
        % Flags - 32/64 bit depending on word size
        0:WordSizeInBits/little,
        % Address - 32/64 bit depending on word size
        0:WordSizeInBits/little,
        % File offset - 32/64 bit depending on word size
        ShStrTabFileOffset:WordSizeInBits/little,
        % Size - 32/64 bit depending on word size
        (byte_size(ShStrTab)):WordSizeInBits/little,
        % Link - always 32-bit
        0:32/little,
        % Info - always 32-bit
        0:32/little,
        % Address align - 32/64 bit depending on word size
        1:WordSizeInBits/little,
        % Entry size - 32/64 bit depending on word size
        0:WordSizeInBits/little
    >>,

    <<NullHeader/binary, SectionHeaders/binary, ShStrTabHeader/binary>>.

%% @doc Add .text section containing native code to existing debug-only ELF
%% @doc Create complete ELF with .text section and debug sections from the start
create_elf_with_text_and_debug_sections(Backend, DebugSections, NativeCode) ->
    % Add .text section as the first section
    TextSection = {<<".text">>, NativeCode},
    AllSections = [TextSection | DebugSections],

    % Calculate text section offset: it's the first section after the ELF header
    WordSize = Backend:word_size(),
    TextSectionOffset =
        case WordSize of
            % ELF64 header size
            8 -> 64;
            % ELF32 header size
            4 -> 52
        end,

    % Create complete ELF with all sections
    ElfBinary = create_elf_header_and_sections(Backend, AllSections),

    {ElfBinary, TextSectionOffset}.

-endif.
