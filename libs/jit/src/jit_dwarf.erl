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
    % Register locations: [{Offset, [{XRegIndex, DwarfRegNum}]}]
    % Records which x registers are cached in native registers at each opcode boundary
    reg_locations = [] :: [{non_neg_integer(), [{non_neg_integer(), non_neg_integer()}]}],
    stream_module :: module(),
    stream :: any(),
    line_resolver :: fun((non_neg_integer()) -> false | {ok, binary(), pos_integer()})
}).

-type state() :: #dwarf{}.

-export([
    new/4,
    new/5,
    extract_x_reg_locations/2,
    opcode/2,
    opcode/3,
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
    map/4,
    flush/1
]).

%%-----------------------------------------------------------------------------
%% @returns A new state
%% @doc     Create a new state with the proxied stream and no line resolver.
%% @end
%%-----------------------------------------------------------------------------
-spec new(module(), module(), module(), pos_integer()) -> state().
new(Backend, ModuleName, StreamModule, MaxSize) ->
    new(Backend, ModuleName, StreamModule, MaxSize, fun(_) -> false end).

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

-spec flush(state()) -> state().
flush(#dwarf{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:flush(Stream0),
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
%% @doc Extract x register → DWARF register number mappings from jit_regs state.
-spec extract_x_reg_locations(jit_regs:regs(), fun((atom()) -> non_neg_integer())) ->
    [{non_neg_integer(), non_neg_integer()}].
extract_x_reg_locations(Regs, DwarfRegNumFn) ->
    Contents = jit_regs:get_all_contents(Regs),
    maps:fold(
        fun
            (CpuReg, {x_reg, N}, Acc) -> [{N, DwarfRegNumFn(CpuReg)} | Acc];
            (_, _, Acc) -> Acc
        end,
        [],
        Contents
    ).

%% opcode/2: convenience wrapper with no register location info
-spec opcode
    (state(), binary() | integer()) -> state();
    (any(), binary() | integer()) -> any().
opcode(State, Opcode) ->
    opcode(State, Opcode, []).

-spec opcode
    (state(), binary() | integer(), [{non_neg_integer(), non_neg_integer()}]) -> state();
    (any(), binary() | integer(), [{non_neg_integer(), non_neg_integer()}]) -> any().
opcode(#dwarf{} = State, Opcode, XRegMap) when is_integer(Opcode) ->
    opcode(State, integer_to_binary(Opcode), XRegMap);
opcode(
    #dwarf{
        stream_module = StreamModule, stream = Stream, opcodes = Opcodes0, reg_locations = RegLocs0
    } = State,
    Opcode,
    XRegMap
) ->
    Offset = StreamModule:offset(Stream),
    Opcodes1 = update_previous_opcode_size(Opcodes0, Offset),
    Opcodes2 = [{Offset, Opcode, 0} | Opcodes1],
    RegLocs1 = [{Offset, XRegMap} | RegLocs0],
    State#dwarf{opcodes = Opcodes2, reg_locations = RegLocs1};
opcode(BackendStateDwarfDisabled, _Opcode, _XRegMap) ->
    BackendStateDwarfDisabled.

%%-----------------------------------------------------------------------------
%% @param State    current state
%% @param Label    the label number to record
%% @returns The updated state with label recorded at current offset
%% @doc     Record a label at the current stream offset
%% @end
%%-----------------------------------------------------------------------------
-spec label
    (state(), non_neg_integer()) -> state();
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
-spec function
    (state(), atom(), non_neg_integer()) -> state();
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
-spec line
    (state(), pos_integer()) -> state();
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
-spec elf(state(), binary()) -> {ok, non_neg_integer(), binary()} | false.
-ifdef(JIT_DWARF).
elf(#dwarf{module_name = ModuleName, backend = Backend} = State, NativeCode) ->
    SourceFile = <<(atom_to_binary(ModuleName, utf8))/binary, ".erl">>,

    % Generate location lists for x registers
    {DebugLocSection, LocListOffsets} = generate_debug_loc_section(State),

    % Generate DWARF sections with relocation offsets for address fields
    {DebugInfoSection, DebugInfoRelocs} =
        generate_debug_info_section_with_opcodes(State, SourceFile, LocListOffsets),
    DebugLineSection = generate_debug_line_section(State, SourceFile),
    DebugAbbrevSection = generate_debug_abbrev_section_with_opcodes(),
    DebugStrSection = generate_debug_str_section(State, SourceFile),
    {DebugArangesSection, ArangesRelocs} = generate_debug_aranges_section(State),
    % Generate symbol table sections for function names (no relocs — LLDB auto-relocates via sh_addr)
    {SymtabSection, StrtabSection} = generate_symbol_table(State, Backend),

    % Create sections list (order matters for reloc offset computation)
    BaseSections = [
        {<<".debug_info">>, DebugInfoSection},
        {<<".debug_line">>, DebugLineSection},
        {<<".debug_abbrev">>, DebugAbbrevSection},
        {<<".debug_str">>, DebugStrSection},
        {<<".debug_aranges">>, DebugArangesSection},
        {<<".debug_loc">>, DebugLocSection},
        {<<".symtab">>, SymtabSection},
        {<<".strtab">>, StrtabSection}
    ],

    % Per-section DWARF reloc offsets (matching BaseSections order)
    % Note: .debug_loc addresses are NOT relocated — LLDB uses the CU base
    % address (DW_AT_low_pc) to resolve them automatically.
    BaseSectionRelocs = [
        DebugInfoRelocs,
        [],
        [],
        [],
        ArangesRelocs,
        [],
        [],
        []
    ],

    {Sections, SectionRelocs} =
        case Backend of
            jit_armv6m ->
                ArmAttributesSection = generate_arm_attributes_section(),
                {
                    BaseSections ++ [{<<".ARM.attributes">>, ArmAttributesSection}],
                    BaseSectionRelocs ++ [[]]
                };
            _ ->
                {BaseSections, BaseSectionRelocs}
        end,

    % Create complete ELF with text section and debug sections
    {CombinedELF, TextSectionOffset, SectionFileOffsets} =
        create_elf_with_text_and_debug_sections(Backend, Sections, NativeCode),

    % Compute ELF-global reloc offsets
    AllRelocs = lists:flatmap(
        fun({FileOffset, WithinSectionRelocs}) ->
            [FileOffset + R || R <- WithinSectionRelocs]
        end,
        lists:zip(SectionFileOffsets, SectionRelocs)
    ),

    % Encode reloc table appended after ELF
    RelocCount = length(AllRelocs),
    RelocData = <<RelocCount:32/little, (<<<<Off:32/little>> || Off <- AllRelocs>>)/binary>>,

    {ok, TextSectionOffset, <<CombinedELF/binary, RelocData/binary>>}.
-else.
elf(_State, _NativeCode) ->
    false.
-endif.

-ifdef(JIT_DWARF).

%% Map JIT backend to ELF machine type
backend_to_machine_type(jit_x86_64) -> ?EM_X86_64;
backend_to_machine_type(jit_aarch64) -> ?EM_AARCH64;
backend_to_machine_type(jit_armv6m) -> ?EM_ARM;
backend_to_machine_type(jit_riscv32) -> ?EM_RISCV;
backend_to_machine_type(jit_riscv64) -> ?EM_RISCV.

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
        <<"AtomVM JIT Compiler v" ?ATOMVM_VERSION>>,
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
    Padding = binary:copy(<<0>>, PaddingSize),

    Header = <<
        2:16/little,
        0:32/little,
        WordSize,
        0
    >>,

    Descriptors = <<
        LowPC:WordSizeInBits/little,
        Length:WordSizeInBits/little,
        0:WordSizeInBits/little,
        0:WordSizeInBits/little
    >>,

    HeaderAndTable = <<Header/binary, Padding/binary, Descriptors/binary>>,
    TotalLength = byte_size(HeaderAndTable),

    LowPCOffset = 4 + byte_size(Header) + byte_size(Padding),
    {<<TotalLength:32/little, HeaderAndTable/binary>>, [LowPCOffset]}.

generate_debug_abbrev_section_with_opcodes() ->
    %% Each entry: {Code, Tag, HasChildren, [{Attr, Form}]}
    Abbrevs = [
        {1, ?DW_TAG_compile_unit, 1, [
            {?DW_AT_name, ?DW_FORM_string},
            {?DW_AT_comp_dir, ?DW_FORM_string},
            {?DW_AT_producer, ?DW_FORM_string},
            {?DW_AT_language, ?DW_FORM_data4},
            {?DW_AT_low_pc, ?DW_FORM_addr},
            {?DW_AT_high_pc, ?DW_FORM_addr},
            {?DW_AT_stmt_list, ?DW_FORM_sec_offset}
        ]},
        {2, ?DW_TAG_lexical_block, 0, [
            {?DW_AT_name, ?DW_FORM_string}, {?DW_AT_low_pc, ?DW_FORM_addr}
        ]},
        {3, ?DW_TAG_label, 0, [
            {?DW_AT_name, ?DW_FORM_string}, {?DW_AT_low_pc, ?DW_FORM_addr}
        ]},
        {4, ?DW_TAG_subprogram, 1, [
            {?DW_AT_name, ?DW_FORM_string},
            {?DW_AT_low_pc, ?DW_FORM_addr},
            {?DW_AT_high_pc, ?DW_FORM_addr}
        ]},
        {5, ?DW_TAG_formal_parameter, 0, [
            {?DW_AT_name, ?DW_FORM_string},
            {?DW_AT_type, ?DW_FORM_ref4},
            {?DW_AT_location, ?DW_FORM_exprloc}
        ]},
        {6, ?DW_TAG_base_type, 0, [
            {?DW_AT_name, ?DW_FORM_string},
            {?DW_AT_byte_size, ?DW_FORM_data1},
            {?DW_AT_encoding, ?DW_FORM_data1}
        ]},
        {7, ?DW_TAG_pointer_type, 0, [
            {?DW_AT_byte_size, ?DW_FORM_data1}, {?DW_AT_type, ?DW_FORM_ref4}
        ]},
        {8, ?DW_TAG_structure_type, 1, [
            {?DW_AT_name, ?DW_FORM_string}, {?DW_AT_byte_size, ?DW_FORM_data4}
        ]},
        {9, ?DW_TAG_member, 0, [
            {?DW_AT_name, ?DW_FORM_string},
            {?DW_AT_type, ?DW_FORM_ref4},
            {?DW_AT_data_member_location, ?DW_FORM_data4}
        ]},
        {10, ?DW_TAG_array_type, 1, [
            {?DW_AT_type, ?DW_FORM_ref4}
        ]},
        {11, ?DW_TAG_subrange_type, 0, [
            {?DW_AT_upper_bound, ?DW_FORM_data1}
        ]},
        {12, ?DW_TAG_variable, 0, [
            {?DW_AT_name, ?DW_FORM_string},
            {?DW_AT_type, ?DW_FORM_ref4},
            {?DW_AT_location, ?DW_FORM_sec_offset}
        ]}
    ],
    iolist_to_binary([encode_abbrev(A) || A <- Abbrevs] ++ [<<0>>]).

encode_abbrev({Code, Tag, HasChildren, Attrs}) ->
    AttrBytes = <<<<Attr, Form>> || {Attr, Form} <- Attrs>>,
    <<Code, Tag, HasChildren, AttrBytes/binary, 0, 0>>.

generate_debug_info_section_with_opcodes(
    #dwarf{
        functions = Functions,
        opcodes = Opcodes,
        labels = Labels,
        module_name = ModuleName,
        backend = Backend
    } = State,
    SourceFile,
    LocListOffsets
) ->
    {LowPC, HighPC} = calculate_address_range(State),

    WordSize = Backend:word_size(),
    WordSizeInBits = WordSize * 8,

    CompileUnitContent = <<
        4:16/little,
        0:32/little,
        WordSize,
        1,
        SourceFile/binary,
        0,
        "/tmp",
        0,
        "AtomVM JIT Compiler v" ?ATOMVM_VERSION,
        0,
        ?DW_LANG_Erlang:32/little,
        LowPC:WordSizeInBits/little,
        HighPC:WordSizeInBits/little,
        0:32/little
    >>,

    % Reloc offsets for LowPC and HighPC in compile unit DIE
    CUSize = byte_size(CompileUnitContent),
    CURelocs = [4 + CUSize - 4 - WordSize - WordSize, 4 + CUSize - 4 - WordSize],

    % Calculate base offset for type DIEs
    % DW_FORM_ref4 offsets are relative to start of compile unit (the length field itself)
    % So we need to add 4 bytes for the length field
    % CompileUnitContent already includes the header (version + abbrev_offset + addr_size)
    TypeDIEsBaseOffset = 4 + byte_size(CompileUnitContent),
    % Generate type DIEs and get the Context* type offset
    {TypeDIEs, ContextPtrTypeOffset} = generate_type_dies(State, TypeDIEsBaseOffset),

    TermTypeOffset = TypeDIEsBaseOffset,

    % Generate DIEs for functions (with x register variables as children), opcodes and labels
    {FunctionDIEs, FuncRelocs0} = generate_function_dies_with_module(
        Functions,
        ModuleName,
        State,
        ContextPtrTypeOffset,
        HighPC,
        LocListOffsets,
        TermTypeOffset
    ),
    {OpcodeDIEs, OpcodeRelocs0} = generate_opcode_dies(Opcodes, Backend),
    {LabelDIEs, LabelRelocs0} = generate_label_dies(Labels, Backend),

    EndMarker = <<0>>,

    Content =
        <<CompileUnitContent/binary, TypeDIEs/binary, FunctionDIEs/binary, OpcodeDIEs/binary,
            LabelDIEs/binary, EndMarker/binary>>,
    UnitLength = byte_size(Content),

    % Adjust reloc offsets to be relative to section start (4 byte length prefix)
    FuncBase = 4 + byte_size(CompileUnitContent) + byte_size(TypeDIEs),
    OpcodeBase = FuncBase + byte_size(FunctionDIEs),
    LabelBase = OpcodeBase + byte_size(OpcodeDIEs),
    AllRelocs =
        CURelocs ++
            [R + FuncBase || R <- FuncRelocs0] ++
            [R + OpcodeBase || R <- OpcodeRelocs0] ++
            [R + LabelBase || R <- LabelRelocs0],

    {<<UnitLength:32/little, Content/binary>>, AllRelocs}.

generate_debug_line_section(#dwarf{lines = Lines}, SourceFile) ->
    % Standard opcode lengths (for opcodes 1-12)
    StdOpcodeLengths = <<0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1>>,

    % Build file table with actual filenames from line data
    UniqueFullPaths =
        case Lines of
            [] -> [SourceFile];
            _ -> lists:usort([Filename || {_Offset, Filename, _LineNum} <- Lines])
        end,

    % Split paths into directories and filenames
    {Directories, FileEntries} = build_file_table(UniqueFullPaths),

    % Build directory and file table binaries
    DirectoryTable = iolist_to_binary([<<(list_to_binary(Dir))/binary, 0>> || Dir <- Directories]),
    FileTableEntries = iolist_to_binary([
        <<(list_to_binary(FName))/binary, 0, (encode_uleb128(DirIdx))/binary, 0, 0>>
     || {FName, DirIdx} <- FileEntries
    ]),
    FileTable = <<DirectoryTable/binary, 0, FileTableEntries/binary, 0>>,

    % Line number program
    FileMapping = lists:zip(UniqueFullPaths, lists:seq(1, length(FileEntries))),
    Program = generate_line_program(Lines, FileMapping),

    % Header fixed fields (after version and header_length)
    HeaderFixedFields = <<2, 1, 1, (-5):8/signed, 14, 13>>,
    HeaderLength =
        byte_size(HeaderFixedFields) + byte_size(StdOpcodeLengths) + byte_size(FileTable),
    Header = <<4:16/little, HeaderLength:32/little, HeaderFixedFields/binary>>,

    Content = <<Header/binary, StdOpcodeLengths/binary, FileTable/binary, Program/binary>>,
    <<(byte_size(Content)):32/little, Content/binary>>.

%% Build directory list and file entries from full paths
build_file_table(Paths) ->
    {Dirs, Files, _} = lists:foldl(
        fun(FullPath, {DirAcc, FileAcc, FileSet}) ->
            case filename:split(binary_to_list(FullPath)) of
                [Basename] ->
                    add_file_entry({Basename, 0}, DirAcc, FileAcc, FileSet);
                PathParts ->
                    Dir = filename:join(lists:droplast(PathParts)),
                    Basename = lists:last(PathParts),
                    {NewDirAcc, DirIndex} = ensure_directory(Dir, DirAcc),
                    add_file_entry({Basename, DirIndex}, NewDirAcc, FileAcc, FileSet)
            end
        end,
        {[], [], sets:new()},
        Paths
    ),
    {Dirs, lists:reverse(Files)}.

ensure_directory(Dir, DirAcc) ->
    case length(lists:takewhile(fun(D) -> D =/= Dir end, DirAcc)) of
        N when N < length(DirAcc) -> {DirAcc, N + 1};
        _ -> {DirAcc ++ [Dir], length(DirAcc) + 1}
    end.

add_file_entry(FileKey, DirAcc, FileAcc, FileSet) ->
    case sets:is_element(FileKey, FileSet) of
        true -> {DirAcc, FileAcc, FileSet};
        false -> {DirAcc, [FileKey | FileAcc], sets:add_element(FileKey, FileSet)}
    end.

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

%% Generate DW_TAG_variable DIEs for x registers with location list references.
generate_x_reg_variable_dies(LocListOffsets, TermTypeOffset) ->
    iolist_to_binary([
        begin
            Name = iolist_to_binary(io_lib:format("x[~B]", [XIdx])),
            % Abbreviation 12: DW_TAG_variable {name, type, location(sec_offset)}
            <<12, Name/binary, 0, TermTypeOffset:32/little, LocOffset:32/little>>
        end
     || {XIdx, LocOffset} <- LocListOffsets
    ]).

%% Generate .debug_loc section with location lists for x registers.
%% Returns {SectionBinary, [{XRegIndex, OffsetInSection}]} for each x register
%% that has at least one location list entry.
generate_debug_loc_section(#dwarf{reg_locations = RegLocs, backend = Backend}) ->
    WordSize = Backend:word_size(),
    WSBits = WordSize * 8,
    CtxRegNum = Backend:dwarf_ctx_register(),
    XArrayOffset =
        case WordSize of
            8 -> 16#30;
            4 -> 16#18
        end,

    % Sort snapshots by offset (they're stored in reverse)
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A =< B end, RegLocs),

    % Build location lists for x registers 0..16
    MaxXReg = 16,
    {LocData, LocOffsets} = lists:foldl(
        fun(XIdx, {AccBin, AccOffsets}) ->
            LocList = build_x_reg_loc_list(XIdx, Sorted, WordSize, WSBits, CtxRegNum, XArrayOffset),
            case LocList of
                <<>> ->
                    {AccBin, AccOffsets};
                _ ->
                    Offset = byte_size(AccBin),
                    {<<AccBin/binary, LocList/binary>>, [{XIdx, Offset} | AccOffsets]}
            end
        end,
        {<<>>, []},
        lists:seq(0, MaxXReg)
    ),
    {LocData, lists:reverse(LocOffsets)}.

%% Build a DWARF location list for a single x register.
%% Walks through opcode-boundary snapshots and emits ranges where the register
%% is cached in a native CPU register.
build_x_reg_loc_list(XIdx, SortedSnapshots, WordSize, WSBits, CtxRegNum, XArrayOffset) ->
    % Build list of {Offset, DwarfRegNum | none} for this x register
    Entries = [
        {Off, find_x_reg_in_snapshot(XIdx, XRegMap)}
     || {Off, XRegMap} <- SortedSnapshots
    ],
    % Merge consecutive entries with the same state
    Merged = merge_loc_entries(Entries),
    % Generate DWARF location list entries, skip zero-length ranges
    LocEntries = lists:filtermap(
        fun
            ({Start, End, _}) when Start =:= End -> false;
            ({Start, End, {reg, DwarfReg}}) ->
                % DW_OP_regN (register 0-31)
                Expr =
                    if
                        DwarfReg =< 31 -> <<(?DW_OP_reg0 + DwarfReg)>>;
                        true -> <<16#90, (encode_uleb128(DwarfReg))/binary>>
                    end,
                {true,
                    <<Start:WSBits/little, End:WSBits/little, (byte_size(Expr)):16/little,
                        Expr/binary>>};
            ({Start, End, memory}) ->
                % DW_OP_bregN(x_array_offset + XIdx * WordSize) where N = ctx register
                % DW_OP_breg0 = 0x70, DW_OP_bregN = 0x70 + N
                MemOffset = XArrayOffset + XIdx * WordSize,
                BregOp = 16#70 + CtxRegNum,
                Expr = <<BregOp, (encode_sleb128(MemOffset))/binary>>,
                {true,
                    <<Start:WSBits/little, End:WSBits/little, (byte_size(Expr)):16/little,
                        Expr/binary>>};
            (_) ->
                false
        end,
        Merged
    ),
    case LocEntries of
        [] ->
            <<>>;
        _ ->
            Terminator = <<0:WSBits, 0:WSBits>>,
            iolist_to_binary(LocEntries ++ [Terminator])
    end.

find_x_reg_in_snapshot(XIdx, XRegMap) ->
    case lists:keyfind(XIdx, 1, XRegMap) of
        {XIdx, DwarfReg} -> {reg, DwarfReg};
        false -> memory
    end.

%% Merge consecutive entries with the same location into ranges.
%% Input: [{Offset, Location}] sorted by offset
%% Output: [{Start, End, Location}]
merge_loc_entries([]) ->
    [];
merge_loc_entries([{Off, Loc}]) ->
    [{Off, Off, Loc}];
merge_loc_entries([{Off1, Loc1}, {_Off2, Loc2} | Rest]) when Loc1 =:= Loc2 ->
    merge_loc_entries([{Off1, Loc1} | Rest]);
merge_loc_entries([{Off1, Loc1}, {Off2, _} | _] = [_ | Tail]) ->
    [{Off1, Off2, Loc1} | merge_loc_entries(Tail)].

%% Helper functions
calculate_address_range(#dwarf{opcodes = []}) ->
    {0, 0};
calculate_address_range(#dwarf{opcodes = Opcodes}) ->
    MinOffset = lists:min([Off || {Off, _, _} <- Opcodes]),
    MaxOffset = lists:max([Off + max(Size, 4) || {Off, _, Size} <- Opcodes]),
    {MinOffset, MaxOffset}.

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
    FileIndex =
        case lists:keyfind(Filename, 1, FileMapping) of
            {Filename, Index} -> Index;
            false -> 1
        end,

    % Emit set_file opcode when file changes or at the start
    SetFile =
        if
            LastOffset == 0; FileIndex =/= LastFileIndex -> <<4, FileIndex>>;
            true -> <<>>
        end,

    Opcodes = <<
        SetFile/binary,
        2,
        (encode_uleb128(Offset - LastOffset))/binary,
        3,
        (encode_sleb128(LineNumber - LastLine))/binary,
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
    encode_sleb128_positive(Value);
encode_sleb128(Value) ->
    encode_sleb128_negative(Value).

encode_sleb128_positive(Value) ->
    Byte = Value band 16#7F,
    NewValue = Value bsr 7,
    if
        NewValue == 0, (Byte band 16#40) == 0 ->
            <<Byte>>;
        true ->
            ByteWithCont = Byte bor 16#80,
            Rest = encode_sleb128_positive(NewValue),
            <<ByteWithCont, Rest/binary>>
    end.

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
        % Abbreviation code
        6,
        % Name
        "term",
        0,
        % Byte size
        WordSize,
        % Encoding (unsigned)
        ?DW_ATE_unsigned
    >>,
    TermTypeOffset = BaseOffset,

    XArraySubrangeDIE = <<
        % Abbreviation code
        11,
        % Upper bound (MAX_REG = 16, so array is [0..16])
        16
    >>,
    XArrayTypeDIE = <<
        % Abbreviation code
        10,
        % Type (term)
        TermTypeOffset:32/little,
        XArraySubrangeDIE/binary,
        % End of children
        0
    >>,
    XArrayTypeOffset = BaseOffset + byte_size(TermTypeDIE),

    % Abbrev 8: Context structure type
    % Only include the x array member for now (most important for debugging)
    XOffset =
        case Backend of
            jit_x86_64 -> 16#30;
            jit_aarch64 -> 16#30;
            jit_riscv64 -> 16#30;
            % riscv32 and armv6m
            _ -> 16#18
        end,
    XMemberDIE = <<
        % Abbreviation code
        9,
        % Name
        "x",
        0,
        % Type (term array)
        XArrayTypeOffset:32/little,
        % Data member location
        XOffset:32/little
    >>,
    % Estimate Context size (actual size varies, but this is good enough)
    ContextSize = 512,
    ContextStructDIE = <<
        % Abbreviation code
        8,
        % Name
        "Context",
        0,
        % Byte size
        ContextSize:32/little,
        XMemberDIE/binary,
        % End of children
        0
    >>,
    ContextStructOffset = BaseOffset + byte_size(TermTypeDIE) + byte_size(XArrayTypeDIE),

    % Abbrev 7: Context* pointer type
    ContextPtrTypeDIE = <<
        % Abbreviation code
        7,
        % Byte size
        WordSize,
        % Type (Context)
        ContextStructOffset:32/little
    >>,
    ContextPtrTypeOffset =
        BaseOffset + byte_size(TermTypeDIE) + byte_size(XArrayTypeDIE) +
            byte_size(ContextStructDIE),

    % Combine all type DIEs
    AllTypes =
        <<TermTypeDIE/binary, XArrayTypeDIE/binary, ContextStructDIE/binary,
            ContextPtrTypeDIE/binary>>,

    {AllTypes, ContextPtrTypeOffset}.

%% Generate DIEs for functions as DW_TAG_subprogram with module.func/arity naming
generate_function_dies_with_module(
    Functions,
    ModuleName,
    #dwarf{backend = Backend},
    ContextPtrTypeOffset,
    CodeSize,
    LocListOffsets,
    TermTypeOffset
) ->
    WordSize = Backend:word_size(),
    WSBits = WordSize * 8,
    CtxRegNum = Backend:dwarf_ctx_register(),
    Sorted = lists:sort([{Off, FN, Ar} || {Off, FN, Ar} <- Functions, Off >= 0]),
    Ends =
        case Sorted of
            [] -> [];
            _ -> [Off || {Off, _, _} <- tl(Sorted)] ++ [CodeSize]
        end,
    CtxParam = ctx_parameter_die(CtxRegNum, ContextPtrTypeOffset),
    XRegVarDIEs = generate_x_reg_variable_dies(LocListOffsets, TermTypeOffset),
    {DIEBin, Relocs} = lists:foldl(
        fun({{Offset, FunctionName, Arity}, EndAddr}, {AccBin, AccRelocs}) ->
            NameBin = iolist_to_binary(
                io_lib:format("~s:~s/~B", [ModuleName, FunctionName, Arity])
            ),
            Prefix = <<4, NameBin/binary, 0>>,
            LowPCOff = byte_size(AccBin) + byte_size(Prefix),
            HighPCOff = LowPCOff + WordSize,
            DIE =
                <<Prefix/binary, Offset:WSBits/little, EndAddr:WSBits/little, CtxParam/binary,
                    XRegVarDIEs/binary, 0>>,
            {<<AccBin/binary, DIE/binary>>, [LowPCOff, HighPCOff | AccRelocs]}
        end,
        {<<>>, []},
        lists:zip(Sorted, Ends)
    ),
    {DIEBin, lists:reverse(Relocs)}.

ctx_parameter_die(CtxRegNum, ContextPtrTypeOffset) ->
    RegOpcode = ?DW_OP_reg0 + CtxRegNum,
    <<5, "ctx", 0, ContextPtrTypeOffset:32/little, 1, RegOpcode>>.

%% Generate DIEs for opcodes as DW_TAG_lexical_block
%% Returns {Binary, RelocOffsets}
generate_opcode_dies(Opcodes, Backend) ->
    WordSize = Backend:word_size(),
    WSBits = WordSize * 8,
    Sorted = lists:sort([{Off, Op} || {Off, Op, _} <- Opcodes, Off >= 0]),
    generate_named_dies_with_relocs(
        2, [{Off, io_lib:format("~s@~B", [Op, Off])} || {Off, Op} <- Sorted], WordSize, WSBits
    ).

%% Returns {Binary, RelocOffsets}
generate_label_dies(Labels, Backend) ->
    WordSize = Backend:word_size(),
    WSBits = WordSize * 8,
    Sorted = lists:sort([{Off, L} || {Off, L} <- Labels, Off >= 0]),
    generate_named_dies_with_relocs(
        3, [{Off, io_lib:format("label_~B", [L])} || {Off, L} <- Sorted], WordSize, WSBits
    ).

generate_named_dies_with_relocs(AbbrevCode, Items, _WordSize, WSBits) ->
    {Bin, Relocs} = lists:foldl(
        fun({Offset, Name}, {AccBin, AccRelocs}) ->
            NameBin = iolist_to_binary(Name),
            Prefix = <<AbbrevCode, NameBin/binary, 0>>,
            AddrOff = byte_size(AccBin) + byte_size(Prefix),
            DIE = <<Prefix/binary, Offset:WSBits/little>>,
            {<<AccBin/binary, DIE/binary>>, [AddrOff | AccRelocs]}
        end,
        {<<>>, []},
        Items
    ),
    {Bin, lists:reverse(Relocs)}.

%% Generate symbol table for function names and opcode symbols
generate_symbol_table(
    #dwarf{functions = Functions, opcodes = Opcodes, labels = Labels, module_name = ModuleName},
    Backend
) ->
    WordSize = Backend:word_size(),

    % Build flat list of {Name, Address, Size, Info} for all symbols
    % Calculate function sizes from sorted offsets
    CodeEnd = lists:max([Off + max(Sz, 4) || {Off, _, Sz} <- Opcodes] ++ [0]),
    SortedFuncs = lists:sort([{Off, FN, Ar} || {Off, FN, Ar} <- Functions, Off >= 0]),
    FuncEnds =
        case SortedFuncs of
            [] -> [];
            _ -> [Off || {Off, _, _} <- tl(SortedFuncs)] ++ [CodeEnd]
        end,
    FuncSymbols = lists:zipwith(
        fun({Offset, FName, Arity}, EndAddr) ->
            {io_lib:format("~s:~s/~B", [ModuleName, FName, Arity]), Offset, EndAddr - Offset, 16#12}
        end,
        SortedFuncs,
        FuncEnds
    ),
    OpcodeSyms = [
        {io_lib:format("~s:op_~s@~w", [ModuleName, Op, Off]), Off, Size, 16#10}
     || {Off, Op, Size} <- Opcodes
    ],
    LabelSyms = [
        {io_lib:format("~s:label_~w@~w", [ModuleName, LNum, Off]), Off, 0, 16#10}
     || {Off, LNum} <- Labels
    ],
    % ARM Thumb mapping symbol (local, must come before global symbols)
    MappingSyms =
        case Backend of
            jit_armv6m -> [{"$t", 0, 0, 16#00}];
            _ -> []
        end,

    % All symbols: mapping (local) first, then global
    AllSymbols = MappingSyms ++ FuncSymbols ++ OpcodeSyms ++ LabelSyms,

    % Build string table and collect offsets
    {StrtabContent, StringOffsets} = lists:foldl(
        fun({Name, _, _, _}, {Strtab, Offsets}) ->
            NameBin = iolist_to_binary(Name),
            Offset = byte_size(Strtab),
            {<<Strtab/binary, NameBin/binary, 0>>, [Offset | Offsets]}
        end,
        {<<0>>, []},
        AllSymbols
    ),

    % Generate symbol entries
    SymbolEntries = lists:zipwith(
        fun({_Name, Addr, Size, Info}, StrOff) ->
            elf_symbol(WordSize, StrOff, Addr, Size, Info)
        end,
        AllSymbols,
        lists:reverse(StringOffsets)
    ),

    NullSymbol = elf_symbol(WordSize, 0, 0, 0, 0),
    SymtabContent = iolist_to_binary([NullSymbol | SymbolEntries]),
    {SymtabContent, StrtabContent}.

%% Encode a single ELF symbol table entry
elf_symbol(8, NameOff, Value, Size, Info) ->
    <<NameOff:32/little, Info, 0, 1:16/little, Value:64/little, Size:64/little>>;
elf_symbol(4, NameOff, Value, Size, Info) ->
    <<NameOff:32/little, Value:32/little, Size:32/little, Info, 0, 1:16/little>>.

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
    NullHeaderSize =
        case WordSizeInBits of
            64 -> 64;
            32 -> 40
        end,
    NullHeader = binary:copy(<<0>>, NullHeaderSize),

    % SectionOffsets: [ShStrTabOffset, ...SectionOffsets in order...]
    [ShStrTabOffset | SectionOffsetsInOrder] = SectionOffsets,

    % All content sections
    ContentHeaders = [
        begin
            {SType, SFlags, Link, Info, EntSize} =
                section_properties(SectionName, SectionNames, Backend, WordSizeInBits),
            section_header(
                SectionName,
                SectionData,
                FileOffset,
                ShStrTab,
                WordSizeInBits,
                {SType, SFlags, Link, Info, EntSize}
            )
        end
     || {{SectionName, SectionData}, FileOffset} <- lists:zip(Sections, SectionOffsetsInOrder)
    ],

    % shstrtab header
    ShStrTabHdr = section_header(
        <<".shstrtab">>,
        ShStrTab,
        ShStrTabOffset,
        ShStrTab,
        WordSizeInBits,
        {?SHT_STRTAB, 0, 0, 0, 0}
    ),

    iolist_to_binary([NullHeader | ContentHeaders] ++ [ShStrTabHdr]).

section_header(Name, Data, FileOffset, ShStrTab, WSBits, {SType, SFlags, Link, Info, EntSize}) ->
    {NameOffset, _} = binary:match(ShStrTab, <<Name/binary, 0>>),
    <<NameOffset:32/little, SType:32/little, SFlags:WSBits/little, 0:WSBits/little,
        FileOffset:WSBits/little, (byte_size(Data)):WSBits/little, Link:32/little, Info:32/little,
        1:WSBits/little, EntSize:WSBits/little>>.

section_properties(<<".symtab">>, SectionNames, Backend, WordSizeInBits) ->
    StrtabIndex = find_section_index(<<".strtab">>, SectionNames),
    NumLocal =
        case Backend of
            jit_armv6m -> 2;
            _ -> 1
        end,
    EntSize =
        case WordSizeInBits of
            32 -> 16;
            64 -> 24
        end,
    {?SHT_SYMTAB, 0, StrtabIndex, NumLocal, EntSize};
section_properties(<<".strtab">>, _, _, _) ->
    {3, 0, 0, 0, 0};
section_properties(<<".ARM.attributes">>, _, _, _) ->
    {?SHT_ARM_ATTRIBUTES, 0, 0, 0, 0};
section_properties(<<".text">>, _, _, _) ->
    {?SHT_PROGBITS, ?SHF_ALLOC bor ?SHF_EXECINSTR, 0, 0, 0};
section_properties(_, _, _, _) ->
    {?SHT_PROGBITS, 0, 0, 0, 0}.

%% @doc Create complete ELF with .text section and debug sections
create_elf_with_text_and_debug_sections(Backend, DebugSections, NativeCode) ->
    TextSection = {<<".text">>, NativeCode},
    AllSections = [TextSection | DebugSections],

    WordSize = Backend:word_size(),
    ElfHeaderSize =
        case WordSize of
            8 -> 64;
            4 -> 52
        end,

    ElfBinary = create_elf_header_and_sections(Backend, AllSections),

    % Compute section file offsets for reloc computation
    SectionNames =
        [<<>>] ++ [SN || {SN, _} <- AllSections] ++ [<<".shstrtab">>],
    ShStrTab = create_string_table(SectionNames),
    {_Data, AllOffsets} = layout_sections(AllSections, ShStrTab, ElfHeaderSize),
    % AllOffsets = [ShStrTabOffset, TextOff, DebugSection1Off, ...]
    [_ShStrTabOff, _TextOff | DebugSectionOffsets] = AllOffsets,

    {ElfBinary, ElfHeaderSize, DebugSectionOffsets}.

-endif.
