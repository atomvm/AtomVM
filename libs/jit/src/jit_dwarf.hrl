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

%% DWARF Tag constants
-define(DW_TAG_compile_unit, 16#11).
-define(DW_TAG_subprogram, 16#2e).
-define(DW_TAG_lexical_block, 16#0b).
-define(DW_TAG_label, 16#0a).
-define(DW_TAG_formal_parameter, 16#05).
-define(DW_TAG_pointer_type, 16#0f).
-define(DW_TAG_structure_type, 16#13).
-define(DW_TAG_member, 16#0d).
-define(DW_TAG_array_type, 16#01).
-define(DW_TAG_subrange_type, 16#21).
-define(DW_TAG_base_type, 16#24).

%% DWARF Attribute constants
-define(DW_AT_name, 16#03).
-define(DW_AT_comp_dir, 16#1b).
-define(DW_AT_producer, 16#25).
-define(DW_AT_language, 16#13).
-define(DW_AT_low_pc, 16#11).
-define(DW_AT_high_pc, 16#12).
-define(DW_AT_stmt_list, 16#10).
-define(DW_AT_type, 16#49).
-define(DW_AT_data_member_location, 16#38).
-define(DW_AT_byte_size, 16#0b).
-define(DW_AT_encoding, 16#3e).
-define(DW_AT_location, 16#02).
-define(DW_AT_upper_bound, 16#2f).

%% DWARF Form constants
-define(DW_FORM_string, 16#08).
-define(DW_FORM_addr, 16#01).
-define(DW_FORM_data4, 16#06).
-define(DW_FORM_data1, 16#0b).
-define(DW_FORM_udata, 16#0f).
-define(DW_FORM_ref4, 16#13).
-define(DW_FORM_sec_offset, 16#17).
-define(DW_FORM_exprloc, 16#18).

%% DWARF Encoding constants
-define(DW_ATE_unsigned, 16#07).
-define(DW_ATE_signed, 16#05).

%% DWARF Location expression opcodes
-define(DW_OP_reg0, 16#50).
-define(DW_OP_fbreg, 16#91).

%% DWARF Language constants
-define(DW_LANG_C, 16#02).
-define(DW_LANG_Erlang, 16#46).
-define(DW_LANG_Elixir, 16#47).
-define(DW_LANG_Gleam, 16#48).

%% ELF constants
-define(EI_MAG0, 16#7f).
-define(EI_MAG1, $E).
-define(EI_MAG2, $L).
-define(EI_MAG3, $F).
-define(ELFCLASS32, 1).
-define(ELFCLASS64, 2).
-define(ELFDATA2LSB, 1).
-define(EV_CURRENT, 1).
-define(ET_REL, 1).
-define(EM_ARM, 40).
-define(EM_X86_64, 62).
-define(EM_AARCH64, 183).
-define(EM_RISCV, 243).
-define(SHT_PROGBITS, 1).
-define(SHT_SYMTAB, 2).
-define(SHT_STRTAB, 3).
-define(SHT_ARM_ATTRIBUTES, 16#70000003).
-define(SHF_ALLOC, 2).
-define(SHF_EXECINSTR, 4).

%% ARM EABI flags
-define(EF_ARM_EABI_VER5, 16#05000000).
-define(EF_ARM_ABI_FLOAT_SOFT, 16#00000200).
-define(EF_ARM_ARCH_V6M, 16#00000009).

%% DWARF register numbers
%% These follow the DWARF register numbering conventions for each architecture
-define(DWARF_RDI_REG_X86_64, 5).   % rdi register in x86_64
-define(DWARF_X0_REG_AARCH64, 0).   % x0 register in aarch64
-define(DWARF_A0_REG_RISCV32, 10).  % a0 register in RISC-V
-define(DWARF_R0_REG_ARMV6M, 0).    % r0 register in ARM
