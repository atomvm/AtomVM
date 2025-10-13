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

-define(TERM_PRIMARY_MASK, 16#3).
-define(TERM_PRIMARY_CP, 16#0).
-define(TERM_PRIMARY_LIST, 16#1).
-define(TERM_PRIMARY_BOXED, 16#2).
% ~3
-define(TERM_PRIMARY_CLEAR_MASK, -4).

-define(TERM_IMMED_TAG_MASK, 16#F).
-define(TERM_PID_TAG, 16#3).
-define(TERM_PORT_TAG, 16#7).
-define(TERM_INTEGER_TAG, 16#F).

-define(TERM_BOXED_TAG_MASK, 16#3F).
-define(TERM_BOXED_TUPLE, 16#0).
-define(TERM_BOXED_BIN_MATCH_STATE, 16#4).
-define(TERM_BOXED_POSITIVE_INTEGER, 16#8).
-define(TERM_BOXED_NEGATIVE_INTEGER, 16#C).
-define(TERM_BOXED_REF, 16#10).
-define(TERM_BOXED_FUN, 16#14).
-define(TERM_BOXED_FLOAT, 16#18).
-define(TERM_BOXED_REFC_BINARY, 16#20).
-define(TERM_BOXED_HEAP_BINARY, 16#24).
-define(TERM_BOXED_SUB_BINARY, 16#28).
-define(TERM_BOXED_MAP, 16#2C).
-define(TERM_BOXED_EXTERNAL_PID, 16#30).
-define(TERM_BOXED_EXTERNAL_PORT, 16#34).
-define(TERM_BOXED_EXTERNAL_REF, 16#38).

-define(TERM_BOXED_TAG_MASK_NO_SIGN, 16#3B).
% Optimization : ((Reg & 0x3F) != 0x8) && ((Reg & 0x3F) != 0xC) && ((Reg & 0x3F) != 0x18)
% is (almost) equivalent to (Reg & 0x2B) != 0x8. It will misidentify 0x1C,
% but we are not using it and it has been marked as unavailable in term.h
-define(TERM_BOXED_TAG_MASK_INTEGER_OR_FLOAT, 16#2B).
-define(TERM_BOXED_TAG_POSITIVE_INTEGER_OR_FLOAT, 16#8).

-define(TERM_IMMED2_TAG_MASK, 16#3F).
-define(TERM_IMMED2_TAG_SIZE, 6).
-define(TERM_IMMED2_ATOM, 16#B).
-define(TERM_IMMED2_CATCH, 16#1B).
-define(TERM_NIL, 16#3B).

-define(TERM_COMPARE_NO_OPTS, 0).
-define(TERM_COMPARE_EXACT, 1).

-define(TERM_COMPARE_MEMORY_ALLOC_FAIL, 0).
-define(TERM_EQUALS, 1).
-define(TERM_LESS_THAN, 2).
-define(TERM_GREATER_THAN, 4).

-define(LIST_HEAD_INDEX, 1).
-define(LIST_TAIL_INDEX, 0).

-define(TERM_BOXED_BIN_MATCH_STATE_SIZE, 4).

-define(TERM_MAP_NOT_FOUND, -1).
-define(TERM_MAP_MEMORY_ALLOC_FAIL, -2).

-define(REFC_BINARY_MIN_32, 32).
-define(REFC_BINARY_MIN_64, 64).
-define(TERM_BOXED_REFC_BINARY_SIZE, 6).
-define(BINARY_HEADER_SIZE, 2).
