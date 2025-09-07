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


-define(COMPACT_LITERAL, 0).
-define(COMPACT_INTEGER, 1).
-define(COMPACT_ATOM, 2).
-define(COMPACT_XREG, 3).
-define(COMPACT_YREG, 4).
-define(COMPACT_LABEL, 5).
-define(COMPACT_EXTENDED, 7).
-define(COMPACT_LARGE_LITERAL, 8).
-define(COMPACT_LARGE_INTEGER, 9).
-define(COMPACT_LARGE_ATOM, 10).
-define(COMPACT_LARGE_XREG, 11).
-define(COMPACT_LARGE_YREG, 12).

% OTP-20+ format
-define(COMPACT_EXTENDED_LIST, 16#17).
-define(COMPACT_EXTENDED_FP_REGISTER, 16#27).
-define(COMPACT_EXTENDED_ALLOCATION_LIST, 16#37).
-define(COMPACT_EXTENDED_LITERAL, 16#47).
% https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_asm.erl#L433
-define(COMPACT_EXTENDED_TYPED_REGISTER, 16#57).

-define(COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_WORDS, 0).
-define(COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FLOATS, 1).
-define(COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FUNS, 2).

-define(COMPACT_LARGE_IMM_MASK, 16#18).
-define(COMPACT_11BITS_VALUE, 16#8).
-define(COMPACT_NBITS_VALUE, 16#18).

-define(COMPACT_LARGE_INTEGER_11BITS, (?COMPACT_LARGE_INTEGER bor ?COMPACT_11BITS_VALUE)).
-define(COMPACT_LARGE_INTEGER_NBITS, (?COMPACT_LARGE_INTEGER bor ?COMPACT_NBITS_VALUE)).
