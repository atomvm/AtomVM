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

% Remember to keep list in sync with ModuleNativeInterface in src/libAtomVM/jit.h

-define(PRIM_RAISE_ERROR, 0).
-define(PRIM_RETURN, 1).
-define(PRIM_SCHEDULE_NEXT_CP, 2).
-define(PRIM_MODULE_GET_ATOM_TERM_BY_ID, 3).
-define(PRIM_CALL_EXT, 4).
-define(PRIM_ALLOCATE, 5).
-define(PRIM_HANDLE_ERROR, 6).
-define(PRIM_TRIM_LIVE_REGS, 7).
-define(PRIM_GET_IMPORTED_BIF, 8).
-define(PRIM_DEALLOCATE, 9).
-define(PRIM_TERMINATE_CONTEXT, 10).
-define(PRIM_TERM_COMPARE, 11).
-define(PRIM_TEST_HEAP, 12).
-define(PRIM_PUT_LIST, 13).
-define(PRIM_MODULE_LOAD_LITERAL, 14).
-define(PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, 15).
-define(PRIM_TERM_ALLOC_TUPLE, 16).
-define(PRIM_SEND, 17).
-define(PRIM_EXTENDED_REGISTER_PTR, 18).
-define(PRIM_RAISE_ERROR_TUPLE, 19).
-define(PRIM_TERM_ALLOC_FUN, 20).
-define(PRIM_PROCESS_SIGNAL_MESSAGES, 21).
-define(PRIM_MAILBOX_PEEK, 22).
-define(PRIM_MAILBOX_REMOVE_MESSAGE, 23).
-define(PRIM_TIMEOUT, 24).
-define(PRIM_MAILBOX_NEXT, 25).
-define(PRIM_CANCEL_TIMEOUT, 26).
-define(PRIM_CLEAR_TIMEOUT_FLAG, 27).
-define(PRIM_RAISE, 28).
-define(PRIM_SCHEDULE_WAIT_CP, 29).
-define(PRIM_WAIT_TIMEOUT, 30).
-define(PRIM_WAIT_TIMEOUT_TRAP_HANDLER, 31).
-define(PRIM_CALL_FUN, 32).
-define(PRIM_CONTEXT_GET_FLAGS, 33).
-define(PRIM_CONTEXT_ENSURE_FPREGS, 34).
-define(PRIM_TERM_FROM_FLOAT, 35).
-define(PRIM_TERM_IS_NUMBER, 36).
-define(PRIM_TERM_CONV_TO_FLOAT, 37).
-define(PRIM_FADD, 38).
-define(PRIM_FSUB, 39).
-define(PRIM_FMUL, 40).
-define(PRIM_FDIV, 41).
-define(PRIM_FNEGATE, 42).
-define(PRIM_CATCH_END, 43).
-define(PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, 44).
-define(PRIM_TERM_ALLOC_BIN_MATCH_STATE, 45).
-define(PRIM_BITSTRING_EXTRACT_INTEGER, 46).
-define(PRIM_TERM_SUB_BINARY_HEAP_SIZE, 47).
-define(PRIM_TERM_MAYBE_CREATE_SUB_BINARY, 48).
-define(PRIM_TERM_FIND_MAP_POS, 49).
-define(PRIM_BITSTRING_UTF8_SIZE, 50).
-define(PRIM_BITSTRING_UTF16_SIZE, 51).
-define(PRIM_TERM_CREATE_EMPTY_BINARY, 52).
-define(PRIM_DECODE_FLAGS_LIST, 53).
-define(PRIM_BITSTRING_INSERT_UTF8, 54).
-define(PRIM_BITSTRING_INSERT_UTF16, 55).
-define(PRIM_BITSTRING_INSERT_UTF32, 56).
-define(PRIM_BITSTRING_INSERT_INTEGER, 57).
-define(PRIM_BITSTRING_COPY_MODULE_STR, 58).
-define(PRIM_BITSTRING_COPY_BINARY, 59).
-define(PRIM_APPLY, 60).
-define(PRIM_MALLOC, 61).
-define(PRIM_FREE, 62).
-define(PRIM_PUT_MAP_ASSOC, 63).
-define(PRIM_BITSTRING_EXTRACT_FLOAT, 64).
-define(PRIM_MODULE_GET_FUN_ARITY, 65).
-define(PRIM_BITSTRING_MATCH_MODULE_STR, 66).
-define(PRIM_BITSTRING_GET_UTF8, 67).
-define(PRIM_BITSTRING_GET_UTF16, 68).
-define(PRIM_BITSTRING_GET_UTF32, 69).
-define(PRIM_TERM_COPY_MAP, 70).
-define(PRIM_STACKTRACE_BUILD, 71).
-define(PRIM_ALLOC_BIG_INTEGER_FRAGMENT, 72).

% Parameters to ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS
% -define(MEMORY_NO_SHRINK, 0).
-define(MEMORY_CAN_SHRINK, 1).
% -define(MEMORY_FORCE_SHRINK, 2).
% -define(MEMORY_NO_GC, 3).

% term_integer_sign_t sign parameter for PRIM_ALLOC_BIG_INTEGER_FRAGMENT
-define(TERM_POSITIVE_INTEGER, 0).
-define(TERM_NEGATIVE_INTEGER, 4).
