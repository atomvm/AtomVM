/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef _OPCODES_H_
#define _OPCODES_H_

#define OP_LABEL 1
#define OP_FUNC_INFO 2
#define OP_INT_CALL_END 3
#define OP_CALL 4
#define OP_CALL_LAST 5
#define OP_CALL_ONLY 6
#define OP_CALL_EXT 7
#define OP_CALL_EXT_LAST 8
#define OP_BIF0 9
#define OP_BIF1 10
#define OP_BIF2 11
#define OP_ALLOCATE 12
#define OP_ALLOCATE_HEAP 13
#define OP_ALLOCATE_ZERO 14
#define OP_ALLOCATE_HEAP_ZERO 15
#define OP_TEST_HEAP 16
#define OP_KILL 17
#define OP_DEALLOCATE 18
#define OP_RETURN 19
#define OP_SEND 20
#define OP_REMOVE_MESSAGE 21
#define OP_TIMEOUT 22
#define OP_LOOP_REC 23
#define OP_LOOP_REC_END 24
#define OP_WAIT 25
#define OP_WAIT_TIMEOUT 26
#define OP_IS_LT 39
#define OP_IS_GE 40
#define OP_IS_EQUAL 41
#define OP_IS_NOT_EQUAL 42
#define OP_IS_EQ_EXACT 43
#define OP_IS_NOT_EQ_EXACT 44
#define OP_IS_INTEGER 45
#define OP_IS_FLOAT 46
#define OP_IS_NUMBER 47
#define OP_IS_ATOM 48
#define OP_IS_PID 49
#define OP_IS_REFERENCE 50
#define OP_IS_PORT 51
#define OP_IS_NIL 52
#define OP_IS_BINARY 53
#define OP_IS_LIST 55
#define OP_IS_NONEMPTY_LIST 56
#define OP_IS_TUPLE 57
#define OP_TEST_ARITY 58
#define OP_SELECT_VAL 59
#define OP_SELECT_TUPLE_ARITY 60
#define OP_JUMP 61
#define OP_CATCH 62
#define OP_CATCH_END 63
#define OP_MOVE 64
#define OP_GET_LIST 65
#define OP_GET_TUPLE_ELEMENT 66
#define OP_SET_TUPLE_ELEMENT 67
#define OP_PUT_LIST 69
#define OP_PUT_TUPLE 70
#define OP_PUT 71
#define OP_BADMATCH 72
#define OP_IF_END 73
#define OP_CASE_END 74
#define OP_CALL_FUN 75
#define OP_IS_FUNCTION 77
#define OP_CALL_EXT_ONLY 78
#define OP_BS_PUT_INTEGER 89
#define OP_BS_PUT_BINARY 90
#define OP_BS_PUT_STRING 92
#define OP_FCLEARERROR 94
#define OP_FCHECKERROR 95
#define OP_FMOVE 96
#define OP_FCONV 97
#define OP_FADD 98
#define OP_FSUB 99
#define OP_FMUL 100
#define OP_FDIV 101
#define OP_FNEGATE 102
#define OP_MAKE_FUN2 103
#define OP_TRY 104
#define OP_TRY_END 105
#define OP_TRY_CASE 106
#define OP_TRY_CASE_END 107
#define OP_RAISE 108
#define OP_BS_INIT2 109
#define OP_BS_ADD 111
#define OP_APPLY 112
#define OP_APPLY_LAST 113
#define OP_IS_BOOLEAN 114
#define OP_IS_FUNCTION2 115
#define OP_BS_START_MATCH2 116
#define OP_BS_GET_INTEGER2 117
#define OP_BS_GET_FLOAT2 118
#define OP_BS_GET_BINARY2 119
#define OP_BS_SKIP_BITS2 120
#define OP_BS_TEST_TAIL2 121
#define OP_BS_SAVE2 122
#define OP_BS_RESTORE2 123
#define OP_GC_BIF1 124
#define OP_GC_BIF2 125
#define OP_IS_BITSTR 129
#define OP_BS_CONTEXT_TO_BINARY 130
#define OP_BS_TEST_UNIT 131
#define OP_BS_MATCH_STRING 132
#define OP_BS_INIT_WRITABLE 133
#define OP_BS_APPEND 134
#define OP_BS_PRIVATE_APPEND 135
#define OP_TRIM 136
#define OP_BS_INIT_BITS 137
#define OP_BS_GET_UTF8 138
#define OP_BS_SKIP_UTF8 139
#define OP_BS_GET_UTF16 140
#define OP_BS_SKIP_UTF16 141
#define OP_BS_GET_UTF32 142
#define OP_BS_SKIP_UTF32 143
#define OP_BS_UTF8_SIZE 144
#define OP_BS_PUT_UTF8 145
#define OP_BS_UTF16_SIZE 146
#define OP_BS_PUT_UTF16 147
#define OP_BS_PUT_UTF32 148
#define OP_RECV_MARK 150
#define OP_RECV_SET 151
#define OP_GC_BIF3 152
#define OP_LINE 153
#define OP_PUT_MAP_ASSOC 154
#define OP_PUT_MAP_EXACT 155
#define OP_IS_MAP 156
#define OP_HAS_MAP_FIELDS 157
#define OP_GET_MAP_ELEMENTS 158
#define OP_IS_TAGGED_TUPLE 159
#define OP_BUILD_STACKTRACE 160
#define OP_GET_HD 162
#define OP_GET_TL 163
#define OP_PUT_TUPLE2 164
#define OP_BS_GET_TAIL 165
#define OP_BS_START_MATCH3 166
#define OP_BS_GET_POSITION 167
#define OP_BS_SET_POSITION 168
#define OP_SWAP 169
#define OP_BS_START_MATCH4 170
#define OP_MAKE_FUN3 171
#define OP_INIT_YREGS 172
#define OP_RECV_MARKER_BIND 173
#define OP_RECV_MARKER_CLEAR 174
#define OP_RECV_MARKER_RESERVE 175
#define OP_RECV_MARKER_USE 176
#define OP_BS_CREATE_BIN 177
#define OP_CALL_FUN2 178
#define OP_BADRECORD 180
#define OP_UPDATE_RECORD 181
#define OP_BS_MATCH 182

#endif
