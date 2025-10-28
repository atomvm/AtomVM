/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

#ifndef _JIT_H_
#define _JIT_H_

#include "bitstring.h"
#include "exportedfunction.h"
#include "term.h"
#include "term_typedef.h"

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

struct Context;

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

#ifndef TYPEDEF_JITSTATE
#define TYPEDEF_JITSTATE
typedef struct JITState JITState;
#endif

#ifndef TYPEDEF_MODULENATIVEINTERFACE
#define TYPEDEF_MODULENATIVEINTERFACE
typedef struct ModuleNativeInterface ModuleNativeInterface;
#endif

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

// Interface to native code:
// Entry point returns the current (or new) context
// jit_state->remaining_reductions is updated.
// If returned context is different from passed context, scheduler resumes in
// schedule_in.
typedef Context *(*ModuleNativeEntryPoint)(Context *ctx, JITState *jit_state, const ModuleNativeInterface *p);

struct JITState
{
    Module *module;
    union
    {
        ModuleNativeEntryPoint continuation;
        const void *continuation_pc;
    };
    int remaining_reductions;
};

// Remember to keep this struct in sync with libs/jit/src/primitives.hrl

struct ModuleNativeInterface
{
    // Helpers
    Context *(*raise_error)(Context *ctx, JITState *jit_state, int offset, term error_term);
    Context *(*do_return)(Context *ctx, JITState *jit_state);
    Context *(*schedule_next_cp)(Context *ctx, JITState *jit_state);
    term (*module_get_atom_term_by_id)(JITState *jit_state, int atom_index);
    Context *(*call_ext)(Context *ctx, JITState *jit_state, int offset, int arity, int index, int n_words);
    bool (*allocate)(Context *ctx, JITState *jit_state, uint32_t stack_need, uint32_t heap_need, uint32_t live);
    Context *(*handle_error)(Context *ctx, JITState *jit_state, int offset);
    void (*jit_trim_live_regs)(Context *ctx, uint32_t live);
    BifImpl0 (*get_imported_bif)(JITState *jit_state, uint32_t bif);
    bool (*deallocate)(Context *ctx, JITState *jit_state, uint32_t n_words);
    Context *(*terminate_context)(Context *ctx, JITState *jit_state);
    TermCompareResult (*term_compare)(Context *ctx, JITState *jit_state, term t, term other, TermCompareOpts opts);
    bool (*test_heap)(Context *ctx, JITState *jit_state, uint32_t heap_need, uint32_t live);
    term (*put_list)(Context *ctx, term head, term tail);
    term (*module_load_literal)(Context *ctx, JITState *jit_state, int index);
    term (*alloc_boxed_integer_fragment)(Context *ctx, avm_int64_t value);
    term (*term_alloc_tuple)(Context *ctx, uint32_t size);
    bool (*send)(Context *ctx, JITState *jit_state);
    term *(*extended_register_pointer)(Context *ctx, unsigned int index);
    Context *(*raise_error_tuple)(Context *ctx, JITState *jit_state, int offset, term error_atom, term arg1);
    term (*term_alloc_fun)(Context *ctx, JITState *jit_state, uint32_t fun_index, uint32_t numfree);
    Context *(*process_signal_messages)(Context *ctx, JITState *jit_state);
    term (*mailbox_peek)(Context *ctx);
    void (*mailbox_remove_message)(Context *ctx);
    void (*timeout)(Context *ctx);
    void (*mailbox_next)(Context *ctx);
    void (*cancel_timeout)(Context *ctx);
    void (*clear_timeout_flag)(Context *ctx);
    Context *(*raise)(Context *ctx, JITState *jit_state, int offset, term stacktrace, term exc_value);
    Context *(*schedule_wait_cp)(Context *ctx, JITState *jit_state);
    Context *(*wait_timeout)(Context *ctx, JITState *jit_state, term timeout, int label);
    Context *(*wait_timeout_trap_handler)(Context *ctx, JITState *jit_state, int label);
    Context *(*call_fun)(Context *ctx, JITState *jit_state, int offset, term fun, unsigned int args_count);
    int (*context_get_flags)(Context *ctx, int mask);
    void (*context_ensure_fpregs)(Context *ctx);
    term (*term_from_float)(Context *ctx, int fpreg);
    bool (*term_is_number)(term t);
    void (*term_conv_to_float)(Context *ctx, term t, int fpreg);
    bool (*fadd)(Context *ctx, int fpreg_1, int fpreg_2, int fpreg_3);
    bool (*fsub)(Context *ctx, int fpreg_1, int fpreg_2, int fpreg_3);
    bool (*fmul)(Context *ctx, int fpreg_1, int fpreg_2, int fpreg_3);
    bool (*fdiv)(Context *ctx, int fpreg_1, int fpreg_2, int fpreg_3);
    void (*fnegate)(Context *ctx, int fpreg_1, int fpreg_2);
    bool (*catch_end)(Context *ctx, JITState *jit_state);
    bool (*memory_ensure_free_with_roots)(Context *ctx, JITState *jit_state, int sz, int live, int flags);
    term (*term_alloc_bin_match_state)(Context *ctx, term src, int slots);
    term (*bitstring_extract_integer)(Context *ctx, JITState *jit_state, term *bin_ptr, size_t offset, int n, int bs_flags);
    size_t (*term_sub_binary_heap_size)(term *bin_ptr, size_t size);
    term (*term_maybe_create_sub_binary)(Context *ctx, term bin, size_t offset, size_t len);
    int (*term_find_map_pos)(Context *ctx, term map, term key);
    int (*bitstring_utf8_size)(int c);
    int (*bitstring_utf16_size)(int c);
    term (*term_create_empty_binary)(Context *ctx, size_t len);
    int (*decode_flags_list)(Context *ctx, JITState *jit_state, term l);
    int (*bitstring_insert_utf8)(term bin, size_t offset, int c);
    int (*bitstring_insert_utf16)(term bin, size_t offset, int c, enum BitstringFlags flags);
    bool (*bitstring_insert_utf32)(term bin, size_t offset, uint32_t c, enum BitstringFlags flags);
    bool (*bitstring_insert_integer)(term bin, size_t offset, term value, size_t n, enum BitstringFlags flags);
    void (*bitstring_copy_module_str)(Context *ctx, JITState *jit_state, term bin, size_t offset, int str_id, size_t len);
    int (*bitstring_copy_binary)(Context *ctx, JITState *jit_state, term t, size_t offset, term src, term size);
    Context *(*apply)(Context *ctx, JITState *jit_state, int offset, term module, term function, unsigned int arity);
    void *(*malloc)(Context *ctx, JITState *jit_state, size_t sz);
    void (*free)(void *ptr);
    term (*put_map_assoc)(Context *ctx, JITState *jit_state, term src, size_t new_entries, size_t num_elements, term *kv);
    term (*bitstring_extract_float)(Context *ctx, term *bin_ptr, size_t offset, int n, int bs_flags);
    int (*module_get_fun_arity)(Module *fun_module, uint32_t fun_index);
    bool (*bitstring_match_module_str)(Context *ctx, JITState *jit_state, term bin, size_t offset, int str_id, size_t len);
    term (*bitstring_get_utf8)(term src);
    term (*bitstring_get_utf16)(term src, int flags_value);
    term (*bitstring_get_utf32)(term src, int flags_value);
    term (*term_copy_map)(Context *ctx, term src);
    term (*stacktrace_build)(Context *ctx);
    term (*alloc_big_integer_fragment)(Context *ctx, size_t digits_len, term_integer_sign_t sign);
};

extern const ModuleNativeInterface module_native_interface;

enum TrapAndLoadResult
{
    TRAP_AND_LOAD_OK,
    TRAP_AND_LOAD_CODE_SERVER_NOT_FOUND
};

#define JIT_FORMAT_VERSION 1

#define JIT_ARCH_X86_64 1
#define JIT_ARCH_AARCH64 2
#define JIT_ARCH_ARMV6M 3

#define JIT_VARIANT_PIC 1
#define JIT_VARIANT_FLOAT32 2

#ifndef AVM_NO_JIT

#ifdef __x86_64__
#define JIT_ARCH_TARGET JIT_ARCH_X86_64
#define JIT_JUMPTABLE_ENTRY_SIZE 5
#endif

#if defined(__arm64__) || defined(__aarch64__)
#define JIT_ARCH_TARGET JIT_ARCH_AARCH64
#define JIT_JUMPTABLE_ENTRY_SIZE 4
#endif

#ifdef __arm__
#define JIT_ARCH_TARGET JIT_ARCH_ARMV6M
#define JIT_JUMPTABLE_ENTRY_SIZE 12
#endif

#ifndef JIT_ARCH_TARGET
#error Unknown JIT target
#endif
#endif

/**
 * @brief Return the entry point from a given jit stream
 *
 * @details Platform implementing JIT must provide this function which
 * is called by code_server:set_native_code/2
 * @param ctx the current context (code_server)
 * @param jit_stream the jit stream term
 * @returns the pointer to the first function
 */
ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream);

/**
 * @brief Trap a process and load module, process shall be resumed at given label.
 *
 * @param ctx the process to trap
 * @param mod the module to load
 * @param label the label to resume the process to
 */
enum TrapAndLoadResult jit_trap_and_load(Context *ctx, Module *mod, uint32_t label);

#ifdef __cplusplus
}
#endif

#endif
