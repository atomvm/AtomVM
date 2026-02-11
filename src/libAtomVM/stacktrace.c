/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Fred Dushin <fred@dushin.net>
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

#include "stacktrace.h"

#include "bif.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "memory.h"
#include "nifs.h"

#ifndef AVM_CREATE_STACKTRACES

term stacktrace_create_raw(Context *ctx, Module *mod, int current_offset, term exception_class)
{
    UNUSED(ctx);
    UNUSED(mod);
    UNUSED(current_offset);
    return exception_class;
}

term stacktrace_create_raw_mfa(Context *ctx, Module *mod, int current_offset, term exception_class, term module_atom, term function_atom, int arity)
{
    UNUSED(ctx);
    UNUSED(mod);
    UNUSED(current_offset);
    UNUSED(exception_class);
    UNUSED(module_atom);
    UNUSED(function_atom);
    UNUSED(arity);

    return exception_class;
}

term stacktrace_build(Context *ctx, term *stack_info, uint32_t live)
{
    UNUSED(ctx);
    UNUSED(stack_info);
    UNUSED(live);
    return UNDEFINED_ATOM;
}

term stacktrace_exception_class(term stack_info)
{
    return stack_info;
}

#else

static bool location_sets_append(GlobalContext *global, Module *mod, const uint8_t *filename, size_t filename_len, size_t *total_filename_len, const void ***io_locations_set, size_t *io_locations_set_size)
{
    const void **locations_set = *io_locations_set;
    size_t locations_set_size = *io_locations_set_size;
    const void *key = filename;
    if (IS_NULL_PTR(filename)) {
        key = mod;
        size_t module_name_len;
        const uint8_t *module_name_data = atom_table_get_atom_string(global->atom_table, term_to_atom_index(module_get_name(mod)), &module_name_len);
        UNUSED(module_name_data);
        filename_len = module_name_len + 4; // ".erl"
    }
    for (size_t i = 0; i < locations_set_size; i++) {
        if (locations_set[i] == key) {
            return true;
        }
    }
    const void **new_locations_set = realloc(locations_set, (locations_set_size + 1) * sizeof(const uint8_t *));
    if (IS_NULL_PTR(new_locations_set)) {
        // Some versions of gcc don't know that if allocation fails, original pointer should still be freed
#pragma GCC diagnostic push
#if (defined(__GNUC__) && !defined(__clang__) && __GNUC__ >= 12)
#pragma GCC diagnostic ignored "-Wuse-after-free"
#endif
        free(locations_set);
#pragma GCC diagnostic pop
        *io_locations_set = NULL;
        fprintf(stderr, "Unable to allocate space for locations set.  No stacktrace will be created\n");
        return false;
    }
    *io_locations_set = new_locations_set;
    new_locations_set[locations_set_size] = key;
    *io_locations_set_size = locations_set_size + 1;
    *total_filename_len += filename_len;

    return true;
}

term stacktrace_create_raw(Context *ctx, Module *mod, int current_offset, term exception_class)
{
    return stacktrace_create_raw_mfa(ctx, mod, current_offset, exception_class, UNDEFINED_ATOM, UNDEFINED_ATOM, 0);
}

term stacktrace_create_raw_mfa(Context *ctx, Module *mod, int current_offset, term exception_class, term module_atom, term function_atom, int arity)
{
    if (term_is_nonempty_list(ctx->exception_stacktrace)) {
        // there is already a built stacktrace, nothing to do here
        // (this happens when re-raising with raise/3
        return ctx->exception_stacktrace;
    }

    unsigned int num_frames = 0;
    unsigned int num_aux_terms = 0;
    size_t filename_lens = 0;
    Module *prev_mod = NULL;
    long prev_mod_offset = -1;
    term *ct = ctx->e;
    term *stack_base = context_stack_base(ctx);

    const void **locations = NULL;
    size_t num_locations = 0;

    while (ct != stack_base) {
        if (term_is_cp(*ct)) {

            Module *cp_mod;
            long mod_offset;

            module_cp_to_label_offset(*ct, &cp_mod, NULL, NULL, &mod_offset, ctx->global);
            // TODO: investigate
            // mod_offset is currently never equal to cp_mod->end_instruction_ii with native code
            if (mod_offset != cp_mod->end_instruction_ii && !(prev_mod == cp_mod && mod_offset == prev_mod_offset)) {
                ++num_frames;
                prev_mod = cp_mod;
                prev_mod_offset = mod_offset;
                if (module_has_line_chunk(cp_mod)) {
                    uint32_t line;
                    const uint8_t *filename;
                    size_t filename_len;
                    if (LIKELY(module_find_line(cp_mod, (unsigned int) mod_offset, &line, &filename_len, &filename))) {
                        if (!location_sets_append(ctx->global, cp_mod, filename, filename_len, &filename_lens, &locations, &num_locations)) {
                            return UNDEFINED_ATOM;
                        }
                        num_aux_terms++;
                    }
                }
            }
        } else if (term_is_catch_label(*ct)) {
            int module_index;
            int label = term_to_catch_label_and_module(*ct, &module_index);

            Module *cl_mod = globalcontext_get_module_by_index(ctx->global, module_index);
            int mod_offset = module_label_code_offset(cl_mod, label);

            if (!(prev_mod == cl_mod && mod_offset == prev_mod_offset)) {
                ++num_frames;
                prev_mod = cl_mod;
                prev_mod_offset = mod_offset;
                if (module_has_line_chunk(cl_mod)) {
                    uint32_t line;
                    const uint8_t *filename;
                    size_t filename_len;
                    if (LIKELY(module_find_line(cl_mod, (unsigned int) mod_offset, &line, &filename_len, &filename))) {
                        if (!location_sets_append(ctx->global, cl_mod, filename, filename_len, &filename_lens, &locations, &num_locations)) {
                            return UNDEFINED_ATOM;
                        }
                        num_aux_terms++;
                    }
                }
            }
        }
        ct++;
    }

    num_frames++;
    if (module_has_line_chunk(mod)) {
        uint32_t line;
        const uint8_t *filename;
        size_t filename_len;
        if (LIKELY(module_find_line(mod, (unsigned int) current_offset, &line, &filename_len, &filename))) {
            if (!location_sets_append(ctx->global, mod, filename, filename_len, &filename_lens, &locations, &num_locations)) {
                return UNDEFINED_ATOM;
            }
            num_aux_terms++;
        }
    }

    free(locations);

    // there is an additional x register available as a temporary storage, we'll use it
    // we'll backup the exception_reason in it
    int live_x_regs = MINI(arity, MAX_REG) + 1;
    ctx->x[live_x_regs - 1] = ctx->exception_reason;

    // {num_frames, num_aux_terms, filename_lens, num_mods, [{module, offset}, ...]}
    size_t requested_size = TUPLE_SIZE(6) + num_frames * (2 + TUPLE_SIZE(2)) + LIST_SIZE(1, arity) + TUPLE_SIZE(5);
    // NOLINT(term-use-after-gc) exception_class is always an atom
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, live_x_regs, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        fprintf(stderr, "WARNING: Unable to allocate heap space for raw stacktrace\n");
        return OUT_OF_MEMORY_ATOM;
    }

    ctx->exception_reason = ctx->x[live_x_regs - 1];

    term raw_stacktrace = term_nil();

    term frame_info;

    // on OTP <= 22 module_atom is set to erlang, when calling a function such as erlang:throw
    // making this heuristic unreliable since hides the throw caller from the stacktrace
    // this means that either this heuristic is not 100% correct, or something changed in OTP-23
    // anyway on OTP >= 23 seem to work as expected.
    if (module_atom == UNDEFINED_ATOM) {
        // module_atom has not been provided, let's use mod->module_index

        frame_info = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(frame_info, 0, term_from_int(mod->module_index));
        term_put_tuple_element(frame_info, 1, term_from_int(current_offset));
    } else {
        // This branch allows overwriting found module name, with the name provided (module_atom)
        // same applies to function_atom and arity. This allows having NIFs and BIFs on the
        // stacktrace

        term args_list = term_nil();
        for (int arg_i = arity - 1; arg_i >= 0; arg_i--) {
            args_list = term_list_prepend(ctx->x[arg_i], args_list, &ctx->heap);
        }

        frame_info = term_alloc_tuple(5, &ctx->heap);
        term_put_tuple_element(frame_info, 0, term_from_int(mod->module_index));
        term_put_tuple_element(frame_info, 1, term_from_int(current_offset));
        term_put_tuple_element(frame_info, 2, module_atom);
        term_put_tuple_element(frame_info, 3, function_atom);
        term_put_tuple_element(frame_info, 4, args_list);
    }
    raw_stacktrace = term_list_prepend(frame_info, raw_stacktrace, &ctx->heap);

    prev_mod = NULL;
    prev_mod_offset = -1;
    // GC may have moved stack
    ct = ctx->e;
    stack_base = context_stack_base(ctx);
    while (ct != stack_base) {
        if (term_is_cp(*ct)) {
            Module *cp_mod;
            long mod_offset;

            module_cp_to_label_offset(*ct, &cp_mod, NULL, NULL, &mod_offset, ctx->global);
            if (mod_offset != cp_mod->end_instruction_ii && !(prev_mod == cp_mod && mod_offset == prev_mod_offset)) {

                prev_mod = cp_mod;
                prev_mod_offset = mod_offset;

                term frame_info = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(frame_info, 0, term_from_int(cp_mod->module_index));
                term_put_tuple_element(frame_info, 1, term_from_int(mod_offset));

                raw_stacktrace = term_list_prepend(frame_info, raw_stacktrace, &ctx->heap);
            }
        } else if (term_is_catch_label(*ct)) {

            int module_index;
            int label = term_to_catch_label_and_module(*ct, &module_index);
            Module *cl_mod = globalcontext_get_module_by_index(ctx->global, module_index);
            int mod_offset = module_label_code_offset(cl_mod, label);

            if (!(prev_mod == cl_mod && mod_offset == prev_mod_offset)) {

                prev_mod = cl_mod;
                prev_mod_offset = mod_offset;

                term frame_info = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(frame_info, 0, term_from_int(module_index));
                term_put_tuple_element(frame_info, 1, term_from_int(mod_offset));

                raw_stacktrace = term_list_prepend(frame_info, raw_stacktrace, &ctx->heap);
            }
        }
        ct++;
    }

    term stack_info = term_alloc_tuple(6, &ctx->heap);
    term_put_tuple_element(stack_info, 0, term_from_int(num_frames));
    term_put_tuple_element(stack_info, 1, term_from_int(num_aux_terms));
    term_put_tuple_element(stack_info, 2, term_from_int(filename_lens));
    term_put_tuple_element(stack_info, 3, term_from_int(num_locations));
    term_put_tuple_element(stack_info, 4, raw_stacktrace);
    term_put_tuple_element(stack_info, 5, exception_class);

    return stack_info;
}

term stacktrace_exception_class(term stack_info)
{
    assert(term_is_tuple(stack_info) && term_get_tuple_arity(stack_info) >= 6);
    return term_get_tuple_element(stack_info, 5);
}

struct ModulePathPair
{
    const void *key;
    term path;
};

static term find_path_created(const void *key, struct ModulePathPair *module_paths, int len)
{
    for (int i = 0; i < len; ++i) {
        if (module_paths[i].key == key) {
            return module_paths[i].path;
        }
    }
    return term_invalid_term();
}

static bool is_bif_or_nif(
    term module_atom, term function_atom, avm_int_t arity, const GlobalContext *glb)
{
    atom_index_t module_atom_index = term_to_atom_index(module_atom);
    atom_index_t function_atom_index = term_to_atom_index(function_atom);
    char mfa[MAX_MFA_NAME_LEN];
    atom_table_write_mfa(
        glb->atom_table, mfa, sizeof(mfa), module_atom_index, function_atom_index, arity);

    return (bif_registry_get_handler(mfa) != NULL) || (nifs_get(mfa) != NULL);
}

term stacktrace_build(Context *ctx, term *stack_info, uint32_t live)
{
    GlobalContext *glb = ctx->global;

    if (term_is_nonempty_list(*stack_info)) {
        // stacktrace has been already built. Nothing to do here
        // This may happen when re-raising with raise/3
        return *stack_info;
    }

    if (*stack_info == OUT_OF_MEMORY_ATOM) {
        return *stack_info;
    }
    if (!term_is_tuple(*stack_info)) {
        return UNDEFINED_ATOM;
    }

    int num_frames = term_to_int(term_get_tuple_element(*stack_info, 0));
    int num_aux_terms = term_to_int(term_get_tuple_element(*stack_info, 1));
    int filename_lens = term_to_int(term_get_tuple_element(*stack_info, 2));
    int num_mods = term_to_int(term_get_tuple_element(*stack_info, 3));

    struct ModulePathPair *module_paths = malloc(num_mods * sizeof(struct ModulePathPair));
    if (IS_NULL_PTR(module_paths)) {
        fprintf(stderr, "Unable to allocate space for module paths.  Returning raw stacktrace.\n");
        return *stack_info;
    }

    //
    // [{module, function, arity, [{file, string()}, {line, int}]}, ...]
    //
    size_t requested_size = (TUPLE_SIZE(4) + 2) * num_frames + num_aux_terms * (4 + 2 * TUPLE_SIZE(2)) + 2 * filename_lens;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        free(module_paths);
        return OUT_OF_MEMORY_ATOM;
    }

    term raw_stacktrace = term_get_tuple_element(*stack_info, 4);

    term stacktrace = term_nil();
    term el = raw_stacktrace;
    int module_path_idx = 0;
    while (!term_is_nil(el)) {
        term mod_index_tuple = term_get_list_head(el);

        size_t mod_index_tuple_arity = term_get_tuple_arity(mod_index_tuple);
        assert((mod_index_tuple_arity == 2) || (mod_index_tuple_arity == 5));

        term cp = module_address(
            term_to_int(term_get_tuple_element(mod_index_tuple, 0)),
            term_to_int(term_get_tuple_element(mod_index_tuple, 1)));

        Module *cp_mod;
        int label;
        long mod_offset;
        module_cp_to_label_offset(cp, &cp_mod, &label, NULL, &mod_offset, ctx->global);

        term module_name = module_get_name(cp_mod);

        term aux_data = term_nil();
        if (module_has_line_chunk(cp_mod)) {
            uint32_t line;
            const uint8_t *filename;
            size_t filename_len;
            if (LIKELY(module_find_line(cp_mod, (unsigned int) mod_offset, &line, &filename_len, &filename))) {
                term line_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(line_tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x4", "line")));
                term_put_tuple_element(line_tuple, 1, term_from_int(line));
                aux_data = term_list_prepend(line_tuple, aux_data, &ctx->heap);

                term file_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(file_tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x4", "file")));

                const void *key = IS_NULL_PTR(filename) ? (const void *) cp_mod : (const void *) filename;
                term path = find_path_created(key, module_paths, module_path_idx);
                if (term_is_invalid_term(path)) {
                    if (IS_NULL_PTR(filename)) {
                        size_t module_name_len;
                        const uint8_t *module_name_data = atom_table_get_atom_string(ctx->global->atom_table, term_to_atom_index(module_get_name(cp_mod)), &module_name_len);
                        uint8_t *default_filename = malloc(module_name_len + 4);
                        if (IS_NULL_PTR(default_filename)) {
                            free(module_paths);
                            return OUT_OF_MEMORY_ATOM;
                        }
                        memcpy(default_filename, module_name_data, module_name_len);
                        memcpy(default_filename + module_name_len, ".erl", 4);
                        path = term_from_string(default_filename, module_name_len + 4, &ctx->heap);
                        free(default_filename);
                    } else {
                        path = term_from_string(filename, filename_len, &ctx->heap);
                    }
                    module_paths[module_path_idx].key = key;
                    module_paths[module_path_idx].path = path;
                    module_path_idx++;
                }
                term_put_tuple_element(file_tuple, 1, path);
                aux_data = term_list_prepend(file_tuple, aux_data, &ctx->heap);
            }
        }

        term frame_i = term_alloc_tuple(4, &ctx->heap);
        if (mod_index_tuple_arity == 5) {
            term raw_module = term_get_tuple_element(mod_index_tuple, 2);
            term raw_function = term_get_tuple_element(mod_index_tuple, 3);
            term raw_arity_or_args = term_get_tuple_element(mod_index_tuple, 4);
            term_put_tuple_element(frame_i, 0, raw_module);
            term_put_tuple_element(frame_i, 1, raw_function);
            term_put_tuple_element(frame_i, 2, raw_arity_or_args);

            avm_int_t arity;
            if (term_is_int(raw_arity_or_args)) {
                arity = term_to_int(raw_arity_or_args);
            } else if (term_is_list(raw_arity_or_args)) {
                int is_proper;
                arity = term_list_length(raw_arity_or_args, &is_proper);
                assert(is_proper);
            } else {
                AVM_ABORT();
            }

            if ((module_name == raw_module) && !is_bif_or_nif(raw_module, raw_function, arity, glb)) {
                term_put_tuple_element(frame_i, 3, aux_data);
            } else {
                // mismatch means that error likely happened somewhere else, like a NIF, hence:
                // aux_data is misleading and should be discarded
                term_put_tuple_element(frame_i, 3, term_nil());
            }
        } else {
            term_put_tuple_element(frame_i, 0, module_name);
            atom_index_t function_name;
            int arity = 0;
            if (LIKELY(module_get_function_from_label(cp_mod, label, &function_name, &arity))) {
                term_put_tuple_element(frame_i, 1, term_from_atom_index(function_name));
                term_put_tuple_element(frame_i, 2, term_from_int(arity));
            } else {
                term_put_tuple_element(frame_i, 1, UNDEFINED_ATOM);
                term_put_tuple_element(frame_i, 2, term_from_int(0));
            }
            term_put_tuple_element(frame_i, 3, aux_data);
        }
        stacktrace = term_list_prepend(frame_i, stacktrace, &ctx->heap);

        el = term_get_list_tail(el);
    }
    free(module_paths);

    return stacktrace;
}

#endif
