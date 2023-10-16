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
#include "defaultatoms.h"
#include "globalcontext.h"

#ifndef AVM_CREATE_STACKTRACES

term stacktrace_create_raw(Context *ctx, Module *mod, int current_offset, term exception_class)
{
    UNUSED(ctx);
    UNUSED(mod);
    UNUSED(current_offset);
    return exception_class;
}

term stacktrace_build(Context *ctx, term *stack_info)
{
    UNUSED(ctx);
    UNUSED(stack_info);
    return UNDEFINED_ATOM;
}

term stacktrace_exception_class(term stack_info)
{
    return stack_info;
}

#else

static void cp_to_mod_lbl_off(term cp, Context *ctx, Module **cp_mod, int *label, int *l_off, long *mod_offset)
{
    int module_index = cp >> 24;
    Module *mod = globalcontext_get_module_by_index(ctx->global, module_index);
    *mod_offset = (cp & 0xFFFFFF) >> 2;

    *cp_mod = mod;

    uint8_t *code = &mod->code->code[0];
    int labels_count = ENDIAN_SWAP_32(mod->code->labels);

    int i = 1;
    const uint8_t *l = mod->labels[1];
    while (*mod_offset > l - code) {
        i++;
        if (i >= labels_count) {
            // last label + 1 is reserved for end of module.
            *label = i;
            *l_off = 0;
            return;
        }
        l = mod->labels[i];
    }

    *label = i - 1;
    *l_off = *mod_offset - (mod->labels[*label] - code);
}

static bool is_module_member(Module *mod, Module **mods, unsigned long len)
{
    for (unsigned long i = 0; i < len; ++i) {
        if (mods[i] == mod) {
            return true;
        }
    }
    return false;
}

term stacktrace_create_raw(Context *ctx, Module *mod, int current_offset, term exception_class)
{
    unsigned int num_frames = 0;
    unsigned int num_aux_terms = 0;
    unsigned int filename_lens = 0;
    Module *prev_mod = NULL;
    long prev_mod_offset = -1;
    term *ct = ctx->e;
    term *stack_base = context_stack_base(ctx);

    unsigned long stack_size = context_stack_size(ctx);
    Module **modules = malloc(stack_size * sizeof(Module *));
    if (IS_NULL_PTR(modules)) {
        fprintf(stderr, "Unable to allocate space for modules list.  No stacktrace will be created\n");
        return UNDEFINED_ATOM;
    }

    size_t num_mods = 0;

    while (ct != stack_base) {
        if (term_is_cp(*ct)) {

            Module *cp_mod;
            int label;
            int offset;
            long mod_offset;

            cp_to_mod_lbl_off(*ct, ctx, &cp_mod, &label, &offset, &mod_offset);
            if (mod_offset != cp_mod->end_instruction_ii && !(prev_mod == cp_mod && mod_offset == prev_mod_offset)) {
                ++num_frames;
                prev_mod = cp_mod;
                prev_mod_offset = mod_offset;
                if (module_has_line_chunk(cp_mod)) {
                    if (!is_module_member(cp_mod, modules, num_mods)) {
                        modules[num_mods] = cp_mod;
                        filename_lens += cp_mod->filenames[0].len;
                        num_mods++;
                    }
                    num_aux_terms++;
                }
            }
        } else if (term_is_catch_label(*ct)) {
            int module_index;
            int label = term_to_catch_label_and_module(*ct, &module_index);

            Module *cl_mod = globalcontext_get_module_by_index(ctx->global, module_index);
            uint8_t *code = &cl_mod->code->code[0];
            int mod_offset = cl_mod->labels[label] - code;

            if (!(prev_mod == cl_mod && mod_offset == prev_mod_offset)) {
                ++num_frames;
                prev_mod = cl_mod;
                prev_mod_offset = mod_offset;
                if (module_has_line_chunk(cl_mod)) {
                    if (!is_module_member(cl_mod, modules, num_mods)) {
                        modules[num_mods] = cl_mod;
                        filename_lens += cl_mod->filenames[0].len;
                        num_mods++;
                    }
                    num_aux_terms++;
                }
            }
        }
        ct++;
    }

    num_frames++;
    if (module_has_line_chunk(mod)) {
        if (!is_module_member(mod, modules, num_mods)) {
            filename_lens += mod->filenames[0].len;
            num_mods++;
        }
        num_aux_terms++;
    }

    free(modules);

    // {num_frames, num_aux_terms, filename_lens, num_mods, [{module, offset}, ...]}
    size_t requested_size = TUPLE_SIZE(6) + num_frames * (2 + TUPLE_SIZE(2));
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "WARNING: Unable to allocate heap space for raw stacktrace\n");
        return OUT_OF_MEMORY_ATOM;
    }

    term raw_stacktrace = term_nil();

    term frame_info = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(frame_info, 0, term_from_int(mod->module_index));
    term_put_tuple_element(frame_info, 1, term_from_int(current_offset));
    raw_stacktrace = term_list_prepend(frame_info, raw_stacktrace, &ctx->heap);

    prev_mod = NULL;
    prev_mod_offset = -1;
    // GC may have moved stack
    ct = ctx->e;
    stack_base = context_stack_base(ctx);
    while (ct != stack_base) {
        if (term_is_cp(*ct)) {
            Module *cp_mod;
            int label;
            int offset;
            long mod_offset;

            cp_to_mod_lbl_off(*ct, ctx, &cp_mod, &label, &offset, &mod_offset);
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
            uint8_t *code = &cl_mod->code->code[0];
            int mod_offset = cl_mod->labels[label] - code;

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
    term_put_tuple_element(stack_info, 3, term_from_int(num_mods));
    term_put_tuple_element(stack_info, 4, raw_stacktrace);
    term_put_tuple_element(stack_info, 5, exception_class);

    return stack_info;
}

term stacktrace_exception_class(term stack_info)
{
    return term_get_tuple_element(stack_info, 5);
}

struct ModulePathPair
{
    term module;
    term path;
};

static term find_path_created(term module_name, struct ModulePathPair *module_paths, int len)
{
    for (int i = 0; i < len; ++i) {
        if (module_paths[i].module == module_name) {
            return module_paths[i].path;
        }
    }
    return term_invalid_term();
}

term stacktrace_build(Context *ctx, term *stack_info)
{
    GlobalContext *glb = ctx->global;

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
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        free(module_paths);
        return OUT_OF_MEMORY_ATOM;
    }

    // Note.  Safe to get stacktrace after GC when stack_info comes from x[0]
    term raw_stacktrace = term_get_tuple_element(*stack_info, 4);

    term stacktrace = term_nil();
    term el = raw_stacktrace;
    int module_path_idx = 0;
    while (!term_is_nil(el)) {
        term mod_index_tuple = term_get_list_head(el);
        term cp = module_address(
            term_to_int(term_get_tuple_element(mod_index_tuple, 0)),
            term_to_int(term_get_tuple_element(mod_index_tuple, 1)));

        Module *cp_mod;
        int label;
        int offset;
        long mod_offset;
        cp_to_mod_lbl_off(cp, ctx, &cp_mod, &label, &offset, &mod_offset);

        term module_name = module_get_name(cp_mod);

        term frame_i = term_alloc_tuple(4, &ctx->heap);
        term_put_tuple_element(frame_i, 0, module_name);

        term aux_data = term_nil();
        if (module_has_line_chunk(cp_mod)) {
            term line_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(line_tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x4", "line")));
            int line = module_find_line(cp_mod, (unsigned int) mod_offset);
            term_put_tuple_element(line_tuple, 1, line == -1 ? UNDEFINED_ATOM : term_from_int(line));
            aux_data = term_list_prepend(line_tuple, aux_data, &ctx->heap);

            term file_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(file_tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x4", "file")));

            term path = find_path_created(module_name, module_paths, module_path_idx);
            if (term_is_invalid_term(path)) {
                path = term_from_string((const uint8_t *) cp_mod->filenames[0].data, cp_mod->filenames[0].len, &ctx->heap);
                module_paths[module_path_idx].module = module_name;
                module_paths[module_path_idx].path = path;
                module_path_idx++;
            }
            term_put_tuple_element(file_tuple, 1, path);
            aux_data = term_list_prepend(file_tuple, aux_data, &ctx->heap);
        }
        term_put_tuple_element(frame_i, 3, aux_data);

        AtomString function_name = NULL;
        int arity = 0;
        bool result = module_get_function_from_label(cp_mod, label, &function_name, &arity, glb);

        if (LIKELY(result)) {
            term_put_tuple_element(frame_i, 1, globalcontext_make_atom(glb, function_name));
            term_put_tuple_element(frame_i, 2, term_from_int(arity));
        } else {
            term_put_tuple_element(frame_i, 1, UNDEFINED_ATOM);
            term_put_tuple_element(frame_i, 2, term_from_int(0));
        }
        stacktrace = term_list_prepend(frame_i, stacktrace, &ctx->heap);

        el = term_get_list_tail(el);
    }
    free(module_paths);

    return stacktrace;
}

#endif
