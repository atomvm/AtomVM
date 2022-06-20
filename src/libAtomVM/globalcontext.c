/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

#include <limits.h>
#include <string.h>

#include "globalcontext.h"

#include "atomshashtable.h"
#include "context.h"
#include "defaultatoms.h"
#include "list.h"
#include "sys.h"
#include "utils.h"
#include "valueshashtable.h"

struct RegisteredProcess
{
    struct ListHead registered_processes_list_head;

    int atom_index;
    int local_process_id;
};

GlobalContext *globalcontext_new()
{
    GlobalContext *glb = malloc(sizeof(GlobalContext));
    if (IS_NULL_PTR(glb)) {
        return NULL;
    }
    list_init(&glb->ready_processes);
    list_init(&glb->waiting_processes);
    list_init(&glb->avmpack_data);
    list_init(&glb->refc_binaries);
    list_init(&glb->processes_table);
    glb->registered_processes = NULL;

    glb->last_process_id = 0;

    glb->atoms_table = atomshashtable_new();
    if (IS_NULL_PTR(glb->atoms_table)) {
        free(glb);
        return NULL;
    }
    glb->atoms_ids_table = valueshashtable_new();
    if (IS_NULL_PTR(glb->atoms_ids_table)) {
        free(glb->atoms_table);
        free(glb);
        return NULL;
    }

    defaultatoms_init(glb);

    glb->modules_by_index = NULL;
    glb->loaded_modules_count = 0;
    glb->modules_table = atomshashtable_new();
    if (IS_NULL_PTR(glb->modules_table)) {
        free(glb->atoms_ids_table);
        free(glb->atoms_table);
        free(glb);
        return NULL;
    }

    glb->timer_wheel = timer_wheel_new(16);
    if (IS_NULL_PTR(glb->timer_wheel)) {
        free(glb->modules_table);
        free(glb->atoms_ids_table);
        free(glb->atoms_table);
        free(glb);
        return NULL;
    }
    glb->last_seen_millis = 0;

    glb->ref_ticks = 0;

    sys_init_platform(glb);

    return glb;
}

COLD_FUNC void globalcontext_destroy(GlobalContext *glb)
{
    sys_stop_millis_timer();
    free(glb);
}

Context *globalcontext_get_process(GlobalContext *glb, int32_t process_id)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &glb->processes_table) {
        Context *p = GET_LIST_ENTRY(item, Context, processes_table_head);

        if (p->process_id == process_id) {
            return p;
        }
    }

    return NULL;
}

int32_t globalcontext_get_new_process_id(GlobalContext *glb)
{
    glb->last_process_id++;

    return glb->last_process_id;
}

void globalcontext_register_process(GlobalContext *glb, int atom_index, int local_process_id)
{
    struct RegisteredProcess *registered_process = malloc(sizeof(struct RegisteredProcess));
    if (IS_NULL_PTR(registered_process)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    registered_process->atom_index = atom_index;
    registered_process->local_process_id = local_process_id;

    linkedlist_append(&glb->registered_processes, &registered_process->registered_processes_list_head);
}

int globalcontext_get_registered_process(GlobalContext *glb, int atom_index)
{
    if (!glb->registered_processes) {
        return 0;
    }

    const struct RegisteredProcess *registered_processes = GET_LIST_ENTRY(glb->registered_processes, struct RegisteredProcess, registered_processes_list_head);

    const struct RegisteredProcess *p = registered_processes;
    do {
        if (p->atom_index == atom_index) {
            return p->local_process_id;
        }

        p = GET_LIST_ENTRY(p->registered_processes_list_head.next, struct RegisteredProcess, registered_processes_list_head);
    } while (p != registered_processes);

    return 0;
}

int globalcontext_insert_atom(GlobalContext *glb, AtomString atom_string)
{
    return globalcontext_insert_atom_maybe_copy(glb, atom_string, 0);
}

int globalcontext_insert_atom_maybe_copy(GlobalContext *glb, AtomString atom_string, int copy)
{
    struct AtomsHashTable *htable = glb->atoms_table;

    unsigned long atom_index = atomshashtable_get_value(htable, atom_string, ULONG_MAX);
    if (atom_index == ULONG_MAX) {
        if (copy) {
            uint8_t len = *((uint8_t *) atom_string);
            uint8_t *buf = malloc(1 + len);
            if (UNLIKELY(IS_NULL_PTR(buf))) {
                fprintf(stderr, "Unable to allocate memory for atom string\n");
                abort();
            }
            memcpy(buf, atom_string, 1 + len);
            atom_string = buf;
        }
        atom_index = htable->count;
        if (!atomshashtable_insert(htable, atom_string, atom_index)) {
            return -1;
        }
        if (!valueshashtable_insert(glb->atoms_ids_table, atom_index, (unsigned long) atom_string)) {
            return -1;
        }
    }

    return (int) atom_index;
}

bool globalcontext_is_atom_index_equal_to_atom_string(GlobalContext *glb, int atom_index_a, AtomString atom_string_b)
{
    AtomString atom_string_a = (AtomString) valueshashtable_get_value(glb->atoms_ids_table, atom_index_a, NULL);
    return atom_are_equals(atom_string_a, atom_string_b);
}

AtomString globalcontext_atomstring_from_term(GlobalContext *glb, term t)
{
    if (!term_is_atom(t)) {
        abort();
    }
    unsigned long atom_index = term_to_atom_index(t);
    unsigned long ret = valueshashtable_get_value(glb->atoms_ids_table, atom_index, ULONG_MAX);
    if (ret == ULONG_MAX) {
        return NULL;
    }
    return (AtomString) ret;
}

int globalcontext_insert_module(GlobalContext *global, Module *module, AtomString module_name_atom)
{
    if (!atomshashtable_insert(global->modules_table, module_name_atom, TO_ATOMSHASHTABLE_VALUE(module))) {
        return -1;
    }

    int module_index = global->loaded_modules_count;

    Module **new_modules_by_index = calloc(module_index + 1, sizeof(Module *));
    if (IS_NULL_PTR(new_modules_by_index)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    if (global->modules_by_index) {
        for (int i = 0; i < module_index; i++) {
            new_modules_by_index[i] = global->modules_by_index[i];
        }
        free(global->modules_by_index);
    }

    module->module_index = module_index;

    global->modules_by_index = new_modules_by_index;
    global->modules_by_index[module_index] = module;
    global->loaded_modules_count++;

    return module_index;
}

void globalcontext_insert_module_with_filename(GlobalContext *glb, Module *module, const char *filename)
{
    int len = strnlen(filename, 260);
    int len_without_ext = len - strlen(".beam");

    if (strcmp(filename + len_without_ext, ".beam") != 0) {
        printf("File isn't a .beam file\n");
        abort();
    }

    char *atom_string = malloc(len_without_ext + 1);
    if (IS_NULL_PTR(atom_string)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    memcpy(atom_string + 1, filename, len_without_ext);
    atom_string[0] = len_without_ext;

    if (UNLIKELY(globalcontext_insert_module(glb, module, atom_string) < 0)) {
        abort();
    }
}

Module *globalcontext_get_module(GlobalContext *global, AtomString module_name_atom)
{
    Module *found_module = (Module *) atomshashtable_get_value(global->modules_table, module_name_atom, (unsigned long) NULL);

    if (!found_module) {
        char *module_name = malloc(256 + 5);
        if (IS_NULL_PTR(module_name)) {
            return NULL;
        }

        atom_string_to_c(module_name_atom, module_name, 256);
        strcat(module_name, ".beam");
        Module *loaded_module = sys_load_module(global, module_name);
        free(module_name);

        if (UNLIKELY(!loaded_module || (globalcontext_insert_module(global, loaded_module, module_name_atom) < 0))) {
            return NULL;
        }

        return loaded_module;
    }

    return found_module;
}

void globalcontext_demonitor(GlobalContext *global, uint64_t ref_ticks)
{
    struct ListHead *pitem;
    LIST_FOR_EACH (pitem, &global->processes_table) {
        Context *p = GET_LIST_ENTRY(pitem, Context, processes_table_head);

        struct ListHead *item;
        LIST_FOR_EACH (item, &p->monitors_head) {
            struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
            if (monitor->ref_ticks == ref_ticks) {
                list_remove(&monitor->monitor_list_head);
                free(monitor);
                return;
            }
        }
    }
}
