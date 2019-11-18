/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include <limits.h>
#include <string.h>

#include "globalcontext.h"

#include "atomshashtable.h"
#include "defaultatoms.h"
#include "list.h"
#include "utils.h"
#include "valueshashtable.h"
#include "sys.h"
#include "context.h"

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
    glb->listeners = NULL;
    glb->processes_table = NULL;
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

    return glb;
}

COLD_FUNC void globalcontext_destroy(GlobalContext *glb)
{
    free(glb);
}

Context *globalcontext_get_process(GlobalContext *glb, int32_t process_id)
{
    Context *processes = GET_LIST_ENTRY(glb->processes_table, Context, processes_table_head);

    Context *p = processes;
    do {
        if (p->process_id == process_id) {
            return p;
        }

        p = GET_LIST_ENTRY(p->processes_table_head.next, Context, processes_table_head);
    } while (processes != p);

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
    struct AtomsHashTable *htable = glb->atoms_table;

    unsigned long atom_index = atomshashtable_get_value(htable, atom_string, ULONG_MAX);
    if (atom_index == ULONG_MAX) {
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
