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

#include "atom_table.h"
#include "atomshashtable.h"
#include "avmpack.h"
#include "context.h"
#include "defaultatoms.h"
#include "erl_nif_priv.h"
#include "list.h"
#include "posix_nifs.h"
#include "refc_binary.h"
#include "resources.h"
#include "synclist.h"
#include "sys.h"
#include "utils.h"
#include "valueshashtable.h"

#ifndef AVM_NO_SMP
#define SMP_SPINLOCK_LOCK(spinlock) smp_spinlock_lock(spinlock)
#define SMP_SPINLOCK_UNLOCK(spinlock) smp_spinlock_unlock(spinlock)
#define SMP_MUTEX_LOCK(mutex) smp_mutex_lock(mutex)
#define SMP_MUTEX_UNLOCK(mutex) smp_mutex_unlock(mutex)
#define SMP_RWLOCK_RDLOCK(lock) smp_rwlock_rdlock(lock)
#define SMP_RWLOCK_WRLOCK(lock) smp_rwlock_wrlock(lock)
#define SMP_RWLOCK_UNLOCK(lock) smp_rwlock_unlock(lock)
#else
#define SMP_SPINLOCK_LOCK(spinlock)
#define SMP_SPINLOCK_UNLOCK(spinlock)
#define SMP_MUTEX_LOCK(mutex)
#define SMP_MUTEX_UNLOCK(mutex)
#define SMP_RWLOCK_RDLOCK(lock)
#define SMP_RWLOCK_WRLOCK(lock)
#define SMP_RWLOCK_UNLOCK(lock)
#endif

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
    list_init(&glb->running_processes);
    list_init(&glb->waiting_processes);
#ifndef AVM_NO_SMP
    smp_spinlock_init(&glb->processes_spinlock);
#endif
    synclist_init(&glb->avmpack_data);
    synclist_init(&glb->refc_binaries);
    synclist_init(&glb->processes_table);
    synclist_init(&glb->registered_processes);
    synclist_init(&glb->listeners);
    synclist_init(&glb->resource_types);
    synclist_init(&glb->select_events);

    glb->last_process_id = 0;

    glb->atom_table = atom_table_new();
    if (IS_NULL_PTR(glb->atom_table)) {
        free(glb);
        return NULL;
    }

    defaultatoms_init(glb);

    glb->modules_by_index = NULL;
    glb->loaded_modules_count = 0;
    glb->modules_table = atomshashtable_new();
    if (IS_NULL_PTR(glb->modules_table)) {
        atom_table_destroy(glb->atom_table);
        free(glb);
        return NULL;
    }
#ifndef AVM_NO_SMP
    glb->modules_lock = smp_rwlock_create();
    if (IS_NULL_PTR(glb->modules_lock)) {
        free(glb->modules_table);
        atom_table_destroy(glb->atom_table);
        free(glb);
        return NULL;
    }
#endif

    timer_list_init(&glb->timer_list);
#ifndef AVM_NO_SMP
    smp_spinlock_init(&glb->timer_spinlock);
#endif

    glb->ref_ticks = 0;
#if !defined(AVM_NO_SMP) && ATOMIC_LLONG_LOCK_FREE != 2
    smp_spinlock_init(&glb->ref_ticks_spinlock);
#endif

#if HAVE_OPEN && HAVE_CLOSE
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);
    glb->posix_fd_resource_type = enif_init_resource_type(&env, "posix_fd", &posix_fd_resource_type_init, ERL_NIF_RT_CREATE, NULL);
    if (IS_NULL_PTR(glb->posix_fd_resource_type)) {
#ifndef AVM_NO_SMP
        smp_rwlock_destroy(glb->modules_lock);
#endif
        free(glb->modules_table);
        atom_table_destroy(glb->atom_table);
        free(glb);
        return NULL;
    }
#endif

    sys_init_platform(glb);

#ifndef AVM_NO_SMP
    glb->schedulers_mutex = smp_mutex_create();
    if (IS_NULL_PTR(glb->schedulers_mutex)) {
#if HAVE_OPEN && HAVE_CLOSE
        resource_type_destroy(glb->posix_fd_resource_type);
#endif
        smp_rwlock_destroy(glb->modules_lock);
        free(glb->modules_table);
        atom_table_destroy(glb->atom_table);
        free(glb);
        return NULL;
    }
    glb->schedulers_cv = smp_condvar_create();
    if (IS_NULL_PTR(glb->schedulers_cv)) {
        smp_mutex_destroy(glb->schedulers_mutex);
#if HAVE_OPEN && HAVE_CLOSE
        resource_type_destroy(glb->posix_fd_resource_type);
#endif
        smp_rwlock_destroy(glb->modules_lock);
        free(glb->modules_table);
        atom_table_destroy(glb->atom_table);
        free(glb);
        return NULL;
    }
    glb->online_schedulers = smp_get_online_processors();
    glb->running_schedulers = 0;
    glb->waiting_scheduler = false;

    smp_spinlock_init(&glb->env_spinlock);
#endif
    glb->scheduler_stop_all = false;

    return glb;
}

COLD_FUNC void globalcontext_destroy(GlobalContext *glb)
{
    sys_free_platform(glb);

    struct ListHead *item;
    struct ListHead *tmp;

    struct ListHead *open_avm_packs = synclist_nolock(&glb->avmpack_data);
    MUTABLE_LIST_FOR_EACH (item, tmp, open_avm_packs) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        avmpack_data_destroy(avmpack_data, glb);
    }
    synclist_destroy(&glb->avmpack_data);

    struct ListHead *listeners = synclist_nolock(&glb->listeners);
    MUTABLE_LIST_FOR_EACH (item, tmp, listeners) {
        sys_listener_destroy(item);
    }
    synclist_destroy(&glb->listeners);

    // Destroy select events before resources
    struct ListHead *select_events = synclist_nolock(&glb->select_events);
    MUTABLE_LIST_FOR_EACH (item, tmp, select_events) {
        struct SelectEvent *select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
        free((void *) select_event);
    }
    synclist_destroy(&glb->select_events);

    // Destroy refc binaries including resources
    // (this list should be empty if resources were properly refcounted)
    struct ListHead *refc_binaries = synclist_nolock(&glb->refc_binaries);
    MUTABLE_LIST_FOR_EACH (item, tmp, refc_binaries) {
        struct RefcBinary *refc = GET_LIST_ENTRY(item, struct RefcBinary, head);
#ifndef NDEBUG
        if (refc->resource_type) {
            fprintf(stderr, "Warning, dangling resource of type %s, ref_count = %d\n", refc->resource_type->name, (int) refc->ref_count);
        } else {
            fprintf(stderr, "Warning, dangling refc binary, ref_count = %d\n", (int) refc->ref_count);
        }
#endif
        refc_binary_destroy(refc, glb);
    }
    synclist_destroy(&glb->refc_binaries);

    // Destroy resource types
    struct ListHead *resource_types = synclist_nolock(&glb->resource_types);
    MUTABLE_LIST_FOR_EACH (item, tmp, resource_types) {
        struct ResourceType *resource_type = GET_LIST_ENTRY(item, struct ResourceType, head);
        resource_type_destroy(resource_type);
    }
    synclist_destroy(&glb->resource_types);

#ifndef AVM_NO_SMP
    smp_condvar_destroy(glb->schedulers_cv);
    smp_mutex_destroy(glb->schedulers_mutex);
    smp_rwlock_destroy(glb->modules_lock);
#endif
    synclist_destroy(&glb->registered_processes);
    synclist_destroy(&glb->processes_table);

    free(glb);
}

Context *globalcontext_get_process_nolock(GlobalContext *glb, int32_t process_id)
{
    struct ListHead *item;
    Context *p = NULL;

    struct ListHead *processes_table_list = synclist_nolock(&glb->processes_table);
    LIST_FOR_EACH (item, processes_table_list) {
        p = GET_LIST_ENTRY(item, Context, processes_table_head);

        if (p->process_id == process_id) {
            return p;
        }
    }

    return NULL;
}

Context *globalcontext_get_process_lock(GlobalContext *glb, int32_t process_id)
{
    struct ListHead *item;
    Context *p = NULL;

    struct ListHead *processes_table_list = synclist_rdlock(&glb->processes_table);
    LIST_FOR_EACH (item, processes_table_list) {
        p = GET_LIST_ENTRY(item, Context, processes_table_head);

        if (p->process_id == process_id) {
            return p;
        }
    }
    synclist_unlock(&glb->processes_table);

    return NULL;
}

void globalcontext_get_process_unlock(GlobalContext *glb, Context *c)
{
    if (c) {
        synclist_unlock(&glb->processes_table);
    }
}

bool globalcontext_process_exists(GlobalContext *glb, int32_t process_id)
{
    Context *p = globalcontext_get_process_lock(glb, process_id);
    globalcontext_get_process_unlock(glb, p);
    return p != NULL;
}

void globalcontext_send_message(GlobalContext *glb, int32_t process_id, term t)
{
    Context *p = globalcontext_get_process_lock(glb, process_id);
    if (p) {
        mailbox_send(p, t);
        globalcontext_get_process_unlock(glb, p);
    }
}

void globalcontext_send_message_nolock(GlobalContext *glb, int32_t process_id, term t)
{
    Context *p = globalcontext_get_process_nolock(glb, process_id);
    if (p) {
        mailbox_send(p, t);
    }
}

void globalcontext_init_process(GlobalContext *glb, Context *ctx)
{
    ctx->global = glb;

    synclist_append(&glb->processes_table, &ctx->processes_table_head);
    SMP_SPINLOCK_LOCK(&glb->processes_spinlock);
    ctx->process_id = ++glb->last_process_id;
    list_append(&glb->waiting_processes, &ctx->processes_list_head);
    SMP_SPINLOCK_UNLOCK(&glb->processes_spinlock);
}

bool globalcontext_register_process(GlobalContext *glb, int atom_index, int local_process_id)
{
    struct ListHead *registered_processes_list = synclist_wrlock(&glb->registered_processes);
    struct ListHead *item;
    LIST_FOR_EACH (item, registered_processes_list) {
        const struct RegisteredProcess *registered_process = GET_LIST_ENTRY(item, struct RegisteredProcess, registered_processes_list_head);
        if (registered_process->atom_index == atom_index) {
            synclist_unlock(&glb->registered_processes);
            return false;
        }
    }

    struct RegisteredProcess *registered_process = malloc(sizeof(struct RegisteredProcess));
    if (IS_NULL_PTR(registered_process)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    registered_process->atom_index = atom_index;
    registered_process->local_process_id = local_process_id;

    list_append(registered_processes_list, &registered_process->registered_processes_list_head);
    synclist_unlock(&glb->registered_processes);

    return true;
}

bool globalcontext_unregister_process(GlobalContext *glb, int atom_index)
{
    struct ListHead *registered_processes_list = synclist_wrlock(&glb->registered_processes);
    struct ListHead *item;
    LIST_FOR_EACH (item, registered_processes_list) {
        struct RegisteredProcess *registered_process = GET_LIST_ENTRY(item, struct RegisteredProcess, registered_processes_list_head);
        if (registered_process->atom_index == atom_index) {
            list_remove(item);
            free(registered_process);
            synclist_unlock(&glb->registered_processes);
            return true;
        }
    }

    synclist_unlock(&glb->registered_processes);

    return false;
}

void globalcontext_maybe_unregister_process_id(GlobalContext *glb, int target_process_id)
{
    struct ListHead *registered_processes_list = synclist_wrlock(&glb->registered_processes);
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, registered_processes_list) {
        struct RegisteredProcess *registered_process = GET_LIST_ENTRY(item, struct RegisteredProcess, registered_processes_list_head);
        if (registered_process->local_process_id == target_process_id) {
            list_remove(item);
            free(registered_process);
        }
    }
    synclist_unlock(&glb->registered_processes);
}

int globalcontext_get_registered_process(GlobalContext *glb, int atom_index)
{
    struct ListHead *registered_processes_list = synclist_rdlock(&glb->registered_processes);
    struct ListHead *item;
    LIST_FOR_EACH (item, registered_processes_list) {
        const struct RegisteredProcess *registered_process = GET_LIST_ENTRY(item, struct RegisteredProcess, registered_processes_list_head);
        if (registered_process->atom_index == atom_index) {
            int result = registered_process->local_process_id;
            synclist_unlock(&glb->registered_processes);
            return result;
        }
    }

    synclist_unlock(&glb->registered_processes);

    return 0;
}

bool globalcontext_is_atom_index_equal_to_atom_string(GlobalContext *glb, int atom_index_a, AtomString atom_string_b)
{
    AtomString atom_string_a;
    atom_string_a = atom_table_get_atom_string(glb->atom_table, atom_index_a);
    return atom_are_equals(atom_string_a, atom_string_b);
}

AtomString globalcontext_atomstring_from_term(GlobalContext *glb, term t)
{
    if (!term_is_atom(t)) {
        AVM_ABORT();
    }
    unsigned long atom_index = term_to_atom_index(t);
    AtomString str = atom_table_get_atom_string(glb->atom_table, atom_index);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }
    return str;
}

term globalcontext_existing_term_from_atom_string(GlobalContext *glb, AtomString atom_string)
{
    long atom_index = atom_table_get_index(glb->atom_table, atom_string);
    if (UNLIKELY(atom_index == ATOM_TABLE_NOT_FOUND)) {
        return term_invalid_term();
    }
    return term_from_atom_index(atom_index);
}

int globalcontext_insert_module(GlobalContext *global, Module *module)
{
    SMP_RWLOCK_WRLOCK(global->modules_lock);
    AtomString module_name_atom = module_get_atom_string_by_id(module, 1, global);
    if (!atomshashtable_insert(global->modules_table, module_name_atom, TO_ATOMSHASHTABLE_VALUE(module))) {
        SMP_RWLOCK_UNLOCK(global->modules_lock);
        return -1;
    }

    int module_index = global->loaded_modules_count;

    Module **new_modules_by_index = calloc(module_index + 1, sizeof(Module *));
    if (IS_NULL_PTR(new_modules_by_index)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        SMP_RWLOCK_UNLOCK(global->modules_lock);
        AVM_ABORT();
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
    SMP_RWLOCK_UNLOCK(global->modules_lock);

    return module_index;
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

        if (UNLIKELY(!loaded_module || (globalcontext_insert_module(global, loaded_module) < 0))) {
            return NULL;
        }

        return loaded_module;
    }

    return found_module;
}

Module *globalcontext_get_module_by_index(GlobalContext *global, int index)
{
    SMP_RWLOCK_RDLOCK(global->modules_lock);
    Module *result = global->modules_by_index[index];
    SMP_RWLOCK_UNLOCK(global->modules_lock);
    return result;
}

bool globalcontext_demonitor(GlobalContext *global, uint64_t ref_ticks)
{
    struct ListHead *pitem;

    struct ListHead *processes_table_list = synclist_wrlock(&global->processes_table);
    LIST_FOR_EACH (pitem, processes_table_list) {
        Context *p = GET_LIST_ENTRY(pitem, Context, processes_table_head);

        struct ListHead *item;
        LIST_FOR_EACH (item, &p->monitors_head) {
            struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
            if (monitor->ref_ticks == ref_ticks) {
                list_remove(&monitor->monitor_list_head);
                free(monitor);
                synclist_unlock(&global->processes_table);
                return true;
            }
        }
    }

    synclist_unlock(&global->processes_table);
    return false;
}
