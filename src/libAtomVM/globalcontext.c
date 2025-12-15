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

#include "dist_nifs.h"
#include "globalcontext.h"

#include "atom_table.h"
#include "avmpack.h"
#include "context.h"
#include "defaultatoms.h"
#include "erl_nif_priv.h"
#include "interop.h"
#include "list.h"
#include "mailbox.h"
#include "posix_nifs.h"
#include "refc_binary.h"
#include "resources.h"
#include "scheduler.h"
#include "smp.h"
#include "synclist.h"
#include "sys.h"
#include "term.h"
#include "utils.h"
#include "valueshashtable.h"

#ifdef HAVE_PLATFORM_ATOMIC_H
#include "platform_atomic.h"
#else
#if defined(HAVE_ATOMIC)
#include <stdatomic.h>
#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR atomic_compare_exchange_weak
#endif
#endif

struct RegisteredProcess
{
    struct ListHead registered_processes_list_head;

    int atom_index;
    term local_pid_or_port;
};

GlobalContext *globalcontext_new(void)
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
#ifdef AVM_TASK_DRIVER_ENABLED
    glb->message_queue = NULL;
    glb->refc_queue = NULL;
#endif
    synclist_init(&glb->avmpack_data);
    synclist_init(&glb->refc_binaries);
    synclist_init(&glb->processes_table);
    synclist_init(&glb->registered_processes);
    synclist_init(&glb->listeners);
    synclist_init(&glb->resource_types);
    synclist_init(&glb->select_events);

    ets_init(&glb->ets);

    glb->last_process_id = 0;

    glb->atom_table = atom_table_new();
    if (IS_NULL_PTR(glb->atom_table)) {
        free(glb);
        return NULL;
    }

    defaultatoms_init(glb);

    glb->modules_by_index = NULL;
    glb->loaded_modules_count = 0;
    glb->modules_table = valueshashtable_new();
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

    glb->node_name = NONODE_AT_NOHOST_ATOM;
    glb->creation = 0;
    synclist_init(&glb->dist_connections);

    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);
    glb->resource_binary_resource_type = enif_init_resource_type(&env, "resource_binary", &resource_binary_resource_type_init, ERL_NIF_RT_CREATE, NULL);
    glb->dist_connection_resource_type = enif_init_resource_type(&env, "dist_connection", &dist_connection_resource_type_init, ERL_NIF_RT_CREATE, NULL);

#if HAVE_OPEN && HAVE_CLOSE
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

#if HAVE_OPENDIR && HAVE_READDIR && HAVE_CLOSEDIR
    ErlNifEnv dir_env;
    erl_nif_env_partial_init_from_globalcontext(&dir_env, glb);
    glb->posix_dir_resource_type = enif_init_resource_type(&env, "posix_dir", &posix_dir_resource_type_init, ERL_NIF_RT_CREATE, NULL);
    if (IS_NULL_PTR(glb->posix_dir_resource_type)) {
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

    int module_index = glb->loaded_modules_count;
    for (int i = 0; i < module_index; i++) {
        module_destroy(glb->modules_by_index[i]);
    }

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

    ets_destroy(&glb->ets, glb);

    // Destroy refc binaries including resources
    // (this list should be empty if resources were properly refcounted)
    struct ListHead *refc_binaries = synclist_nolock(&glb->refc_binaries);
    MUTABLE_LIST_FOR_EACH (item, tmp, refc_binaries) {
        struct RefcBinary *refc = GET_LIST_ENTRY(item, struct RefcBinary, head);
#ifndef NDEBUG
        if (refc->resource_type) {
            fprintf(stderr, "Warning, dangling resource of type %s, ref_count = %d, data = %p\n", refc->resource_type->name, (int) refc->ref_count, (void *) refc->data);
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

#if !defined(AVM_NO_SMP) && defined(AVM_TASK_DRIVER_ENABLED)
static bool globalcontext_get_process_trylock(GlobalContext *glb, int32_t process_id, Context **output)
{
    struct ListHead *item;
    Context *p = NULL;

    struct ListHead *processes_table_list = synclist_tryrdlock(&glb->processes_table);
    if (processes_table_list == NULL) {
        return false;
    }
    LIST_FOR_EACH (item, processes_table_list) {
        p = GET_LIST_ENTRY(item, Context, processes_table_head);

        if (p->process_id == process_id) {
            *output = p;
            break;
        }
    }

    return true;
}
#endif

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

enum SendMessageResult globalcontext_post_message(GlobalContext *glb, int32_t process_id, Message *m)
{
    Context *p = globalcontext_get_process_lock(glb, process_id);
    enum SendMessageResult result = SEND_MESSAGE_PROCESS_NOT_FOUND;
    if (p) {
        mailbox_post_message(p, &m->base);
        globalcontext_get_process_unlock(glb, p);
        result = SEND_MESSAGE_OK;
    }
    return result;
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

#ifdef AVM_TASK_DRIVER_ENABLED
static inline enum SendMessageResult globalcontext_send_message_from_task_common(GlobalContext *glb, int32_t process_id, MailboxMessage *message, enum MessageType type, term t)
{
    enum SendMessageResult result = SEND_MESSAGE_PROCESS_NOT_FOUND;
    bool postponed = false;
#ifndef AVM_NO_SMP
    Context *p = NULL;
    if (globalcontext_get_process_trylock(glb, process_id, &p)) {
        if (p) {
            if (message == NULL) {
                message = mailbox_message_create_from_term(type, t);
            }
            // Ensure we can acquire the spinlock
            if (smp_spinlock_trylock(&glb->processes_spinlock)) {
                // We can send the message.
                mailbox_enqueue_message(p, message);
                scheduler_signal_message_from_task(p);
                smp_spinlock_unlock(&glb->processes_spinlock);
            } else {
                postponed = true;
            }
            globalcontext_get_process_unlock(glb, p);
            result = SEND_MESSAGE_OK;
        }
    } else {
        postponed = true;
    }
#else
    // Without SMP, we have no lock, so we must always enqueue.
    postponed = true;
#endif
    if (postponed) {
        if (message == NULL) {
            message = mailbox_message_create_from_term(type, t);
        }
        struct MessageQueueItem *queued_item = malloc(sizeof(struct MessageQueueItem));
        if (IS_NULL_PTR(queued_item)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            AVM_ABORT();
        }
        queued_item->message = message;
        queued_item->process_id = process_id;

        struct MessageQueueItem *current_first = NULL;
        do {
            queued_item->next = current_first;
        } while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&glb->message_queue, &current_first, queued_item));
        // Make sure the scheduler is busy
        sys_signal(glb);

        result = SEND_MESSAGE_OK;
    }
    return result;
}

enum SendMessageResult globalcontext_post_message_from_task(GlobalContext *glb, int32_t process_id, Message *message)
{
    return globalcontext_send_message_from_task_common(glb, process_id, &message->base, NormalMessage, term_nil());
}

void globalcontext_send_message_from_task(GlobalContext *glb, int32_t process_id, enum MessageType type, term t)
{
    globalcontext_send_message_from_task_common(glb, process_id, NULL, type, t);
}

static inline void globalcontext_process_message_queue(GlobalContext *glb)
{
    struct MessageQueueItem *current = glb->message_queue;
    // Empty outer list using CAS
    if (current) {
        while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&glb->message_queue, &current, NULL)) {
        };
        (void) synclist_rdlock(&glb->processes_table);
        while (current) {
            Context *context = globalcontext_get_process_nolock(glb, current->process_id);
            if (context) {
                mailbox_enqueue_message(context, current->message);
                scheduler_signal_message(context);
            }
            struct MessageQueueItem *old = current;
            current = old->next;
            free(old);
        }
        synclist_unlock(&glb->processes_table);
    }
}

static inline void globalcontext_process_refc_queue(GlobalContext *glb)
{
    struct RefcBinaryQueueItem *current = glb->refc_queue;
    // Empty outer list using CAS
    if (current) {
        while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&glb->refc_queue, &current, NULL)) {
        };
        while (current) {
            refc_binary_decrement_refcount(current->refc, glb);
            struct RefcBinaryQueueItem *old = current;
            current = old->next;
            free(old);
        }
    }
}

void globalcontext_refc_decrement_refcount_from_task(GlobalContext *glb, struct RefcBinary *refc)
{
    struct RefcBinaryQueueItem *queued_item = malloc(sizeof(struct RefcBinaryQueueItem));
    if (IS_NULL_PTR(queued_item)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    queued_item->refc = refc;

    struct RefcBinaryQueueItem *current_first = NULL;
    do {
        queued_item->next = current_first;
    } while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&glb->refc_queue, &current_first, queued_item));

    // We are deliberately not waking up scheduler here. If scheduler is
    // sleeping, there is no need to refc-decrement the binary to free memory,
    // as there is no guarantee we need to hold on when resource destructors
    // are called.
}

void globalcontext_process_task_driver_queues(GlobalContext *glb)
{
    globalcontext_process_message_queue(glb);
    globalcontext_process_refc_queue(glb);
}
#endif

void globalcontext_init_process(GlobalContext *glb, Context *ctx)
{
    ctx->global = glb;

    synclist_append(&glb->processes_table, &ctx->processes_table_head);
    SMP_SPINLOCK_LOCK(&glb->processes_spinlock);
    ctx->process_id = ++glb->last_process_id;
    list_append(&glb->waiting_processes, &ctx->processes_list_head);
    SMP_SPINLOCK_UNLOCK(&glb->processes_spinlock);
}

bool globalcontext_register_process(GlobalContext *glb, int atom_index, term local_pid_or_port)
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
    registered_process->local_pid_or_port = local_pid_or_port;

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
        if (term_to_local_process_id(registered_process->local_pid_or_port) == target_process_id) {
            list_remove(item);
            free(registered_process);
        }
    }
    synclist_unlock(&glb->registered_processes);
}

term globalcontext_get_registered_process(GlobalContext *glb, int atom_index)
{
    struct ListHead *registered_processes_list = synclist_rdlock(&glb->registered_processes);
    struct ListHead *item;
    LIST_FOR_EACH (item, registered_processes_list) {
        const struct RegisteredProcess *registered_process = GET_LIST_ENTRY(item, struct RegisteredProcess, registered_processes_list_head);
        if (registered_process->atom_index == atom_index) {
            term result = registered_process->local_pid_or_port;
            synclist_unlock(&glb->registered_processes);
            return result;
        }
    }

    synclist_unlock(&glb->registered_processes);

    return UNDEFINED_ATOM;
}

term globalcontext_get_registered_name_process(GlobalContext *glb, int local_process_id)
{
    struct ListHead *registered_processes_list = synclist_rdlock(&glb->registered_processes);
    struct ListHead *item;
    LIST_FOR_EACH (item, registered_processes_list) {
        struct RegisteredProcess *registered_process = GET_LIST_ENTRY(item, struct RegisteredProcess, registered_processes_list_head);
        if (term_to_local_process_id(registered_process->local_pid_or_port) == local_process_id) {
            int result = registered_process->atom_index;
            synclist_unlock(&glb->registered_processes);
            return term_from_atom_index(result);
        }
    }
    synclist_unlock(&glb->registered_processes);
    return term_invalid_term();
}

bool globalcontext_is_atom_index_equal_to_atom_string(GlobalContext *glb, atom_index_t atom_index_a, AtomString atom_string_b)
{
    return atom_table_is_equal_to_atom_string(glb->atom_table, atom_index_a, atom_string_b);
}

term globalcontext_existing_term_from_atom_string(GlobalContext *glb, AtomString atom_string)
{
    atom_index_t global_atom_index;
    enum AtomTableEnsureAtomResult ensure_result = atom_table_ensure_atom(
        glb->atom_table, atom_string_data(atom_string), atom_string_len(atom_string), AtomTableAlreadyExisting, &global_atom_index);
    if (UNLIKELY(ensure_result != AtomTableEnsureAtomOk)) {
        return term_invalid_term();
    }
    return term_from_atom_index(global_atom_index);
}

int globalcontext_insert_module(GlobalContext *global, Module *module)
{
    term module_name = module_get_name(module);
    SMP_RWLOCK_WRLOCK(global->modules_lock);
    if (!valueshashtable_insert(global->modules_table, term_to_atom_index(module_name), TO_VALUESHASHTABLE_VALUE(module))) {
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

Module *globalcontext_load_module_from_avm(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    struct ListHead *item;
    struct ListHead *avmpack_data = synclist_rdlock(&global->avmpack_data);
    LIST_FOR_EACH (item, avmpack_data) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        avmpack_data->in_use = true;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    if (IS_NULL_PTR(beam_module)) {
        return NULL;
    }

    return module_new_from_iff_binary(global, beam_module, beam_module_size);
}

Module *globalcontext_get_module(GlobalContext *global, atom_index_t module_name_atom)
{
    Module *found_module = (Module *) valueshashtable_get_value(global->modules_table, module_name_atom, TO_VALUESHASHTABLE_VALUE(NULL));

    if (!found_module) {
        size_t module_name_len;
        const uint8_t *module_name = atom_table_get_atom_string(global->atom_table, module_name_atom, &module_name_len);

        char *module_file_name = malloc(module_name_len + 6);
        if (IS_NULL_PTR(module_file_name)) {
            return NULL;
        }
        memcpy(module_file_name, module_name, module_name_len);
        memcpy(module_file_name + module_name_len, ".beam", 6);
        Module *loaded_module = globalcontext_load_module_from_avm(global, module_file_name);
        if (IS_NULL_PTR(loaded_module)) {
            // Platform may implement sys_load_module_from_file
            loaded_module = sys_load_module_from_file(global, module_file_name);
        }
        if (UNLIKELY(!loaded_module || (globalcontext_insert_module(global, loaded_module) < 0))) {
            fprintf(stderr, "Failed load module: %s\n", module_file_name);
            free(module_file_name);
            if (loaded_module) {
                module_destroy(loaded_module);
            }
            return NULL;
        }

        free(module_file_name);

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

run_result_t globalcontext_run(GlobalContext *glb, Module *startup_module, FILE *out_f, int argc, char **argv)
{
    Context *ctx = context_new(glb);
    ctx->leader = 1;
    Module *init_module = globalcontext_get_module(glb, INIT_ATOM_INDEX);
    if (IS_NULL_PTR(init_module)) {
        if (IS_NULL_PTR(startup_module)) {
            fprintf(stderr, "Unable to locate entrypoint.\n");
            return RUN_NO_ENTRY_POINT;
        }
        context_execute_loop(ctx, startup_module, "start", 0);
    } else {
        // Build boot arguments based on whether we're in embedded mode
        if (argc > 0 && argv != NULL) {
            // Embedded mode: ["-s", escript, "--" | argv_strings]
            // Calculate heap size needed
            size_t heap_needed = term_binary_heap_size(2) + // "-s"
                term_binary_heap_size(2) + // "--"
                LIST_SIZE(3, 0); // list for ["-s", escript, "--"]

            // Calculate space for argv strings
            for (int i = 0; i < argc; i++) {
                size_t arg_len = strlen(argv[i]);
                heap_needed += CONS_SIZE * arg_len + LIST_SIZE(1, 0);
            }

            if (UNLIKELY(memory_ensure_free(ctx, heap_needed) != MEMORY_GC_OK)) {
                fprintf(stderr, "Unable to allocate arguments.\n");
                return RUN_MEMORY_FAILURE;
            }

            // Build the argv list in reverse: [argvn, ..., argv0, "--", escript, "-s"]
            term args_list = term_nil();
            for (int i = argc - 1; i >= 0; i--) {
                term arg = interop_chars_to_list(argv[i], strlen(argv[i]), &ctx->heap);
                args_list = term_list_prepend(arg, args_list, &ctx->heap);
            }

            term separator = term_from_literal_binary("--", strlen("--"), &ctx->heap, glb);
            args_list = term_list_prepend(separator, args_list, &ctx->heap);

            term escript_atom = globalcontext_make_atom(glb, ATOM_STR("\x7", "escript"));
            args_list = term_list_prepend(escript_atom, args_list, &ctx->heap);

            term s_opt = term_from_literal_binary("-s", strlen("-s"), &ctx->heap, glb);
            ctx->x[0] = term_list_prepend(s_opt, args_list, &ctx->heap);
        } else {
            // Non-embedded mode: ["-s", startup_module]
            if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(2) + LIST_SIZE(2, 0)) != MEMORY_GC_OK)) {
                fprintf(stderr, "Unable to allocate arguments.\n");
                return RUN_MEMORY_FAILURE;
            }
            term s_opt = term_from_literal_binary("-s", strlen("-s"), &ctx->heap, glb);
            term list = term_list_prepend(module_get_name(startup_module), term_nil(), &ctx->heap);
            ctx->x[0] = term_list_prepend(s_opt, list, &ctx->heap);
        }

        context_execute_loop(ctx, init_module, "boot", 1);
    }

    term ret_value = ctx->x[0];
    if (out_f) {
        fprintf(out_f, "Return value: ");
        term_display(out_f, ret_value, ctx);
        fprintf(out_f, "\n");
    }

    run_result_t result;
    // ok or 0. 0 is required for running tests for emscripten
    if (ret_value == OK_ATOM || ret_value == term_from_int(0)) {
        result = RUN_SUCCESS;
    } else {
        result = RUN_RESULT_NOT_OK;
    }

    context_destroy(ctx);

    return result;
}
