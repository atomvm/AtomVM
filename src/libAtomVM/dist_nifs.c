/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

/**
 * @file dist_nifs.c
 * @brief Implementation of distribution NIFs and resources
 */

#include "dist_nifs.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "externalterm.h"
#include "globalcontext.h"
#include "list.h"
#include "memory.h"
#include "nifs.h"
#include "synclist.h"
#include "term.h"
#include "utils.h"

enum
{
    OPERATION_LINK = 1,
    //  OPERATION_SEND = 2,
    //  OPERATION_EXIT = 3,
    //  OPERATION_UNLINK = 4,
    OPERATION_NODE_LINK = 5,
    OPERATION_REG_SEND = 6,
    OPERATION_GROUP_LEADER = 7,
    //  OPERATION_EXIT2 = 8,
    //  OPERATION_SEND_TT = 12,
    //  OPERATION_EXIT_TT = 13,
    OPERATION_REG_SEND_TT = 16,
    //  OPERATION_EXIT2_TT = 18,
    OPERATION_MONITOR_P = 19,
    OPERATION_DEMONITOR_P = 20,
    //  OPERATION_MONITOR_P_EXIT = 21,
    OPERATION_SEND_SENDER = 22,
    OPERATION_SEND_SENDER_TT = 23,
    OPERATION_PAYLOAD_EXIT = 24,
    OPERATION_PAYLOAD_EXIT_TT = 25,
    OPERATION_PAYLOAD_EXIT2 = 26,
    OPERATION_PAYLOAD_EXIT2_TT = 27,
    OPERATION_PAYLOAD_MONITOR_P_EXIT = 28,
    OPERATION_SPAWN_REQUEST = 29,
    OPERATION_SPAWN_REQUEST_TT = 30,
    OPERATION_SPAWN_REPLY = 31,
    OPERATION_SPAWN_REPLY_TT = 32,
    OPERATION_UNLINK_ID = 35,
    OPERATION_UNLINK_ID_ACK = 36,
    OPERATION_ALIAS_SEND = 33,
    OPERATION_ALIAS_SEND_TT = 34,
};

struct DistributionPacket
{
    struct ListHead head;
    size_t size;
    uint8_t bytes[];
};

struct RemoteMonitor
{
    struct ListHead head;
    term target_proc; // atom or local pid
    uint8_t ref_len;
    uint32_t ref_words[5];
    uint32_t pid_number;
    uint32_t pid_serial;
    ErlNifMonitor process_monitor;
};

struct DistConnection
{
    struct ListHead head;
    int node_atom_index;
    uint32_t node_creation;
    int32_t selecting_process_id;
    int32_t connection_process_id;
    ErlNifMonitor connection_process_monitor;
    struct SyncList remote_monitors;
    struct SyncList pending_packets;
};

static void dist_connection_dtor(ErlNifEnv *caller_env, void *obj)
{
    struct DistConnection *conn_obj = (struct DistConnection *) obj;
    if (conn_obj->connection_process_id != INVALID_PROCESS_ID) {
        enif_demonitor_process(caller_env, conn_obj, &conn_obj->connection_process_monitor);
    }
    struct ListHead *remote_monitors = synclist_wrlock(&conn_obj->remote_monitors);
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, remote_monitors) {
        struct RemoteMonitor *remote_monitor = GET_LIST_ENTRY(item, struct RemoteMonitor, head);
        enif_demonitor_process(caller_env, conn_obj, &remote_monitor->process_monitor);
        list_remove(item);
        free(item);
    }
    synclist_unlock(&conn_obj->remote_monitors);
    synclist_destroy(&conn_obj->remote_monitors);
    struct ListHead *pending_packets = synclist_wrlock(&conn_obj->pending_packets);
    MUTABLE_LIST_FOR_EACH (item, tmp, pending_packets) {
        list_remove(item);
        free(item);
    }
    synclist_unlock(&conn_obj->pending_packets);
    synclist_destroy(&conn_obj->pending_packets);
    synclist_remove(&caller_env->global->dist_connections, &conn_obj->head);
}

static void dist_enqueue_message(term control_message, term payload, struct DistConnection *connection, GlobalContext *global)
{
    size_t control_message_size = 0; // some compilers including esp-idf 5.0.7 is not smart enough
    enum ExternalTermResult serialize_result = externalterm_compute_external_size(control_message, &control_message_size, global);
    if (LIKELY(serialize_result == EXTERNAL_TERM_OK)) {
        size_t payload_size = 0;
        if (!term_is_invalid_term(payload)) {
            serialize_result = externalterm_compute_external_size(payload, &payload_size, global);
        }
        if (LIKELY(serialize_result == EXTERNAL_TERM_OK)) {
            struct DistributionPacket *packet = malloc(sizeof(struct DistributionPacket) + 1 + control_message_size + payload_size);
            if (LIKELY(packet != NULL)) {
                packet->size = 1 + control_message_size + payload_size;
                packet->bytes[0] = 112;
                externalterm_serialize_term(&packet->bytes[1], control_message, global);
                if (!term_is_invalid_term(payload)) {
                    externalterm_serialize_term(&packet->bytes[1 + control_message_size], payload, global);
                }
                // Use the lock on the list of pending packets to notify process
                struct ListHead *pending_packets = synclist_wrlock(&connection->pending_packets);
                list_append(pending_packets, &packet->head);
                int32_t selecting_process_id = connection->selecting_process_id;
                if (selecting_process_id != INVALID_PROCESS_ID) {
                    connection->selecting_process_id = INVALID_PROCESS_ID;
                }
                synclist_unlock(&connection->pending_packets);
                if (selecting_process_id != INVALID_PROCESS_ID) {
                    globalcontext_send_message(global, selecting_process_id, DIST_DATA_ATOM);
                }
            }
        }
    }
}

static void dist_enqueue_send_sender_message(int32_t local_process_id, term remote_process_id, term payload, struct DistConnection *connection, GlobalContext *global)
{
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(3), heap)
    term control_message = term_alloc_tuple(3, &heap);
    term_put_tuple_element(control_message, 0, term_from_int(OPERATION_SEND_SENDER));
    term_put_tuple_element(control_message, 1, term_from_local_process_id(local_process_id));
    term_put_tuple_element(control_message, 2, remote_process_id);

    dist_enqueue_message(control_message, payload, connection, global);
    END_WITH_STACK_HEAP(heap, global)
}

static void dist_enqueue_monitor_exit_message(struct RemoteMonitor *monitor, term reason, struct DistConnection *connection, GlobalContext *global)
{
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(4) + EXTERNAL_PID_SIZE + EXTERNAL_REF_SIZE(5), heap)
    term control_message = term_alloc_tuple(4, &heap);
    term_put_tuple_element(control_message, 0, term_from_int(OPERATION_PAYLOAD_MONITOR_P_EXIT));
    term_put_tuple_element(control_message, 1, monitor->target_proc);
    term external_pid = term_make_external_process_id(term_from_atom_index(connection->node_atom_index), monitor->pid_number, monitor->pid_serial, connection->node_creation, &heap);
    term_put_tuple_element(control_message, 2, external_pid);
    term external_ref = term_make_external_reference(term_from_atom_index(connection->node_atom_index), monitor->ref_len, monitor->ref_words, connection->node_creation, &heap);
    term_put_tuple_element(control_message, 3, external_ref);

    dist_enqueue_message(control_message, reason, connection, global);
    END_WITH_STACK_HEAP(heap, global)
}

static void dist_connection_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(pid);
    UNUSED(mon);

    struct DistConnection *conn_obj = (struct DistConnection *) obj;

    if (UNLIKELY(enif_compare_monitors(&conn_obj->connection_process_monitor, mon) == 0)) {
        struct RefcBinary *rsrc_refc = refc_binary_from_data(obj);
        refc_binary_decrement_refcount(rsrc_refc, caller_env->global);
    } else {
        struct ListHead *remote_monitors = synclist_wrlock(&conn_obj->remote_monitors);
        struct ListHead *item;
        LIST_FOR_EACH (item, remote_monitors) {
            struct RemoteMonitor *remote_monitor = GET_LIST_ENTRY(item, struct RemoteMonitor, head);
            if (enif_compare_monitors(&remote_monitor->process_monitor, mon) == 0) {
                // Found monitor.
                // We need to find out the exit reason. This function is called
                // from context_destroy. The process is no longer in the table
                // and cannot be found anymore. However, it is invoked from
                // caller_env which really is the destroyed process.
                term reason;
                if (erl_nif_env_is_context(caller_env)) {
                    Context *ctx = (Context *) caller_env;
                    reason = ctx->exit_reason;
                } else {
                    reason = UNDEFINED_ATOM;
                }
                dist_enqueue_monitor_exit_message(remote_monitor, reason, conn_obj, caller_env->global);
                list_remove(&remote_monitor->head);
                free(remote_monitor);
                break;
            }
        }
        synclist_unlock(&conn_obj->remote_monitors);
    }
}

const ErlNifResourceTypeInit dist_connection_resource_type_init = {
    .members = 3,
    .dtor = dist_connection_dtor,
    .stop = NULL,
    .down = dist_connection_down,
};

static term nif_erlang_setnode_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_local_pid_or_port);
    VALIDATE_VALUE(argv[2], term_is_tuple);
    if (UNLIKELY(term_get_tuple_arity(argv[2]) != 2)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(term_get_tuple_element(argv[2], 0), term_is_any_integer);
    VALIDATE_VALUE(term_get_tuple_element(argv[2], 1), term_is_any_integer);
    // Ignore flags for now
    uint32_t creation = term_maybe_unbox_int(term_get_tuple_element(argv[2], 1));
    int node_atom_index = term_to_atom_index(argv[0]);

    // Ensure we don't already know this node.
    struct ListHead *dist_connections = synclist_wrlock(&ctx->global->dist_connections);
    struct ListHead *item;
    LIST_FOR_EACH (item, dist_connections) {
        struct DistConnection *dist_connection = GET_LIST_ENTRY(item, struct DistConnection, head);
        if (dist_connection->node_atom_index == node_atom_index && dist_connection->node_creation == creation) {
            synclist_unlock(&ctx->global->dist_connections);
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    // Create a resource object
    struct DistConnection *conn_obj = enif_alloc_resource(ctx->global->dist_connection_resource_type, sizeof(struct DistConnection));
    if (IS_NULL_PTR(conn_obj)) {
        synclist_unlock(&ctx->global->dist_connections);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    conn_obj->node_atom_index = node_atom_index;
    conn_obj->node_creation = creation;
    synclist_init(&conn_obj->remote_monitors);
    synclist_init(&conn_obj->pending_packets);

    ErlNifEnv *env = erl_nif_env_from_context(ctx);
    conn_obj->connection_process_id = term_to_local_process_id(argv[1]);
    if (UNLIKELY(enif_monitor_process(env, conn_obj, &conn_obj->connection_process_id, &conn_obj->connection_process_monitor) != 0)) {
        synclist_unlock(&ctx->global->dist_connections);
        RAISE_ERROR(BADARG_ATOM);
    }
    list_prepend(dist_connections, &conn_obj->head);
    synclist_unlock(&ctx->global->dist_connections);

    // Increment reference count as the resource should be alive until controller process dies
    struct RefcBinary *rsrc_refc = refc_binary_from_data(conn_obj);
    refc_binary_increment_refcount(rsrc_refc);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TERM_BOXED_RESOURCE_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_resource(conn_obj, &ctx->heap);
}

static term nif_erlang_dist_ctrl_get_data_notification(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctx->global->dist_connection_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct DistConnection *conn_obj = (struct DistConnection *) rsrc_obj_ptr;

    struct ListHead *pending_packets = synclist_wrlock(&conn_obj->pending_packets);
    UNUSED(pending_packets); // without SMP, this would appear as a statement with no effect
    conn_obj->selecting_process_id = ctx->process_id;
    synclist_unlock(&conn_obj->pending_packets);

    return OK_ATOM;
}

static term nif_erlang_dist_ctrl_get_data(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctx->global->dist_connection_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct DistConnection *conn_obj = (struct DistConnection *) rsrc_obj_ptr;

    term result;

    struct ListHead *pending_packets = synclist_wrlock(&conn_obj->pending_packets);
    if (list_is_empty(pending_packets)) {
        result = NONE_ATOM;
    } else {
        struct ListHead *first = list_first(pending_packets);
        struct DistributionPacket *packet = GET_LIST_ENTRY(first, struct DistributionPacket, head);
        if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(packet->size)) != MEMORY_GC_OK)) {
            synclist_unlock(&conn_obj->pending_packets);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        result = term_from_literal_binary(packet->bytes, packet->size, &ctx->heap, ctx->global);
        list_remove(first);
        free(first);
    }
    synclist_unlock(&conn_obj->pending_packets);

    return result;
}

static term nif_erlang_dist_ctrl_put_data(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_binary);
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t binary_len = term_binary_size(argv[1]);
    if (binary_len < 1 || data[0] != 112) {
        RAISE_ERROR(BADARG_ATOM);
    }

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctx->global->dist_connection_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct DistConnection *conn_obj = (struct DistConnection *) rsrc_obj_ptr;

    size_t bytes_read = 0;
    term control = externalterm_to_term_with_roots(data + 1, binary_len - 1, ctx, ExternalTermCopy, &bytes_read, 2, argv);

    if (UNLIKELY(!term_is_tuple(control))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t arity = term_get_tuple_arity(control);
    if (UNLIKELY(arity < 1)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term operation = term_get_tuple_element(control, 0);
    if (UNLIKELY(!term_is_integer(operation))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    switch (term_to_int(operation)) {
        case OPERATION_REG_SEND: {
            if (UNLIKELY(arity != 4)) {
                RAISE_ERROR(BADARG_ATOM);
            }
            term to_name = term_get_tuple_element(control, 3);
            term target_process_pid = globalcontext_get_registered_process(ctx->global, term_to_atom_index(to_name));
            if (term_is_local_pid(target_process_pid)) {
                term payload = externalterm_to_term_with_roots(data + 1 + bytes_read, binary_len - 1 - bytes_read, ctx, ExternalTermCopy, &bytes_read, 2, argv);
                globalcontext_send_message(ctx->global, term_to_local_process_id(target_process_pid), payload);
            }
            break;
        }
        case OPERATION_MONITOR_P: {
            if (UNLIKELY(arity != 4)) {
                RAISE_ERROR(BADARG_ATOM);
            }
            term from_pid = term_get_tuple_element(control, 1);
            term target_proc = term_get_tuple_element(control, 2);
            term monitor_ref = term_get_tuple_element(control, 3);
            if (term_is_atom(target_proc)) {
                target_proc = globalcontext_get_registered_process(ctx->global, term_to_atom_index(target_proc));
            }
            int target_process_id = 0;
            if (term_is_local_pid(target_proc)) {
                target_process_id = term_to_local_process_id(target_proc);
            } else {
                RAISE_ERROR(BADARG_ATOM);
            }
            struct RemoteMonitor *monitor = malloc(sizeof(struct RemoteMonitor));
            monitor->target_proc = target_proc;
            monitor->pid_number = term_get_external_pid_process_id(from_pid);
            monitor->pid_serial = term_get_external_pid_serial(from_pid);
            monitor->ref_len = term_get_external_reference_len(monitor_ref);
            memcpy(monitor->ref_words, term_get_external_reference_words(monitor_ref), sizeof(uint32_t) * monitor->ref_len);
            if (target_process_id) {
                synclist_append(&conn_obj->remote_monitors, &monitor->head);
                ErlNifPid target_process_pid = target_process_id;
                if (UNLIKELY(enif_monitor_process(erl_nif_env_from_context(ctx), conn_obj, &target_process_pid, &monitor->process_monitor) != 0)) {
                    synclist_remove(&conn_obj->remote_monitors, &monitor->head);
                    dist_enqueue_monitor_exit_message(monitor, NOPROC_ATOM, conn_obj, ctx->global);
                    free(monitor);
                }
            } else {
                dist_enqueue_monitor_exit_message(monitor, NOPROC_ATOM, conn_obj, ctx->global);
                free(monitor);
            }

            break;
        }
        case OPERATION_DEMONITOR_P: {
            if (UNLIKELY(arity != 4)) {
                RAISE_ERROR(BADARG_ATOM);
            }
            term target_proc = term_get_tuple_element(control, 2);
            term monitor_ref = term_get_tuple_element(control, 3);
            uint8_t ref_len = term_get_external_reference_len(monitor_ref);
            const uint32_t *ref_words = term_get_external_reference_words(monitor_ref);
            struct ListHead *remote_monitors = synclist_wrlock(&conn_obj->remote_monitors);
            struct ListHead *item;
            LIST_FOR_EACH (item, remote_monitors) {
                struct RemoteMonitor *remote_monitor = GET_LIST_ENTRY(item, struct RemoteMonitor, head);
                if (remote_monitor->target_proc == target_proc
                    && remote_monitor->ref_len == ref_len
                    && memcmp(remote_monitor->ref_words, ref_words, ref_len * sizeof(uint32_t)) == 0) {
                    enif_demonitor_process(erl_nif_env_from_context(ctx), conn_obj, &remote_monitor->process_monitor);
                    list_remove(item);
                    break;
                }
            }
            synclist_unlock(&conn_obj->remote_monitors);
            break;
        }
        default:
            printf("Unknown distribution protocol operation id %d\n", (int) term_to_int(operation));
            RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

void dist_send_message(term external_pid, term payload, Context *ctx)
{
    // Search for dhandle.
    int node_atom_index = term_to_atom_index(term_get_external_node(external_pid));
    uint32_t node_creation = term_get_external_node_creation(external_pid);
    struct ListHead *dist_connections = synclist_rdlock(&ctx->global->dist_connections);
    struct ListHead *item;
    LIST_FOR_EACH (item, dist_connections) {
        struct DistConnection *dist_connection = GET_LIST_ENTRY(item, struct DistConnection, head);
        if (dist_connection->node_atom_index == node_atom_index && dist_connection->node_creation == node_creation) {
            dist_enqueue_send_sender_message(ctx->process_id, external_pid, payload, dist_connection, ctx->global);
            break;
        }
    }
    synclist_unlock(&ctx->global->dist_connections);
}

const struct Nif setnode_3_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_setnode_3
};

const struct Nif dist_ctrl_get_data_notification_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_dist_ctrl_get_data_notification
};

const struct Nif dist_ctrl_get_data_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_dist_ctrl_get_data
};

const struct Nif dist_ctrl_put_data_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_dist_ctrl_put_data
};
