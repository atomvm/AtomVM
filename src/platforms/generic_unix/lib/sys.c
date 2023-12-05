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

#include "sys.h"
#include "generic_unix_sys.h"

#include "avmpack.h"
#include "defaultatoms.h"
#include "iff.h"
#include "mapped_file.h"
#include "otp_net.h"
#include "otp_socket.h"
#include "scheduler.h"
#include "smp.h"
#include "utils.h"

#if ATOMVM_HAS_MBEDTLS
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>

#if defined(MBEDTLS_VERSION_NUMBER) && (MBEDTLS_VERSION_NUMBER >= 0x03000000)
#include <mbedtls/build_info.h>
#else
#include <mbedtls/config.h>
#endif

#include "otp_ssl.h"
#include "sys_mbedtls.h"
#endif

#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#ifdef HAVE_KQUEUE
#include <sys/event.h>
#include <sys/types.h>
#else
#include <poll.h>
#ifdef HAVE_EVENTFD
#include <sys/eventfd.h>
#else
#include <sys/types.h>
#include <sys/uio.h>
#endif
#endif

// Platform uses listeners
#include "listeners.h"

#ifndef AVM_NO_SMP
#define SMP_MUTEX_LOCK(mtx) smp_mutex_lock(mtx)
#define SMP_MUTEX_UNLOCK(mtx) smp_mutex_unlock(mtx)
#else
#define SMP_MUTEX_LOCK(mtx)
#define SMP_MUTEX_UNLOCK(mtx)
#endif

#ifdef DYNLOAD_PORT_DRIVERS
#include <dlfcn.h>

typedef Context *(*create_port_t)(GlobalContext *global, term opts);
#endif

#include "trace.h"

struct GenericUnixPlatformData
{
    struct ListHead listeners;
#ifndef AVM_NO_SMP
    Mutex *listeners_mutex;
#endif
#ifdef HAVE_KQUEUE
    int kqueue_fd;
#else
    struct pollfd *fds;
#endif
    int ATOMIC listeners_poll_count; // can be invalidated by being set to -1
    int ATOMIC select_events_poll_count; // can be invalidated by being set to -1
#ifndef AVM_NO_SMP
#ifndef HAVE_KQUEUE
#ifdef HAVE_EVENTFD
    int signal_fd;
#else
    int signal_pipe[2];
#endif
#endif
#endif

#ifdef ATOMVM_HAS_MBEDTLS
#ifndef AVM_NO_SMP
    Mutex *entropy_mutex;
#endif
    mbedtls_entropy_context entropy_ctx;
    bool entropy_is_initialized;

#ifndef AVM_NO_SMP
    Mutex *random_mutex;
#endif
    mbedtls_ctr_drbg_context random_ctx;
    bool random_is_initialized;
#endif
};

static void mapped_file_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global);

struct MappedFileAVMPack
{
    struct AVMPackData base;
    MappedFile *mapped;
};

static const struct AVMPackInfo mapped_file_avm_pack_info = {
    .destructor = mapped_file_avm_pack_destructor
};

#ifdef HAVE_KQUEUE
#define SIGNAL_IDENTIFIER 1
#endif

#ifdef HAVE_KQUEUE
static inline void sys_poll_events_with_kqueue(GlobalContext *glb, int timeout_ms)
{
    struct GenericUnixPlatformData *platform = glb->platform_data;
    int select_events_poll_count = platform->select_events_poll_count;
    if (select_events_poll_count < 0) {
        size_t either;
        struct ListHead *select_events = synclist_wrlock(&glb->select_events);
        select_event_count_and_destroy_closed(select_events, NULL, NULL, &either, glb);
        synclist_unlock(&glb->select_events);
        select_events_poll_count = either;
        platform->select_events_poll_count = either;
    }
    int poll_count = platform->listeners_poll_count + select_events_poll_count + 1;
    struct kevent notified[poll_count];
    struct timespec ts;
    struct timespec *ts_ptr = &ts;
    if (timeout_ms < 0) {
        ts_ptr = NULL;
    } else {
        ts.tv_sec = timeout_ms / 1000;
        ts.tv_nsec = (uint_least64_t) (timeout_ms % 1000) * 1000000;
    }

    int nbEvents = kevent(platform->kqueue_fd, NULL, 0, notified, poll_count, ts_ptr);
    struct ListHead *listeners = NULL;
    for (int i = 0; i < nbEvents; i++) {
        if (notified[i].filter == EVFILT_USER && notified[i].ident == SIGNAL_IDENTIFIER) {
            // We've been signaled.
            continue;
        }
        if (notified[i].filter == EVFILT_READ) {
            if (listeners == NULL) {
                listeners = synclist_wrlock(&glb->listeners);
            }
            if (!process_listener_handler(glb, (int) notified[i].ident, listeners, NULL, NULL)) {
                select_event_notify(notified[i].ident, true, false, glb);
            }
        } else if (notified[i].filter == EVFILT_WRITE) {
            select_event_notify(notified[i].ident, false, true, glb);
        }
    }
    if (listeners) {
        synclist_unlock(&glb->listeners);
    }
}
#else
static inline void sys_poll_events_with_poll(GlobalContext *glb, int timeout_ms)
{
    struct GenericUnixPlatformData *platform = glb->platform_data;
    struct pollfd *fds = platform->fds;
    int listeners_poll_count = platform->listeners_poll_count;
    int select_events_poll_count = platform->select_events_poll_count;
    int poll_count;
#ifndef AVM_NO_SMP
    poll_count = 1;
#else
    poll_count = 0;
#endif
    int fd_index;
    if (listeners_poll_count < 0 || select_events_poll_count < 0) {
        // Means it is dirty and should be rebuilt.
        // The array of polling fds is composed of, in this order:
        // - the signaling fd (for SMP), which is eventfd or a pipe
        // - the listeners fd
        // - the sockets fd
        struct ListHead *select_events = synclist_wrlock(&glb->select_events);
        size_t select_events_new_count;
        if (select_events_poll_count < 0) {
            select_event_count_and_destroy_closed(select_events, NULL, NULL, &select_events_new_count, glb);
        } else {
            select_events_new_count = select_events_poll_count;
        }

        size_t listeners_new_count = 0;
        struct ListHead *listeners = NULL;
        struct ListHead *item;
        // We avoid acquiring a lock on the list of listeners and rebuild the list of fds for listeners if we can
        if (listeners_poll_count < 0) {
            listeners = synclist_rdlock(&glb->listeners);
            LIST_FOR_EACH (item, listeners) {
                EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
                int listener_fd = listener->fd;
                if (listener_fd >= 0) {
                    listeners_new_count++;
                }
            }
        } else {
            listeners_new_count = listeners_poll_count;
        }

        fds = realloc(fds, sizeof(struct pollfd) * (poll_count + select_events_new_count + listeners_new_count));
        platform->fds = fds;

#ifndef AVM_NO_SMP
#ifdef HAVE_EVENTFD
        fds[0].fd = platform->signal_fd;
#else
        fds[0].fd = platform->signal_pipe[0];
#endif
        fds[0].events = POLLIN;
        fds[0].revents = 0;
#endif

        fd_index = poll_count;

        // Rebuild the list of fds for listeners if it is dirty
        if (listeners_poll_count < 0) {
            // We put listeners first
            LIST_FOR_EACH (item, listeners) {
                EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
                int listener_fd = listener->fd;
                if (listener_fd >= 0) {
                    fds[fd_index].fd = listener_fd;
                    fds[fd_index].events = POLLIN;
                    fds[fd_index].revents = 0;

                    fd_index++;
                }
            }
            synclist_unlock(&glb->listeners);
        } else {
            // Else we can reuse the previous list
            fd_index += listeners_new_count;
        }

        // We put select events next
        LIST_FOR_EACH (item, select_events) {
            struct SelectEvent *select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
            if (select_event->read || select_event->write) {
                fds[fd_index].fd = select_event->event;
                fds[fd_index].events = (select_event->read ? POLLIN : 0) | (select_event->write ? POLLOUT : 0);
                fds[fd_index].revents = 0;

                fd_index++;
            }
        }
        synclist_unlock(&glb->select_events);

        listeners_poll_count = listeners_new_count;
        select_events_poll_count = select_events_new_count;
        platform->listeners_poll_count = listeners_new_count;
        platform->select_events_poll_count = select_events_new_count;
    }

    poll_count += listeners_poll_count + select_events_poll_count;

    int nb_descriptors = poll(fds, poll_count, timeout_ms);

    // After poll, process the list of fds in order, using fd_index as the index
    // on the list and nb_descriptors as the number of fds to process left
    fd_index = 0;
#ifndef AVM_NO_SMP
    if (nb_descriptors > 0) {
        if ((fds[0].revents & fds[0].events)) {
            // We've been signaled
            // Read can fail if the byte/event is also read concurrently by
            // sys_signal
#ifdef HAVE_EVENTFD
            eventfd_t ignored;
            (void) eventfd_read(platform->signal_fd, &ignored);
#else
            char ignored;
            (void) read(platform->signal_pipe[0], &ignored, sizeof(ignored));
#endif
            nb_descriptors--;
        }
        fd_index++;
    }
#endif
    if (nb_descriptors > 0) {
        struct ListHead *listeners = synclist_wrlock(&glb->listeners);
        struct ListHead *item = listeners->next;
        struct ListHead *previous = listeners;
        for (int i = 0; i < listeners_poll_count && nb_descriptors > 0; i++, fd_index++) {
            if (!(fds[fd_index].revents & fds[fd_index].events)) {
                continue;
            }
            fds[fd_index].revents = 0;
            nb_descriptors--;

            process_listener_handler(glb, fds[fd_index].fd, listeners, &item, &previous);
        }
        synclist_unlock(&glb->listeners);
    }

    for (int i = 0; i < select_events_poll_count && nb_descriptors > 0; i++, fd_index++) {
        if (!(fds[fd_index].revents & fds[fd_index].events)) {
            continue;
        }
        bool is_read = fds[fd_index].revents & POLLIN;
        bool is_write = fds[fd_index].revents & POLLOUT;
        fds[fd_index].revents = 0;
        nb_descriptors--;

        select_event_notify(fds[fd_index].fd, is_read, is_write, glb);
    }
}
#endif

void sys_poll_events(GlobalContext *glb, int timeout_ms) CLANG_THREAD_SANITIZE_SAFE
{
    // Optimization: do not go into poll(2) or kqueue(2) if we have nothing to
    // wait on.
    if (timeout_ms == 0 && synclist_is_empty(&glb->listeners) && synclist_is_empty(&glb->select_events)) {
        return;
    }

#ifdef HAVE_KQUEUE
    sys_poll_events_with_kqueue(glb, timeout_ms);
#else
    sys_poll_events_with_poll(glb, timeout_ms);
#endif
}

#ifndef AVM_NO_SMP
void sys_signal(GlobalContext *glb)
{
    struct GenericUnixPlatformData *platform = glb->platform_data;
#ifdef HAVE_KQUEUE
    struct timespec ts = { 0, 0 };
    struct kevent kev;
    EV_SET(&kev, SIGNAL_IDENTIFIER, EVFILT_USER, 0, NOTE_TRIGGER, 0, NULL);
    if (UNLIKELY(kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts) == -1)) {
        AVM_ABORT();
    }
#else
#ifdef HAVE_EVENTFD
    // Write can fail if the counter overflows
    // (very unlikely, 2^64)
    if (UNLIKELY(eventfd_write(platform->signal_fd, 1))) {
        eventfd_t ignored;
        (void) eventfd_read(platform->signal_fd, &ignored);
        (void) eventfd_write(platform->signal_fd, 1);
    }
#else
    // Pipe can quickly fill up, purge-read it and write to it
    char c = 0;
    (void) read(platform->signal_pipe[0], &c, sizeof(c));
    (void) write(platform->signal_pipe[1], &c, sizeof(c));
#endif
#endif
}
#endif

void sys_time(struct timespec *t)
{
    if (UNLIKELY(clock_gettime(CLOCK_REALTIME, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }
}

void sys_monotonic_time(struct timespec *t)
{
    if (UNLIKELY(clock_gettime(CLOCK_MONOTONIC, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }
}

uint64_t sys_monotonic_time_u64()
{
    // On generic unix, native format is timespec.
    struct timespec ts;
    sys_monotonic_time(&ts);
    // 2^64/10^9/86400/365 around 585 years
    return ((uint_least64_t) ts.tv_sec * 1000000000) + ts.tv_nsec;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms * 1000000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t / 1000000;
}

enum OpenAVMResult sys_open_avm_from_file(
    GlobalContext *global, const char *path, struct AVMPackData **data)
{
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);

    UNUSED(global);

    MappedFile *mapped = mapped_file_open_beam(path);
    if (IS_NULL_PTR(mapped)) {
        return AVM_OPEN_CANNOT_OPEN;
    }
    if (UNLIKELY(!avmpack_is_valid(mapped->mapped, mapped->size))) {
        return AVM_OPEN_INVALID;
    }

    struct MappedFileAVMPack *avmpack_data = malloc(sizeof(struct MappedFileAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        mapped_file_close(mapped);
        return AVM_OPEN_FAILED_ALLOC;
    }
    avmpack_data_init(&avmpack_data->base, &mapped_file_avm_pack_info);
    avmpack_data->base.data = mapped->mapped;
    avmpack_data->mapped = mapped;

    *data = &avmpack_data->base;
    return AVM_OPEN_OK;
}

static void mapped_file_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global)
{
    UNUSED(global);

    struct MappedFileAVMPack *mmapped_avm = CONTAINER_OF(obj, struct MappedFileAVMPack, base);
    mapped_file_close(mmapped_avm->mapped);
    free(obj);
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    TRACE("sys_load_module_from_file: Going to load: %s\n", path);

    MappedFile *beam_file = mapped_file_open_beam(path);
    if (IS_NULL_PTR(beam_file)) {
        return NULL;
    }
    if (UNLIKELY(!iff_is_valid_beam(beam_file->mapped))) {
        fprintf(stderr, "%s is not a valid BEAM file.\n", path);
        return NULL;
    }
    const void *beam_module = beam_file->mapped;
    uint32_t beam_module_size = beam_file->size;

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    if (IS_NULL_PTR(new_module)) {
        return NULL;
    }
    new_module->module_platform_data = beam_file;

    return new_module;
}

Module *sys_find_and_load_module_from_avm(GlobalContext *global, const char *module_name)
{
    TRACE("sys_find_and_load_module_from_avm: Going to load: %s\n", module_name);

    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    Module *new_module = NULL;

    struct ListHead *item;
    struct ListHead *avmpack_data = synclist_rdlock(&global->avmpack_data);
    LIST_FOR_EACH (item, avmpack_data) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        bool prev_in_use = avmpack_data->in_use;
        avmpack_data->in_use = true;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
            if (IS_NULL_PTR(new_module)) {
                avmpack_data->in_use = prev_in_use;
                synclist_unlock(&global->avmpack_data);
                return NULL;
            }
            new_module->module_platform_data = NULL;

            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    return new_module;
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    TRACE("sys_load_module: Going to load: %s\n", module_name);

    Module *new_module = sys_find_and_load_module_from_avm(global, module_name);
    if (new_module) {
        return new_module;
    }

    return sys_load_module_from_file(global, module_name);
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    if (!strcmp(driver_name, "socket")) {
        return socket_init(glb, opts);
    } else {
#ifdef DYNLOAD_PORT_DRIVERS
        void *handle;
        {
            char port_driver_name[64 + strlen("avm_"
                                              "_port_driver.so")
                + 1];
            snprintf(port_driver_name, sizeof(port_driver_name), "./avm_%s_port_driver.so", driver_name);
            handle = dlopen(port_driver_name, RTLD_NOW);
            if (!handle) {
                return NULL;
            }
        }
        char port_driver_func_name[64 + strlen("_create_port") + 1];
        snprintf(port_driver_func_name, sizeof(port_driver_func_name), "%s_create_port", driver_name);
        create_port_t create_port
            = (create_port_t) CAST_VOID_TO_FUNC_PTR(dlsym(handle, port_driver_func_name));
        if (IS_NULL_PTR(create_port)) {
            return NULL;
        }
        return create_port(glb, opts);
#else
        return NULL;
#endif
    }
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}

void sys_init_platform(GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = malloc(sizeof(struct GenericUnixPlatformData));
    if (UNLIKELY(!platform)) {
        AVM_ABORT();
    }
    list_init(&platform->listeners);
#ifndef AVM_NO_SMP
    platform->listeners_mutex = smp_mutex_create();
#endif
#ifdef HAVE_KQUEUE
    platform->kqueue_fd = kqueue();
    platform->listeners_poll_count = 0;
    platform->select_events_poll_count = 0;
#ifndef AVM_NO_SMP
    struct timespec ts = { 0, 0 };
    struct kevent kev;
    EV_SET(&kev, SIGNAL_IDENTIFIER, EVFILT_USER, EV_ADD | EV_CLEAR, 0, 0, NULL);
    if (UNLIKELY(kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts))) {
        AVM_ABORT();
    }
#endif
#else
    platform->listeners_poll_count = -1;
    platform->select_events_poll_count = -1;
    platform->fds = malloc(0);
#ifndef AVM_NO_SMP
#ifdef HAVE_EVENTFD
    int signal_fd = eventfd(0, EFD_NONBLOCK);
    if (UNLIKELY(signal_fd < 0)) {
        AVM_ABORT();
    }
    platform->signal_fd = signal_fd;
#else
    if (UNLIKELY(pipe(platform->signal_pipe))) {
        AVM_ABORT();
    }
    int flags = fcntl(platform->signal_pipe[0], F_GETFL);
    if (UNLIKELY(flags < 0)) {
        AVM_ABORT();
    }
    flags |= O_NONBLOCK;
    if (UNLIKELY(fcntl(platform->signal_pipe[0], F_SETFL, flags) < 0)) {
        AVM_ABORT();
    }
#endif
#endif
#endif

    otp_net_init(global);
    otp_socket_init(global);
#if ATOMVM_HAS_MBEDTLS
#ifndef AVM_NO_SMP
    platform->entropy_mutex = smp_mutex_create();
    if (IS_NULL_PTR(platform->entropy_mutex)) {
        AVM_ABORT();
    }
    platform->random_mutex = smp_mutex_create();
    if (IS_NULL_PTR(platform->random_mutex)) {
        AVM_ABORT();
    }
#endif
    platform->entropy_is_initialized = false;
    platform->random_is_initialized = false;
    otp_ssl_init(global);
#endif

    global->platform_data = platform;
}

void sys_free_platform(GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
#ifndef AVM_NO_SMP
    smp_mutex_destroy(platform->listeners_mutex);
#endif

#ifdef HAVE_KQUEUE
    close(platform->kqueue_fd);
#else
    free(platform->fds);
#endif
#ifndef AVM_NO_SMP
#ifndef HAVE_KQUEUE
#ifdef HAVE_EVENTFD
    close(platform->signal_fd);
#else
    close(platform->signal_pipe[0]);
    close(platform->signal_pipe[1]);
#endif
#endif
#endif

#if !defined(AVM_NO_SMP) && ATOMVM_HAS_MBEDTLS
    smp_mutex_destroy(platform->entropy_mutex);
    smp_mutex_destroy(platform->random_mutex);
#endif

#if ATOMVM_HAS_MBEDTLS
    if (platform->random_is_initialized) {
        mbedtls_ctr_drbg_free(&platform->random_ctx);
    }

    if (platform->entropy_is_initialized) {
        mbedtls_entropy_free(&platform->entropy_ctx);
    }
#endif

    free(platform);
}

void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
#ifdef HAVE_KQUEUE
    if (listener->fd) {
        struct timespec ts = { 0, 0 };
        struct kevent kev;
        EV_SET(&kev, listener->fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
        if (UNLIKELY(kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts))) {
            AVM_ABORT();
        }
    }
    platform->listeners_poll_count++;
#else
    UNUSED(listener);
    platform->listeners_poll_count = -1;
#endif
}

void sys_register_listener(GlobalContext *global, struct EventListener *listener)
{
    struct ListHead *listeners = synclist_wrlock(&global->listeners);
    event_listener_add_to_polling_set(listener, global);
#ifndef AVM_NO_SMP
#ifndef HAVE_KQUEUE
    // With kqueue (like epoll), there is no need to restart the call
    sys_signal(global);
#endif
#endif
    list_append(listeners, &listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

static void listener_event_remove_from_polling_set(listener_event_t listener_fd, GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
#ifdef HAVE_KQUEUE
    if (listener_fd) {
        struct timespec ts = { 0, 0 };
        struct kevent kev;
        EV_SET(&kev, listener_fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
        (void) kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts);
        // File descriptor is automatically removed when closed
    }
    platform->listeners_poll_count--;
#else
    UNUSED(listener_fd);
    platform->listeners_poll_count = -1;
#endif
}

void sys_unregister_listener(GlobalContext *global, struct EventListener *listener)
{
    listener_event_remove_from_polling_set(listener->fd, global);
    synclist_remove(&global->listeners, &listener->listeners_list_head);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
#ifdef HAVE_KQUEUE
    struct timespec ts = { 0, 0 };
    struct kevent kev;
    EV_SET(&kev, event, is_write ? EVFILT_WRITE : EVFILT_READ, EV_ADD, 0, 0, NULL);
    if (UNLIKELY(kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts))) {
        AVM_ABORT();
    }
    // We need this count to be the number of select events either read or write, so force a count
    platform->select_events_poll_count = -1;
#else
    UNUSED(event);
    UNUSED(is_write);
    platform->select_events_poll_count = -1;
#ifndef AVM_NO_SMP
    sys_signal(global);
#endif
#endif
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
#ifdef HAVE_KQUEUE
    struct timespec ts = { 0, 0 };
    struct kevent kev;
    EV_SET(&kev, event, is_write ? EVFILT_WRITE : EVFILT_READ, EV_DELETE, 0, 0, NULL);
    (void) kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts);
    platform->select_events_poll_count = -1;
#else
    UNUSED(event);
    UNUSED(is_write);
    platform->select_events_poll_count = -1;
#ifndef AVM_NO_SMP
    sys_signal(global);
#endif
#endif
}

bool event_listener_is_event(EventListener *listener, listener_event_t event)
{
    return listener->fd == event;
}

#ifdef ATOMVM_HAS_MBEDTLS
int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size)
{
#ifndef MBEDTLS_THREADING_C
    struct GenericUnixPlatformData *platform
        = CONTAINER_OF(entropy, struct GenericUnixPlatformData, entropy_ctx);
    SMP_MUTEX_LOCK(platform->entropy_mutex);
    int result = mbedtls_entropy_func(entropy, buf, size);
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);

    return result;
#else
    return mbedtls_entropy_func(entropy, buf, size);
#endif
}

mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = global->platform_data;

    SMP_MUTEX_LOCK(platform->entropy_mutex);

    if (!platform->entropy_is_initialized) {
        mbedtls_entropy_init(&platform->entropy_ctx);
        platform->entropy_is_initialized = true;
    }

    return &platform->entropy_ctx;
}

void sys_mbedtls_entropy_context_unlock(GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);
}

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = global->platform_data;

    SMP_MUTEX_LOCK(platform->random_mutex);

    if (!platform->random_is_initialized) {
        mbedtls_ctr_drbg_init(&platform->random_ctx);

        mbedtls_entropy_context *entropy_ctx = sys_mbedtls_get_entropy_context_lock(global);
        // Safe to unlock it now, sys_mbedtls_entropy_func will lock it again later
        sys_mbedtls_entropy_context_unlock(global);

        const char *seed = "AtomVM Mbed-TLS initial seed.";
        int seed_len = strlen(seed);
        int seed_err = mbedtls_ctr_drbg_seed(&platform->random_ctx, sys_mbedtls_entropy_func,
            entropy_ctx, (const unsigned char *) seed, seed_len);
        if (UNLIKELY(seed_err != 0)) {
            abort();
        }
        platform->random_is_initialized = true;
    }

    return &platform->random_ctx;
}

void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
    SMP_MUTEX_UNLOCK(platform->random_mutex);
}

#endif
