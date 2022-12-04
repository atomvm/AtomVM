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
#include "scheduler.h"
#include "smp.h"
#include "utils.h"

#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#ifdef HAVE_KQUEUE
#include <sys/types.h>
#include <sys/event.h>
#else
#include <poll.h>
#ifdef HAVE_EVENTFD
#include <sys/eventfd.h>
#else
#include <sys/types.h>
#include <sys/uio.h>
#endif
#endif

#ifndef AVM_NO_SMP
#include <stdatomic.h>
#else
#ifndef _Atomic
#define _Atomic
#endif
#endif

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
    int _Atomic poll_count;
#ifndef AVM_NO_SMP
#ifndef HAVE_KQUEUE
#ifdef HAVE_EVENTFD
    int signal_fd;
#else
    int signal_pipe[2];
#endif
#endif
#endif
};

#ifdef HAVE_KQUEUE
#define SIGNAL_IDENTIFIER 1
#endif

static void do_register_listener(struct GenericUnixPlatformData *platform, struct EventListener *listener);
static void do_unregister_listener(struct GenericUnixPlatformData *platform, int listener_fd);

/**
 * @brief process listener handlers advancing order, especially useful with poll(2) which returns fd in the provided order.
 *
 * @detail mutex on listeners should be hold when this function is called.
 */
static inline void process_listener_handler(GlobalContext *glb, int current_fd, struct ListHead *listeners, struct ListHead **item_ptr, struct ListHead **previous_ptr)
{
    struct ListHead *item;
    struct ListHead *previous;
    if (item_ptr) {
        item = *item_ptr;
        previous = *previous_ptr;
    } else {
        item = listeners->next;
        previous = listeners;
    }

    while (item != listeners) {
        struct ListHead *next = item->next;
        EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
        if (listener->fd == current_fd) {
            EventListener *new_listener = listener->handler(glb, listener);
            if (new_listener == NULL) {
                do_unregister_listener(glb->platform_data, current_fd);
                previous->next = next;
                next->prev = previous;
                item = next;
            } else if (new_listener != listener) {
                struct GenericUnixPlatformData *platform = glb->platform_data;
                do_unregister_listener(platform, current_fd);
                do_register_listener(platform, new_listener);
                // Replace listener with new_listener in the list
                // listener was freed by handler.
                previous->next = &new_listener->listeners_list_head;
                next->prev = &new_listener->listeners_list_head;
                new_listener->listeners_list_head.prev = previous;
                new_listener->listeners_list_head.next = next;
                item = &new_listener->listeners_list_head;
            }
            break;
        }
        previous = item;
        item = next;
    }
    if (item_ptr) {
        *previous_ptr = previous;
        *item_ptr = item;
    }
}

#ifdef HAVE_KQUEUE
static inline void sys_poll_events_with_kqueue(GlobalContext *glb, struct GenericUnixPlatformData *platform, int timeout_ms)
{
    int poll_count = platform->poll_count;
    struct kevent notified[platform->poll_count];
    struct timespec ts;
    struct timespec *ts_ptr = &ts;
    if (timeout_ms < 0) {
        ts_ptr = NULL;
    } else {
        ts.tv_sec = timeout_ms / 1000;
        ts.tv_nsec = (timeout_ms % 1000) * 1000000UL;
    }

    int nbEvents = kevent(platform->kqueue_fd, NULL, 0, notified, poll_count, ts_ptr);
    bool acquired = false;
    for (int i = 0; i < nbEvents; i++) {
        if (notified[i].filter == EVFILT_USER && notified[i].ident == SIGNAL_IDENTIFIER) {
            // We've been signaled.
            continue;
        }
        if (notified[i].filter == EVFILT_READ) {
            if (!acquired) {
                SMP_MUTEX_LOCK(platform->listeners_mutex);
                acquired = true;
            }
            process_listener_handler(glb, (int) notified[i].ident, &platform->listeners, NULL, NULL);
        }
    }
    if (acquired) {
        SMP_MUTEX_UNLOCK(platform->listeners_mutex);
    }
}
#else
static inline void sys_poll_events_with_poll(GlobalContext *glb, struct GenericUnixPlatformData *platform, int timeout_ms)
{
    struct pollfd *fds = platform->fds;
    int poll_count = platform->poll_count;
    if (poll_count < 0) {
        // Means it is dirty and should be rebuilt.
#ifndef AVM_NO_SMP
        poll_count = 1;
        int fd_index = 1;
#else
        poll_count = 0;
        int fd_index = 0;
#endif
        SMP_MUTEX_LOCK(platform->listeners_mutex);
        struct ListHead *item;
        LIST_FOR_EACH (item, &platform->listeners) {
            EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
            int listener_fd = listener->fd;
            if (listener_fd >= 0) {
                poll_count++;
            }
        }

        fds = realloc(fds, sizeof(struct pollfd) * poll_count);
        platform->fds = fds;
        platform->poll_count = poll_count;

#ifndef AVM_NO_SMP
#ifdef HAVE_EVENTFD
        fds[0].fd = platform->signal_fd;
#else
        fds[0].fd = platform->signal_pipe[0];
#endif
        fds[0].events = POLLIN;
        fds[0].revents = 0;
#endif

        LIST_FOR_EACH (item, &platform->listeners) {
            EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
            int listener_fd = listener->fd;
            if (listener_fd >= 0) {
                fds[fd_index].fd = listener_fd;
                fds[fd_index].events = POLLIN;
                fds[fd_index].revents = 0;

                fd_index++;
            }
        }
        SMP_MUTEX_UNLOCK(platform->listeners_mutex);
    }

    int nb_descriptors = poll(fds, poll_count, timeout_ms);
    if (nb_descriptors > 0) {
        SMP_MUTEX_LOCK(platform->listeners_mutex);
        struct ListHead *item = platform->listeners.next;
        struct ListHead *previous = &platform->listeners;
        for (int i = 0; i < poll_count && nb_descriptors > 0; i++) {
            if (!(fds[i].revents & fds[i].events)) {
                continue;
            }
            fds[i].revents = 0;
            nb_descriptors--;

#ifndef AVM_NO_SMP
            if (i == 0) {
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
                continue;
            }
#endif

            process_listener_handler(glb, fds[i].fd, &platform->listeners, &item, &previous);
        }
        SMP_MUTEX_UNLOCK(platform->listeners_mutex);
    }
}
#endif

void sys_poll_events(GlobalContext *glb, int timeout_ms) CLANG_THREAD_SANITIZE_SAFE
{
    struct GenericUnixPlatformData *platform = glb->platform_data;

    // Optimization: do not go into poll(2) or kqueue(2) if we have nothing to
    // wait on. We don't acquire the mutex here for optimization purposes
    if (platform->listeners.next == &platform->listeners && timeout_ms == 0) {
        return;
    }

#ifdef HAVE_KQUEUE
    sys_poll_events_with_kqueue(glb, platform, timeout_ms);
#else
    sys_poll_events_with_poll(glb, platform, timeout_ms);
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

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    TRACE("sys_load_module: Going to load: %s\n", module_name);

    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    MappedFile *beam_file = NULL;

    struct ListHead *item;
    struct ListHead *avmpack_data = synclist_rdlock(&global->avmpack_data);
    LIST_FOR_EACH (item, avmpack_data) {
        struct AVMPackData *avmpack_data = (struct AVMPackData *) item;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    if (IS_NULL_PTR(beam_module)) {
        beam_file = mapped_file_open_beam(module_name);
        if (IS_NULL_PTR(beam_file)) {
            return NULL;
        }
        if (UNLIKELY(!iff_is_valid_beam(beam_file->mapped))) {
            fprintf(stderr, "%s is not a valid BEAM file.\n", module_name);
        }
        beam_module = beam_file->mapped;
        beam_module_size = beam_file->size;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    if (IS_NULL_PTR(new_module)) {
        return NULL;
    }
    new_module->module_platform_data = beam_file;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    if (!strcmp(driver_name, "socket")) {
        return socket_init(glb, opts);
    } else {
#ifdef DYNLOAD_PORT_DRIVERS
        void *handle;
        {
            char port_driver_name[64 + strlen("avm_" "_port_driver.so") + 1];
            snprintf(port_driver_name, sizeof(port_driver_name), "./avm_%s_port_driver.so", driver_name);
            handle = dlopen(port_driver_name, RTLD_NOW);
            if (!handle) {
                return NULL;
            }
        }
        char port_driver_func_name[64 + strlen("_create_port") + 1];
        snprintf(port_driver_func_name, sizeof(port_driver_func_name), "%s_create_port", driver_name);
        create_port_t create_port = (create_port_t) dlsym(handle, port_driver_func_name);
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
#ifndef AVM_NO_SMP
    struct timespec ts = { 0, 0 };
    struct kevent kev;
    EV_SET(&kev, SIGNAL_IDENTIFIER, EVFILT_USER, EV_ADD | EV_CLEAR, 0, 0, NULL);
	if (UNLIKELY(kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts))) {
	    AVM_ABORT();
	}
    platform->poll_count = 1;
#else
    platform->poll_count = 0;
#endif
#else
    platform->poll_count = -1;
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
    free(platform);
}

uint64_t sys_millis(GlobalContext *glb)
{
    UNUSED(glb);
    struct timespec ts;
    if (UNLIKELY(clock_gettime(CLOCK_MONOTONIC, &ts))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }

    return (ts.tv_nsec / 1000000UL) + (ts.tv_sec * 1000UL);
}

void do_register_listener(struct GenericUnixPlatformData *platform, struct EventListener *listener)
{
#ifdef HAVE_KQUEUE
    if (listener->fd) {
        struct timespec ts = { 0, 0 };
        struct kevent kev;
        EV_SET(&kev, listener->fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
        if (UNLIKELY(kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts))) {
            AVM_ABORT();
        }
    }
    platform->poll_count++;
#else
    platform->poll_count = -1;
#endif
}

void sys_register_listener(GlobalContext *global, struct EventListener *listener)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
    SMP_MUTEX_LOCK(platform->listeners_mutex);
    do_register_listener(platform, listener);
#ifndef AVM_NO_SMP
#ifndef HAVE_KQUEUE
    // With kqueue (like epoll), there is no need to restart the call
    sys_signal(global);
#endif
#endif
    list_append(&platform->listeners, &listener->listeners_list_head);
    SMP_MUTEX_UNLOCK(platform->listeners_mutex);
}

static void do_unregister_listener(struct GenericUnixPlatformData *platform, int listener_fd) {
#ifdef HAVE_KQUEUE
    if (listener_fd) {
        struct timespec ts = { 0, 0 };
        struct kevent kev;
        EV_SET(&kev, listener_fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
        (void) kevent(platform->kqueue_fd, &kev, 1, NULL, 0, &ts);
        // File descriptor is automatically removed when closed
    }
    platform->poll_count--;
#else
    platform->poll_count = -1;
#endif
}

void sys_unregister_listener(GlobalContext *global, struct EventListener *listener)
{
    struct GenericUnixPlatformData *platform = global->platform_data;
    SMP_MUTEX_LOCK(platform->listeners_mutex);
    do_unregister_listener(platform, listener->fd);
    list_remove(&listener->listeners_list_head);
    SMP_MUTEX_UNLOCK(platform->listeners_mutex);
}
