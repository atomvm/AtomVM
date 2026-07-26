/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <sys.h>

#include <avmpack.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <scheduler.h>

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/random.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <list.h>
#include <smp.h>
#include <term.h>
#include <trace.h>

#ifdef __wasip2__
#include <poll.h>
#include <stdlib.h>
#include <sys/types.h>

#include <otp_net.h>
#include <otp_socket.h>
#include <resources.h>
#include <synclist.h>
#endif

#if ATOMVM_HAS_MBEDTLS
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#include <mbedtls/platform_time.h>
#if defined(MBEDTLS_VERSION_NUMBER) && (MBEDTLS_VERSION_NUMBER >= 0x03000000)
#include <mbedtls/build_info.h>
#else
#include <mbedtls/config.h>
#endif
#include "otp_ssl.h"
#include "sys_mbedtls.h"
#endif

#include "platform_defaultatoms.h"
#include "wasi_sys.h"

#define WASI_POLL_COUNT_DIRTY (-1)

// Keep the access pattern explicit so the code is ready for a SMP + network
#ifndef AVM_NO_SMP
#include <stdatomic.h>
#define WASI_POLL_COUNT_LOAD(p) atomic_load_explicit((p), memory_order_acquire)
#define WASI_POLL_COUNT_STORE(p, v) atomic_store_explicit((p), (v), memory_order_release)
#else
#define WASI_POLL_COUNT_LOAD(p) (*(p))
#define WASI_POLL_COUNT_STORE(p, v) (*(p) = (v))
#endif

struct WASIPlatformData
{
#ifndef AVM_NO_SMP
    pthread_mutex_t poll_mutex;
    pthread_cond_t poll_cond;
    bool should_poll;
#if ATOMVM_HAS_MBEDTLS
    Mutex *entropy_mutex;
    Mutex *random_mutex;
#endif
#endif
#ifdef __wasip2__
    struct pollfd *fds;
    int ATOMIC listeners_poll_count;
    int ATOMIC select_events_poll_count;
#endif
#if ATOMVM_HAS_MBEDTLS
    mbedtls_entropy_context entropy_ctx;
    bool entropy_is_initialized;
    mbedtls_ctr_drbg_context random_ctx;
    bool random_is_initialized;
#endif
};

#if ATOMVM_HAS_MBEDTLS
static inline void wasi_entropy_lock(struct WASIPlatformData *platform)
{
#ifndef AVM_NO_SMP
    SMP_MUTEX_LOCK(platform->entropy_mutex);
#else
    UNUSED(platform);
#endif
}

static inline void wasi_entropy_unlock(struct WASIPlatformData *platform)
{
#ifndef AVM_NO_SMP
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);
#else
    UNUSED(platform);
#endif
}

static inline void wasi_random_lock(struct WASIPlatformData *platform)
{
#ifndef AVM_NO_SMP
    SMP_MUTEX_LOCK(platform->random_mutex);
#else
    UNUSED(platform);
#endif
}

static inline void wasi_random_unlock(struct WASIPlatformData *platform)
{
#ifndef AVM_NO_SMP
    SMP_MUTEX_UNLOCK(platform->random_mutex);
#else
    UNUSED(platform);
#endif
}
#endif

#ifdef __wasip2__
static void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *global);
static void listener_event_remove_from_polling_set(listener_event_t listener_fd, GlobalContext *global);
static bool event_listener_is_event(EventListener *listener, listener_event_t event);

#include <listeners.h>
#endif

void sys_init_platform(GlobalContext *glb)
{
    struct WASIPlatformData *platform = malloc(sizeof(struct WASIPlatformData));
    if (IS_NULL_PTR(platform)) {
        fprintf(stderr, "Cannot allocate platform data\n");
        AVM_ABORT();
    }
#ifndef AVM_NO_SMP
    if (UNLIKELY(pthread_mutex_init(&platform->poll_mutex, NULL))) {
        fprintf(stderr, "Cannot initialize pthread_mutex\n");
        AVM_ABORT();
    }
    pthread_condattr_t cond_attr;
    if (UNLIKELY(pthread_condattr_init(&cond_attr))) {
        fprintf(stderr, "Cannot initialize pthread_condattr\n");
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_condattr_setclock(&cond_attr, CLOCK_MONOTONIC))) {
        fprintf(stderr, "Cannot set pthread_condattr clock\n");
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_cond_init(&platform->poll_cond, &cond_attr))) {
        fprintf(stderr, "Cannot initialize pthread_cond\n");
        AVM_ABORT();
    }
    pthread_condattr_destroy(&cond_attr);
    platform->should_poll = false;
#if ATOMVM_HAS_MBEDTLS
    platform->entropy_mutex = smp_mutex_create();
    if (IS_NULL_PTR(platform->entropy_mutex)) {
        AVM_ABORT();
    }
    platform->random_mutex = smp_mutex_create();
    if (IS_NULL_PTR(platform->random_mutex)) {
        AVM_ABORT();
    }
#endif
#endif
#ifdef __wasip2__
    platform->fds = NULL;
    WASI_POLL_COUNT_STORE(&platform->listeners_poll_count, WASI_POLL_COUNT_DIRTY);
    WASI_POLL_COUNT_STORE(&platform->select_events_poll_count, WASI_POLL_COUNT_DIRTY);
#endif
#if ATOMVM_HAS_MBEDTLS
    platform->entropy_is_initialized = false;
    platform->random_is_initialized = false;
#endif

    glb->platform_data = platform;

#ifdef __wasip2__
    otp_net_init(glb);
    otp_socket_init(glb);
#if ATOMVM_HAS_MBEDTLS
    otp_ssl_init(glb);
#endif
#endif
}

void sys_free_platform(GlobalContext *glb)
{
    struct WASIPlatformData *platform = glb->platform_data;
#ifndef AVM_NO_SMP
    pthread_cond_destroy(&platform->poll_cond);
    pthread_mutex_destroy(&platform->poll_mutex);
#if ATOMVM_HAS_MBEDTLS
    smp_mutex_destroy(platform->entropy_mutex);
    smp_mutex_destroy(platform->random_mutex);
#endif
#endif
#ifdef __wasip2__
    free(platform->fds);
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

static void sleep_ms(int timeout_ms)
{
    if (timeout_ms <= 0) {
        return;
    }
    struct timespec req;
    req.tv_sec = timeout_ms / 1000;
    req.tv_nsec = (long) (timeout_ms % 1000) * 1000000L;
    nanosleep(&req, NULL);
}

#ifdef __wasip2__
static void sys_poll_events_with_poll(GlobalContext *glb, int timeout_ms)
{
    struct WASIPlatformData *platform = glb->platform_data;
    struct pollfd *fds = platform->fds;
    int listeners_poll_count = WASI_POLL_COUNT_LOAD(&platform->listeners_poll_count);
    int select_events_poll_count = WASI_POLL_COUNT_LOAD(&platform->select_events_poll_count);

    if (listeners_poll_count < 0 || select_events_poll_count < 0) {
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
        if (listeners_poll_count < 0) {
            listeners = synclist_rdlock(&glb->listeners);
            LIST_FOR_EACH (item, listeners) {
                EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
                if (listener->fd >= 0) {
                    listeners_new_count++;
                }
            }
        } else {
            listeners_new_count = listeners_poll_count;
        }

        size_t new_size = sizeof(struct pollfd) * (select_events_new_count + listeners_new_count);
        struct pollfd *new_fds = realloc(fds, new_size);
        if (UNLIKELY(new_fds == NULL && new_size > 0)) {
            /* Original fds is still valid; leave platform->fds untouched. */
            fprintf(stderr, "Cannot allocate pollfd array\n");
            AVM_ABORT();
        }
        fds = new_fds;
        platform->fds = fds;

        int fd_index = 0;
        if (listeners_poll_count < 0) {
            LIST_FOR_EACH (item, listeners) {
                EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
                if (listener->fd >= 0) {
                    fds[fd_index].fd = listener->fd;
                    fds[fd_index].events = POLLIN;
                    fds[fd_index].revents = 0;
                    fd_index++;
                }
            }
            WASI_POLL_COUNT_STORE(&platform->listeners_poll_count, (int) listeners_new_count);
            synclist_unlock(&glb->listeners);
        } else {
            fd_index += listeners_new_count;
        }

        LIST_FOR_EACH (item, select_events) {
            struct SelectEvent *select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
            if (select_event->read || select_event->write) {
                fds[fd_index].fd = select_event->event;
                fds[fd_index].events = (select_event->read ? POLLIN : 0) | (select_event->write ? POLLOUT : 0);
                fds[fd_index].revents = 0;
                fd_index++;
            }
        }
        WASI_POLL_COUNT_STORE(&platform->select_events_poll_count, (int) select_events_new_count);
        synclist_unlock(&glb->select_events);

        listeners_poll_count = listeners_new_count;
        select_events_poll_count = select_events_new_count;
    }

    int poll_count = listeners_poll_count + select_events_poll_count;
    if (poll_count == 0) {
        if (timeout_ms > 0) {
            sleep_ms(timeout_ms);
        }
        return;
    }

    int nb_descriptors = poll(fds, poll_count, timeout_ms);
    if (UNLIKELY(nb_descriptors < 0)) {
        if (errno == EINTR) {
            return;
        }
        // EBADF / EINVAL / EFAULT here mean a listener or select-event fd has
        // gone bad: the cached pollfd array is no longer valid.
        fprintf(stderr, "poll() failed: errno=%i\n", errno);
        WASI_POLL_COUNT_STORE(&platform->listeners_poll_count, WASI_POLL_COUNT_DIRTY);
        WASI_POLL_COUNT_STORE(&platform->select_events_poll_count, WASI_POLL_COUNT_DIRTY);
        return;
    }

    // poll() also signals POLLERR / POLLHUP / POLLNVAL in revents even if
    // we did not request them in events; treat those as wake-ups so the
    // waiting handler can observe the error / peer-close
    const short revents_mask = POLLIN | POLLOUT | POLLERR | POLLHUP | POLLNVAL;

    int fd_index = 0;
    if (nb_descriptors > 0) {
        struct ListHead *listeners = synclist_wrlock(&glb->listeners);
        struct ListHead *item = listeners->next;
        struct ListHead *previous = listeners;
        for (int i = 0; i < listeners_poll_count && nb_descriptors > 0; i++, fd_index++) {
            if (!(fds[fd_index].revents & revents_mask)) {
                continue;
            }
            fds[fd_index].revents = 0;
            nb_descriptors--;
            process_listener_handler(glb, fds[fd_index].fd, listeners, &item, &previous);
        }
        synclist_unlock(&glb->listeners);
    } else {
        fd_index += listeners_poll_count;
    }

    for (int i = 0; i < select_events_poll_count && nb_descriptors > 0; i++, fd_index++) {
        if (!(fds[fd_index].revents & revents_mask)) {
            continue;
        }
        short revents = fds[fd_index].revents;
        bool error_event = revents & (POLLERR | POLLHUP | POLLNVAL);
        bool is_read = (revents & POLLIN) || error_event;
        bool is_write = (revents & POLLOUT) || error_event;
        fds[fd_index].revents = 0;
        nb_descriptors--;
        select_event_notify(fds[fd_index].fd, is_read, is_write, glb);
    }
}
#endif

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
#ifdef __wasip2__
    if (timeout_ms == 0
        && synclist_is_empty(&glb->listeners)
        && synclist_is_empty(&glb->select_events)) {
        return;
    }
    sys_poll_events_with_poll(glb, timeout_ms);
#elif !defined(AVM_NO_SMP)
    struct WASIPlatformData *platform = glb->platform_data;

    if (timeout_ms > 0) {
        struct timespec abstime;
        sys_monotonic_time(&abstime);
        int timeout_secs_part = timeout_ms / 1000;
        int timeout_ms_part = timeout_ms - (timeout_secs_part * 1000);
        abstime.tv_nsec += timeout_ms_part * 1000000;
        if (abstime.tv_nsec >= 1000000000) {
            timeout_secs_part += 1;
            abstime.tv_nsec -= 1000000000;
        }
        abstime.tv_sec += timeout_secs_part;
        pthread_mutex_lock(&platform->poll_mutex);
        if (!platform->should_poll) {
            (void) pthread_cond_timedwait(&platform->poll_cond, &platform->poll_mutex, &abstime);
        }
    } else if (timeout_ms < 0) {
        pthread_mutex_lock(&platform->poll_mutex);
        if (!platform->should_poll) {
            (void) pthread_cond_wait(&platform->poll_cond, &platform->poll_mutex);
        }
    } else {
        pthread_mutex_lock(&platform->poll_mutex);
    }
    platform->should_poll = false;
    pthread_mutex_unlock(&platform->poll_mutex);
#else
    sleep_ms(timeout_ms);
    UNUSED(glb);
#endif
}

#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
void sys_signal(GlobalContext *glb)
{
#ifndef AVM_NO_SMP
    struct WASIPlatformData *platform = glb->platform_data;
    pthread_mutex_lock(&platform->poll_mutex);
    platform->should_poll = true;
    pthread_cond_signal(&platform->poll_cond);
    pthread_mutex_unlock(&platform->poll_mutex);
#else
    UNUSED(glb);
#endif
}
#endif

void sys_wakeup(GlobalContext *glb)
{
#ifndef AVM_NO_SMP
    struct WASIPlatformData *platform = glb->platform_data;
    pthread_mutex_lock(&platform->poll_mutex);
    platform->should_poll = true;
    pthread_cond_signal(&platform->poll_cond);
    pthread_mutex_unlock(&platform->poll_mutex);
#else
    UNUSED(glb);
#endif
}

// No-op tzset for nifs.c compatibility
void tzset(void) {}

#ifdef __wasip2__
static void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *global)
{
    struct WASIPlatformData *platform = global->platform_data;
    UNUSED(listener);
    WASI_POLL_COUNT_STORE(&platform->listeners_poll_count, WASI_POLL_COUNT_DIRTY);
}

static void listener_event_remove_from_polling_set(listener_event_t listener_fd, GlobalContext *global)
{
    struct WASIPlatformData *platform = global->platform_data;
    UNUSED(listener_fd);
    WASI_POLL_COUNT_STORE(&platform->listeners_poll_count, WASI_POLL_COUNT_DIRTY);
}

bool event_listener_is_event(EventListener *listener, listener_event_t event)
{
    return listener->fd == event;
}

void sys_register_listener(GlobalContext *global, struct EventListener *listener)
{
    struct ListHead *listeners = synclist_wrlock(&global->listeners);
    event_listener_add_to_polling_set(listener, global);
    list_append(listeners, &listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

void sys_unregister_listener(GlobalContext *global, struct EventListener *listener)
{
    listener_event_remove_from_polling_set(listener->fd, global);
    synclist_remove(&global->listeners, &listener->listeners_list_head);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    struct WASIPlatformData *platform = global->platform_data;
    UNUSED(event);
    UNUSED(is_write);
    WASI_POLL_COUNT_STORE(&platform->select_events_poll_count, WASI_POLL_COUNT_DIRTY);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    struct WASIPlatformData *platform = global->platform_data;
    UNUSED(event);
    UNUSED(is_write);
    WASI_POLL_COUNT_STORE(&platform->select_events_poll_count, WASI_POLL_COUNT_DIRTY);
}
#else
void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_register_listener(GlobalContext *global, EventListener *listener)
{
    UNUSED(global);
    UNUSED(listener);
}

void sys_unregister_listener(GlobalContext *global, EventListener *listener)
{
    UNUSED(global);
    UNUSED(listener);
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

uint64_t sys_monotonic_time_u64(void)
{
    struct timespec ts;
    if (UNLIKELY(clock_gettime(CLOCK_MONOTONIC, &ts))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }
    return (uint64_t) ts.tv_sec * 1000000000ULL + (uint64_t) ts.tv_nsec;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms * 1000000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t / 1000000;
}

static void load_file_log_error(const char *path, const char *what, int err)
{
    if (err != 0) {
        fprintf(stderr, "load_file: %s failed for %s: %s\n", what, path, strerror(err));
    } else {
        fprintf(stderr, "load_file: %s failed for %s\n", what, path);
    }
}

static void *load_file(const char *path, size_t *out_size)
{
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        load_file_log_error(path, "open", errno);
        return NULL;
    }
    off_t fsize = lseek(fd, 0, SEEK_END);
    if (fsize < 0) {
        load_file_log_error(path, "lseek", errno);
        close(fd);
        return NULL;
    }
    lseek(fd, 0, SEEK_SET);
    void *data = malloc((size_t) fsize);
    if (IS_NULL_PTR(data)) {
        load_file_log_error(path, "malloc", 0);
        close(fd);
        return NULL;
    }
    size_t remaining = (size_t) fsize;
    uint8_t *ptr = (uint8_t *) data;
    while (remaining > 0) {
        ssize_t r = read(fd, ptr, remaining);
        if (UNLIKELY(r <= 0)) {
            load_file_log_error(path, r < 0 ? "read" : "read (short)", r < 0 ? errno : 0);
            close(fd);
            free(data);
            return NULL;
        }
        ptr += r;
        remaining -= (size_t) r;
    }
    close(fd);
    if (out_size) {
        *out_size = (size_t) fsize;
    }
    return data;
}

enum OpenAVMResult sys_open_avm_from_file(
    GlobalContext *global, const char *path, struct AVMPackData **avm_data)
{
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);
    UNUSED(global);

    size_t size;
    void *data = load_file(path, &size);
    if (IS_NULL_PTR(data)) {
        return AVM_OPEN_CANNOT_OPEN;
    }
    if (UNLIKELY(!avmpack_is_valid(data, size))) {
        free(data);
        return AVM_OPEN_INVALID;
    }

    struct ConstAVMPack *const_avm = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(const_avm)) {
        free(data);
        return AVM_OPEN_FAILED_ALLOC;
    }
    avmpack_data_init(&const_avm->base, &const_avm_pack_info);
    const_avm->base.data = (const uint8_t *) data;

    *avm_data = &const_avm->base;
    return AVM_OPEN_OK;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    TRACE("sys_load_module_from_file: Going to load: %s\n", path);

    size_t size;
    void *data = load_file(path, &size);
    if (IS_NULL_PTR(data)) {
        return NULL;
    }
    if (UNLIKELY(!iff_is_valid_beam(data))) {
        fprintf(stderr, "%s is not a valid BEAM file.\n", path);
        free(data);
        return NULL;
    }
    Module *new_module = module_new_from_iff_binary(global, data, size);
    if (IS_NULL_PTR(new_module)) {
        free(data);
        return NULL;
    }
    new_module->module_platform_data = data;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    UNUSED(glb);
    UNUSED(driver_name);
    UNUSED(opts);
    return NULL;
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}

#if ATOMVM_HAS_MBEDTLS

mbedtls_ms_time_t mbedtls_ms_time(void)
{
    struct timespec ts;
    if (UNLIKELY(clock_gettime(CLOCK_MONOTONIC, &ts) != 0)) {
        AVM_ABORT();
    }
    return (mbedtls_ms_time_t) ts.tv_sec * 1000 + (mbedtls_ms_time_t) ts.tv_nsec / 1000000;
}

int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size)
{
#ifndef MBEDTLS_THREADING_C
    struct WASIPlatformData *platform
        = CONTAINER_OF(entropy, struct WASIPlatformData, entropy_ctx);
    wasi_entropy_lock(platform);
    int result = mbedtls_entropy_func(entropy, buf, size);
    wasi_entropy_unlock(platform);
    return result;
#else
    return mbedtls_entropy_func(entropy, buf, size);
#endif
}

mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global)
{
    struct WASIPlatformData *platform = global->platform_data;
    wasi_entropy_lock(platform);
    if (!platform->entropy_is_initialized) {
        mbedtls_entropy_init(&platform->entropy_ctx);
        platform->entropy_is_initialized = true;
    }
    return &platform->entropy_ctx;
}

void sys_mbedtls_entropy_context_unlock(GlobalContext *global)
{
    struct WASIPlatformData *platform = global->platform_data;
    wasi_entropy_unlock(platform);
}

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global)
{
    struct WASIPlatformData *platform = global->platform_data;
    wasi_random_lock(platform);
    if (!platform->random_is_initialized) {
        mbedtls_ctr_drbg_init(&platform->random_ctx);
        mbedtls_entropy_context *entropy_ctx = sys_mbedtls_get_entropy_context_lock(global);
        sys_mbedtls_entropy_context_unlock(global);
        const char *seed = "AtomVM Mbed-TLS initial seed.";
        int seed_len = strlen(seed);
        int seed_err = mbedtls_ctr_drbg_seed(&platform->random_ctx, sys_mbedtls_entropy_func,
            entropy_ctx, (const unsigned char *) seed, seed_len);
        if (UNLIKELY(seed_err != 0)) {
            wasi_random_unlock(platform);
            AVM_ABORT();
        }
        platform->random_is_initialized = true;
    }
    return &platform->random_ctx;
}

void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global)
{
    struct WASIPlatformData *platform = global->platform_data;
    wasi_random_unlock(platform);
}

int mbedtls_hardware_poll(void *data, unsigned char *output, size_t len, size_t *olen)
{
    UNUSED(data);

    if (UNLIKELY(wasi_get_random(output, len) != 0)) {
        return -1;
    }
    *olen = len;
    return 0;
}

#endif /* ATOMVM_HAS_MBEDTLS */

int wasi_get_random(uint8_t *output, size_t len)
{
    size_t total = 0;
    while (total < len) {
        size_t chunk = len - total;
        if (chunk > 256) {
            chunk = 256;
        }
        if (UNLIKELY(getentropy(output + total, chunk) != 0)) {
            return -1;
        }
        total += chunk;
    }
    return 0;
}
