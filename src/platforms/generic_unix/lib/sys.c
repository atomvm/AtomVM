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
#include "utils.h"

#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#ifdef DYNLOAD_PORT_DRIVERS
    #include <dlfcn.h>

    typedef Context *(*create_port_t)(GlobalContext *global, term opts);
#endif

#include "trace.h"

static volatile uint32_t millis;
static bool has_signal_handler;

static void alarm_handler(int sig);

//#define USE_SELECT
#ifdef USE_SELECT
#include <socket.h>
#else
#endif

void sys_consume_pending_events(GlobalContext *glb)
{
    struct GenericUnixPlatformData *platform = glb->platform_data;
    struct ListHead *listeners_list = platform->listeners;

    if (!platform->listeners) {
        return;
    }

    EventListener *listeners = GET_LIST_ENTRY(listeners_list, EventListener, listeners_list_head);

    EventListener *listener = listeners;

    int fds_count = 0;

    do {
        int listener_fd = listener->fd;
        if (listener_fd >= 0) {
            fds_count++;
        }
        listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
    } while (listeners != NULL && listener != listeners);

    if (fds_count == 0) {
        return;
    }

    listeners = GET_LIST_ENTRY(listeners_list, EventListener, listeners_list_head);

    listener = listeners;

    struct pollfd *fds = malloc(fds_count * sizeof(struct pollfd));
    int fd_index = 0;

    do {
        int listener_fd = listener->fd;
        if (listener_fd >= 0) {
            fds[fd_index].fd = listener_fd;
            fds[fd_index].events = POLLIN;
            fds[fd_index].revents = 0;

            fd_index++;
        }
        listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
    } while (listeners != NULL && listener != listeners);

    listeners = GET_LIST_ENTRY(listeners_list, EventListener, listeners_list_head);

    if (poll(fds, fd_index, 0) > 0) {
        for (int i = 0; i < fd_index; i++) {
            if (!(fds[i].revents & fds[i].events)) {
                continue;
            }

            int current_fd = fds[i].fd;

            EventListener *listener = listeners;

            if (!listener) {
                fprintf(stderr, "warning: no listeners.\n");
                free(fds);
                return;
            }

            do {
                if (listener->fd == current_fd) {
                    listener->handler(listener);
                    break;
                }
                listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
            } while (listeners != NULL && listener != listeners);
        }
    }

    free(fds);
}

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
    LIST_FOR_EACH (item, &global->avmpack_data) {
        struct AVMPackData *avmpack_data = (struct AVMPackData *) item;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }

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
    // this is required since GlobalContext might be re-created again (which is what happens while
    // executing tests).
    millis = 0;

    struct GenericUnixPlatformData *platform = malloc(sizeof(struct GenericUnixPlatformData));
    if (UNLIKELY(!platform)) {
        AVM_ABORT();
    }
    platform->listeners = 0;
    global->platform_data = platform;
}

void sys_start_millis_timer()
{
    if (!has_signal_handler) {
        struct sigaction saction = {
            .sa_handler = alarm_handler,
            .sa_flags = SA_RESTART
        };

        sigaction(SIGALRM, &saction, NULL);
    }

    struct itimerval ival = {
        .it_interval = {
            .tv_sec = 0,
            .tv_usec = 1000
        },
        .it_value = {
            .tv_sec = 0,
            .tv_usec = 1000
        }
    };

    setitimer(ITIMER_REAL, &ival, NULL);
}

void sys_stop_millis_timer()
{
    struct itimerval ival = {
        .it_interval = {
            .tv_sec = 0,
            .tv_usec = 0
        },
        .it_value = {
            .tv_sec = 0,
            .tv_usec = 0
        }
    };

    setitimer(ITIMER_REAL, &ival, NULL);
}

uint32_t sys_millis()
{
    return millis;
}

void sys_sleep(GlobalContext *glb)
{
    UNUSED(glb);
}

static void alarm_handler(int sig)
{
    UNUSED(sig);
    millis++;
}
