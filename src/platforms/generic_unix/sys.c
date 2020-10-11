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

#include "sys.h"
#include "generic_unix_sys.h"

#include "avmpack.h"
#include "defaultatoms.h"
#include "gpio_driver.h"
#include "iff.h"
#include "mapped_file.h"
#include "network.h"
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

    typedef int (*port_driver_init_t)(Context *, term);
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
        abort();
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
    Context *new_ctx = context_new(glb);

    if (!strcmp(driver_name, "socket")) {
        socket_init(new_ctx, opts);
    } else if (!strcmp(driver_name, "network")) {
        network_init(new_ctx, opts);
    } else if (!strcmp(driver_name, "gpio")) {
        gpiodriver_init(new_ctx);
    } else {
#ifdef DYNLOAD_PORT_DRIVERS
        void *handle;
        {
            char port_driver_name[83];
            snprintf(port_driver_name, 83, "./avm_%s_port_driver.so", driver_name);
            handle = dlopen(port_driver_name, RTLD_NOW);
            if (!handle) {
                context_destroy(new_ctx);
                return NULL;
            }
        }
        char port_driver_func_name[81];
        snprintf(port_driver_func_name, 81, "%s_port_driver_init", driver_name);
        port_driver_init_t port_driver_init = dlsym(handle, port_driver_func_name);
        if (!port_driver_init) {
            context_destroy(new_ctx);
            return NULL;
        }
        port_driver_init(new_ctx, opts);
#else
        context_destroy(new_ctx);
        return NULL;
#endif
    }

    return new_ctx;
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
        abort();
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
