/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

/*
 * RTEMS port of the NimBLE porting layer used by the ESP32-C5 Bluetooth
 * controller.
 *
 * The controller library calls the NPL through a function table registered
 * with esp_register_npl_funcs(), so the on-wire ABI is the layout of struct
 * npl_funcs_t, not the names of these functions.  Event queues are linked
 * lists guarded by an interrupt lock, with a binary semaphore used only to
 * wake the controller task. Producers may also run in interrupt context;
 * callouts run on the shared RTEMS timer server.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <rtems.h>
#include <rtems/rtems/semimpl.h>
#include <rtems/score/coresemimpl.h>

#include "nimble/nimble_npl.h"

#define RTEMS_NPL_FOREVER UINT32_MAX

typedef struct rtems_npl_event
{
    struct ble_npl_event *ev;
    ble_npl_event_fn *fn;
    void *arg;
    struct rtems_npl_event *next;
    struct rtems_npl_eventq *queue;
    bool queued;
} rtems_npl_event_t;

typedef struct rtems_npl_eventq
{
    rtems_npl_event_t *head;
    rtems_npl_event_t *tail;
    rtems_id sem;
} rtems_npl_eventq_t;

typedef struct rtems_npl_callout
{
    rtems_id timer;
    struct ble_npl_eventq *evq;
    struct ble_npl_event ev;
    rtems_npl_event_t *event;
    ble_npl_time_t deadline;
    bool active;
    bool running;
    bool deleted;
    uintptr_t cookie;
    struct rtems_npl_callout *next;
} rtems_npl_callout_t;

typedef struct
{
    rtems_id id;
    rtems_id owner;
    uint32_t depth;
} rtems_npl_mutex_t;

typedef struct
{
    rtems_id id;
} rtems_npl_sem_t;

static bool npl_rtems_os_started(void)
{
    return true;
}

static void *npl_rtems_get_current_task_id(void)
{
    return (void *) (uintptr_t) rtems_task_self();
}

/* RTEMS 7 exposes no Classic semaphore count directive. Read the same core
 * count as obtain/release under its queue lock, rather than a racy cache. */
static uint16_t npl_rtems_sem_count(rtems_id id)
{
    Thread_queue_Context context;
    Semaphore_Control *sem = _Semaphore_Get(id, &context);
    if (sem == NULL) {
        return 0;
    }
    _CORE_semaphore_Acquire_critical(&sem->Core_control.Semaphore, &context);
    uint32_t count = _CORE_semaphore_Get_count(&sem->Core_control.Semaphore);
    _CORE_semaphore_Release(&sem->Core_control.Semaphore, &context);
    return count > UINT16_MAX ? UINT16_MAX : (uint16_t) count;
}

static void npl_rtems_event_unlink(rtems_npl_event_t *event)
{
    rtems_interrupt_level level;
    rtems_interrupt_disable(level);
    rtems_npl_eventq_t *q = event->queue;
    if (q != NULL) {
        rtems_npl_event_t **link = &q->head;
        rtems_npl_event_t *previous = NULL;
        while (*link != NULL && *link != event) {
            previous = *link;
            link = &previous->next;
        }
        if (*link == event) {
            *link = event->next;
            if (q->tail == event) {
                q->tail = previous;
            }
        }
    }
    event->queue = NULL;
    event->next = NULL;
    event->queued = false;
    rtems_interrupt_enable(level);
}

/* === events ============================================================ */

static void npl_rtems_event_init(
    struct ble_npl_event *ev,
    ble_npl_event_fn *fn,
    void *arg)
{
    rtems_npl_event_t *event;

    if (ev->event == NULL) {
        ev->event = calloc(1, sizeof(*event));
    }
    event = ev->event;
    if (event == NULL) {
        return;
    }

    npl_rtems_event_unlink(event);
    event->ev = ev;
    event->fn = fn;
    event->arg = arg;
}

static void npl_rtems_event_deinit(struct ble_npl_event *ev)
{
    if (ev->event != NULL) {
        npl_rtems_event_unlink(ev->event);
    }
    free(ev->event);
    ev->event = NULL;
}

static void npl_rtems_event_reset(struct ble_npl_event *ev)
{
    rtems_npl_event_t *event = ev->event;

    if (event != NULL) {
        npl_rtems_event_unlink(event);
    }
}

static bool npl_rtems_event_is_queued(struct ble_npl_event *ev)
{
    rtems_npl_event_t *event = ev->event;

    return event != NULL && event->queued;
}

static void *npl_rtems_event_get_arg(struct ble_npl_event *ev)
{
    rtems_npl_event_t *event = ev->event;

    return event != NULL ? event->arg : NULL;
}

static void npl_rtems_event_set_arg(struct ble_npl_event *ev, void *arg)
{
    rtems_npl_event_t *event = ev->event;

    if (event != NULL) {
        event->arg = arg;
    }
}

static void npl_rtems_event_run(struct ble_npl_event *ev)
{
    rtems_npl_event_t *event = ev->event;

    if (event != NULL && event->fn != NULL) {
        event->fn(ev);
    }
}

/* === event queues ====================================================== */

static void npl_rtems_eventq_init(struct ble_npl_eventq *evq)
{
    rtems_npl_eventq_t *eventq;

    if (evq->eventq != NULL) {
        eventq = evq->eventq;
        while (eventq->head != NULL) {
            npl_rtems_event_unlink(eventq->head);
        }
        (void) rtems_semaphore_obtain(eventq->sem, RTEMS_NO_WAIT, 0);
        return;
    }
    evq->eventq = calloc(1, sizeof(*eventq));
    eventq = evq->eventq;
    if (eventq == NULL) {
        return;
    }

    eventq->head = NULL;
    eventq->tail = NULL;
    if (rtems_semaphore_create(
            rtems_build_name('B', 'T', 'E', 'Q'),
            0,
            RTEMS_SIMPLE_BINARY_SEMAPHORE | RTEMS_PRIORITY,
            0,
            &eventq->sem)
        != RTEMS_SUCCESSFUL) {
        free(eventq);
        evq->eventq = NULL;
    }
}

static void npl_rtems_eventq_deinit(struct ble_npl_eventq *evq)
{
    rtems_npl_eventq_t *eventq = evq->eventq;

    if (eventq == NULL) {
        return;
    }
    while (eventq->head != NULL) {
        npl_rtems_event_unlink(eventq->head);
    }
    if (eventq->sem != RTEMS_INVALID_ID) {
        rtems_semaphore_delete(eventq->sem);
    }
    free(eventq);
    evq->eventq = NULL;
}

/* Caller holds the interrupt lock. Return a wakeup ID to release after
 * restoring interrupts: RTEMS may dispatch a waiting task on release. */
static rtems_id npl_rtems_eventq_link(
    struct ble_npl_eventq *evq, struct ble_npl_event *ev, bool at_front)
{
    rtems_npl_eventq_t *eventq = evq->eventq;
    rtems_npl_event_t *event = ev->event;
    if (eventq == NULL || event == NULL || event->queued) {
        return RTEMS_INVALID_ID;
    }
    event->queued = true;
    event->queue = eventq;
    if (at_front) {
        event->next = eventq->head;
        eventq->head = event;
        if (eventq->tail == NULL) {
            eventq->tail = event;
        }
    } else if (eventq->tail != NULL) {
        event->next = NULL;
        eventq->tail->next = event;
        eventq->tail = event;
    } else {
        event->next = NULL;
        eventq->head = event;
        eventq->tail = event;
    }
    return eventq->sem;
}

static void npl_rtems_eventq_enqueue(
    struct ble_npl_eventq *evq, struct ble_npl_event *ev, bool at_front)
{
    rtems_interrupt_level level;
    rtems_interrupt_disable(level);
    rtems_id sem = npl_rtems_eventq_link(evq, ev, at_front);
    rtems_interrupt_enable(level);
    if (sem != RTEMS_INVALID_ID) {
        (void) rtems_semaphore_release(sem);
    }
}

static void npl_rtems_eventq_put(struct ble_npl_eventq *evq, struct ble_npl_event *ev)
{
    npl_rtems_eventq_enqueue(evq, ev, false);
}

static void npl_rtems_eventq_put_to_front(struct ble_npl_eventq *evq, struct ble_npl_event *ev)
{
    npl_rtems_eventq_enqueue(evq, ev, true);
}

static struct ble_npl_event *npl_rtems_eventq_get(
    struct ble_npl_eventq *evq,
    ble_npl_time_t tmo)
{
    rtems_npl_eventq_t *eventq = evq->eventq;
    ble_npl_time_t start = (ble_npl_time_t) rtems_clock_get_ticks_since_boot();

    if (eventq == NULL) {
        return NULL;
    }

    for (;;) {
        rtems_npl_event_t *event;
        rtems_interrupt_level level;

        /* Notifications can coalesce: the list, not the semaphore count,
         * determines whether an event is ready. */
        rtems_interrupt_disable(level);
        event = eventq->head;
        if (event != NULL) {
            eventq->head = event->next;
            if (eventq->head == NULL) {
                eventq->tail = NULL;
            }
            event->next = NULL;
            event->queued = false;
            event->queue = NULL;
        }
        rtems_interrupt_enable(level);

        if (event != NULL) {
            return event->ev;
        }
        if (tmo == 0 || eventq->sem == RTEMS_INVALID_ID) {
            return NULL;
        }

        rtems_interval timeout = RTEMS_NO_TIMEOUT;
        if (tmo != RTEMS_NPL_FOREVER) {
            ble_npl_time_t elapsed = (ble_npl_time_t) rtems_clock_get_ticks_since_boot() - start;

            if (elapsed >= tmo) {
                return NULL;
            }
            timeout = tmo - elapsed;
        }
        /* A stale notification only causes another list check. Keep the
         * original timeout across such wakeups. */
        if (rtems_semaphore_obtain(eventq->sem, RTEMS_WAIT, timeout)
            != RTEMS_SUCCESSFUL) {
            return NULL;
        }
    }
}

static void npl_rtems_eventq_remove(
    struct ble_npl_eventq *evq,
    struct ble_npl_event *ev)
{
    rtems_npl_eventq_t *eventq = evq->eventq;
    rtems_npl_event_t *event = ev->event;
    if (event != NULL && event->queue == eventq) {
        npl_rtems_event_unlink(event);
    }
}

static bool npl_rtems_eventq_is_empty(struct ble_npl_eventq *evq)
{
    rtems_npl_eventq_t *eventq = evq->eventq;

    return eventq == NULL || eventq->head == NULL;
}

/* === mutexes =========================================================== */

static ble_npl_error_t npl_rtems_mutex_init(struct ble_npl_mutex *mu)
{
    rtems_npl_mutex_t *mutex;

    if (mu->mutex != NULL) {
        return BLE_NPL_OK;
    }
    mu->mutex = calloc(1, sizeof(*mutex));
    mutex = mu->mutex;
    if (mutex == NULL) {
        return BLE_NPL_ENOMEM;
    }

    if (rtems_semaphore_create(
            rtems_build_name('B', 'T', 'M', 'X'),
            1,
            RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY | RTEMS_INHERIT_PRIORITY,
            0,
            &mutex->id)
        != RTEMS_SUCCESSFUL) {
        free(mutex);
        mu->mutex = NULL;
        return BLE_NPL_ERROR;
    }
    return BLE_NPL_OK;
}

static ble_npl_error_t npl_rtems_mutex_deinit(struct ble_npl_mutex *mu)
{
    rtems_npl_mutex_t *mutex = mu->mutex;

    if (mutex == NULL) {
        return BLE_NPL_OK;
    }
    rtems_semaphore_delete(mutex->id);
    free(mutex);
    mu->mutex = NULL;
    return BLE_NPL_OK;
}

static ble_npl_error_t npl_rtems_semaphore_obtain(
    rtems_id id,
    ble_npl_time_t timeout)
{
    rtems_option option;
    rtems_interval interval;
    rtems_status_code sc;

    if (timeout == 0) {
        option = RTEMS_NO_WAIT;
        interval = 0;
    } else if (timeout == RTEMS_NPL_FOREVER) {
        option = RTEMS_WAIT;
        interval = RTEMS_NO_TIMEOUT;
    } else {
        option = RTEMS_WAIT;
        interval = timeout;
    }

    sc = rtems_semaphore_obtain(id, option, interval);
    if (sc == RTEMS_SUCCESSFUL) {
        return BLE_NPL_OK;
    }
    /* FreeRTOS NPL reports every failed take as BLE_NPL_TIMEOUT. */
    return BLE_NPL_TIMEOUT;
}

static ble_npl_error_t npl_rtems_mutex_pend(
    struct ble_npl_mutex *mu,
    ble_npl_time_t timeout)
{
    rtems_npl_mutex_t *mutex = mu->mutex;
    rtems_id self = rtems_task_self();
    rtems_interrupt_level level;
    ble_npl_error_t rc;

    if (mutex == NULL) {
        return BLE_NPL_BAD_MUTEX;
    }

    /* FreeRTOS uses xSemaphoreTakeRecursive(); RTEMS 7 has no recursive
     * binary semaphore attribute, so track the owner and nesting depth. */
    rtems_interrupt_disable(level);
    if (mutex->owner == self) {
        mutex->depth++;
        rtems_interrupt_enable(level);

        return BLE_NPL_OK;
    }
    rtems_interrupt_enable(level);

    rc = npl_rtems_semaphore_obtain(mutex->id, timeout);
    if (rc == BLE_NPL_OK) {
        rtems_interrupt_disable(level);
        mutex->owner = self;
        mutex->depth = 1;
        rtems_interrupt_enable(level);
    }

    return rc;
}

static ble_npl_error_t npl_rtems_mutex_release(struct ble_npl_mutex *mu)
{
    rtems_npl_mutex_t *mutex = mu->mutex;
    rtems_id self = rtems_task_self();
    rtems_interrupt_level level;

    if (mutex == NULL) {
        return BLE_NPL_BAD_MUTEX;
    }

    rtems_interrupt_disable(level);
    if (mutex->owner != self) {
        rtems_interrupt_enable(level);

        return BLE_NPL_BAD_MUTEX;
    }
    if (--mutex->depth > 0) {
        rtems_interrupt_enable(level);

        return BLE_NPL_OK;
    }
    mutex->owner = 0;
    rtems_interrupt_enable(level);

    return rtems_semaphore_release(mutex->id) == RTEMS_SUCCESSFUL
        ? BLE_NPL_OK
        : BLE_NPL_ERROR;
}

/* === semaphores ======================================================== */

static ble_npl_error_t npl_rtems_sem_init(
    struct ble_npl_sem *sem,
    uint16_t tokens)
{
    rtems_npl_sem_t *rtems_sem;

    if (sem->sem != NULL) {
        return BLE_NPL_OK;
    }
    sem->sem = calloc(1, sizeof(*rtems_sem));
    rtems_sem = sem->sem;
    if (rtems_sem == NULL) {
        return BLE_NPL_ENOMEM;
    }

    if (rtems_semaphore_create(
            rtems_build_name('B', 'T', 'S', 'E'),
            tokens,
            RTEMS_COUNTING_SEMAPHORE | RTEMS_PRIORITY,
            0,
            &rtems_sem->id)
        != RTEMS_SUCCESSFUL) {
        free(rtems_sem);
        sem->sem = NULL;
        return BLE_NPL_ERROR;
    }
    return BLE_NPL_OK;
}

static ble_npl_error_t npl_rtems_sem_deinit(struct ble_npl_sem *sem)
{
    rtems_npl_sem_t *rtems_sem = sem->sem;

    if (rtems_sem == NULL) {
        return BLE_NPL_OK;
    }
    rtems_semaphore_delete(rtems_sem->id);
    free(rtems_sem);
    sem->sem = NULL;
    return BLE_NPL_OK;
}

static ble_npl_error_t npl_rtems_sem_pend(
    struct ble_npl_sem *sem,
    ble_npl_time_t timeout)
{
    rtems_npl_sem_t *rtems_sem = sem->sem;
    ble_npl_error_t error;

    if (rtems_sem == NULL) {
        return BLE_NPL_INVALID_PARAM;
    }
    error = npl_rtems_semaphore_obtain(rtems_sem->id, timeout);
    return error;
}

static ble_npl_error_t npl_rtems_sem_release(struct ble_npl_sem *sem)
{
    rtems_npl_sem_t *rtems_sem = sem->sem;

    if (rtems_sem == NULL) {
        return BLE_NPL_INVALID_PARAM;
    }
    if (rtems_semaphore_release(rtems_sem->id) != RTEMS_SUCCESSFUL) {
        return BLE_NPL_ERROR;
    }
    return BLE_NPL_OK;
}

static uint16_t npl_rtems_sem_get_count(struct ble_npl_sem *sem)
{
    rtems_npl_sem_t *rtems_sem = sem->sem;

    return rtems_sem != NULL ? npl_rtems_sem_count(rtems_sem->id) : 0;
}

/* === time ============================================================== */

static ble_npl_time_t npl_rtems_time_get(void)
{
    return (ble_npl_time_t) rtems_clock_get_ticks_since_boot();
}

static ble_npl_error_t npl_rtems_time_ms_to_ticks(
    uint32_t ms,
    ble_npl_time_t *out_ticks)
{
    if (out_ticks == NULL) {
        return BLE_NPL_INVALID_PARAM;
    }
    *out_ticks = (ble_npl_time_t) ms;
    return BLE_NPL_OK;
}

static ble_npl_error_t npl_rtems_time_ticks_to_ms(
    ble_npl_time_t ticks,
    uint32_t *out_ms)
{
    if (out_ms == NULL) {
        return BLE_NPL_INVALID_PARAM;
    }
    *out_ms = (uint32_t) ticks;
    return BLE_NPL_OK;
}

static ble_npl_time_t npl_rtems_time_ms_to_ticks32(uint32_t ms)
{
    return (ble_npl_time_t) ms;
}

static uint32_t npl_rtems_time_ticks_to_ms32(ble_npl_time_t ticks)
{
    return (uint32_t) ticks;
}

static void npl_rtems_time_delay(ble_npl_time_t ticks)
{
    rtems_task_wake_after((rtems_interval) ticks);
}

static uint32_t npl_rtems_get_time_forever(void)
{
    return RTEMS_NPL_FOREVER;
}

/* === callouts ========================================================== */

extern int rtems_esp_timer_server_ensure(void);
static rtems_npl_callout_t *npl_callouts;
static uintptr_t npl_callout_cookie;

static void npl_rtems_callout_free(rtems_npl_callout_t *callout)
{
    npl_rtems_event_deinit(&callout->ev);
    free(callout);
}

static void npl_rtems_callout_fire(rtems_id timer, void *arg)
{
    rtems_interrupt_level level;
    rtems_interrupt_disable(level);
    /* A server callback may have been preempted before entering here. Look
     * up its generation without dereferencing its possibly deleted argument. */
    rtems_npl_callout_t *callout = npl_callouts;
    while (callout != NULL && callout->cookie != (uintptr_t) arg) {
        callout = callout->next;
    }
    if (callout == NULL || !callout->active || callout->timer != timer) {
        rtems_interrupt_enable(level);
        return;
    }
    callout->active = false;
    if (callout->evq != NULL) {
        rtems_id sem = npl_rtems_eventq_link(callout->evq, &callout->ev, false);
        rtems_interrupt_enable(level);
        if (sem != RTEMS_INVALID_ID) {
            (void) rtems_semaphore_release(sem);
        }
    } else {
        callout->running = true;
        rtems_interrupt_enable(level);
        npl_rtems_event_run(&callout->ev);
        rtems_interrupt_disable(level);
        callout->running = false;
        bool deleted = callout->deleted;
        rtems_interrupt_enable(level);
        if (deleted) {
            npl_rtems_callout_free(callout);
        }
    }
}

static void npl_rtems_callout_stop(struct ble_npl_callout *co)
{
    rtems_npl_callout_t *callout = co->co;
    rtems_interrupt_level level;
    if (callout == NULL) {
        return;
    }
    rtems_interrupt_disable(level);
    callout->active = false;
    rtems_id timer = callout->timer;
    npl_rtems_event_reset(&callout->ev);
    rtems_interrupt_enable(level);
    (void) rtems_timer_cancel(timer);
}

static int npl_rtems_callout_init(
    struct ble_npl_callout *co,
    struct ble_npl_eventq *evq,
    ble_npl_event_fn *ev_cb,
    void *ev_arg)
{
    rtems_npl_callout_t *callout = co->co;
    if (callout != NULL) {
        callout->evq = evq;
        npl_rtems_event_init(&callout->ev, ev_cb, ev_arg);
        return 0;
    }
    if (rtems_esp_timer_server_ensure() != 0) {
        return -1;
    }
    callout = calloc(1, sizeof(*callout));
    if (callout == NULL) {
        return -1;
    }
    callout->evq = evq;
    npl_rtems_event_init(&callout->ev, ev_cb, ev_arg);
    callout->event = callout->ev.event;
    if (callout->event == NULL || rtems_timer_create(rtems_build_name('B', 'T', 'C', 'O'), &callout->timer) != RTEMS_SUCCESSFUL) {
        npl_rtems_callout_free(callout);
        return -1;
    }
    rtems_interrupt_level level;
    rtems_interrupt_disable(level);
    callout->next = npl_callouts;
    npl_callouts = callout;
    rtems_interrupt_enable(level);
    co->co = callout;
    return 0;
}

static void npl_rtems_callout_deinit(struct ble_npl_callout *co)
{
    rtems_npl_callout_t *callout = co->co;
    if (callout == NULL) {
        return;
    }
    npl_rtems_callout_stop(co);
    rtems_interrupt_level level;
    rtems_interrupt_disable(level);
    rtems_npl_callout_t **link = &npl_callouts;
    while (*link != callout) {
        link = &(*link)->next;
    }
    *link = callout->next;
    callout->deleted = true;
    bool running = callout->running;
    co->co = NULL;
    rtems_id timer = callout->timer;
    rtems_interrupt_enable(level);
    rtems_timer_delete(timer);
    if (!running) {
        npl_rtems_callout_free(callout);
    }
}

static void npl_rtems_callout_mem_reset(struct ble_npl_callout *co)
{
    rtems_npl_callout_t *callout = co->co;
    if (callout != NULL) {
        npl_rtems_event_reset(&callout->ev);
    }
}

static bool npl_rtems_callout_is_active(struct ble_npl_callout *co)
{
    rtems_npl_callout_t *callout = co->co;

    return callout != NULL && callout->active;
}

static ble_npl_time_t npl_rtems_callout_get_ticks(struct ble_npl_callout *co)
{
    rtems_npl_callout_t *callout = co->co;

    return callout != NULL ? callout->deadline : 0;
}

static uint32_t npl_rtems_callout_remaining_ticks(
    struct ble_npl_callout *co,
    ble_npl_time_t now)
{
    rtems_npl_callout_t *callout = co->co;

    if (callout == NULL || !callout->active || callout->deadline <= now) {
        return 0;
    }
    return (uint32_t) (callout->deadline - now);
}

static void npl_rtems_callout_set_arg(struct ble_npl_callout *co, void *arg)
{
    rtems_npl_callout_t *callout = co->co;

    if (callout != NULL) {
        npl_rtems_event_set_arg(&callout->ev, arg);
    }
}

static ble_npl_error_t npl_rtems_callout_reset(
    struct ble_npl_callout *co,
    ble_npl_time_t ticks)
{
    rtems_npl_callout_t *callout = co->co;

    if (callout == NULL) {
        return BLE_NPL_INVALID_PARAM;
    }

    npl_rtems_callout_stop(co);
    rtems_interrupt_level level;
    rtems_interrupt_disable(level);
    if (ticks == 0) {
        ticks = 1;
    }
    callout->deadline = npl_rtems_time_get() + ticks;
    callout->active = true;
    /* A new cookie also rejects an old expiration racing a reset. */
    callout->cookie = ++npl_callout_cookie;
    rtems_id timer = callout->timer;
    void *cookie = (void *) callout->cookie;
    rtems_interrupt_enable(level);
    if (rtems_timer_server_fire_after(timer, (rtems_interval) ticks,
            npl_rtems_callout_fire, cookie)
        != RTEMS_SUCCESSFUL) {
        callout->active = false;
        return BLE_NPL_ERROR;
    }
    return BLE_NPL_OK;
}

/* === critical sections ================================================= */

static uint32_t npl_critical_depth;

static uint32_t npl_rtems_hw_enter_critical(void)
{
    rtems_interrupt_level level;

    rtems_interrupt_disable(level);
    npl_critical_depth++;
    return (uint32_t) level;
}

static void npl_rtems_hw_exit_critical(uint32_t ctx)
{
    if (npl_critical_depth != 0) {
        npl_critical_depth--;
    }
    rtems_interrupt_enable((rtems_interrupt_level) ctx);
}

static uint8_t npl_rtems_hw_is_in_critical(void)
{
    return npl_critical_depth != 0;
}

static void npl_rtems_hw_set_isr(int irqn, uint32_t addr)
{
    (void) irqn;
    (void) addr;
}

static const struct npl_funcs_t npl_rtems_funcs = {
    .p_ble_npl_os_started = npl_rtems_os_started,
    .p_ble_npl_get_current_task_id = npl_rtems_get_current_task_id,
    .p_ble_npl_eventq_init = npl_rtems_eventq_init,
    .p_ble_npl_eventq_deinit = npl_rtems_eventq_deinit,
    .p_ble_npl_eventq_get = npl_rtems_eventq_get,
    .p_ble_npl_eventq_put = npl_rtems_eventq_put,
    .p_ble_npl_eventq_remove = npl_rtems_eventq_remove,
    .p_ble_npl_event_run = npl_rtems_event_run,
    .p_ble_npl_eventq_is_empty = npl_rtems_eventq_is_empty,
    .p_ble_npl_event_init = npl_rtems_event_init,
    .p_ble_npl_event_deinit = npl_rtems_event_deinit,
    .p_ble_npl_event_reset = npl_rtems_event_reset,
    .p_ble_npl_event_is_queued = npl_rtems_event_is_queued,
    .p_ble_npl_event_get_arg = npl_rtems_event_get_arg,
    .p_ble_npl_event_set_arg = npl_rtems_event_set_arg,
    .p_ble_npl_mutex_init = npl_rtems_mutex_init,
    .p_ble_npl_mutex_deinit = npl_rtems_mutex_deinit,
    .p_ble_npl_mutex_pend = npl_rtems_mutex_pend,
    .p_ble_npl_mutex_release = npl_rtems_mutex_release,
    .p_ble_npl_sem_init = npl_rtems_sem_init,
    .p_ble_npl_sem_deinit = npl_rtems_sem_deinit,
    .p_ble_npl_sem_pend = npl_rtems_sem_pend,
    .p_ble_npl_sem_release = npl_rtems_sem_release,
    .p_ble_npl_sem_get_count = npl_rtems_sem_get_count,
    .p_ble_npl_callout_init = npl_rtems_callout_init,
    .p_ble_npl_callout_reset = npl_rtems_callout_reset,
    .p_ble_npl_callout_stop = npl_rtems_callout_stop,
    .p_ble_npl_callout_deinit = npl_rtems_callout_deinit,
    .p_ble_npl_callout_mem_reset = npl_rtems_callout_mem_reset,
    .p_ble_npl_callout_is_active = npl_rtems_callout_is_active,
    .p_ble_npl_callout_get_ticks = npl_rtems_callout_get_ticks,
    .p_ble_npl_callout_remaining_ticks = npl_rtems_callout_remaining_ticks,
    .p_ble_npl_callout_set_arg = npl_rtems_callout_set_arg,
    .p_ble_npl_time_get = npl_rtems_time_get,
    .p_ble_npl_time_ms_to_ticks = npl_rtems_time_ms_to_ticks,
    .p_ble_npl_time_ticks_to_ms = npl_rtems_time_ticks_to_ms,
    .p_ble_npl_time_ms_to_ticks32 = npl_rtems_time_ms_to_ticks32,
    .p_ble_npl_time_ticks_to_ms32 = npl_rtems_time_ticks_to_ms32,
    .p_ble_npl_time_delay = npl_rtems_time_delay,
    .p_ble_npl_hw_set_isr = npl_rtems_hw_set_isr,
    .p_ble_npl_hw_enter_critical = npl_rtems_hw_enter_critical,
    .p_ble_npl_hw_exit_critical = npl_rtems_hw_exit_critical,
    .p_ble_npl_get_time_forever = npl_rtems_get_time_forever,
    .p_ble_npl_hw_is_in_critical = npl_rtems_hw_is_in_critical,
    .p_ble_npl_eventq_put_to_front = npl_rtems_eventq_put_to_front,
};

struct npl_funcs_t *npl_funcs = (struct npl_funcs_t *) &npl_rtems_funcs;

struct npl_funcs_t *rtems_bt_npl_funcs(void)
{
    return (struct npl_funcs_t *) &npl_rtems_funcs;
}
