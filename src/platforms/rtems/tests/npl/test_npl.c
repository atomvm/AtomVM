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

#include "../../bt/rtems_npl.c"
#include <assert.h>
#include <stdio.h>
#include <string.h>
int rtems_esp_timer_server_ensure(void) { return 0; }
static struct ble_npl_eventq q;
static struct ble_npl_event a, b, c;
static void cb(struct ble_npl_event *ev) { (void) ev; }
static void produce(void)
{
    mock_task = 2;
    npl_rtems_eventq_put(&q, &a);
    mock_task = 1;
    mock_wait_hook = NULL;
}
static void stale_wakeup(void)
{
    mock_ticks += 4;
    mock_wait_hook = NULL;
}
static void test_queue(void)
{
    assert(!npl_rtems_hw_is_in_critical());
    uint32_t outer = npl_rtems_hw_enter_critical();
    assert(npl_rtems_hw_is_in_critical());
    uint32_t inner = npl_rtems_hw_enter_critical();
    npl_rtems_hw_exit_critical(inner);
    assert(npl_rtems_hw_is_in_critical());
    npl_rtems_hw_exit_critical(outer);
    assert(!npl_rtems_hw_is_in_critical());
    npl_rtems_eventq_init(&q);
    assert(mock_sems[((rtems_npl_eventq_t *) q.eventq)->sem].attr & RTEMS_SIMPLE_BINARY_SEMAPHORE);
    npl_rtems_event_init(&a, cb, NULL);
    npl_rtems_event_init(&b, cb, NULL);
    npl_rtems_event_init(&c, cb, NULL);
    mock_task = 2;
    npl_rtems_eventq_put(&q, &a);
    npl_rtems_eventq_put(&q, &b);
    npl_rtems_eventq_put(&q, &c);
    npl_rtems_eventq_put(&q, &a); /* duplicate is not enqueued twice */
    assert(mock_release_errors == 0);
    mock_task = 1;
    assert(npl_rtems_eventq_get(&q, 0) == &a);
    assert(npl_rtems_eventq_get(&q, 0) == &b);
    assert(npl_rtems_eventq_get(&q, 0) == &c);
    assert(npl_rtems_eventq_get(&q, 0) == NULL);
    assert(mock_obtains == 0 && npl_rtems_eventq_is_empty(&q));
    assert(!npl_rtems_event_is_queued(&a));
    /* Consume stale wake, then wait for a producer after the empty-list check. */
    assert(npl_rtems_eventq_get(&q, 1) == NULL);
    mock_obtains = 0;
    mock_wait_hook = produce;
    assert(npl_rtems_eventq_get(&q, RTEMS_NPL_FOREVER) == &a);
    assert(mock_obtains == 1 && mock_release_errors == 0);
    /* A removed event leaves a stale wake; it must not extend the deadline. */
    npl_rtems_eventq_put(&q, &a);
    npl_rtems_eventq_remove(&q, &a);
    mock_obtains = 0;
    mock_ticks = 100;
    mock_wait_hook = stale_wakeup;
    assert(npl_rtems_eventq_get(&q, 10) == NULL);
    assert(mock_wait_timeouts[0] == 10 && mock_wait_timeouts[1] == 6);
    assert(mock_ticks == 110);
    /* Tick rollover preserves the remaining finite timeout. */
    npl_rtems_eventq_put(&q, &a);
    npl_rtems_eventq_remove(&q, &a);
    mock_obtains = 0;
    mock_ticks = UINT32_MAX - 1;
    mock_wait_hook = stale_wakeup;
    assert(npl_rtems_eventq_get(&q, 10) == NULL);
    assert(mock_wait_timeouts[1] == 6 && mock_ticks == 8);
    npl_rtems_eventq_put(&q, &a);
    npl_rtems_eventq_put(&q, &b);
    ble_npl_eventq_put_to_front(&q, &c);
    ble_npl_eventq_put_to_front(&q, &a); /* queued events remain deduplicated */
    assert(npl_rtems_eventq_get(&q, 0) == &c);
    assert(npl_rtems_eventq_get(&q, 0) == &a);
    assert(npl_rtems_eventq_get(&q, 0) == &b);
    assert(npl_rtems_eventq_is_empty(&q));
    npl_rtems_eventq_deinit(&q);
    npl_rtems_event_deinit(&a);
    npl_rtems_event_deinit(&b);
    npl_rtems_event_deinit(&c);
    puts("PASS: queue ownership, FIFO bursts, deduplication, empty/wakeup race, stale wakes, finite timeout and rollover");
}

static unsigned called, mismatches;
static struct ble_npl_sem waiting_sem;
static void callback(struct ble_npl_event *ev)
{
    (void) ev;
    called++;
}
static void check(bool ok, const char *name)
{
    printf("%s: %s\n", ok ? "MATCH" : "MISMATCH", name);
    mismatches += !ok;
}
static void consume_in_preempting_task(void)
{
    mock_release_hook = NULL;
    assert(npl_rtems_sem_pend(&waiting_sem, 0) == BLE_NPL_OK);
}
static void test_parity(void)
{
    struct ble_npl_callout co = { 0 };
    struct ble_npl_eventq q = { 0 };
    npl_rtems_eventq_init(&q);
    assert(npl_rtems_callout_init(&co, NULL, callback, NULL) == 0);
    assert(npl_rtems_callout_reset(&co, 1) == BLE_NPL_OK);
    npl_rtems_callout_fire(((rtems_npl_callout_t *) co.co)->timer,
        (void *) ((rtems_npl_callout_t *) co.co)->cookie);
    check(called == 1, "expired callout with no event queue runs its callback");
    npl_rtems_callout_deinit(&co);
    assert(npl_rtems_callout_init(&co, &q, callback, NULL) == 0);
    assert(npl_rtems_callout_reset(&co, 1) == BLE_NPL_OK);
    npl_rtems_callout_fire(((rtems_npl_callout_t *) co.co)->timer,
        (void *) ((rtems_npl_callout_t *) co.co)->cookie);
    npl_rtems_callout_stop(&co);
    check(npl_rtems_eventq_is_empty(&q), "stop removes an already queued callout event");
    while (npl_rtems_eventq_get(&q, 0)) {
    }
    assert(npl_rtems_callout_reset(&co, 1) == BLE_NPL_OK);
    npl_rtems_callout_fire(((rtems_npl_callout_t *) co.co)->timer,
        (void *) ((rtems_npl_callout_t *) co.co)->cookie);
    assert(npl_rtems_callout_reset(&co, 5) == BLE_NPL_OK);
    check(npl_rtems_eventq_is_empty(&q), "reset removes the previous queued expiration");
    while (npl_rtems_eventq_get(&q, 0)) {
    }
    npl_rtems_callout_fire(((rtems_npl_callout_t *) co.co)->timer,
        (void *) ((rtems_npl_callout_t *) co.co)->cookie);
    npl_rtems_callout_stop(&co);
    npl_rtems_callout_deinit(&co);
    check(npl_rtems_eventq_is_empty(&q), "deinit leaves no queued pointer to a freed event");
    assert(npl_rtems_callout_init(&co, &q, callback, NULL) == 0);
    void *old_event = ((rtems_npl_callout_t *) co.co)->ev.event;
    assert(npl_rtems_callout_reset(&co, 25) == BLE_NPL_OK);
    unsigned created = mock_timer_creates;
    assert(npl_rtems_callout_init(&co, &q, callback, NULL) == 0);
    assert(npl_rtems_callout_is_active(&co));
    check(mock_timer_creates == created, "reinitializing a callout reuses its timer");
    if (((rtems_npl_callout_t *) co.co)->ev.event != old_event) {
        free(old_event);
    }
    npl_rtems_callout_deinit(&co);
    assert(npl_rtems_callout_init(&co, &q, callback, NULL) == 0);
    assert(npl_rtems_callout_reset(&co, 100) == BLE_NPL_OK);
    npl_rtems_callout_mem_reset(&co);
    check(npl_rtems_callout_is_active(&co), "callout_mem_reset only clears event state and leaves the timer active");
    npl_rtems_callout_deinit(&co);
    unsigned next = mock_next_id;
    npl_rtems_eventq_init(&q);
    check(mock_next_id == next, "reinitializing an event queue reuses its wake semaphore");
    npl_rtems_eventq_deinit(&q);
    assert(npl_rtems_sem_init(&waiting_sem, 0) == BLE_NPL_OK);
    mock_release_hook = consume_in_preempting_task;
    assert(npl_rtems_sem_release(&waiting_sem) == BLE_NPL_OK);
    rtems_npl_sem_t *sem = waiting_sem.sem;
    check(npl_rtems_sem_get_count(&waiting_sem) == mock_sems[sem->id].count,
        "semaphore count agrees with RTEMS after release wakes a higher-priority waiter");
    unsigned before = mock_next_id;
    assert(npl_rtems_sem_init(&waiting_sem, 0) == BLE_NPL_OK);
    check(mock_next_id == before, "reinitializing a semaphore reuses its RTEMS object");
    npl_rtems_sem_deinit(&waiting_sem);
    struct ble_npl_mutex mu = { 0 };
    assert(npl_rtems_mutex_init(&mu) == BLE_NPL_OK);
    before = mock_next_id;
    assert(npl_rtems_mutex_init(&mu) == BLE_NPL_OK);
    check(mock_next_id == before, "reinitializing a mutex reuses its RTEMS object");
    npl_rtems_mutex_deinit(&mu);
    printf("%u semantic mismatches against ESP-IDF behavior\n", mismatches);
    assert(mismatches == 0);
}

static void delete_self(struct ble_npl_event *ev)
{
    npl_rtems_callout_deinit(npl_rtems_event_get_arg(ev));
}

int main(void)
{
    test_queue();
    test_parity();
    struct ble_npl_callout co = { 0 };
    struct ble_npl_eventq q = { 0 };
    struct ble_npl_event a = { 0 }, b = { 0 }, c = { 0 };
    npl_rtems_eventq_init(&q);
    npl_rtems_event_init(&a, callback, NULL);
    npl_rtems_event_init(&b, callback, NULL);
    npl_rtems_event_init(&c, callback, NULL);
    npl_rtems_eventq_put(&q, &a);
    npl_rtems_eventq_put(&q, &b);
    npl_rtems_eventq_put(&q, &c);
    npl_rtems_event_reset(&b);
    assert(npl_rtems_eventq_get(&q, 0) == &a);
    assert(npl_rtems_eventq_get(&q, 0) == &c);
    npl_rtems_eventq_put(&q, &a);
    unsigned next = mock_next_id;
    npl_rtems_eventq_init(&q);
    assert(mock_next_id == next && npl_rtems_eventq_is_empty(&q));
    assert(!npl_rtems_event_is_queued(&a));
    npl_rtems_event_deinit(&a);
    npl_rtems_event_deinit(&b);
    npl_rtems_event_deinit(&c);
    npl_rtems_eventq_deinit(&q);

    assert(npl_rtems_callout_init(&co, NULL, delete_self, &co) == 0);
    assert(npl_rtems_callout_reset(&co, 1) == BLE_NPL_OK);
    rtems_npl_callout_t *callout = co.co;
    rtems_id timer = callout->timer;
    void *old_cookie = (void *) callout->cookie;
    npl_rtems_callout_fire(timer, old_cookie);
    assert(co.co == NULL);
    /* A pending server callback that resumes after deletion must be harmless. */
    npl_rtems_callout_fire(timer, old_cookie);
    assert(npl_rtems_callout_init(&co, NULL, callback, NULL) == 0);
    assert(npl_rtems_callout_reset(&co, 1) == BLE_NPL_OK);
    callout = co.co;
    old_cookie = (void *) callout->cookie;
    assert(npl_rtems_callout_reset(&co, 10) == BLE_NPL_OK);
    unsigned previous = called;
    npl_rtems_callout_fire(callout->timer, old_cookie);
    assert(called == previous && callout->active);
    mock_timer_arm_fail = true;
    assert(npl_rtems_callout_reset(&co, 1) == BLE_NPL_ERROR);
    assert(!callout->active);
    mock_timer_arm_fail = false;
    npl_rtems_callout_deinit(&co);

    mock_create_fail = true;
    struct ble_npl_sem sem = { 0 };
    struct ble_npl_mutex mu = { 0 };
    npl_rtems_eventq_init(&q);
    assert(q.eventq == NULL);
    assert(npl_rtems_sem_init(&sem, 1) == BLE_NPL_ERROR && sem.sem == NULL);
    assert(npl_rtems_mutex_init(&mu) == BLE_NPL_ERROR && mu.mutex == NULL);
    assert(npl_rtems_callout_init(&co, NULL, callback, NULL) == -1 && co.co == NULL);
    puts("PASS: stale expirations, callback self-deletion, queue unlinking and resource failures");
    return 0;
}
