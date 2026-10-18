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

#include "smp.h"

#ifndef AVM_NO_SMP

#include <errno.h>
#include <inttypes.h>
#include <pthread.h>
#include <sched.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <rtems.h>

#include "scheduler.h"
#include "utils.h"

struct Mutex
{
    pthread_mutex_t mutex;
};

struct CondVar
{
    pthread_cond_t condvar;
};

struct RWLock
{
    pthread_rwlock_t lock;
};

struct SchedulerThreadList
{
    pthread_t thread;
    struct SchedulerThreadList *next;
};

static pthread_mutex_t scheduler_threads_lock = PTHREAD_MUTEX_INITIALIZER;
static struct SchedulerThreadList *scheduler_threads = NULL;

static pthread_once_t main_thread_once = PTHREAD_ONCE_INIT;
static pthread_t main_thread_id;

static void init_main_thread(void)
{
    main_thread_id = pthread_self();
}

static void *scheduler_thread_entry_point(void *arg)
{
    return (void *) (uintptr_t) scheduler_entry_point((GlobalContext *) arg);
}

void smp_scheduler_start(GlobalContext *ctx)
{
    (void) pthread_once(&main_thread_once, init_main_thread);

    pthread_attr_t attr;
    if (UNLIKELY(pthread_attr_init(&attr) != 0)) {
        AVM_ABORT();
    }

#if defined(RTEMS_BOARD_ESP32)
    size_t stack_size = 32 * 1024;
#else
    size_t stack_size = 64 * 1024;
#endif
    (void) pthread_attr_setstacksize(&attr, stack_size);

    struct sched_param param;
    int policy;
    if (pthread_getschedparam(pthread_self(), &policy, &param) == 0) {
        (void) pthread_attr_setschedpolicy(&attr, policy);
        (void) pthread_attr_setschedparam(&attr, &param);
    }

    pthread_t thread;
    int err = pthread_create(&thread, &attr, scheduler_thread_entry_point, ctx);
    if (UNLIKELY(err != 0)) {
        (void) pthread_attr_destroy(&attr);
        AVM_ABORT();
    }
    (void) pthread_attr_destroy(&attr);

    struct SchedulerThreadList *node = malloc(sizeof(*node));
    if (IS_NULL_PTR(node)) {
        AVM_ABORT();
    }
    node->thread = thread;

    pthread_mutex_lock(&scheduler_threads_lock);
    node->next = scheduler_threads;
    scheduler_threads = node;
    pthread_mutex_unlock(&scheduler_threads_lock);
}

void smp_scheduler_join_all(void)
{
    struct SchedulerThreadList *list;
    pthread_mutex_lock(&scheduler_threads_lock);
    list = scheduler_threads;
    scheduler_threads = NULL;
    pthread_mutex_unlock(&scheduler_threads_lock);

    while (list != NULL) {
        struct SchedulerThreadList *next = list->next;
        (void) pthread_join(list->thread, NULL);
        free(list);
        list = next;
    }
}

bool smp_is_main_thread(GlobalContext *glb)
{
    UNUSED(glb);
    (void) pthread_once(&main_thread_once, init_main_thread);
    return pthread_equal(pthread_self(), main_thread_id) != 0;
}

Mutex *smp_mutex_create(void)
{
    Mutex *result = malloc(sizeof(Mutex));
    if (UNLIKELY(result == NULL)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_mutex_init(&result->mutex, NULL) != 0)) {
        AVM_ABORT();
    }
    return result;
}

void smp_mutex_destroy(Mutex *mtx)
{
    if (UNLIKELY(pthread_mutex_destroy(&mtx->mutex) != 0)) {
        AVM_ABORT();
    }
    free(mtx);
}

void smp_mutex_lock(Mutex *mtx)
{
    if (UNLIKELY(pthread_mutex_lock(&mtx->mutex) != 0)) {
        AVM_ABORT();
    }
}

bool smp_mutex_trylock(Mutex *mtx)
{
    return pthread_mutex_trylock(&mtx->mutex) == 0;
}

void smp_mutex_unlock(Mutex *mtx)
{
    if (UNLIKELY(pthread_mutex_unlock(&mtx->mutex) != 0)) {
        AVM_ABORT();
    }
}

CondVar *smp_condvar_create(void)
{
    CondVar *result = malloc(sizeof(CondVar));
    if (UNLIKELY(result == NULL)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_cond_init(&result->condvar, NULL) != 0)) {
        AVM_ABORT();
    }
    return result;
}

void smp_condvar_destroy(CondVar *cv)
{
    if (UNLIKELY(pthread_cond_destroy(&cv->condvar) != 0)) {
        AVM_ABORT();
    }
    free(cv);
}

void smp_condvar_wait(CondVar *cv, Mutex *mtx)
{
    if (UNLIKELY(pthread_cond_wait(&cv->condvar, &mtx->mutex) != 0)) {
        AVM_ABORT();
    }
}

void smp_condvar_signal(CondVar *cv)
{
    if (UNLIKELY(pthread_cond_signal(&cv->condvar) != 0)) {
        AVM_ABORT();
    }
}

RWLock *smp_rwlock_create(void)
{
    RWLock *result = malloc(sizeof(RWLock));
    if (UNLIKELY(result == NULL)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_rwlock_init(&result->lock, NULL) != 0)) {
        AVM_ABORT();
    }
    return result;
}

void smp_rwlock_destroy(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_destroy(&lock->lock) != 0)) {
        AVM_ABORT();
    }
    free(lock);
}

void smp_rwlock_rdlock(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_rdlock(&lock->lock) != 0)) {
        AVM_ABORT();
    }
}

bool smp_rwlock_tryrdlock(RWLock *lock)
{
    int r = pthread_rwlock_tryrdlock(&lock->lock);
    if (r == EBUSY) {
        return false;
    }
    if (UNLIKELY(r != 0)) {
        AVM_ABORT();
    }
    return true;
}

void smp_rwlock_wrlock(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_wrlock(&lock->lock) != 0)) {
        AVM_ABORT();
    }
}

void smp_rwlock_unlock(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_unlock(&lock->lock) != 0)) {
        AVM_ABORT();
    }
}

int smp_get_online_processors(void)
{
    uint32_t count = rtems_scheduler_get_processor_maximum();
    if (UNLIKELY(count < 1)) {
        return 1;
    }
    return (int) count;
}

#else
typedef int rtems_smp_unused_translation_unit_t;
#endif /* AVM_NO_SMP */
