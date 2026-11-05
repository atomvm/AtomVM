/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
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

#include <stdint.h>
#include <stdlib.h>

#include <zephyr/kernel.h>

#include "scheduler.h"
#include "utils.h"

struct Mutex
{
    struct k_mutex mutex;
};

struct CondVar
{
    struct k_condvar condvar;
};

struct RWLock
{
    struct k_mutex mutex;
};

struct SchedulerThreadList
{
    struct k_thread thread;
    k_thread_stack_t *stack;
    struct SchedulerThreadList *next;
};

K_MUTEX_DEFINE(scheduler_threads_lock);
static struct SchedulerThreadList *scheduler_threads = NULL;

static void scheduler_thread_entry_point(void *arg, void *unused1, void *unused2)
{
    UNUSED(unused1);
    UNUSED(unused2);
    k_thread_custom_data_set((void *) 1);
    (void) scheduler_entry_point((GlobalContext *) arg);
}

void smp_scheduler_start(GlobalContext *ctx)
{
    struct SchedulerThreadList *node = malloc(sizeof(*node));
    if (IS_NULL_PTR(node)) {
        AVM_ABORT();
    }

    node->stack = k_thread_stack_alloc(CONFIG_MAIN_STACK_SIZE, 0);
    if (IS_NULL_PTR(node->stack)) {
        free(node);
        AVM_ABORT();
    }

    k_tid_t tid = k_thread_create(&node->thread, node->stack, CONFIG_MAIN_STACK_SIZE, scheduler_thread_entry_point, ctx, NULL, NULL, K_PRIO_PREEMPT(CONFIG_MAIN_THREAD_PRIORITY), 0, K_NO_WAIT);
    if (IS_NULL_PTR(tid)) {
        (void) k_thread_stack_free(node->stack);
        free(node);
        AVM_ABORT();
    }

    if (UNLIKELY(k_mutex_lock(&scheduler_threads_lock, K_FOREVER) != 0)) {
        AVM_ABORT();
    }
    node->next = scheduler_threads;
    scheduler_threads = node;
    if (UNLIKELY(k_mutex_unlock(&scheduler_threads_lock) != 0)) {
        AVM_ABORT();
    }
}

void smp_scheduler_join_all(void)
{
    if (UNLIKELY(k_mutex_lock(&scheduler_threads_lock, K_FOREVER) != 0)) {
        AVM_ABORT();
    }
    struct SchedulerThreadList *list = scheduler_threads;
    scheduler_threads = NULL;
    if (UNLIKELY(k_mutex_unlock(&scheduler_threads_lock) != 0)) {
        AVM_ABORT();
    }

    while (list) {
        struct SchedulerThreadList *next = list->next;
        (void) k_thread_join(&list->thread, K_FOREVER);
        if (UNLIKELY(k_thread_stack_free(list->stack) != 0)) {
            AVM_ABORT();
        }
        free(list);
        list = next;
    }
}

bool smp_is_main_thread(GlobalContext *glb)
{
    UNUSED(glb);
    return k_thread_custom_data_get() == NULL;
}

Mutex *smp_mutex_create(void)
{
    Mutex *result = malloc(sizeof(Mutex));
    if (UNLIKELY(result == NULL && sizeof(Mutex) > 0)) {
        AVM_ABORT();
    }
    if (UNLIKELY(k_mutex_init(&result->mutex) != 0)) {
        AVM_ABORT();
    }
    return result;
}

void smp_mutex_destroy(Mutex *mtx)
{
    free(mtx);
}

void smp_mutex_lock(Mutex *mtx)
{
    if (UNLIKELY(k_mutex_lock(&mtx->mutex, K_FOREVER) != 0)) {
        AVM_ABORT();
    }
}

bool smp_mutex_trylock(Mutex *mtx)
{
    return k_mutex_lock(&mtx->mutex, K_NO_WAIT) == 0;
}

void smp_mutex_unlock(Mutex *mtx)
{
    if (UNLIKELY(k_mutex_unlock(&mtx->mutex) != 0)) {
        AVM_ABORT();
    }
}

CondVar *smp_condvar_create(void)
{
    CondVar *result = malloc(sizeof(CondVar));
    if (UNLIKELY(result == NULL && sizeof(CondVar) > 0)) {
        AVM_ABORT();
    }
    if (UNLIKELY(k_condvar_init(&result->condvar) != 0)) {
        AVM_ABORT();
    }
    return result;
}

void smp_condvar_destroy(CondVar *cv)
{
    free(cv);
}

void smp_condvar_wait(CondVar *cv, Mutex *mtx)
{
    if (UNLIKELY(k_condvar_wait(&cv->condvar, &mtx->mutex, K_FOREVER) != 0)) {
        AVM_ABORT();
    }
}

void smp_condvar_signal(CondVar *cv)
{
    if (UNLIKELY(k_condvar_signal(&cv->condvar) != 0)) {
        AVM_ABORT();
    }
}

RWLock *smp_rwlock_create(void)
{
    RWLock *result = malloc(sizeof(RWLock));
    if (UNLIKELY(result == NULL && sizeof(RWLock) > 0)) {
        AVM_ABORT();
    }
    if (UNLIKELY(k_mutex_init(&result->mutex) != 0)) {
        AVM_ABORT();
    }
    return result;
}

void smp_rwlock_destroy(RWLock *lock)
{
    free(lock);
}

void smp_rwlock_rdlock(RWLock *lock)
{
    if (UNLIKELY(k_mutex_lock(&lock->mutex, K_FOREVER) != 0)) {
        AVM_ABORT();
    }
}

bool smp_rwlock_tryrdlock(RWLock *lock)
{
    return k_mutex_lock(&lock->mutex, K_NO_WAIT) == 0;
}

void smp_rwlock_wrlock(RWLock *lock)
{
    if (UNLIKELY(k_mutex_lock(&lock->mutex, K_FOREVER) != 0)) {
        AVM_ABORT();
    }
}

void smp_rwlock_unlock(RWLock *lock)
{
    if (UNLIKELY(k_mutex_unlock(&lock->mutex) != 0)) {
        AVM_ABORT();
    }
}

int smp_get_online_processors(void)
{
#if defined(CONFIG_SMP) && defined(CONFIG_MP_MAX_NUM_CPUS)
    return CONFIG_MP_MAX_NUM_CPUS;
#else
    return 1;
#endif
}

#endif
