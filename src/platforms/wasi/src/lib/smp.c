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

#include "smp.h"

#ifndef AVM_NO_SMP

#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

#include <scheduler.h>
#include <utils.h>

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

/* Track scheduler thread handles so smp_scheduler_join_all can join them.
 * Mirrors the generic_unix implementation: a sub-thread may still execute
 * JIT epilogue code after decrementing running_schedulers; joining prevents
 * use-after-free on resources the main thread is about to destroy. */
struct SchedulerThreadList
{
    pthread_t thread;
    struct SchedulerThreadList *next;
};
static pthread_mutex_t scheduler_threads_lock = PTHREAD_MUTEX_INITIALIZER;
static struct SchedulerThreadList *scheduler_threads = NULL;

static _Thread_local bool g_sub_main_thread = false;

static void *scheduler_thread_entry_point(void *arg)
{
    g_sub_main_thread = true;
    return (void *) (uintptr_t) scheduler_entry_point((GlobalContext *) arg);
}

void smp_scheduler_start(GlobalContext *ctx)
{
    /* Allocate the bookkeeping node before spawning the thread so an OOM
     * here cannot leak a running scheduler thread. */
    struct SchedulerThreadList *node = malloc(sizeof(*node));
    if (IS_NULL_PTR(node)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_create(&node->thread, NULL, scheduler_thread_entry_point, ctx))) {
        free(node);
        AVM_ABORT();
    }
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
    while (list) {
        struct SchedulerThreadList *next = list->next;
        (void) pthread_join(list->thread, NULL);
        free(list);
        list = next;
    }
}

bool smp_is_main_thread(GlobalContext *glb)
{
    UNUSED(glb);
    return !g_sub_main_thread;
}

Mutex *smp_mutex_create(void)
{
    Mutex *result = malloc(sizeof(Mutex));
    if (UNLIKELY(result == NULL)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_mutex_init(&result->mutex, NULL))) {
        AVM_ABORT();
    }
    return result;
}

void smp_mutex_destroy(Mutex *mtx)
{
    if (UNLIKELY(pthread_mutex_destroy(&mtx->mutex))) {
        AVM_ABORT();
    }
    free(mtx);
}

void smp_mutex_lock(Mutex *mtx)
{
    if (UNLIKELY(pthread_mutex_lock(&mtx->mutex))) {
        AVM_ABORT();
    }
}

bool smp_mutex_trylock(Mutex *mtx)
{
    int r = pthread_mutex_trylock(&mtx->mutex);
    return r == 0;
}

void smp_mutex_unlock(Mutex *mtx)
{
    if (UNLIKELY(pthread_mutex_unlock(&mtx->mutex))) {
        AVM_ABORT();
    }
}

CondVar *smp_condvar_create(void)
{
    CondVar *result = malloc(sizeof(CondVar));
    if (UNLIKELY(result == NULL && sizeof(CondVar) > 0)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_cond_init(&result->condvar, NULL))) {
        AVM_ABORT();
    }
    return result;
}

void smp_condvar_destroy(CondVar *cv)
{
    if (UNLIKELY(pthread_cond_destroy(&cv->condvar))) {
        AVM_ABORT();
    }
    free(cv);
}

void smp_condvar_wait(CondVar *cv, Mutex *mtx)
{
    if (UNLIKELY(pthread_cond_wait(&cv->condvar, &mtx->mutex))) {
        AVM_ABORT();
    }
}

void smp_condvar_signal(CondVar *cv)
{
    if (UNLIKELY(pthread_cond_signal(&cv->condvar))) {
        AVM_ABORT();
    }
}

RWLock *smp_rwlock_create(void)
{
    RWLock *result = malloc(sizeof(RWLock));
    if (UNLIKELY(result == NULL && sizeof(RWLock) > 0)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_rwlock_init(&result->lock, NULL))) {
        AVM_ABORT();
    }
    return result;
}

void smp_rwlock_destroy(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_destroy(&lock->lock))) {
        AVM_ABORT();
    }
    free(lock);
}

void smp_rwlock_rdlock(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_rdlock(&lock->lock))) {
        AVM_ABORT();
    }
}

bool smp_rwlock_tryrdlock(RWLock *lock)
{
    int r = pthread_rwlock_tryrdlock(&lock->lock);
    if (r == EBUSY) {
        return false;
    }
    if (UNLIKELY(r)) {
        AVM_ABORT();
    }
    return true;
}

void smp_rwlock_wrlock(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_wrlock(&lock->lock))) {
        AVM_ABORT();
    }
}

void smp_rwlock_unlock(RWLock *lock)
{
    if (UNLIKELY(pthread_rwlock_unlock(&lock->lock))) {
        AVM_ABORT();
    }
}

int smp_get_online_processors(void)
{
    long nprocs = sysconf(_SC_NPROCESSORS_ONLN);
    if (nprocs < 1) {
        return 1;
    }
    return (int) nprocs;
}

#endif /* AVM_NO_SMP */
