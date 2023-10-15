/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

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

// Thread local storage with _Thread_local C11 keyword crashes on i386 with
// valgrind (Ubutun 18 & 20, gcc 6-10). Use POSIX API instead.
#ifdef __i386__
static pthread_key_t g_sub_main_thread_key;
static pthread_once_t g_sub_main_thread_key_once = PTHREAD_ONCE_INIT;

static void sub_main_thread_make_key()
{
    if (UNLIKELY(pthread_key_create(&g_sub_main_thread_key, NULL))) {
        AVM_ABORT();
    }
}
#else
static _Thread_local bool g_sub_main_thread = false;
#endif

static void *scheduler_thread_entry_point(void *arg)
{
#ifdef __i386__
    if (UNLIKELY(pthread_once(&g_sub_main_thread_key_once, sub_main_thread_make_key))) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_setspecific(g_sub_main_thread_key, (void *) 1))) {
        AVM_ABORT();
    }
#else
    g_sub_main_thread = true;
#endif
    return (void *) (uintptr_t) scheduler_entry_point((GlobalContext *) arg);
}

void smp_scheduler_start(GlobalContext *ctx)
{
    pthread_t thread;
    if (UNLIKELY(pthread_create(&thread, NULL, scheduler_thread_entry_point, ctx))) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_detach(thread))) {
        AVM_ABORT();
    }
}

bool smp_is_main_thread(GlobalContext *glb)
{
    UNUSED(glb);
#ifdef __i386__
    (void) pthread_once(&g_sub_main_thread_key_once, sub_main_thread_make_key);
    return pthread_getspecific(g_sub_main_thread_key) == NULL;
#else
    return !g_sub_main_thread;
#endif
}

Mutex *smp_mutex_create()
{
    Mutex *result = malloc(sizeof(Mutex));
    if (UNLIKELY(result == NULL && sizeof(Mutex) > 0)) {
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

CondVar *smp_condvar_create()
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

RWLock *smp_rwlock_create()
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

int smp_get_online_processors()
{
    long sysconf_val = sysconf(_SC_NPROCESSORS_ONLN);
    if (UNLIKELY(sysconf_val <= 0)) {
        return 1;
    }
    return (int) sysconf_val;
}

#endif
