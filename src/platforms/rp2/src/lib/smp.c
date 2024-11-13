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

#include <stdlib.h>

// Pico SDK
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <pico/cond.h>
#include <pico/multicore.h>
#include <pico/platform.h>
#include <pico/sync.h>

#pragma GCC diagnostic pop

#include <scheduler.h>
#include <utils.h>

struct Mutex
{
    mutex_t mutex;
};

struct CondVar
{
    cond_t condvar;
};

struct RWLock
{
    mutex_t lock;
};

static void scheduler_core1_entry_point(void)
{
    _Static_assert(sizeof(uintptr_t) == sizeof(uint32_t), "Expected pointers to be 32 bits");
    uint32_t ctx_int = multicore_fifo_pop_blocking();
    int result = scheduler_entry_point((GlobalContext *) ctx_int);
    UNUSED(result);
}

void smp_scheduler_start(GlobalContext *ctx)
{
    multicore_launch_core1(scheduler_core1_entry_point);
    multicore_fifo_push_blocking((uint32_t) ctx);
}

bool smp_is_main_thread(GlobalContext *glb)
{
    UNUSED(glb);
    return get_core_num() == 0;
}

Mutex *smp_mutex_create()
{
    Mutex *result = malloc(sizeof(Mutex));
    if (UNLIKELY(result == NULL && sizeof(Mutex) > 0)) {
        AVM_ABORT();
    }
    mutex_init(&result->mutex);
    return result;
}

void smp_mutex_destroy(Mutex *mtx)
{
    free(mtx);
}

void smp_mutex_lock(Mutex *mtx)
{
    mutex_enter_blocking(&mtx->mutex);
}

bool smp_mutex_trylock(Mutex *mtx)
{
    return mutex_try_enter(&mtx->mutex, NULL);
}

void smp_mutex_unlock(Mutex *mtx)
{
    mutex_exit(&mtx->mutex);
}

CondVar *smp_condvar_create()
{
    CondVar *result = malloc(sizeof(CondVar));
    if (UNLIKELY(result == NULL && sizeof(CondVar) > 0)) {
        AVM_ABORT();
    }
    cond_init(&result->condvar);
    return result;
}

void smp_condvar_destroy(CondVar *cv)
{
    free(cv);
}

void smp_condvar_wait(CondVar *cv, Mutex *mtx)
{
    cond_wait(&cv->condvar, &mtx->mutex);
}

void smp_condvar_signal(CondVar *cv)
{
    cond_signal(&cv->condvar);
}

RWLock *smp_rwlock_create()
{
    RWLock *result = malloc(sizeof(RWLock));
    if (UNLIKELY(result == NULL && sizeof(RWLock) > 0)) {
        AVM_ABORT();
    }
    mutex_init(&result->lock);
    return result;
}

void smp_rwlock_destroy(RWLock *lock)
{
    free(lock);
}

void smp_rwlock_rdlock(RWLock *lock)
{
    mutex_enter_blocking(&lock->lock);
}

bool smp_rwlock_tryrdlock(RWLock *lock)
{
    return mutex_try_enter(&lock->lock, NULL);
}

void smp_rwlock_wrlock(RWLock *lock)
{
    mutex_enter_blocking(&lock->lock);
}

void smp_rwlock_unlock(RWLock *lock)
{
    mutex_exit(&lock->lock);
}

int smp_get_online_processors()
{
    return 2;
}

#endif
