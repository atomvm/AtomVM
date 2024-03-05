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

#include <sdkconfig.h>

#ifndef AVM_NO_SMP
#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>

#include "esp_pthread.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"

#ifdef HAVE_SOC_CPU_CORES_NUM
#include "soc/soc_caps.h"
#endif

#include "utils.h"
#include "scheduler.h"

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
#ifdef HAVE_PTHREAD_RWLOCK
    pthread_rwlock_t lock;
#else
    pthread_mutex_t lock;
#endif
};

// ESP-IDF SDK explicitly mentions support for C11 with "__thread" keyword
static __thread bool g_sub_main_thread = false;
static uint32_t _Atomic g_pinned_cores = 0x1;

static void *scheduler_thread_entry_point(void *arg)
{
    g_sub_main_thread = true;
    void *result = (void *) scheduler_entry_point((GlobalContext *) arg);
    BaseType_t core = xTaskGetAffinity(NULL);
    if (core != -1) {
        uint32_t desired = 1;
        uint32_t expected = 3;
        do {
            desired = expected & ~(1 << core);
        } while (!atomic_compare_exchange_weak(&g_pinned_cores, &expected, desired));
    }
    return result;
}

void smp_scheduler_start(GlobalContext *ctx)
{
    // When this function is called, the schedulers mutex is held and
    // therefore it is safe to call esp_pthread_set_default_config
    // g_pinned_cores is atomic in case another thread is being stopped.
    esp_pthread_cfg_t esp_pthread_cfg = esp_pthread_get_default_config();
    if (esp_pthread_cfg.pin_to_core != tskNO_AFFINITY) {
        int core;
        uint32_t expected = 1;
        uint32_t desired;
        do {
            core = 1;
            while (expected & 1 << core) {
                core++;
            }
            desired = expected | (1 << core);
        } while (!atomic_compare_exchange_weak(&g_pinned_cores, &expected, desired));

        esp_pthread_cfg.pin_to_core = core;
        if (UNLIKELY(esp_pthread_set_cfg(&esp_pthread_cfg))) {
            AVM_ABORT();
        }
    }

    pthread_attr_t thread_attr;
    if (UNLIKELY(pthread_attr_init(&thread_attr) != 0)) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_attr_setstacksize(&thread_attr, CONFIG_ESP_MAIN_TASK_STACK_SIZE) != 0)) {
        AVM_ABORT();
    }

    pthread_t thread;
    if (UNLIKELY(pthread_create(&thread, &thread_attr, scheduler_thread_entry_point, ctx))) {
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_detach(thread))) {
        AVM_ABORT();
    }

    if (UNLIKELY(pthread_attr_destroy(&thread_attr) != 0)) {
        AVM_ABORT();
    }
}

bool smp_is_main_thread(GlobalContext *glb)
{
    UNUSED(glb);
    return !g_sub_main_thread;
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
#ifdef HAVE_PTHREAD_RWLOCK
    if (UNLIKELY(pthread_rwlock_init(&result->lock, NULL))) {
        AVM_ABORT();
    }
#else
    if (UNLIKELY(pthread_mutex_init(&result->lock, NULL))) {
        AVM_ABORT();
    }
#endif
    return result;
}

void smp_rwlock_destroy(RWLock *lock)
{
#ifdef HAVE_PTHREAD_RWLOCK
    if (UNLIKELY(pthread_rwlock_destroy(&lock->lock))) {
        AVM_ABORT();
    }
#else
    if (UNLIKELY(pthread_mutex_destroy(&lock->lock))) {
        AVM_ABORT();
    }
#endif
    free(lock);
}

void smp_rwlock_rdlock(RWLock *lock)
{
#ifdef HAVE_PTHREAD_RWLOCK
    if (UNLIKELY(pthread_rwlock_rdlock(&lock->lock))) {
        AVM_ABORT();
    }
#else
    if (UNLIKELY(pthread_mutex_lock(&lock->lock))) {
        AVM_ABORT();
    }
#endif
}

void smp_rwlock_wrlock(RWLock *lock)
{
#ifdef HAVE_PTHREAD_RWLOCK
    if (UNLIKELY(pthread_rwlock_wrlock(&lock->lock))) {
        AVM_ABORT();
    }
#else
    if (UNLIKELY(pthread_mutex_lock(&lock->lock))) {
        AVM_ABORT();
    }
#endif
}

bool smp_rwlock_tryrdlock(RWLock *lock)
{
#ifdef HAVE_PTHREAD_RWLOCK
    int r = pthread_rwlock_tryrdlock(&lock->lock);
#else
    int r = pthread_mutex_trylock(&lock->lock);
#endif
    if (r == EBUSY) {
        return false;
    }
    if (UNLIKELY(r)) {
        AVM_ABORT();
    }
    return true;
}

void smp_rwlock_unlock(RWLock *lock)
{
#ifdef HAVE_PTHREAD_RWLOCK
    if (UNLIKELY(pthread_rwlock_unlock(&lock->lock))) {
        AVM_ABORT();
    }
#else
    if (UNLIKELY(pthread_mutex_unlock(&lock->lock))) {
        AVM_ABORT();
    }
#endif
}

int smp_get_online_processors()
{
#ifdef HAVE_SOC_CPU_CORES_NUM
    #if SOC_CPU_CORES_NUM == 1 && !defined(AVM_NO_SMP)
        #warning SMP is not disabled but target only has one core
    #endif
    return SOC_CPU_CORES_NUM;
#else
    return 2;
#endif
}

#endif
