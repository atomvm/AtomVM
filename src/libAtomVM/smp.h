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

/**
 * @file smp.h
 * @brief Multicore support functions.
 *
 * @details This header defines multicore support functions to be implemented
 * for SMP builds.
 */

#ifndef _SMP_H_
#define _SMP_H_

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__has_feature)
#if __has_feature(thread_sanitizer)
#define CLANG_THREAD_SANITIZE_SAFE __attribute__((no_sanitize("thread")))
#endif
#endif
#ifndef CLANG_THREAD_SANITIZE_SAFE
#define CLANG_THREAD_SANITIZE_SAFE
#endif

#ifndef AVM_NO_SMP

#include <stdbool.h>

#ifndef __cplusplus
#include <stdatomic.h>
#define ATOMIC _Atomic

#if ATOMIC_POINTER_LOCK_FREE != 2
#error Current SMP implementation requires lock-free atomic pointers which this platform does not support, please disable SMP
#endif

#if ATOMIC_LONG_LOCK_FREE != 2
#error Current SMP implementation requires lock-free atomic long (32 bits) words which this platform does not support, please disable SMP
#endif

#else
#define ATOMIC
#endif

#ifndef TYPEDEF_MUTEX
#define TYPEDEF_MUTEX
typedef struct Mutex Mutex;
#endif

#ifndef TYPEDEF_SPINLOCK
#define TYPEDEF_SPINLOCK
typedef struct SpinLock SpinLock;
#endif

#ifndef TYPEDEF_CONDVAR
#define TYPEDEF_CONDVAR
typedef struct CondVar CondVar;
#endif

#ifndef TYPEDEF_RWLOCK
#define TYPEDEF_RWLOCK
typedef struct RWLock RWLock;
#endif

#ifndef TYPEDEF_GLOBALCONTEXT
#define TYPEDEF_GLOBALCONTEXT
typedef struct GlobalContext GlobalContext;
#endif

struct SpinLock
{
    int ATOMIC lock;
};

/**
 * @brief Create a new mutex.
 * @return a pointer to a mutex.
 */
Mutex *smp_mutex_create();

/**
 * @brief Destroy a mutex.
 * @param mtx the mutex to destroy
 */
void smp_mutex_destroy(Mutex *mtx);

/**
 * @brief Lock a mutex.
 * @param mtx the mutex to lock
 */
void smp_mutex_lock(Mutex *mtx);

/**
 * @brief Try and lock a mutex.
 * @param mtx the mutex to lock
 * @return \c true if the mutex was acquired.
 */
bool smp_mutex_trylock(Mutex *mtx);

/**
 * @brief Unlock a mutex.
 * @param mtx the mutex to unlock
 */
void smp_mutex_unlock(Mutex *mtx);

/**
 * @brief Create a new condition variable.
 * @return a pointer to a cv.
 */
CondVar *smp_condvar_create();

/**
 * @brief Destroy a condition variable
 * @param cv variable to destroy.
 */
void smp_condvar_destroy(CondVar *cv);

/**
 * @brief Wait on a condition variable, atomically unlocking the mutex.
 * @param cv variable to wait on.
 * @param mtx mutex to unlock/lock.
 */
void smp_condvar_wait(CondVar *cv, Mutex *mtx);

/**
 * @brief Signal a single thread waiting on a condition variable.
 * @param cv variable to signal.
 */
void smp_condvar_signal(CondVar *cv);

/**
 * @brief Create a new rwlock.
 * @detail A RW Lock can be replaced by a mutex if RW Lock are not available
 * on the platform.
 * @return a pointer to a lock.
 */
RWLock *smp_rwlock_create();

/**
 * @brief Destroy a rwlock.
 * @param lock the lock to destroy
 */
void smp_rwlock_destroy(RWLock *lock);

/**
 * @brief Read lock a rwlock.
 * @param lock the lock to read lock
 */
void smp_rwlock_rdlock(RWLock *lock);

/**
 * @brief Write lock a rwlock.
 * @param lock the lock to write lock
 */
void smp_rwlock_wrlock(RWLock *lock);

/**
 * @brief Unlock a rwlock.
 * @param lock the lock to unlock
 */
void smp_rwlock_unlock(RWLock *lock);

#ifndef __cplusplus

/**
 * @brief Initialize a spinlock based on atomics.
 * @param lock the spin lock to initialize
 */
static inline void smp_spinlock_init(SpinLock *lock)
{
    lock->lock = 0;
}

/**
 * @brief Lock a spinlock.
 * @param lock the spin lock to lock
 */
static inline void smp_spinlock_lock(SpinLock *lock)
{
    int current;
    do {
        current = 0;
    } while (!atomic_compare_exchange_weak(&lock->lock, &current, 1));
}

/**
 * @brief Unlock a spinlock.
 * @param lock the spin lock to unlock
 */
static inline void smp_spinlock_unlock(SpinLock *lock)
{
    lock->lock = 0;
}

#endif

/**
 * @brief Get the number of online processors to configure schedulers.
 * @detail This value is one plus the maximum number of times
 * smp_scheduler_start will be called by the scheduler.
 * @return the number of online processors.
 */
int smp_get_online_processors();

/**
 * @brief Start a new scheduler, calling `scheduler_entry_point` with the given
 * global context.
 * @param glb the global context
 */
void smp_scheduler_start(GlobalContext *ctx);

/**
 * @brief Determine if caller is in the main thread, i.e. thread that was not
 * started with acmsmp_scheduler_start.
 */
bool smp_is_main_thread(GlobalContext *glb);

#endif

#ifdef __cplusplus
}
#endif

#endif
