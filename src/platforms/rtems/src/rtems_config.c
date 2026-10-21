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

#define CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER
#ifdef RTEMS_HAS_STORAGE
#define CONFIGURE_FILESYSTEM_DOSFS
#endif
#if defined(RTEMS_BOARD_ESP32)
#define CONFIGURE_FILESYSTEM_JFFS2
#endif
#ifdef RTEMS_HAS_LIBBSD
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK
#define CONFIGURE_BDBUF_BUFFER_MAX_SIZE (64 * 1024)
#define CONFIGURE_BDBUF_MAX_READ_AHEAD_BLOCKS 4
#define CONFIGURE_BDBUF_CACHE_MEMORY_SIZE (1 * 1024 * 1024)
#endif

#define CONFIGURE_UNIFIED_WORK_AREAS

#if defined(RTEMS_BOARD_ESP32)
#ifndef AVM_NO_SMP
#if defined(RTEMS_BOARD_ESP32S31)
#define CONFIGURE_MAXIMUM_PROCESSORS 2
#endif
#define CONFIGURE_MAXIMUM_TASKS 12
#define CONFIGURE_MAXIMUM_SEMAPHORES 48
#define CONFIGURE_MAXIMUM_MESSAGE_QUEUES 12
#define CONFIGURE_MAXIMUM_TIMERS 24
#define CONFIGURE_MAXIMUM_POSIX_THREADS 8
#define CONFIGURE_MAXIMUM_POSIX_KEYS 8
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 16
#define CONFIGURE_MAXIMUM_USER_EXTENSIONS 2
#define CONFIGURE_INIT_TASK_STACK_SIZE (32 * 1024)
#define CONFIGURE_EXTRA_TASK_STACKS (64 * 1024)
#else
#ifdef RTEMS_HAS_BT
#define CONFIGURE_MAXIMUM_TASKS 12
#define CONFIGURE_MAXIMUM_SEMAPHORES 48
#define CONFIGURE_MAXIMUM_MESSAGE_QUEUES 12
#else
#define CONFIGURE_MAXIMUM_TASKS 8
#define CONFIGURE_MAXIMUM_SEMAPHORES 36
#define CONFIGURE_MAXIMUM_MESSAGE_QUEUES 8
#endif
#if defined(RTEMS_BOARD_ESP32C5) && defined(RTEMS_HAS_BT)
/* The C5 controller's NPL callouts and Wi-Fi ETS timers share this pool. */
#define CONFIGURE_MAXIMUM_TIMERS 64
#else
#define CONFIGURE_MAXIMUM_TIMERS 24
#endif
#define CONFIGURE_MAXIMUM_POSIX_THREADS 4
#define CONFIGURE_MAXIMUM_POSIX_KEYS 4
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 16
#define CONFIGURE_MAXIMUM_USER_EXTENSIONS 2
// Measured with the RTEMS stack checker: the deepest chains (module loading,
// JFFS2 mount/scan, crypto, Wi-Fi bring-up) peak at ~22.6 KiB. The old 20 KiB
// stack overflowed into the unified workspace and clobbered the mounted flag of
// the root mount-table entry, so file operations intermittently failed with
// ENXIO. 26 KiB leaves >3 KiB of headroom over the observed peak while keeping
// enough unified heap free for ESP32-C3 Wi-Fi + OTP gen_tcp/socket tests.
#define CONFIGURE_INIT_TASK_STACK_SIZE (26 * 1024)
#ifdef RTEMS_HAS_BT
/* Extra stack above each configured task's minimum, not a fixed stack pool.
 * CONFIGURE_UNIFIED_WORK_AREAS shares dynamic stacks with the malloc heap. */
#define CONFIGURE_EXTRA_TASK_STACKS (12 * 1024)
#else
#define CONFIGURE_EXTRA_TASK_STACKS 0
#endif
#endif
#else
#ifndef AVM_NO_SMP
#if defined(RTEMS_BOARD_QEMU)
#define CONFIGURE_MAXIMUM_PROCESSORS 2
#else
#define CONFIGURE_MAXIMUM_PROCESSORS 4
#endif
#endif
#define CONFIGURE_UNLIMITED_OBJECTS
#define CONFIGURE_UNLIMITED_ALLOCATION_SIZE 32
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 64
#define CONFIGURE_MAXIMUM_USER_EXTENSIONS 8
#define CONFIGURE_INIT_TASK_STACK_SIZE (256 * 1024)
#define CONFIGURE_EXTRA_TASK_STACKS (512 * 1024)
#endif

#define CONFIGURE_MICROSECONDS_PER_TICK 1000
#define CONFIGURE_TICKS_PER_TIMESLICE 10

#if defined(RTEMS_BOARD_ESP32)
/* Leave Wi-Fi timers and event tasks above the VM.
 * SMP scheduler spinlocks cannot inherit their holder's task priority. */
#define CONFIGURE_INIT_TASK_PRIORITY 150
#else
#define CONFIGURE_INIT_TASK_PRIORITY 2
#endif
#define CONFIGURE_INIT_TASK_INITIAL_MODES (RTEMS_PREEMPT | RTEMS_TIMESLICE)
#define CONFIGURE_INIT_TASK_ATTRIBUTES RTEMS_FLOATING_POINT

#define CONFIGURE_RTEMS_INIT_TASKS_TABLE

#define CONFIGURE_INIT

#include <rtems/confdefs.h>
