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
#ifdef RTEMS_HAS_LIBBSD
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK
#define CONFIGURE_BDBUF_BUFFER_MAX_SIZE (64 * 1024)
#define CONFIGURE_BDBUF_MAX_READ_AHEAD_BLOCKS 4
#define CONFIGURE_BDBUF_CACHE_MEMORY_SIZE (1 * 1024 * 1024)
#endif

#define CONFIGURE_UNIFIED_WORK_AREAS

#if defined(RTEMS_BOARD_ESP32C3)
#define CONFIGURE_MAXIMUM_TASKS 12
#define CONFIGURE_MAXIMUM_SEMAPHORES 40
#define CONFIGURE_MAXIMUM_MESSAGE_QUEUES 12
#define CONFIGURE_MAXIMUM_TIMERS 32
#define CONFIGURE_MAXIMUM_POSIX_THREADS 8
#define CONFIGURE_MAXIMUM_POSIX_KEYS 8
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 16
#define CONFIGURE_MAXIMUM_USER_EXTENSIONS 2
#define CONFIGURE_INIT_TASK_STACK_SIZE (20 * 1024)
#define CONFIGURE_EXTRA_TASK_STACKS (12 * 1024)
#else
#define CONFIGURE_UNLIMITED_OBJECTS
#define CONFIGURE_UNLIMITED_ALLOCATION_SIZE 32
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 64
#define CONFIGURE_MAXIMUM_USER_EXTENSIONS 8
#define CONFIGURE_INIT_TASK_STACK_SIZE (256 * 1024)
#define CONFIGURE_EXTRA_TASK_STACKS (512 * 1024)
#endif

#define CONFIGURE_MICROSECONDS_PER_TICK 1000
#define CONFIGURE_TICKS_PER_TIMESLICE 10

#define CONFIGURE_INIT_TASK_PRIORITY 2
#define CONFIGURE_INIT_TASK_INITIAL_MODES RTEMS_DEFAULT_MODES
#define CONFIGURE_INIT_TASK_ATTRIBUTES RTEMS_FLOATING_POINT

#define CONFIGURE_RTEMS_INIT_TASKS_TABLE

#define CONFIGURE_INIT

#include <rtems/confdefs.h>
