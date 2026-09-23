/* Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */
#ifndef APP_HEAP_H
#define APP_HEAP_H
#include <stddef.h>

// Bound allocations made by the linked VM, excluding libc's internal allocations.
#define APP_HEAP_LIMIT (256 * 1024)
extern size_t app_heap_used;
extern unsigned app_heap_denied;
extern unsigned app_heap_fail_import;
#endif
