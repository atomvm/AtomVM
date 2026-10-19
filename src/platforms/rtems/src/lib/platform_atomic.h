/* Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later */
#ifndef RTEMS_PLATFORM_ATOMIC_H
#define RTEMS_PLATFORM_ATOMIC_H

#include <stdatomic.h>

/* RTEMS supplies the GCC atomic helpers on CPUs without atomic instructions. */
#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(object, expected, desired) \
    atomic_compare_exchange_weak(object, expected, desired)
#define ATOMIC_COMPARE_EXCHANGE_WEAK_INT(object, expected, desired) \
    atomic_compare_exchange_weak(object, expected, desired)

#endif
