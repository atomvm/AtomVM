/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <zephyr/kernel.h>
#include <zephyr/pm/policy.h>

const struct pm_state_info *pm_policy_next_state(uint8_t cpu, int32_t ticks)
{
    ARG_UNUSED(cpu);
    ARG_UNUSED(ticks);
    return NULL;
}
