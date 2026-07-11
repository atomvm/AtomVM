/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/*
 * Zephyr's fake PWM driver is built with FFF and needs DEFINE_FFF_GLOBALS
 * linked into the application when CONFIG_PWM_FAKE is enabled.
 */
#include <zephyr/fff.h>

DEFINE_FFF_GLOBALS;
