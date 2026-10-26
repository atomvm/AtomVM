/*
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef AVM_RTEMS_ESP_SLEEP_CONSOLE_H
#define AVM_RTEMS_ESP_SLEEP_CONSOLE_H

#include <stdbool.h>
#include <stdint.h>

struct sleep_console_state
{
    uint32_t suspended_uarts;
    bool usb_clock;
    bool usb_pad;
};

/* Call with the other hart parked and local interrupts disabled, before
 * changing CPU clocks; resume only after clock and cache restoration.
 */
void rtems_esp_sleep_console_prepare(bool deep, struct sleep_console_state *state);
void rtems_esp_sleep_console_resume(bool deep, const struct sleep_console_state *state);

#endif
