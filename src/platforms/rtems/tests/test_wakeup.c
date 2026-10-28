/* Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include "rtc.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Independent IDF wake-source masks in public API priority order. In
 * particular, P4 UART4 is not a timer and H2 EXT1 is not GPIO. */
static const struct
{
    uint32_t mask;
    uint32_t cause;
} sources[] = {
#if defined(RTEMS_BOARD_ESP32C3)
    { 0x8, ESP_WAKEUP_TIMER },
    { 0x4, ESP_WAKEUP_GPIO },
    { 0xC0, ESP_WAKEUP_UART },
    { 0x20, ESP_WAKEUP_WIFI },
    { 0x400, ESP_WAKEUP_BT },
#elif defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32C6)
    { 0x10, ESP_WAKEUP_TIMER },
    { 0x4, ESP_WAKEUP_GPIO },
    { 0xC0, ESP_WAKEUP_UART },
    { 0x2, ESP_WAKEUP_EXT1 },
    { 0x20, ESP_WAKEUP_WIFI },
    { 0x400, ESP_WAKEUP_BT },
    { 0x800, ESP_WAKEUP_ULP },
    { 0x1000, ESP_WAKEUP_COCPU_TRAP },
#elif defined(RTEMS_BOARD_ESP32H2)
    { 0x10, ESP_WAKEUP_TIMER },
    { 0x4, ESP_WAKEUP_GPIO },
    { 0xC0, ESP_WAKEUP_UART },
    { 0x2, ESP_WAKEUP_EXT1 },
    { 0x400, ESP_WAKEUP_BT },
    { 0x8000, ESP_WAKEUP_VBAT_UNDER_VOLT },
#elif defined(RTEMS_BOARD_ESP32P4)
    { 0x2000, ESP_WAKEUP_TIMER },
    { 0x204, ESP_WAKEUP_GPIO },
    { 0x1F0, ESP_WAKEUP_UART },
    { 0x1000, ESP_WAKEUP_EXT1 },
    { 0x800, ESP_WAKEUP_TOUCHPAD },
    { 0x2, ESP_WAKEUP_ULP },
    { 0x10000, ESP_WAKEUP_COCPU_TRAP },
    { 0x80000, ESP_WAKEUP_VAD },
    { 0x8000, ESP_WAKEUP_VBAT_UNDER_VOLT },
#elif defined(RTEMS_BOARD_ESP32S31)
    { 0x2000, ESP_WAKEUP_TIMER },
    { 0x204, ESP_WAKEUP_GPIO },
    { 0x1E0, ESP_WAKEUP_UART },
    { 0x1000, ESP_WAKEUP_EXT1 },
    { 0x800, ESP_WAKEUP_TOUCHPAD },
    { 0x400000, ESP_WAKEUP_WIFI },
    { 0x2000000, ESP_WAKEUP_BT },
    { 0x1, ESP_WAKEUP_ULP },
    { 0x10000, ESP_WAKEUP_COCPU_TRAP },
#endif
};

static void check(uint32_t raw)
{
    uint32_t expected = ESP_WAKEUP_UNDEFINED;
    for (size_t i = 0; i < sizeof(sources) / sizeof(sources[0]); i++) {
        if (raw & sources[i].mask) {
            expected = sources[i].cause;
            break;
        }
    }
    uint32_t actual = rtems_esp_sleep_decode_wakeup_cause(raw);
    if (actual != expected) {
        fprintf(stderr, "raw=0x%08x: expected %u, got %u\n", raw, expected, actual);
        abort();
    }
}

int main(void)
{
    check(0);
    check(UINT32_MAX);
    for (unsigned i = 0; i < 32; i++) {
        check(UINT32_C(1) << i);
        check(~(UINT32_C(1) << i));
        for (unsigned j = 0; j < 32; j++) {
            check((UINT32_C(1) << i) | (UINT32_C(1) << j));
        }
    }
    return 0;
}
