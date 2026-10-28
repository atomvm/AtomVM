/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * Copyright 2019-2026 Espressif Systems (Shanghai) CO LTD
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include "rtc.h"

#if defined(RTEMS_BOARD_ESP32)
#include "esp_sleep_regs.h"

/* ESP-IDF 9a97f6c54ec638111ce55cd36581b3c192f15207:
 * rtc_cntl_ll_get_wakeup_cause(), pmu_ll_hp_get_wakeup_cause(),
 * pmu_bit_defs.h, esp_pmu.h and esp_sleep_get_wakeup_cause().
 * GPIO_WAKE is the source selected by our GPIO entry path (EXT1 on H2),
 * not the full mask or the public cause used by IDF's decoder.
 */
uint32_t rtems_esp_sleep_read_wakeup_cause(void)
{
#if defined(RTEMS_BOARD_ESP32C3)
    return REG(0x600080F8U) & 0x1FFFFU;
#else
    return REG(PMU_BASE + PMU_WAKE_STATUS);
#endif
}

uint32_t rtems_esp_sleep_decode_wakeup_cause(uint32_t raw_cause)
{
    if (raw_cause & TIMER_WAKE) {
        return ESP_WAKEUP_TIMER;
    }
#if defined(RTEMS_BOARD_ESP32P4) || defined(RTEMS_BOARD_ESP32S31)
    if (raw_cause & (BIT(2) | BIT(9))) {
        return ESP_WAKEUP_GPIO;
    }
#if defined(RTEMS_BOARD_ESP32P4)
    const uint32_t uart_mask = BIT(4) | BIT(5) | BIT(6) | BIT(7) | BIT(8);
#else
    const uint32_t uart_mask = BIT(5) | BIT(6) | BIT(7) | BIT(8);
#endif
    const uint32_t ext1_mask = BIT(12);
#else
    if (raw_cause & BIT(2)) {
        return ESP_WAKEUP_GPIO;
    }
    const uint32_t uart_mask = BIT(6) | BIT(7);
#if !defined(RTEMS_BOARD_ESP32C3)
    const uint32_t ext1_mask = BIT(1);
#endif
#endif
    if (raw_cause & uart_mask) {
        return ESP_WAKEUP_UART;
    }
#if !defined(RTEMS_BOARD_ESP32C3)
    if (raw_cause & ext1_mask) {
        return ESP_WAKEUP_EXT1;
    }
#endif
#if defined(RTEMS_BOARD_ESP32P4) || defined(RTEMS_BOARD_ESP32S31)
    if (raw_cause & BIT(11)) {
        return ESP_WAKEUP_TOUCHPAD;
    }
#endif
#if defined(RTEMS_BOARD_ESP32S31)
    if (raw_cause & BIT(22)) {
        return ESP_WAKEUP_WIFI;
    }
    if (raw_cause & BIT(25)) {
        return ESP_WAKEUP_BT;
    }
#else
#if !defined(RTEMS_BOARD_ESP32H2) && !defined(RTEMS_BOARD_ESP32P4)
    if (raw_cause & BIT(5)) {
        return ESP_WAKEUP_WIFI;
    }
#endif
#if !defined(RTEMS_BOARD_ESP32P4)
    if (raw_cause & BIT(10)) {
        return ESP_WAKEUP_BT;
    }
#endif
#endif
#if defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32C6)
    if (raw_cause & BIT(11)) {
        return ESP_WAKEUP_ULP;
    }
    if (raw_cause & BIT(12)) {
        return ESP_WAKEUP_COCPU_TRAP;
    }
#elif defined(RTEMS_BOARD_ESP32P4) || defined(RTEMS_BOARD_ESP32S31)
#if defined(RTEMS_BOARD_ESP32P4)
    const uint32_t lp_core_mask = BIT(1);
#else
    const uint32_t lp_core_mask = BIT(0);
#endif
    if (raw_cause & lp_core_mask) {
        return ESP_WAKEUP_ULP;
    }
    if (raw_cause & BIT(16)) {
        return ESP_WAKEUP_COCPU_TRAP;
    }
#endif
#if defined(RTEMS_BOARD_ESP32P4)
    if (raw_cause & BIT(19)) {
        return ESP_WAKEUP_VAD;
    }
#endif
#if defined(RTEMS_BOARD_ESP32H2) || defined(RTEMS_BOARD_ESP32P4)
    if (raw_cause & BIT(15)) {
        return ESP_WAKEUP_VBAT_UNDER_VOLT;
    }
#endif
    return ESP_WAKEUP_UNDEFINED;
}
#endif
