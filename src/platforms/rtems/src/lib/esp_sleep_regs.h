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

#ifndef AVM_RTEMS_ESP_SLEEP_REGS_H
#define AVM_RTEMS_ESP_SLEEP_REGS_H

#define REG(addr) (*(volatile uint32_t *) (uintptr_t) (addr))
#define BIT(n) (1U << (n))

/* Wake sources, RTC/PMU entry status and GPIO routing from ESP-IDF's
 * pmu_reg.h and rtc_io_ll.h. Chip-specific power-state configuration and
 * CPU/PLL transitions live in esp_sleep_pmu.c and esp_sleep_clock.c.
 */
#if defined(RTEMS_BOARD_ESP32C3)
#define TIMER_WAKE BIT(3)
#define GPIO_WAKE BIT(2)
#define GPIO_COUNT 6
#define SYSTIMER_BASE_ADDR 0x60023000U
#elif defined(RTEMS_BOARD_ESP32S31)
#define TIMER_WAKE BIT(13)
#define GPIO_WAKE BIT(9)
#define GPIO_COUNT 8
#define SYSTIMER_BASE_ADDR 0x20399000U
#define PMU_BASE 0x20704000U
#define PMU_CTRL 0x150U
#define PMU_WAKE_STATUS 0x170U
#define PMU_RAW 0x18CU
#define PMU_CLEAR 0x198U
#define SLEEP_MODE 0x2070004CU
#define LP_PIN0 0x20713030U
#define LP_MUX0 0x20712000U
#define LP_MUX_SELECT 0x207001C4U
#elif defined(RTEMS_BOARD_ESP32H2)
/* The H2 uses the C6 PMU and LP timer layout, with its interrupt raw/clear
 * registers eight bytes higher. It has no LP IOMUX: only GPIO7-GPIO14
 * (rtcio 0-7) wake the SoC, through the HP IO MUX and the LP_AON EXT1
 * comparator that ESP-IDF's ext1_wakeup_prepare() configures.
 */
#define TIMER_WAKE BIT(4)
#define GPIO_WAKE BIT(1) /* LP_AON EXT1 comparator, the H2's GPIO wakeup. */
#define GPIO_COUNT 8
#define SYSTIMER_BASE_ADDR 0x6000B000U
#define PMU_BASE 0x600B0000U
#define PMU_CTRL 0x120U
#define PMU_WAKE_STATUS 0x140U
#define PMU_RAW 0x160U
#define PMU_CLEAR 0x16CU
#define SLEEP_MODE 0x600B1024U
#define LP_AON_GPIO_MUX 0x600B1028U
#define LP_AON_GPIO_HOLD0 0x600B102CU
#define LP_AON_EXT_WAKEUP 0x600B1040U
#define LPPERI_CLK_EN 0x600B2800U
#define SLEEP_IO_MUX_BASE 0x60090000U
#define GPIO_RTCIO_OFFSET 7U
#elif defined(RTEMS_BOARD_ESP32P4)
#define TIMER_WAKE BIT(13)
#define GPIO_WAKE BIT(9)
#define GPIO_COUNT 16
#define SYSTIMER_BASE_ADDR 0x500E2000U
#define PMU_BASE 0x50115000U
#define PMU_CTRL 0x120U
#define PMU_WAKE_STATUS 0x144U
#define PMU_RAW 0x164U
#define PMU_CLEAR 0x170U
#define SLEEP_MODE 0x50110050U
#define LP_PIN0 0x5012A034U
#define LP_MUX0 0x5012B008U
#else
#define TIMER_WAKE BIT(4)
#define GPIO_WAKE BIT(2)
#if defined(RTEMS_BOARD_ESP32C5)
#define GPIO_COUNT 7
#else
#define GPIO_COUNT 8
#endif
#define SYSTIMER_BASE_ADDR 0x6000A000U
#define PMU_BASE 0x600B0000U
#define LP_MUX_SELECT 0x600B1028U
#if defined(RTEMS_BOARD_ESP32C5)
#define PMU_CTRL 0x124U
#define PMU_WAKE_STATUS 0x144U
#define PMU_RAW 0x160U
#define PMU_CLEAR 0x16CU
#define SLEEP_MODE 0x600B1020U
#define LP_PIN0 0x600B4430U
#define LP_MUX0 0x600B4000U
#else
#define PMU_CTRL 0x120U
#define PMU_WAKE_STATUS 0x140U
#define PMU_RAW 0x15CU
#define PMU_CLEAR 0x168U
#define SLEEP_MODE 0x600B1024U
#define LP_PIN0 0x600B2028U
#define LP_MUX0 0x600B2048U
#endif
#endif

#endif
