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

#ifndef _ATOMVM_RTEMS_RTC_H_
#define _ATOMVM_RTEMS_RTC_H_

#include <stdbool.h>
#include <stdint.h>
#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#endif

uint64_t rtems_esp_rtc_get_counter(void);
void rtems_esp_rtc_timekeeper_record(const struct timeval *tv);
bool rtems_esp_rtc_timekeeper_restore(void);

uint32_t rtems_esp_rtc_frequency(void);
/* Measure the RTC clock without installing it as the system timecounter. */
uint32_t rtems_esp_rtc_measure_frequency(void);
uint32_t rtems_esp_sleep_enter(bool deep, uint32_t wakeup, uint64_t deadline);
int rtems_esp_sleep_start(bool deep, uint64_t usecs, uint32_t gpio_mask, int mode, uint32_t *cause);
int rtems_esp_deep_sleep(uint64_t time_in_us);
int rtems_esp_light_sleep(void);
int rtems_esp_sleep_enable_timer_wakeup(uint64_t time_in_us);
int rtems_esp_deep_sleep_enable_gpio_wakeup(uint64_t mask, int mode);
int rtems_esp_sleep_enable_gpio_wakeup(void);
uint32_t rtems_esp_sleep_get_wakeup_cause(void);
int rtems_esp_deep_sleep_hold_en(void);
int rtems_esp_deep_sleep_hold_dis(void);

#ifdef __cplusplus
}
#endif

#endif /* _ATOMVM_RTEMS_RTC_H_ */
