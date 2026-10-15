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

#if defined(RTEMS_BOARD_ESP32C3)
uint64_t esp32c3_rtc_get_counter(void);
void esp32c3_rtc_timekeeper_record(const struct timeval *tv);
bool esp32c3_rtc_timekeeper_restore(void);
#endif

#ifdef __cplusplus
}
#endif

#endif /* _ATOMVM_RTEMS_RTC_H_ */
