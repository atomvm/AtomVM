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

#ifndef _ATOMVM_RTEMS_SNTP_H_
#define _ATOMVM_RTEMS_SNTP_H_

#include <stdbool.h>
#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*rtems_sntp_sync_cb)(const struct timeval *tv, void *arg);

int rtems_sntp_sync_now(const char *hostname, int port, int timeout_ms, struct timeval *tv_out);
int rtems_sntp_start(const char *hostname, int port, rtems_sntp_sync_cb cb, void *arg);
void rtems_sntp_stop(void);

#ifdef __cplusplus
}
#endif

#endif /* _ATOMVM_RTEMS_SNTP_H_ */
