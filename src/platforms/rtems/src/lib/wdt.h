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

#ifndef _ATOMVM_RTEMS_WDT_H_
#define _ATOMVM_RTEMS_WDT_H_

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
    uint32_t timeout_ms;
    uint32_t idle_core_mask;
    bool trigger_panic;
} esp_task_wdt_config_t;

typedef void *esp_task_wdt_user_handle_t;

struct esp_task_wdt_user_handle_and_name
{
    char *user_name;
    esp_task_wdt_user_handle_t user_handle;
};

int esp32c3_task_wdt_init(const esp_task_wdt_config_t *config);
int esp32c3_task_wdt_reconfigure(const esp_task_wdt_config_t *config);
int esp32c3_task_wdt_deinit(void);
int esp32c3_task_wdt_add_user(const char *user_name, esp_task_wdt_user_handle_t *user_handle);
int esp32c3_task_wdt_reset_user(esp_task_wdt_user_handle_t user_handle);
int esp32c3_task_wdt_delete_user(esp_task_wdt_user_handle_t user_handle);
void esp32c3_wdt_feed_hw(void);

#ifdef __cplusplus
}
#endif

#endif /* _ATOMVM_RTEMS_WDT_H_ */
