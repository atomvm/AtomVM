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

#ifndef _NVS_DRIVER_H
#define _NVS_DRIVER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <context.h>
#include <globalcontext.h>
#include <nifs.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_NVS_KEY_SIZE 15
#define MAX_NVS_NAMESPACE_SIZE 15

typedef enum {
    NVS_OK = 0,
    NVS_NOT_FOUND,
    NVS_NAMESPACE_NOT_FOUND,
    NVS_OUT_OF_MEMORY,
    NVS_ERR
} nvs_result_t;

bool esp32c3_nvs_init(void);
nvs_result_t esp32c3_nvs_get(const char *namespace, const char *key, void *dest, size_t *size);
nvs_result_t esp32c3_nvs_set(const char *namespace, const char *key, const void *src, size_t size);
nvs_result_t esp32c3_nvs_erase_key(const char *namespace, const char *key);
nvs_result_t esp32c3_nvs_erase_all(const char *namespace);
nvs_result_t esp32c3_nvs_reformat(void);

void nvs_nif_init(GlobalContext *global);
const struct Nif *nvs_nif_get_nif(const char *nifname);

#ifdef __cplusplus
}
#endif

#endif /* _NVS_DRIVER_H */
