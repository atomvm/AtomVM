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

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "rtems_sys.h"

#ifndef CONFIG_AVM_RTC_SLOW_MAX_SIZE
#define CONFIG_AVM_RTC_SLOW_MAX_SIZE 4086
#endif

// Ensure checksum is incorrect when memory is zeroed.
#define RTC_SLOW_CRC32_MAGIC 0x55555555U

#if defined(RTEMS_BOARD_ESP32C3)
#define ESP32C3_RTC_RAM_BASE 0x50000000U

struct esp32c3_rtc_slow_storage {
    uint32_t checksum;
    uint16_t size;
    uint16_t reserved;
    uint8_t data[CONFIG_AVM_RTC_SLOW_MAX_SIZE];
};
#endif

static uint32_t rtc_crc32_le(uint32_t crc, const uint8_t *buf, size_t len)
{
    crc = ~crc;
    for (size_t i = 0; i < len; i++) {
        crc ^= buf[i];
        for (int j = 0; j < 8; j++) {
            crc = (crc >> 1) ^ (0xEDB88320u & -(crc & 1));
        }
    }
    return ~crc;
}

static term nif_esp_rtc_slow_get_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    volatile struct esp32c3_rtc_slow_storage *storage =
        (volatile struct esp32c3_rtc_slow_storage *) ESP32C3_RTC_RAM_BASE;

    uint16_t size = storage->size;
    if (size > CONFIG_AVM_RTC_SLOW_MAX_SIZE) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t expected_checksum = storage->checksum;
    uint32_t checksum = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, (const uint8_t *) (uintptr_t) storage->data, size);
    if (checksum != expected_checksum) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(size)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary((const void *) (uintptr_t) storage->data, size, &ctx->heap, ctx->global);
#else
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_esp_rtc_slow_set_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_binary);
    term binary = argv[0];
    size_t size = term_binary_size(binary);

#if defined(RTEMS_BOARD_ESP32C3)
    if (size > CONFIG_AVM_RTC_SLOW_MAX_SIZE) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(binary);
    uint32_t checksum = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, data, size);

    volatile struct esp32c3_rtc_slow_storage *storage =
        (volatile struct esp32c3_rtc_slow_storage *) ESP32C3_RTC_RAM_BASE;

    memcpy((void *) (uintptr_t) storage->data, data, size);
    storage->size = (uint16_t) size;
    storage->checksum = checksum;

    return OK_ATOM;
#else
    (void) size;
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static const struct Nif esp_rtc_slow_get_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_rtc_slow_get_binary
};

static const struct Nif esp_rtc_slow_set_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_rtc_slow_set_binary
};

const struct Nif *rtc_slow_nif_get_nif(const char *nifname)
{
    if (strcmp("esp:rtc_slow_get_binary/0", nifname) == 0 ||
        strcmp("atomvm_rtems:rtc_slow_get_binary/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_rtc_slow_get_binary_nif;
    }
    if (strcmp("esp:rtc_slow_set_binary/1", nifname) == 0 ||
        strcmp("atomvm_rtems:rtc_slow_set_binary/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_rtc_slow_set_binary_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(rtc_slow, NULL, NULL, rtc_slow_nif_get_nif)
