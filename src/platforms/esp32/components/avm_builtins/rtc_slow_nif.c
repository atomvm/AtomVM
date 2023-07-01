/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_RTC_SLOW_NIFS

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <stdlib.h>
#include <term.h>

#include <rom/crc.h>

#include "esp32_sys.h"

//#define ENABLE_TRACE
#include "trace.h"

#ifndef CONFIG_AVM_RTC_SLOW_MAX_SIZE
#define CONFIG_AVM_RTC_SLOW_MAX_SIZE 4086
#endif

// Ensure checksum is incorrect when memory is zeroed.
#define RTC_SLOW_CRC32_MAGIC 0x55555555

RTC_NOINIT_ATTR uint32_t rtc_slow_data_checksum;
RTC_NOINIT_ATTR uint16_t rtc_slow_data_size;
RTC_NOINIT_ATTR uint8_t rtc_slow_data[CONFIG_AVM_RTC_SLOW_MAX_SIZE];

static const struct Nif *rtc_slow_nif_get_nif(const char *nifname);

static term nif_esp_rtc_slow_get_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    if (rtc_slow_data_size > sizeof(rtc_slow_data)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t checksum = crc32_le(RTC_SLOW_CRC32_MAGIC, rtc_slow_data, rtc_slow_data_size);
    if (checksum != rtc_slow_data_checksum) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(rtc_slow_data_size)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary(rtc_slow_data, rtc_slow_data_size, &ctx->heap, ctx->global);
}

static term nif_esp_rtc_slow_set_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_binary);
    term binary = argv[0];
    size_t size = term_binary_size(binary);
    if (size > sizeof(rtc_slow_data)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(binary);
    rtc_slow_data_checksum = crc32_le(RTC_SLOW_CRC32_MAGIC, data, size);
    rtc_slow_data_size = size;
    memcpy(rtc_slow_data, data, size);

    return OK_ATOM;
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
    if (strcmp("esp:rtc_slow_get_binary/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_rtc_slow_get_binary_nif;
    }
    if (strcmp("esp:rtc_slow_set_binary/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_rtc_slow_set_binary_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(rtc_slow, NULL, NULL, rtc_slow_nif_get_nif)

#endif
