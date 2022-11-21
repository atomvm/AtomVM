/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Fred Dushin <fred@dushin.net>
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

#define _GNU_SOURCE

#include "atom.h"
#include "defaultatoms.h"
#include "esp32_sys.h"
#include "interop.h"
#include "memory.h"
#include "nifs.h"
#include "platform_defaultatoms.h"
#include "term.h"

#include <esp_partition.h>
#include <esp_sleep.h>
#include <esp_system.h>
#include <soc/soc.h>
#include <stdlib.h>
#if defined __has_include
#  if __has_include (<esp_idf_version.h>)
#    include <esp_idf_version.h>
#  endif
#endif
#if ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(4, 3, 0) && CONFIG_IDF_TARGET_ESP32
#include <esp_rom_md5.h>
#else
#include <rom/md5_hash.h>
#endif
#if CONFIG_IDF_TARGET_ESP32C3
#include "esp32c3/rom/md5_hash.h"
#elif CONFIG_IDF_TARGET_ESP32S2
#include "esp32s2/rom/md5_hash.h"
#elif CONFIG_IDF_TARGET_ESP32S3
#include "esp32s3/rom/md5_hash.h"
#endif

//#define ENABLE_TRACE
#include "trace.h"

#define MD5_DIGEST_LENGTH 16

static const char *const esp_rst_unknown_atom   = "\xF"  "esp_rst_unknown";
static const char *const esp_rst_poweron        = "\xF"  "esp_rst_poweron";
static const char *const esp_rst_ext            = "\xB"  "esp_rst_ext";
static const char *const esp_rst_sw             = "\xA"  "esp_rst_sw";
static const char *const esp_rst_panic          = "\xD"  "esp_rst_panic";
static const char *const esp_rst_int_wdt        = "\xF"  "esp_rst_int_wdt";
static const char *const esp_rst_task_wdt       = "\x10" "esp_rst_task_wdt";
static const char *const esp_rst_wdt            = "\xB"  "esp_rst_wdt";
static const char *const esp_rst_deepsleep      = "\x11" "esp_rst_deepsleep";
static const char *const esp_rst_brownout       = "\x10" "esp_rst_brownout";
static const char *const esp_rst_sdio           = "\xC"  "esp_rst_sdio";
//                                                        123456789ABCDEF01

//
// NIFs
//

static term nif_esp_random(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    uint32_t r = esp_random();
    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_make_boxed_int(r, &ctx->heap);
}

static term nif_esp_random_bytes(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);

    avm_int_t len = term_to_int(argv[0]);
    if (len < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (len == 0) {
        if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(0) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term binary = term_create_empty_binary(0, &ctx->heap, ctx->global);
        return binary;
    } else {
        uint8_t *buf = malloc(len);
        if (UNLIKELY(IS_NULL_PTR(buf))) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        esp_fill_random(buf, len);
        if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term binary = term_from_literal_binary(buf, len, &ctx->heap, ctx->global);
        free(buf);
        return binary;
    }
}

static term nif_esp_restart(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    esp_restart();
    return OK_ATOM;
}

static term nif_esp_reset_reason(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    esp_reset_reason_t reason = esp_reset_reason();
    switch (reason) {
        case ESP_RST_UNKNOWN:
            return globalcontext_make_atom(ctx->global, esp_rst_unknown_atom);
        case ESP_RST_POWERON:
            return globalcontext_make_atom(ctx->global, esp_rst_poweron);
        case ESP_RST_EXT:
            return globalcontext_make_atom(ctx->global, esp_rst_ext);
        case ESP_RST_SW:
            return globalcontext_make_atom(ctx->global, esp_rst_sw);
        case ESP_RST_PANIC:
            return globalcontext_make_atom(ctx->global, esp_rst_panic);
        case ESP_RST_INT_WDT:
            return globalcontext_make_atom(ctx->global, esp_rst_int_wdt);
        case ESP_RST_TASK_WDT:
            return globalcontext_make_atom(ctx->global, esp_rst_task_wdt);
        case ESP_RST_WDT:
            return globalcontext_make_atom(ctx->global, esp_rst_wdt);
        case ESP_RST_DEEPSLEEP:
            return globalcontext_make_atom(ctx->global, esp_rst_deepsleep);
        case ESP_RST_BROWNOUT:
            return globalcontext_make_atom(ctx->global, esp_rst_brownout);
        case ESP_RST_SDIO:
            return globalcontext_make_atom(ctx->global, esp_rst_sdio);
        default:
            return UNDEFINED_ATOM;
    }
}

static term nif_esp_freq_hz(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    return term_from_int(APB_CLK_FREQ);
}

static const esp_partition_t *get_partition(term partition_name_term, bool *valid_string)
{
    int ok;
    char *partition_name = interop_term_to_string(partition_name_term, &ok);
    if (UNLIKELY(!ok)) {
        *valid_string = false;
        return NULL;
    }
    *valid_string = true;

    const esp_partition_t *partition = esp_partition_find_first(ESP_PARTITION_TYPE_DATA, ESP_PARTITION_SUBTYPE_ANY, partition_name);

    free(partition_name);

    return partition;
}

static term nif_esp_partition_erase_range(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    bool valid_partition_string;
    const esp_partition_t *partition = get_partition(argv[0], &valid_partition_string);
    if (UNLIKELY(!valid_partition_string)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(partition)) {
        return ERROR_ATOM;
    }

    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t offset = term_to_int(argv[1]);

    avm_int_t size = 0;
    if (argc == 3) {
        VALIDATE_VALUE(argv[2], term_is_integer);
        size = term_to_int(argv[2]);
    } else {
        size = partition->size - offset;
    }

    esp_err_t res = esp_partition_erase_range(partition, offset, size);
    if (UNLIKELY(res != ESP_OK)) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term nif_esp_partition_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    bool valid_partition_string;
    const esp_partition_t *partition = get_partition(argv[0], &valid_partition_string);
    if (UNLIKELY(!valid_partition_string)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(partition)) {
        return ERROR_ATOM;
    }

    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t offset = term_to_int(argv[1]);

    term binary_term = argv[2];
    VALIDATE_VALUE(binary_term, term_is_binary);
    avm_int_t size = term_binary_size(binary_term);
    const char *data = term_binary_data(binary_term);

    esp_err_t res = esp_partition_write(partition, offset, data, size);
    if (UNLIKELY(res != ESP_OK)) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term nif_esp_deep_sleep(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_any_integer);
    avm_int64_t msecs = term_maybe_unbox_int64(argv[0]);

    esp_deep_sleep(msecs * 1000ULL);

    // technically, this function does not return
    return OK_ATOM;
}
#if SOC_PM_SUPPORT_EXT_WAKEUP
static const char *const sleep_wakeup_ext0_atom = "\x11" "sleep_wakeup_ext0";
static const char *const sleep_wakeup_ext1_atom = "\x11" "sleep_wakeup_ext1";
#endif
static const char *const sleep_wakeup_timer_atom = "\x12" "sleep_wakeup_timer";
static const char *const sleep_wakeup_touchpad_atom = "\x15" "sleep_wakeup_touchpad";
static const char *const sleep_wakeup_ulp_atom = "\x10" "sleep_wakeup_ulp";
static const char *const sleep_wakeup_gpio_atom = "\x11" "sleep_wakeup_gpio";
static const char *const sleep_wakeup_uart_atom = "\x11" "sleep_wakeup_uart";
#ifdef ESP_SLEEP_WAKEUP_WIFI
static const char *const sleep_wakeup_wifi_atom = "\x11" "sleep_wakeup_wifi";
#endif
#ifdef ESP_SLEEP_WAKEUP_COCPU
static const char *const sleep_wakeup_cocpu_atom = "\x12" "sleep_wakeup_cocpu";
#endif
#ifdef ESP_SLEEP_WAKEUP_COCPU_TRAP_TRIG
static const char *const sleep_wakeup_cocpu_trap_trig_atom = "\x1C" "sleep_wakeup_cocpu_trap_trig";
#endif
#ifdef ESP_SLEEP_WAKEUP_BT
static const char *const sleep_wakeup_bt_atom = "\xF" "sleep_wakeup_bt";
#endif

static term nif_esp_sleep_get_wakeup_cause(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    esp_sleep_wakeup_cause_t cause = esp_sleep_get_wakeup_cause();

    switch (cause) {
        case ESP_SLEEP_WAKEUP_UNDEFINED:
            return UNDEFINED_ATOM;
#if SOC_PM_SUPPORT_EXT_WAKEUP
        case ESP_SLEEP_WAKEUP_EXT0:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_ext0_atom);
        case ESP_SLEEP_WAKEUP_EXT1:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_ext1_atom);
#endif
        case ESP_SLEEP_WAKEUP_TIMER:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_timer_atom);
        case ESP_SLEEP_WAKEUP_TOUCHPAD:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_touchpad_atom);
        case ESP_SLEEP_WAKEUP_ULP:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_ulp_atom);
        case ESP_SLEEP_WAKEUP_GPIO:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_gpio_atom);
        case ESP_SLEEP_WAKEUP_UART:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_uart_atom);
#ifdef ESP_SLEEP_WAKEUP_WIFI
        case ESP_SLEEP_WAKEUP_WIFI:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_wifi_atom);
#endif
#ifdef ESP_SLEEP_WAKEUP_COCPU
        case ESP_SLEEP_WAKEUP_COCPU:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_cocpu_atom);
#endif
#ifdef ESP_SLEEP_WAKEUP_COCPU_TRAP_TRIG
        case ESP_SLEEP_WAKEUP_COCPU_TRAP_TRIG:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_cocpu_trap_trig_atom);
#endif
#ifdef ESP_SLEEP_WAKEUP_BT
        case ESP_SLEEP_WAKEUP_BT:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_bt_atom);
#endif
        default:
            return ERROR_ATOM;
    }
}

#if SOC_PM_SUPPORT_EXT_WAKEUP

static term nif_esp_sleep_enable_ext0_wakeup(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);
    gpio_num_t pin = term_to_int(argv[0]);
    int level = term_to_int(argv[1]);
    esp_err_t err = esp_sleep_enable_ext0_wakeup(pin, level);
    if (UNLIKELY(err == ESP_ERR_INVALID_ARG)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(err != ESP_OK)) {
        return ERROR_ATOM;
    }
    return OK_ATOM;
}

static term nif_esp_sleep_enable_ext1_wakeup(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_any_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int64_t mask = term_maybe_unbox_int64(argv[0]);
    esp_sleep_ext1_wakeup_mode_t mode = term_to_int(argv[1]);
    esp_err_t err = esp_sleep_enable_ext1_wakeup(mask, mode);
    if (UNLIKELY(err == ESP_ERR_INVALID_ARG)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(err != ESP_OK)) {
        return ERROR_ATOM;
    }
    return OK_ATOM;
}

#endif

static term nif_rom_md5(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term data = argv[0];
    VALIDATE_VALUE(data, term_is_binary);

    unsigned char digest[MD5_DIGEST_LENGTH];
    struct MD5Context md5;
    #if __has_include (<esp_idf_version.h>) && ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(4, 3, 0) && CONFIG_IDF_TARGET_ESP32
    esp_rom_md5_init(&md5);
    esp_rom_md5_update(&md5, (const unsigned char *) term_binary_data(data), term_binary_size(data));
    esp_rom_md5_final(digest, &md5);
    #else
    MD5Init(&md5);
    MD5Update(&md5, (const unsigned char *) term_binary_data(data), term_binary_size(data));
    MD5Final(digest, &md5);
    #endif

    return term_from_literal_binary(digest, MD5_DIGEST_LENGTH, &ctx->heap, ctx->global);
}

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return ESP32_ATOM;
}

//
// NIF structures and distpatch
//

static const struct Nif esp_random_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_random
};
static const struct Nif esp_random_bytes_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_random_bytes
};
static const struct Nif esp_restart_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_restart
};
static const struct Nif esp_reset_reason_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_reset_reason
};
static const struct Nif esp_freq_hz_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_freq_hz
};
static const struct Nif esp_partition_erase_range_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_partition_erase_range
};
static const struct Nif esp_partition_write_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_partition_write
};
static const struct Nif esp_deep_sleep_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_deep_sleep
};
static const struct Nif esp_sleep_get_wakeup_cause_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sleep_get_wakeup_cause
};
#if SOC_PM_SUPPORT_EXT_WAKEUP
static const struct Nif esp_sleep_enable_ext0_wakeup_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sleep_enable_ext0_wakeup
};
static const struct Nif esp_sleep_enable_ext1_wakeup_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sleep_enable_ext1_wakeup
};
#endif
static const struct Nif rom_md5_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rom_md5
};
static const struct Nif atomvm_platform_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:random/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_random_nif;
    }
    if (strcmp("atomvm:rand_bytes/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_random_bytes_nif;
    }
    if (strcmp("esp:restart/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_restart_nif;
    }
    if (strcmp("esp:reset_reason/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_reset_reason_nif;
    }
    if (strcmp("esp:freq_hz/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_freq_hz_nif;
    }
    if (strcmp("esp:partition_erase_range/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_partition_erase_range_nif;
    }
    if (strcmp("esp:partition_erase_range/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_partition_erase_range_nif;
    }
    if (strcmp("esp:partition_write/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_partition_write_nif;
    }
    if (strcmp("esp:deep_sleep/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_deep_sleep_nif;
    }
    if (strcmp("esp:sleep_get_wakeup_cause/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sleep_get_wakeup_cause_nif;
    }
#if SOC_PM_SUPPORT_EXT_WAKEUP
    if (strcmp("esp:sleep_enable_ext0_wakeup/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sleep_enable_ext0_wakeup_nif;
    }
    if (strcmp("esp:sleep_enable_ext1_wakeup/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sleep_enable_ext1_wakeup_nif;
    }
#endif
    if (strcmp("erlang:md5/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rom_md5_nif;
    }
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
    const struct Nif *nif = nif_collection_resolve_nif(nifname);
    if (nif) {
        return nif;
    }
    return NULL;
}
