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

#include "wdt.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <rtems.h>

#include <atom.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>
#include <utils.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "rtems_sys.h"

#define MAX_TWDT_USERS 16

#define ESP_OK                    0
#define ESP_ERR_NO_MEM            0x101
#define ESP_ERR_INVALID_ARG       0x102
#define ESP_ERR_INVALID_STATE     0x103
#define ESP_ERR_NOT_FOUND         0x105

#if defined(RTEMS_BOARD_ESP32)
#define WRITE_PERI_REG(addr, val) (*((volatile uint32_t *) (addr)) = (uint32_t) (val))
#define READ_PERI_REG(addr)       (*((volatile uint32_t *) (addr)))

#if defined(RTEMS_BOARD_ESP32C6)
#define ESP32_TIMG0_BASE             0x60008000U
#define ESP32_RTC_CNTL_BASE          0x600B1C00U
#define ESP32_RTC_CNTL_OPTIONS0_REG  0x600B1000U
#define ESP32_RTC_CNTL_WDTCONFIG0    (ESP32_RTC_CNTL_BASE + 0x00U)
#define ESP32_RTC_CNTL_WDTCONFIG1    (ESP32_RTC_CNTL_BASE + 0x04U)
#define ESP32_RTC_CNTL_WDTFEED       (ESP32_RTC_CNTL_BASE + 0x14U)
#define ESP32_RTC_CNTL_WDTWPROTECT   (ESP32_RTC_CNTL_BASE + 0x18U)
#else
#define ESP32_TIMG0_BASE             0x6001F000U
#define ESP32_RTC_CNTL_BASE          0x60008000U
#define ESP32_RTC_CNTL_OPTIONS0_REG  (ESP32_RTC_CNTL_BASE + 0x00U)
#define ESP32_RTC_CNTL_WDTCONFIG0    (ESP32_RTC_CNTL_BASE + 0x90U)
#define ESP32_RTC_CNTL_WDTCONFIG1    (ESP32_RTC_CNTL_BASE + 0x94U)
#define ESP32_RTC_CNTL_WDTFEED       (ESP32_RTC_CNTL_BASE + 0xA0U)
#define ESP32_RTC_CNTL_WDTWPROTECT   (ESP32_RTC_CNTL_BASE + 0xA4U)
#endif

#define ESP32_TIMG0_WDTCONFIG0_REG   (ESP32_TIMG0_BASE + 0x48U)
#define ESP32_TIMG0_WDTCONFIG1_REG   (ESP32_TIMG0_BASE + 0x4CU)
#define ESP32_TIMG0_WDTCONFIG2_REG   (ESP32_TIMG0_BASE + 0x50U)
#define ESP32_TIMG0_WDTCONFIG3_REG   (ESP32_TIMG0_BASE + 0x54U)
#define ESP32_TIMG0_WDTFEED_REG      (ESP32_TIMG0_BASE + 0x60U)
#define ESP32_TIMG0_WDTWPROTECT_REG  (ESP32_TIMG0_BASE + 0x64U)

#define WDT_WKEY_VALUE                 0x50D83AA1U
#endif

struct twdt_user {
    bool in_use;
    char *name;
    int64_t last_feed_ms;
};

struct twdt_state {
    bool initialized;
    esp_task_wdt_config_t config;
    struct twdt_user users[MAX_TWDT_USERS];
    rtems_id timer_id;
    bool timer_active;
};

static struct twdt_state s_twdt;

static const char *const already_started_atom = ATOM_STR("\xF", "already_started");

void esp32c3_wdt_feed_hw(void)
{
#if defined(RTEMS_BOARD_ESP32)
    // Feed Timer Group 0 Watchdog (MWDT0)
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, WDT_WKEY_VALUE);
    WRITE_PERI_REG(ESP32_TIMG0_WDTFEED_REG, 1);
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, 0);

    // Feed RTC Watchdog (RWDT)
    WRITE_PERI_REG(ESP32_RTC_CNTL_WDTWPROTECT, WDT_WKEY_VALUE);
    WRITE_PERI_REG(ESP32_RTC_CNTL_WDTFEED, 1);
    WRITE_PERI_REG(ESP32_RTC_CNTL_WDTWPROTECT, 0);
#endif
}

static void twdt_timer_tick(rtems_id timer_id, void *arg)
{
    UNUSED(arg);

    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);
    if (!s_twdt.initialized) {
        rtems_interrupt_local_enable(level);
        return;
    }

    int64_t now_ms = (int64_t) (rtems_clock_get_uptime_nanoseconds() / 1000000ULL);
    bool any_user_timeout = false;
    char timed_out_name[32] = {0};

    for (int i = 0; i < MAX_TWDT_USERS; i++) {
        if (s_twdt.users[i].in_use) {
            if ((now_ms - s_twdt.users[i].last_feed_ms) > (int64_t) s_twdt.config.timeout_ms) {
                any_user_timeout = true;
                if (s_twdt.users[i].name) {
                    strncpy(timed_out_name, s_twdt.users[i].name, sizeof(timed_out_name) - 1);
                }
                break;
            }
        }
    }
    bool trigger_panic = s_twdt.config.trigger_panic;
    rtems_interrupt_local_enable(level);

    if (any_user_timeout) {
        if (trigger_panic) {
            fprintf(stderr, "\nTask watchdog got triggered. The following tasks/users did not reset the watchdog in time:\n - %s\n", timed_out_name);
#if defined(RTEMS_BOARD_ESP32)
            WRITE_PERI_REG(ESP32_RTC_CNTL_OPTIONS0_REG, (1U << 31)); // SW_SYS_RST
#endif
            rtems_fatal(RTEMS_FATAL_SOURCE_APPLICATION, 1);
        } else {
            static int64_t s_last_warn_ms = 0;
            if (now_ms - s_last_warn_ms > 2000) {
                s_last_warn_ms = now_ms;
                fprintf(stderr, "task_wdt: Task watchdog got triggered (panic disabled). User '%s' timed out.\n", timed_out_name);
            }
            esp32c3_wdt_feed_hw();
        }
    } else {
        esp32c3_wdt_feed_hw();
    }

    rtems_interrupt_local_disable(level);
    if (s_twdt.initialized && s_twdt.timer_active) {
        rtems_interval ticks = rtems_clock_get_ticks_per_second() / 4;
        if (ticks == 0) {
            ticks = 1;
        }
        rtems_timer_fire_after(timer_id, ticks, twdt_timer_tick, NULL);
    }
    rtems_interrupt_local_enable(level);
}

int esp32c3_task_wdt_init(const esp_task_wdt_config_t *config)
{
    if (config == NULL || config->timeout_ms == 0) {
        return ESP_ERR_INVALID_ARG;
    }

    if (s_twdt.initialized) {
        return ESP_ERR_INVALID_STATE;
    }

    if (!s_twdt.timer_active) {
        rtems_status_code sc = rtems_timer_create(rtems_build_name('T', 'W', 'D', 'T'), &s_twdt.timer_id);
        if (sc != RTEMS_SUCCESSFUL) {
            return ESP_ERR_NO_MEM;
        }
        s_twdt.timer_active = true;
    }

    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);

    s_twdt.config = *config;
    s_twdt.initialized = true;

    int64_t now_ms = (int64_t) (rtems_clock_get_uptime_nanoseconds() / 1000000ULL);
    for (int i = 0; i < MAX_TWDT_USERS; i++) {
        if (s_twdt.users[i].in_use) {
            s_twdt.users[i].last_feed_ms = now_ms;
        }
    }

#if defined(RTEMS_BOARD_ESP32)
    // Configure hardware MWDT0
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, WDT_WKEY_VALUE);
    // Prescaler: 80 MHz APB / 80000 = 1 kHz (1 ms per tick)
    WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG1_REG, (80000U << 16));
    WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG2_REG, config->timeout_ms);
    if (config->trigger_panic) {
        // Enable WDT + Stage 0 reset CPU (2) or system (3)
        WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG0_REG, (1U << 31) | (3U << 29));
    } else {
        // Enable WDT with Stage 0 disabled (software timer will handle notifications)
        WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG0_REG, (1U << 31));
    }
    WRITE_PERI_REG(ESP32_TIMG0_WDTFEED_REG, 1);
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, 0);
#endif

    rtems_interval ticks = rtems_clock_get_ticks_per_second() / 4;
    if (ticks == 0) {
        ticks = 1;
    }
    rtems_timer_fire_after(s_twdt.timer_id, ticks, twdt_timer_tick, NULL);

    rtems_interrupt_local_enable(level);
    return ESP_OK;
}

int esp32c3_task_wdt_reconfigure(const esp_task_wdt_config_t *config)
{
    if (config == NULL || config->timeout_ms == 0) {
        return ESP_ERR_INVALID_ARG;
    }

    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);
    if (!s_twdt.initialized) {
        rtems_interrupt_local_enable(level);
        return ESP_ERR_INVALID_STATE;
    }

    s_twdt.config = *config;

    int64_t now_ms = (int64_t) (rtems_clock_get_uptime_nanoseconds() / 1000000ULL);
    for (int i = 0; i < MAX_TWDT_USERS; i++) {
        if (s_twdt.users[i].in_use) {
            s_twdt.users[i].last_feed_ms = now_ms;
        }
    }

#if defined(RTEMS_BOARD_ESP32)
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, WDT_WKEY_VALUE);
    WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG2_REG, config->timeout_ms);
    if (config->trigger_panic) {
        WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG0_REG, (1U << 31) | (3U << 29));
    } else {
        WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG0_REG, (1U << 31));
    }
    WRITE_PERI_REG(ESP32_TIMG0_WDTFEED_REG, 1);
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, 0);
#endif

    rtems_interrupt_local_enable(level);
    return ESP_OK;
}

int esp32c3_task_wdt_deinit(void)
{
    char *names_to_free[MAX_TWDT_USERS] = {0};

    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);
    if (!s_twdt.initialized) {
        rtems_interrupt_local_enable(level);
        return ESP_ERR_INVALID_STATE;
    }

    s_twdt.initialized = false;

#if defined(RTEMS_BOARD_ESP32)
    // Disable hardware MWDT0
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, WDT_WKEY_VALUE);
    WRITE_PERI_REG(ESP32_TIMG0_WDTCONFIG0_REG, 0);
    WRITE_PERI_REG(ESP32_TIMG0_WDTFEED_REG, 1);
    WRITE_PERI_REG(ESP32_TIMG0_WDTWPROTECT_REG, 0);
#endif

    for (int i = 0; i < MAX_TWDT_USERS; i++) {
        if (s_twdt.users[i].in_use) {
            names_to_free[i] = s_twdt.users[i].name;
            s_twdt.users[i].name = NULL;
            s_twdt.users[i].in_use = false;
        }
    }
    rtems_interrupt_local_enable(level);

    for (int i = 0; i < MAX_TWDT_USERS; i++) {
        if (names_to_free[i]) {
            free(names_to_free[i]);
        }
    }

    if (s_twdt.timer_active) {
        rtems_timer_cancel(s_twdt.timer_id);
        rtems_timer_delete(s_twdt.timer_id);
        s_twdt.timer_active = false;
    }

    return ESP_OK;
}

int esp32c3_task_wdt_add_user(const char *user_name, esp_task_wdt_user_handle_t *user_handle)
{
    if (user_name == NULL || user_handle == NULL) {
        return ESP_ERR_INVALID_ARG;
    }

    char *name_copy = strdup(user_name);
    if (name_copy == NULL) {
        return ESP_ERR_NO_MEM;
    }

    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);
    if (!s_twdt.initialized) {
        rtems_interrupt_local_enable(level);
        free(name_copy);
        return ESP_ERR_INVALID_STATE;
    }

    for (int i = 0; i < MAX_TWDT_USERS; i++) {
        if (!s_twdt.users[i].in_use) {
            s_twdt.users[i].in_use = true;
            s_twdt.users[i].name = name_copy;
            s_twdt.users[i].last_feed_ms = (int64_t) (rtems_clock_get_uptime_nanoseconds() / 1000000ULL);
            *user_handle = &s_twdt.users[i];
            rtems_interrupt_local_enable(level);
            return ESP_OK;
        }
    }

    rtems_interrupt_local_enable(level);
    free(name_copy);
    return ESP_ERR_NO_MEM;
}

int esp32c3_task_wdt_reset_user(esp_task_wdt_user_handle_t user_handle)
{
    if (user_handle == NULL) {
        return ESP_ERR_INVALID_ARG;
    }

    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);
    if (!s_twdt.initialized) {
        rtems_interrupt_local_enable(level);
        return ESP_ERR_INVALID_STATE;
    }

    struct twdt_user *user = (struct twdt_user *) user_handle;
    if (user < &s_twdt.users[0] || user >= &s_twdt.users[MAX_TWDT_USERS] || !user->in_use) {
        rtems_interrupt_local_enable(level);
        return ESP_ERR_NOT_FOUND;
    }

    user->last_feed_ms = (int64_t) (rtems_clock_get_uptime_nanoseconds() / 1000000ULL);
    esp32c3_wdt_feed_hw();

    rtems_interrupt_local_enable(level);
    return ESP_OK;
}

int esp32c3_task_wdt_delete_user(esp_task_wdt_user_handle_t user_handle)
{
    if (user_handle == NULL) {
        return ESP_ERR_INVALID_ARG;
    }

    char *name_to_free = NULL;
    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);
    if (!s_twdt.initialized) {
        rtems_interrupt_local_enable(level);
        return ESP_ERR_INVALID_STATE;
    }

    struct twdt_user *user = (struct twdt_user *) user_handle;
    if (user < &s_twdt.users[0] || user >= &s_twdt.users[MAX_TWDT_USERS] || !user->in_use) {
        rtems_interrupt_local_enable(level);
        return ESP_ERR_NOT_FOUND;
    }

    name_to_free = user->name;
    user->name = NULL;
    user->in_use = false;
    rtems_interrupt_local_enable(level);

    if (name_to_free) {
        free(name_to_free);
    }
    return ESP_OK;
}

static term parse_task_wdt_config(Context *ctx, esp_task_wdt_config_t *config, term argv[])
{
    VALIDATE_VALUE(argv[0], term_is_tuple);
    size_t tuple_size = term_get_tuple_arity(argv[0]);
    if (tuple_size != 3) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term timeout_ms_term = term_get_tuple_element(argv[0], 0);
    VALIDATE_VALUE(timeout_ms_term, term_is_integer);
    avm_int_t timeout_ms = term_to_int(timeout_ms_term);
    if (timeout_ms <= 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term core_mask_term = term_get_tuple_element(argv[0], 1);
    VALIDATE_VALUE(core_mask_term, term_is_integer);
    avm_int_t core_mask = term_to_int(core_mask_term);
    if (core_mask < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (core_mask > (1 << 2)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term trigger_panic_term = term_get_tuple_element(argv[0], 2);
    if (trigger_panic_term != TRUE_ATOM && trigger_panic_term != FALSE_ATOM) {
        RAISE_ERROR(BADARG_ATOM);
    }

    config->timeout_ms = (uint32_t) timeout_ms;
    config->idle_core_mask = (uint32_t) core_mask;
    config->trigger_panic = (trigger_panic_term != FALSE_ATOM);

    return OK_ATOM;
}

static term nif_esp_task_wdt_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    esp_task_wdt_config_t config;

    if (term_is_invalid_term(parse_task_wdt_config(ctx, &config, argv))) {
        return term_invalid_term();
    }

    int result = esp32c3_task_wdt_init(&config);
    if (result == ESP_OK) {
        return OK_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    if (result == ESP_ERR_INVALID_STATE) {
        term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(ctx->global, already_started_atom));
    } else {
        term_put_tuple_element(error_tuple, 1, term_from_int(result));
    }
    return error_tuple;
}

static term nif_esp_task_wdt_reconfigure(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    esp_task_wdt_config_t config;

    if (term_is_invalid_term(parse_task_wdt_config(ctx, &config, argv))) {
        return term_invalid_term();
    }

    int result = esp32c3_task_wdt_reconfigure(&config);
    if (result == ESP_OK) {
        return OK_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    if (result == ESP_ERR_INVALID_STATE) {
        term_put_tuple_element(error_tuple, 1, NOPROC_ATOM);
    } else {
        term_put_tuple_element(error_tuple, 1, term_from_int(result));
    }
    return error_tuple;
}

static term nif_esp_task_wdt_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    int result = esp32c3_task_wdt_deinit();
    if (result == ESP_OK) {
        return OK_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, term_from_int(result));
    return error_tuple;
}

static term nif_esp_task_wdt_add_user(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    int ok;
    char *user_name = interop_term_to_string(argv[0], &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(sizeof(struct esp_task_wdt_user_handle_and_name))) != MEMORY_GC_OK)) {
        free(user_name);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term binary = term_create_empty_binary(sizeof(struct esp_task_wdt_user_handle_and_name), &ctx->heap, ctx->global);
    struct esp_task_wdt_user_handle_and_name *handle = (struct esp_task_wdt_user_handle_and_name *) term_binary_data(binary);
    handle->user_name = user_name;

    int result = esp32c3_task_wdt_add_user(handle->user_name, &handle->user_handle);
    if (result == ESP_OK) {
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result_tuple, 0, OK_ATOM);
        term_put_tuple_element(result_tuple, 1, binary);
        return result_tuple;
    } else {
        free(user_name);
        handle->user_name = NULL;
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(result_tuple, 1, term_from_int(result));
        return result_tuple;
    }
}

static term nif_esp_task_wdt_reset_user(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_binary);
    size_t binary_size = term_binary_size(argv[0]);
    if (binary_size != sizeof(struct esp_task_wdt_user_handle_and_name)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct esp_task_wdt_user_handle_and_name *handle = (struct esp_task_wdt_user_handle_and_name *) term_binary_data(argv[0]);
    int result = esp32c3_task_wdt_reset_user(handle->user_handle);
    if (result == ESP_OK) {
        return OK_ATOM;
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, term_from_int(result));
        return error_tuple;
    }
}

static term nif_esp_task_wdt_delete_user(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_binary);
    size_t binary_size = term_binary_size(argv[0]);
    if (binary_size != sizeof(struct esp_task_wdt_user_handle_and_name)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct esp_task_wdt_user_handle_and_name *handle = (struct esp_task_wdt_user_handle_and_name *) term_binary_data(argv[0]);
    int result = esp32c3_task_wdt_delete_user(handle->user_handle);
    if (result == ESP_OK) {
        if (handle->user_name) {
            free(handle->user_name);
            handle->user_name = NULL;
        }
        return OK_ATOM;
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, term_from_int(result));
        return error_tuple;
    }
}

static term nif_esp_wdt_feed(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    esp32c3_wdt_feed_hw();
    return OK_ATOM;
}

static const struct Nif esp_task_wdt_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_task_wdt_init
};

static const struct Nif esp_task_wdt_reconfigure_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_task_wdt_reconfigure
};

static const struct Nif esp_task_wdt_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_task_wdt_deinit
};

static const struct Nif esp_task_wdt_add_user_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_task_wdt_add_user
};

static const struct Nif esp_task_wdt_reset_user_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_task_wdt_reset_user
};

static const struct Nif esp_task_wdt_delete_user_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_task_wdt_delete_user
};

static const struct Nif esp_wdt_feed_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_wdt_feed
};

static const struct Nif *wdt_get_nif(const char *nifname)
{
    if (strcmp("esp:task_wdt_init/1", nifname) == 0 ||
        strcmp("atomvm_rtems:task_wdt_init/1", nifname) == 0) {
        return &esp_task_wdt_init_nif;
    }
    if (strcmp("esp:task_wdt_reconfigure/1", nifname) == 0 ||
        strcmp("atomvm_rtems:task_wdt_reconfigure/1", nifname) == 0) {
        return &esp_task_wdt_reconfigure_nif;
    }
    if (strcmp("esp:task_wdt_deinit/0", nifname) == 0 ||
        strcmp("atomvm_rtems:task_wdt_deinit/0", nifname) == 0) {
        return &esp_task_wdt_deinit_nif;
    }
    if (strcmp("esp:task_wdt_add_user/1", nifname) == 0 ||
        strcmp("atomvm_rtems:task_wdt_add_user/1", nifname) == 0) {
        return &esp_task_wdt_add_user_nif;
    }
    if (strcmp("esp:task_wdt_reset_user/1", nifname) == 0 ||
        strcmp("atomvm_rtems:task_wdt_reset_user/1", nifname) == 0) {
        return &esp_task_wdt_reset_user_nif;
    }
    if (strcmp("esp:task_wdt_delete_user/1", nifname) == 0 ||
        strcmp("atomvm_rtems:task_wdt_delete_user/1", nifname) == 0) {
        return &esp_task_wdt_delete_user_nif;
    }
    if (strcmp("esp:wdt_feed/0", nifname) == 0 ||
        strcmp("atomvm_rtems:wdt_feed/0", nifname) == 0) {
        return &esp_wdt_feed_nif;
    }
    return NULL;
}

static void wdt_init_cb(GlobalContext *global)
{
    UNUSED(global);
    // Initialize default Task Watchdog Timer on startup (5000 ms, core 0, trigger_panic false)
    esp_task_wdt_config_t default_config = {
        .timeout_ms = 5000,
        .idle_core_mask = 0,
        .trigger_panic = false
    };
    esp32c3_task_wdt_init(&default_config);
}

static void wdt_destroy_cb(GlobalContext *global)
{
    UNUSED(global);
    esp32c3_task_wdt_deinit();
}

REGISTER_NIF_COLLECTION(wdt, wdt_init_cb, wdt_destroy_cb, wdt_get_nif)
