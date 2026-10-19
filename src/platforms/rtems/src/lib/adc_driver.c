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

#include "adc_driver.h"

#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include <atom.h>
#include <defaultatoms.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>
#include <utils.h>

#include "rtems_sys.h"

#if defined(RTEMS_BOARD_ESP32)
#include <bsp/gpio.h>
#include <bsp/pin.h>
#include <rtems/counter.h>

#define WRITE_PERI_REG(addr, val) (*((volatile uint32_t *) (uintptr_t) (addr)) = (uint32_t) (val))
#define READ_PERI_REG(addr) (*((volatile uint32_t *) (uintptr_t) (addr)))

/* ESP-IDF adc_ll.h and the chip's ADC, PCR and IO_MUX register definitions. */
#if defined(RTEMS_BOARD_ESP32C3)
#define ESP_ADC_BASE 0x60040000U
#define ESP_ADC_GPIO_ENABLE_CLEAR 0x60004028U
#define ESP_ADC_IOMUX_BASE 0x60009004U
#elif defined(RTEMS_BOARD_ESP32C5)
#define ESP_ADC_BASE 0x6000E000U
#define ESP_ADC_GPIO_ENABLE_CLEAR 0x6009103CU
#define ESP_ADC_IOMUX_BASE 0x60090000U
#define ESP_ADC_CLOCK_REG 0x60096088U
#elif defined(RTEMS_BOARD_ESP32C6)
#define ESP_ADC_BASE 0x6000E000U
#define ESP_ADC_GPIO_ENABLE_CLEAR 0x60091028U
#define ESP_ADC_IOMUX_BASE 0x60090004U
#define ESP_ADC_CLOCK_REG 0x60096080U
#elif defined(RTEMS_BOARD_ESP32S31)
#define ESP_ADC_BASE 0x20815000U
#define ESP_ADC_GPIO_ENABLE_CLEAR 0x2058303CU
#define ESP_ADC_IOMUX_BASE 0x20582000U
#endif
#define ESP_ADC_IOMUX_REG(pin) (ESP_ADC_IOMUX_BASE + (uint32_t) (pin) * 4U)
static pthread_mutex_t adc_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#define DEFAULT_SAMPLES 64
#define MAX_SAMPLES 100000

struct ChannelResource
{
    adc_atten_t attenuation;
    adc_bitwidth_t width;
    adc_unit_t adc_unit;
    adc_channel_t channel;
    int pin;
    uint32_t saved_mux;
    struct UnitResource *owner;
    char pin_owner[16];
};

struct UnitResource
{
    uint32_t unit_id;
    atomic_bool initialized;
};

static ErlNifResourceType *adc_unit_resource;
static ErlNifResourceType *adc_channel_resource;

static const AtomStringIntPair bit_width_table[] = {
    { ATOM_STR("\x7", "bit_max"), ADC_BITWIDTH_DEFAULT },
    { ATOM_STR("\x6", "bit_12"), ADC_BITWIDTH_12 },
    { ATOM_STR("\x6", "bit_11"), ADC_BITWIDTH_11 },
    { ATOM_STR("\x6", "bit_10"), ADC_BITWIDTH_10 },
    { ATOM_STR("\x5", "bit_9"), ADC_BITWIDTH_9 },
    SELECT_INT_DEFAULT(-1)
};

static const AtomStringIntPair attenuation_table[] = {
    { ATOM_STR("\x4", "db_0"), ADC_ATTEN_DB_0 },
    { ATOM_STR("\x6", "db_2_5"), ADC_ATTEN_DB_2_5 },
    { ATOM_STR("\x4", "db_6"), ADC_ATTEN_DB_6 },
    { ATOM_STR("\x5", "db_11"), ADC_ATTEN_DB_11 },
    { ATOM_STR("\x5", "db_12"), ADC_ATTEN_DB_12 },
    SELECT_INT_DEFAULT(-1)
};

static bool is_adc_resource(GlobalContext *global, term t)
{
    return term_is_tuple(t)
        && term_get_tuple_arity(t) == 3
        && globalcontext_is_term_equal_to_atom_string(global, term_get_tuple_element(t, 0), ATOM_STR("\x4", "$adc"))
        && term_is_resource_reference(term_get_tuple_element(t, 1))
        && term_is_reference(term_get_tuple_element(t, 2));
}

static bool to_channel_resource(term chan_resource, struct ChannelResource **rsrc_obj, Context *ctx)
{
    if (!is_adc_resource(ctx->global, chan_resource)) {
        return false;
    }
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), term_get_tuple_element(chan_resource, 1), adc_channel_resource, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct ChannelResource *) rsrc_obj_ptr;
    return true;
}

static bool to_unit_resource(term unit_resource, struct UnitResource **rsrc_obj, Context *ctx)
{
    if (!is_adc_resource(ctx->global, unit_resource)) {
        return false;
    }
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), term_get_tuple_element(unit_resource, 1), adc_unit_resource, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct UnitResource *) rsrc_obj_ptr;
    return true;
}

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static term error_return_tuple(Context *ctx, term term)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, ERROR_ATOM, term);
}

static int approximate_millivolts(int adc_reading, adc_atten_t attenuation, adc_bitwidth_t width)
{
    int digi_max = 1 << width;
    int millivolt_max = 2450;

    switch (attenuation) {
        case ADC_ATTEN_DB_0:
            millivolt_max = 950;
            break;
        case ADC_ATTEN_DB_2_5:
            millivolt_max = 1250;
            break;
        case ADC_ATTEN_DB_6:
            millivolt_max = 1750;
            break;
        case ADC_ATTEN_DB_11:
        default:
            millivolt_max = 2450;
            break;
    }

    if (digi_max <= 0) {
        digi_max = 4096;
    }

    return (adc_reading * millivolt_max) / digi_max;
}

#if defined(RTEMS_BOARD_ESP32)
static bool esp_pin_to_adc_channel(int pin, adc_unit_t *unit, adc_channel_t *channel)
{
    *unit = ADC_UNIT_1;
#if defined(RTEMS_BOARD_ESP32C3)
    if (pin < 0 || pin > 5) {
        return false;
    }
    *unit = pin == 5 ? ADC_UNIT_2 : ADC_UNIT_1;
    *channel = pin == 5 ? ADC_CHANNEL_0 : (adc_channel_t) pin;
#elif defined(RTEMS_BOARD_ESP32C5)
    if (pin < 1 || pin > 6) {
        return false;
    }
    *channel = (adc_channel_t) (pin - 1);
#elif defined(RTEMS_BOARD_ESP32C6)
    if (pin < 0 || pin > 6) {
        return false;
    }
    *channel = (adc_channel_t) pin;
#elif defined(RTEMS_BOARD_ESP32S31)
    if (pin < 42 || pin > 57) {
        return false;
    }
    *unit = pin < 50 ? ADC_UNIT_1 : ADC_UNIT_2;
    *channel = (adc_channel_t) ((pin - 42) % 8);
#endif
    return true;
}

static void esp_adc_hw_init(void)
{
#if defined(RTEMS_BOARD_ESP32C3)
    WRITE_PERI_REG(0x600C0010U, READ_PERI_REG(0x600C0010U) | (1U << 28) | (1U << 30));
    WRITE_PERI_REG(0x600C0018U, READ_PERI_REG(0x600C0018U) & ~((1U << 28) | (1U << 30)));
    WRITE_PERI_REG(ESP_ADC_BASE + 0x54U, (2U << 21) | (1U << 20) | 15U);
    WRITE_PERI_REG(0x6000E044U, READ_PERI_REG(0x6000E044U) & ~(1U << 18));
    WRITE_PERI_REG(0x6000E048U, READ_PERI_REG(0x6000E048U) | (1U << 16));
#elif defined(RTEMS_BOARD_ESP32S31)
    WRITE_PERI_REG(0x20704208U, READ_PERI_REG(0x20704208U) & ~(1U << 31));
    rtems_counter_delay_nanoseconds(1000);
    WRITE_PERI_REG(0x20704208U, READ_PERI_REG(0x20704208U) | (3U << 30));
    WRITE_PERI_REG(0x20710020U, (READ_PERI_REG(0x20710020U) | (1U << 30)) & ~(1U << 31));
    WRITE_PERI_REG(ESP_ADC_BASE + 0x3FCU, READ_PERI_REG(ESP_ADC_BASE + 0x3FCU) | (1U << 31));
    WRITE_PERI_REG(ESP_ADC_BASE + 0x78U, READ_PERI_REG(ESP_ADC_BASE + 0x78U) | (7U << 29));
#else
#if defined(RTEMS_BOARD_ESP32C5)
    const uintptr_t analog_power = 0x600B0158U;
    WRITE_PERI_REG(0x600A9C14U, READ_PERI_REG(0x600A9C14U) | (1U << 13) | (1U << 15));
#else
    const uintptr_t analog_power = 0x600B0154U;
    WRITE_PERI_REG(0x600A9814U, READ_PERI_REG(0x600A9814U) | (1U << 13) | (1U << 16));
#endif
    WRITE_PERI_REG(analog_power, READ_PERI_REG(analog_power) & ~(1U << 26));
    rtems_counter_delay_nanoseconds(1000);
    WRITE_PERI_REG(analog_power, READ_PERI_REG(analog_power) | (1U << 26) | (1U << 27));
    /* Enable both the SAR and register clocks, then use XTAL / 16. */
    WRITE_PERI_REG(ESP_ADC_CLOCK_REG, (READ_PERI_REG(ESP_ADC_CLOCK_REG) | 5U) & ~10U);
    WRITE_PERI_REG(ESP_ADC_CLOCK_REG + 4U, (1U << 22) | (15U << 12) | (1U << 6));
#if defined(RTEMS_BOARD_ESP32C5)
    WRITE_PERI_REG(0x60096148U, (READ_PERI_REG(0x60096148U) & ~(0xFFU << 8)) | (1U << 8));
#endif
#endif
#if !defined(RTEMS_BOARD_ESP32S31)
    uint32_t ctrl = READ_PERI_REG(ESP_ADC_BASE);
    ctrl = (ctrl & ~((0xFFU << 7) | (3U << 27))) | (1U << 6) | (1U << 7) | (3U << 27);
    WRITE_PERI_REG(ESP_ADC_BASE, ctrl);
    WRITE_PERI_REG(ESP_ADC_BASE + 0x0CU, 5U | (8U << 8) | (100U << 16));
#endif
}

static void esp_adc_configure_pin(int pin)
{
    WRITE_PERI_REG(ESP_ADC_GPIO_ENABLE_CLEAR + (pin >= 32 ? 12U : 0U), 1U << (pin % 32));
    uint32_t mux = READ_PERI_REG(ESP_ADC_IOMUX_REG(pin));
    WRITE_PERI_REG(ESP_ADC_IOMUX_REG(pin), mux & ~((1U << 9) | (1U << 7) | (1U << 8)));
}

static void esp_adc_restore_pin(struct ChannelResource *channel)
{
    WRITE_PERI_REG(ESP_ADC_IOMUX_REG(channel->pin), channel->saved_mux);
}

static void esp_adc_release_channel(struct ChannelResource *channel)
{
    pthread_mutex_lock(&adc_lock);
    if (channel->pin >= 0) {
        esp_adc_restore_pin(channel);
        bsp_pin_release((uint32_t) channel->pin);
        channel->pin = -1;
    }
    pthread_mutex_unlock(&adc_lock);
}

static int esp_adc_read_raw(adc_unit_t unit, adc_channel_t channel, adc_atten_t atten)
{
    uint32_t done = 1U << (unit == ADC_UNIT_1 ? 31 : 30);
#if defined(RTEMS_BOARD_ESP32S31)
    UNUSED(atten); /* S31's fixed-range ADC has no attenuation control in ESP-IDF. */
    uintptr_t ctrl_reg = ESP_ADC_BASE + (uint32_t) unit * 4U;
    uintptr_t pattern_reg = ESP_ADC_BASE + 0x1CU + (uint32_t) unit * 16U;
    uintptr_t event_reg = ESP_ADC_BASE + 0x68U;
    uintptr_t clear_reg = ESP_ADC_BASE + 0x70U;
    uintptr_t data_reg = ESP_ADC_BASE + 0x40U + (uint32_t) unit * 4U;
    uint32_t ctrl = READ_PERI_REG(ctrl_reg);
    ctrl &= ~((3U << 2) | (15U << 14) | (1U << 25));
    ctrl |= (3U << 26) | (1U << 23) | (2U << 2);
    WRITE_PERI_REG(ESP_ADC_BASE + 8U, READ_PERI_REG(ESP_ADC_BASE + 8U) & ~(1U << 24));
    WRITE_PERI_REG(pattern_reg, (uint32_t) channel << 20);
    WRITE_PERI_REG(ctrl_reg, ctrl | (1U << 22));
    WRITE_PERI_REG(ctrl_reg, ctrl);
    WRITE_PERI_REG(clear_reg, done);
    WRITE_PERI_REG(ctrl_reg, ctrl | (1U << 1));
#else
    uintptr_t event_reg = ESP_ADC_BASE + 0x44U;
    uintptr_t clear_reg = ESP_ADC_BASE + 0x4CU;
    uintptr_t data_reg = ESP_ADC_BASE + 0x2CU + (uint32_t) unit * 4U;
    uint32_t onetime = ((uint32_t) atten << 23) | (((uint32_t) unit << 3 | (uint32_t) channel) << 25) | done;
    WRITE_PERI_REG(ESP_ADC_BASE + 0x20U, onetime);
    /* C6 requires 50 us after enabling oneshot for channel selection to settle. */
    rtems_counter_delay_nanoseconds(50000);
    WRITE_PERI_REG(clear_reg, done);
    WRITE_PERI_REG(ESP_ADC_BASE + 0x20U, onetime | (1U << 29));
    rtems_counter_delay_nanoseconds(3000);
    WRITE_PERI_REG(ESP_ADC_BASE + 0x20U, onetime);
#endif
    rtems_counter_ticks start = rtems_counter_read();
    rtems_counter_ticks timeout = rtems_counter_nanoseconds_to_ticks(1000000);
    while (!(READ_PERI_REG(event_reg) & done)) {
        if (rtems_counter_difference(rtems_counter_read(), start) >= timeout) {
            WRITE_PERI_REG(clear_reg, done);
            return -1;
        }
    }
    uint32_t raw = READ_PERI_REG(data_reg);
    WRITE_PERI_REG(clear_reg, done);
#if defined(RTEMS_BOARD_ESP32S31)
    WRITE_PERI_REG(ctrl_reg, ctrl & ~(3U << 2));
    return (int) ((raw & 0x1FFFFU) >> 5); /* Preserve the API's 12-bit default. */
#else
    return (int) (raw & 0xFFFU);
#endif
}
#endif

//
// NIF Callbacks
//

static term nif_adc_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

#if !defined(RTEMS_BOARD_ESP32)
    return error_return_tuple(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "enotsup")));
#endif
    struct UnitResource *unit_rsrc = enif_alloc_resource(adc_unit_resource, sizeof(struct UnitResource));
    if (IS_NULL_PTR(unit_rsrc)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    unit_rsrc->unit_id = 0;
    unit_rsrc->initialized = true;

#if defined(RTEMS_BOARD_ESP32)
    pthread_mutex_lock(&adc_lock);
    esp_adc_hw_init();
    pthread_mutex_unlock(&adc_lock);
#endif

    size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(3) + TERM_BOXED_REFERENCE_SHORT_SIZE + TERM_BOXED_REFERENCE_RESOURCE_SIZE;
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        enif_release_resource(unit_rsrc);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term unit_obj = term_from_resource(unit_rsrc, &ctx->heap);
    enif_release_resource(unit_rsrc);

    term unit_resource = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(unit_resource, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "$adc")));
    term_put_tuple_element(unit_resource, 1, unit_obj);
    uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
    term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
    term_put_tuple_element(unit_resource, 2, ref);

    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, OK_ATOM);
    term_put_tuple_element(ret, 1, unit_resource);

    return ret;
}

static term nif_adc_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term unit_term = argv[0];
    struct UnitResource *unit_rsrc = NULL;
    if (UNLIKELY(!to_unit_resource(unit_term, &unit_rsrc, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    unit_rsrc->initialized = false;
    return OK_ATOM;
}

static term nif_adc_acquire(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term pin = argv[0];
    VALIDATE_VALUE(pin, term_is_integer);
    int pin_num = term_to_int(pin);

    term unit_term = argv[1];
    struct UnitResource *unit_rsrc = NULL;
    if (UNLIKELY(!to_unit_resource(unit_term, &unit_rsrc, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (!unit_rsrc->initialized) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term width_term = argv[2];
    VALIDATE_VALUE(width_term, term_is_atom);
    int bits = interop_atom_term_select_int(bit_width_table, width_term, ctx->global);
    if (UNLIKELY(bits < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    adc_bitwidth_t bit_width = (adc_bitwidth_t) bits;

    term atten_term = argv[3];
    VALIDATE_VALUE(atten_term, term_is_atom);
    int atten_val = interop_atom_term_select_int(attenuation_table, atten_term, ctx->global);
    if (UNLIKELY(atten_val < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    adc_atten_t atten = (adc_atten_t) atten_val;

    adc_unit_t adc_unit = ADC_UNIT_1;
    adc_channel_t adc_chan = ADC_CHANNEL_0;

#if defined(RTEMS_BOARD_ESP32)
    if (!esp_pin_to_adc_channel(pin_num, &adc_unit, &adc_chan)) {
        RAISE_ERROR(BADARG_ATOM);
    }

#endif
    struct ChannelResource *chan_rsrc = enif_alloc_resource(adc_channel_resource, sizeof(struct ChannelResource));
    if (IS_NULL_PTR(chan_rsrc)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    chan_rsrc->pin = -1;
    chan_rsrc->owner = unit_rsrc;
    enif_keep_resource(unit_rsrc);
    strcpy(chan_rsrc->pin_owner, "AtomVM ADC");
#if defined(RTEMS_BOARD_ESP32)
    if (!bsp_pin_claim((uint32_t) pin_num, chan_rsrc->pin_owner)) {
        enif_release_resource(chan_rsrc);
        return error_return_tuple(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "busy")));
    }
    chan_rsrc->saved_mux = READ_PERI_REG(ESP_ADC_IOMUX_REG(pin_num));
    esp_adc_configure_pin(pin_num);
#endif
    chan_rsrc->attenuation = atten;
    chan_rsrc->width = bit_width;
    chan_rsrc->adc_unit = adc_unit;
    chan_rsrc->channel = adc_chan;
    chan_rsrc->pin = pin_num;

    size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(3) + TERM_BOXED_REFERENCE_SHORT_SIZE + TERM_BOXED_REFERENCE_RESOURCE_SIZE;
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        enif_release_resource(chan_rsrc);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term chan_obj = term_from_resource(chan_rsrc, &ctx->heap);
    enif_release_resource(chan_rsrc);

    term chan_resource = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(chan_resource, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "$adc")));
    term_put_tuple_element(chan_resource, 1, chan_obj);
    uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
    term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
    term_put_tuple_element(chan_resource, 2, ref);

    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, OK_ATOM);
    term_put_tuple_element(ret, 1, chan_resource);

    return ret;
}

static term nif_adc_acquire_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term args[4];
    args[0] = argv[0];
    args[1] = argv[1];
    args[2] = globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "bit_max"));
    args[3] = globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "db_11"));
    return nif_adc_acquire(ctx, 4, args);
}

static term nif_adc_release_channel(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term chan_term = argv[0];
    struct ChannelResource *chan_rsrc = NULL;
    if (UNLIKELY(!to_channel_resource(chan_term, &chan_rsrc, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if defined(RTEMS_BOARD_ESP32)
    esp_adc_release_channel(chan_rsrc);
#endif

    return OK_ATOM;
}

static term nif_adc_sample(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term chan_term = argv[0];
    struct ChannelResource *chan_rsrc = NULL;
    if (UNLIKELY(!to_channel_resource(chan_term, &chan_rsrc, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term unit_term = argv[1];
    struct UnitResource *unit_rsrc = NULL;
    if (UNLIKELY(!to_unit_resource(unit_term, &unit_rsrc, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term read_options = argv[2];
    VALIDATE_VALUE(read_options, term_is_list);

    term samples = interop_kv_get_value_default(read_options, ATOM_STR("\x7", "samples"), term_from_int28(DEFAULT_SAMPLES), ctx->global);
    if (UNLIKELY(!term_is_integer(samples))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int samples_val = term_to_int32(samples);
    if (UNLIKELY(samples_val < 1 || samples_val > MAX_SAMPLES)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term raw_opt = interop_kv_get_value_default(read_options, ATOM_STR("\x3", "raw"), FALSE_ATOM, ctx->global);
    term voltage_opt = interop_kv_get_value_default(read_options, ATOM_STR("\x7", "voltage"), FALSE_ATOM, ctx->global);

    bool want_raw = (raw_opt == TRUE_ATOM);
    bool want_voltage = (voltage_opt == TRUE_ATOM);
#if defined(RTEMS_BOARD_ESP32S31)
    /* ESP-IDF provides no attenuation/calibration transfer function for S31.
     * The C3 voltage estimate would change with an attenuation the S31 ignores.
     */
    want_voltage = false;
#endif

    if (!unit_rsrc->initialized || chan_rsrc->owner != unit_rsrc) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t sum_raw = 0;
#if defined(RTEMS_BOARD_ESP32)
    pthread_mutex_lock(&adc_lock);
    if (chan_rsrc->pin < 0) {
        pthread_mutex_unlock(&adc_lock);
        RAISE_ERROR(BADARG_ATOM);
    }
    for (int i = 0; i < samples_val; ++i) {
        int raw = esp_adc_read_raw(chan_rsrc->adc_unit, chan_rsrc->channel, chan_rsrc->attenuation);
        if (raw < 0) {
            pthread_mutex_unlock(&adc_lock);
            return error_return_tuple(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "timeout")));
        }
        raw >>= ADC_BITWIDTH_12 - chan_rsrc->width;
        sum_raw += raw;
    }
    pthread_mutex_unlock(&adc_lock);
#else
    return error_return_tuple(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "enotsup")));
#endif

    int avg_raw = (int) (sum_raw / samples_val);
    term raw_term = want_raw ? term_from_int28(avg_raw) : UNDEFINED_ATOM;

    term voltage_term = UNDEFINED_ATOM;
    if (want_voltage) {
        int millivolts = approximate_millivolts(avg_raw, chan_rsrc->attenuation, chan_rsrc->width);
        voltage_term = term_from_int28(millivolts);
    }

    size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free_opt(ctx, requested_size, MEMORY_NO_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term values = create_pair(ctx, raw_term, voltage_term);
    term ret = create_pair(ctx, OK_ATOM, values);

    return ret;
}

static term nif_adc_sample_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term args[3] = { argv[0], argv[1], term_nil() };

    /* Default options: [raw, voltage, {samples, 64}] */
    size_t requested_size = TUPLE_SIZE(2) + (CONS_SIZE * 3);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 2, args, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term sample_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(sample_tuple, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "samples")));
    term_put_tuple_element(sample_tuple, 1, term_from_int28(DEFAULT_SAMPLES));

    term list = term_nil();
    list = term_list_prepend(sample_tuple, list, &ctx->heap);
    list = term_list_prepend(globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "voltage")), list, &ctx->heap);
    list = term_list_prepend(globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "raw")), list, &ctx->heap);

    args[2] = list;
    return nif_adc_sample(ctx, 3, args);
}

//
// Lifecycle and Registration
//

static void nif_adc_chan_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct ChannelResource *chan_rsrc = (struct ChannelResource *) obj;
#if defined(RTEMS_BOARD_ESP32)
    esp_adc_release_channel(chan_rsrc);
#else
    (void) chan_rsrc;
#endif
    enif_release_resource(chan_rsrc->owner);
}

static void nif_adc_unit_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct UnitResource *unit_rsrc = (struct UnitResource *) obj;
    unit_rsrc->initialized = false;
}

static const ErlNifResourceTypeInit ChannelResourceTypeInit = {
    .members = 1,
    .dtor = nif_adc_chan_resource_dtor,
};

static const ErlNifResourceTypeInit UnitResourceTypeInit = {
    .members = 1,
    .dtor = nif_adc_unit_resource_dtor,
};

static const struct Nif adc_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_init
};
static const struct Nif adc_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_deinit
};
static const struct Nif adc_acquire_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_acquire
};
static const struct Nif adc_acquire_2_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_acquire_2
};
static const struct Nif adc_release_channel_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_release_channel
};
static const struct Nif adc_sample_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_sample
};
static const struct Nif adc_sample_2_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_sample_2
};

void adc_driver_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    adc_channel_resource = enif_init_resource_type(&env, "adc_channel_resource", &ChannelResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
    adc_unit_resource = enif_init_resource_type(&env, "adc_unit_resource", &UnitResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

const struct Nif *adc_driver_get_nif(const char *nifname)
{
    if (strcmp("esp_adc:init/0", nifname) == 0 || strcmp("Elixir.Esp.ADC:init/0", nifname) == 0 || strcmp("atomvm_rtems:adc_init/0", nifname) == 0) {
        return &adc_init_nif;
    }
    if (strcmp("esp_adc:deinit/1", nifname) == 0 || strcmp("Elixir.Esp.ADC:deinit/1", nifname) == 0 || strcmp("atomvm_rtems:adc_deinit/1", nifname) == 0) {
        return &adc_deinit_nif;
    }
    if (strcmp("esp_adc:acquire/4", nifname) == 0 || strcmp("Elixir.Esp.ADC:acquire/4", nifname) == 0 || strcmp("atomvm_rtems:adc_acquire/4", nifname) == 0) {
        return &adc_acquire_nif;
    }
    if (strcmp("esp_adc:acquire/2", nifname) == 0 || strcmp("Elixir.Esp.ADC:acquire/2", nifname) == 0 || strcmp("atomvm_rtems:adc_acquire/2", nifname) == 0) {
        return &adc_acquire_2_nif;
    }
    if (strcmp("esp_adc:release_channel/1", nifname) == 0 || strcmp("Elixir.Esp.ADC:release_channel/1", nifname) == 0 || strcmp("atomvm_rtems:adc_release_channel/1", nifname) == 0) {
        return &adc_release_channel_nif;
    }
    if (strcmp("esp_adc:sample/3", nifname) == 0 || strcmp("Elixir.Esp.ADC:sample/3", nifname) == 0 || strcmp("atomvm_rtems:adc_sample/3", nifname) == 0) {
        return &adc_sample_nif;
    }
    if (strcmp("esp_adc:sample/2", nifname) == 0 || strcmp("Elixir.Esp.ADC:sample/2", nifname) == 0 || strcmp("atomvm_rtems:adc_sample/2", nifname) == 0) {
        return &adc_sample_2_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(adc, adc_driver_init, NULL, adc_driver_get_nif)
