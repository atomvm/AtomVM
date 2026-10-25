/*
 * This file is part of AtomVM.
 *
 * Copyright 2020-2023 Fred Dushin <fred@dushin.net>
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

#include "ledc_driver.h"

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

#if defined(RTEMS_BOARD_ESP32)
#include <bsp/gpio.h>
#include <bsp/pin.h>

#define WRITE_PERI_REG(addr, val) (*((volatile uint32_t *) (uintptr_t) (addr)) = (uint32_t) (val))
#define READ_PERI_REG(addr) (*((volatile uint32_t *) (uintptr_t) (addr)))

/* Register layout follows ESP-IDF ledc_reg.h and hal/ledc_ll.h for each chip. */
#if defined(RTEMS_BOARD_ESP32C3)
#define ESP_LEDC_BASE 0x60019000U
#define ESP_LEDC_GPIO_OUT_SEL 0x60004554U
#define ESP_LEDC_SIGNAL0 45U
#define ESP_LEDC_IOMUX0 0x60009004U
#define ESP_LEDC_GPIO_ENABLE_SET 0x60004024U
#define ESP_LEDC_MAX_BITS 14U
#define ESP_LEDC_RES_BITS 4U
#define ESP_LEDC_CHANNELS 6
#elif defined(RTEMS_BOARD_ESP32C5)
#define ESP_LEDC_BASE 0x60007000U
#define ESP_LEDC_GPIO_OUT_SEL 0x60091AD4U
#define ESP_LEDC_SIGNAL0 0U
#define ESP_LEDC_IOMUX0 0x60090000U
#define ESP_LEDC_GPIO_ENABLE_SET 0x60091038U
#define ESP_LEDC_CLOCK_REG 0x60096048U
#define ESP_LEDC_CHANNELS 6
#elif defined(RTEMS_BOARD_ESP32C6)
#define ESP_LEDC_BASE 0x60007000U
#define ESP_LEDC_GPIO_OUT_SEL 0x60091554U
#define ESP_LEDC_SIGNAL0 0U
#define ESP_LEDC_IOMUX0 0x60090004U
#define ESP_LEDC_GPIO_ENABLE_SET 0x60091024U
#define ESP_LEDC_CLOCK_REG 0x60096034U
#define ESP_LEDC_CHANNELS 6
#elif defined(RTEMS_BOARD_ESP32P4)
#define ESP_LEDC_BASE 0x500D3000U
#define ESP_LEDC_GPIO_OUT_SEL 0x500E0558U
#define ESP_LEDC_SIGNAL0 126U
#define ESP_LEDC_IOMUX0 0x500E1004U
#define ESP_LEDC_GPIO_ENABLE_SET 0x500E0024U
#define ESP_LEDC_CHANNELS 8
#elif defined(RTEMS_BOARD_ESP32S31)
/* Expose LEDC0; LEDC1 is a separate controller. */
#define ESP_LEDC_BASE 0x20392000U
#define ESP_LEDC_GPIO_OUT_SEL 0x20583AF4U
#define ESP_LEDC_SIGNAL0 126U
#define ESP_LEDC_IOMUX0 0x20582000U
#define ESP_LEDC_GPIO_ENABLE_SET 0x20583038U
#define ESP_LEDC_CLOCK_REG 0x20587148U
#define ESP_LEDC_CHANNELS 8
#endif

#ifndef ESP_LEDC_MAX_BITS
#define ESP_LEDC_MAX_BITS 20U
#define ESP_LEDC_RES_BITS 5U
#endif
#define ESP_LEDC_CH_REG(ch, offset) (ESP_LEDC_BASE + (ch) * 0x14U + (offset))
#define ESP_LEDC_CH_CONF0_REG(ch) ESP_LEDC_CH_REG(ch, 0U)
#define ESP_LEDC_CH_HPOINT_REG(ch) ESP_LEDC_CH_REG(ch, 4U)
#define ESP_LEDC_CH_DUTY_REG(ch) ESP_LEDC_CH_REG(ch, 8U)
#define ESP_LEDC_CH_CONF1_REG(ch) ESP_LEDC_CH_REG(ch, 12U)
#define ESP_LEDC_CH_DUTY_R_REG(ch) ESP_LEDC_CH_REG(ch, 16U)
#define ESP_LEDC_TIMER_REG(timer) (ESP_LEDC_BASE + 0xA0U + (timer) * 8U)
#define ESP_LEDC_HPOINT_MASK ((1U << ESP_LEDC_MAX_BITS) - 1)
#define ESP_LEDC_DUTY_MASK ((1U << (ESP_LEDC_MAX_BITS + 1)) - 1)
static const char ledc_pin_owner[8][16] = {
    "AtomVM LEDC 0", "AtomVM LEDC 1", "AtomVM LEDC 2", "AtomVM LEDC 3",
    "AtomVM LEDC 4", "AtomVM LEDC 5", "AtomVM LEDC 6", "AtomVM LEDC 7"
};

static void esp_ledc_enable_clock(void)
{
#if defined(RTEMS_BOARD_ESP32C3)
    WRITE_PERI_REG(0x600C0010U, READ_PERI_REG(0x600C0010U) | (1U << 11));
    WRITE_PERI_REG(0x600C0018U, READ_PERI_REG(0x600C0018U) & ~(1U << 11));
    /* XTAL is stable across APB frequency changes. */
    WRITE_PERI_REG(ESP_LEDC_BASE + 0xD0U, (READ_PERI_REG(ESP_LEDC_BASE + 0xD0U) & ~3U) | 3U | (1U << 31));
#elif defined(RTEMS_BOARD_ESP32P4)
    WRITE_PERI_REG(0x500E6020U, READ_PERI_REG(0x500E6020U) | 1U);
    WRITE_PERI_REG(0x500E609CU, READ_PERI_REG(0x500E609CU) | (1U << 2));
    WRITE_PERI_REG(0x500E60C4U, READ_PERI_REG(0x500E60C4U) & ~(1U << 29));
#elif defined(RTEMS_BOARD_ESP32S31)
    uint32_t clock = READ_PERI_REG(ESP_LEDC_CLOCK_REG);
    clock = (clock & ~((3U << 3) | (1U << 1))) | (1U << 0) | (1U << 5);
    WRITE_PERI_REG(ESP_LEDC_CLOCK_REG, clock);
#else
    WRITE_PERI_REG(ESP_LEDC_CLOCK_REG, (READ_PERI_REG(ESP_LEDC_CLOCK_REG) | 1U) & ~2U);
    uint32_t clock = READ_PERI_REG(ESP_LEDC_CLOCK_REG + 4U);
    WRITE_PERI_REG(ESP_LEDC_CLOCK_REG + 4U, (clock & ~(3U << 20)) | (1U << 22));
#endif
}
#else
#define ESP_LEDC_MAX_BITS 14U
#define ESP_LEDC_CHANNELS 6
#endif

#define LEDC_TIMERS_COUNT 4
#define LEDC_CHANNELS_COUNT ESP_LEDC_CHANNELS

struct ledc_timer_info
{
    bool configured;
    uint32_t duty_resolution;
    uint32_t freq_hz;
};

struct ledc_channel_info
{
    bool configured;
    bool claimed;
    int gpio_num;
    ledc_timer_t timer_sel;
    uint32_t duty;
    uint32_t target_duty;
    int hpoint;
};

static struct
{
    struct ledc_timer_info timers[LEDC_TIMERS_COUNT];
    struct ledc_channel_info channels[LEDC_CHANNELS_COUNT];
    bool fade_installed;
} s_ledc;

static const char *const ledc_duty_resolution = "\xF"
                                                "duty_resolution";
static const char *const ledc_freq_hz = "\x7"
                                        "freq_hz";
static const char *const ledc_speed_mode = "\xA"
                                           "speed_mode";
static const char *const ledc_timer_num = "\x9"
                                          "timer_num";
static const char *const ledc_channel = "\x7"
                                        "channel";
static const char *const ledc_duty = "\x4"
                                     "duty";
static const char *const ledc_gpio_num = "\x8"
                                         "gpio_num";
static const char *const ledc_hpoint = "\x6"
                                       "hpoint";
static const char *const ledc_timer_sel = "\x9"
                                          "timer_sel";

static uint32_t ledc_source_clock_hz(void)
{
#if defined(RTEMS_BOARD_ESP32C5)
    /* PCR_CLK_XTAL_FREQ follows the C5's 40/48 MHz crystal selection. */
    uint32_t mhz = (READ_PERI_REG(0x60096110U) >> 24) & 0x7FU;
    return mhz == 48 ? 48000000U : 40000000U;
#else
    return 40000000U;
#endif
}

int ledc_timer_config(const ledc_timer_config_t *timer_conf)
{
    if (timer_conf == NULL) {
        return -1;
    }
    if ((unsigned) timer_conf->timer_num >= LEDC_TIMERS_COUNT || timer_conf->speed_mode != LEDC_LOW_SPEED_MODE) {
        return -1;
    }
    if (timer_conf->duty_resolution < 1 || timer_conf->duty_resolution > ESP_LEDC_MAX_BITS) {
        return -1;
    }
    if (timer_conf->freq_hz == 0) {
        return -1;
    }

    uint64_t ticks = 1ULL << timer_conf->duty_resolution;
    uint64_t source_clock = ledc_source_clock_hz();
    uint64_t divider = (source_clock * 256ULL) / ((uint64_t) timer_conf->freq_hz * ticks);
    if (divider < 256 || divider > 0x3FFFF) {
        return -1;
    }
    uint32_t timer_num = timer_conf->timer_num;
#if defined(RTEMS_BOARD_ESP32)
    esp_ledc_enable_clock();
    uint32_t conf = timer_conf->duty_resolution | ((uint32_t) divider << ESP_LEDC_RES_BITS);
#if defined(RTEMS_BOARD_ESP32C3)
    conf |= 1U << 24; /* Use the global low-speed clock. */
#endif
    uint32_t reset = 1U << (ESP_LEDC_RES_BITS + 19);
    uint32_t update = 1U << (ESP_LEDC_RES_BITS + 21);
    WRITE_PERI_REG(ESP_LEDC_TIMER_REG(timer_num), conf | reset | update);
    WRITE_PERI_REG(ESP_LEDC_TIMER_REG(timer_num), conf | update);
#endif
    s_ledc.timers[timer_num].configured = true;
    s_ledc.timers[timer_num].duty_resolution = timer_conf->duty_resolution;
    s_ledc.timers[timer_num].freq_hz = (uint32_t) ((source_clock * 256ULL) / (divider * ticks));

    return 0;
}

int ledc_channel_config(const ledc_channel_config_t *chan_conf)
{
    if (chan_conf == NULL) {
        return -1;
    }
    if ((unsigned) chan_conf->channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    if ((unsigned) chan_conf->timer_sel >= LEDC_TIMERS_COUNT) {
        return -1;
    }
    if (chan_conf->speed_mode != LEDC_LOW_SPEED_MODE || !s_ledc.timers[chan_conf->timer_sel].configured
        || chan_conf->duty > (1U << s_ledc.timers[chan_conf->timer_sel].duty_resolution) || chan_conf->hpoint < 0) {
        return -1;
    }
#if defined(RTEMS_BOARD_ESP32)
#if defined(RTEMS_BOARD_ESP32S31)
    if (chan_conf->gpio_num == 29 || chan_conf->gpio_num == 41) {
        return -1;
    }
#endif
    if ((unsigned) chan_conf->gpio_num >= BSP_PIN_COUNT || (uint32_t) chan_conf->hpoint > ESP_LEDC_HPOINT_MASK
        || !bsp_pin_claim((uint32_t) chan_conf->gpio_num, ledc_pin_owner[chan_conf->channel])) {
        return -1;
    }
#endif

    uint32_t ch = chan_conf->channel;
#if defined(RTEMS_BOARD_ESP32)
    if (s_ledc.channels[ch].claimed && s_ledc.channels[ch].gpio_num != chan_conf->gpio_num) {
        uint32_t old_pin = (uint32_t) s_ledc.channels[ch].gpio_num;
        WRITE_PERI_REG(ESP_LEDC_GPIO_ENABLE_SET + 4U + (old_pin >= 32 ? 12U : 0U), 1U << (old_pin % 32));
        bsp_pin_release(old_pin);
    }
#endif
    s_ledc.channels[ch].configured = true;
    s_ledc.channels[ch].claimed = true;
    s_ledc.channels[ch].gpio_num = chan_conf->gpio_num;
    s_ledc.channels[ch].timer_sel = chan_conf->timer_sel;
    s_ledc.channels[ch].duty = chan_conf->duty;
    s_ledc.channels[ch].target_duty = chan_conf->duty;
    s_ledc.channels[ch].hpoint = chan_conf->hpoint;

#if defined(RTEMS_BOARD_ESP32)
    WRITE_PERI_REG(ESP_LEDC_CH_HPOINT_REG(ch), (uint32_t) chan_conf->hpoint & ESP_LEDC_HPOINT_MASK);
    WRITE_PERI_REG(ESP_LEDC_CH_DUTY_REG(ch), ((uint32_t) chan_conf->duty & ESP_LEDC_DUTY_MASK) << 4);
    // timer_sel, sig_out_en, para_up
    uint32_t conf0 = ((uint32_t) chan_conf->timer_sel & 0x03U) | (1U << 2) | (1U << 4);
    WRITE_PERI_REG(ESP_LEDC_CH_CONF0_REG(ch), conf0);

#endif

    return ledc_update_duty(chan_conf->speed_mode, chan_conf->channel);
}

int ledc_fade_func_install(int flags)
{
    UNUSED(flags);
    s_ledc.fade_installed = true;
    return 0;
}

void ledc_fade_func_uninstall(void)
{
    s_ledc.fade_installed = false;
}

int ledc_set_fade_with_time(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, int max_fade_time_ms)
{
    UNUSED(speed_mode);
    UNUSED(max_fade_time_ms);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    s_ledc.channels[channel].target_duty = target_duty;
    return 0;
}

int ledc_set_fade_with_step(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, uint32_t scale, uint32_t cycle_num)
{
    UNUSED(speed_mode);
    UNUSED(scale);
    UNUSED(cycle_num);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    s_ledc.channels[channel].target_duty = target_duty;
    return 0;
}

int ledc_fade_start(ledc_mode_t speed_mode, ledc_channel_t channel, int fade_mode)
{
    UNUSED(speed_mode);
    UNUSED(fade_mode);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    return ledc_set_duty_and_update(speed_mode, channel, s_ledc.channels[channel].target_duty, s_ledc.channels[channel].hpoint);
}

int ledc_fade_stop(ledc_mode_t speed_mode, ledc_channel_t channel)
{
    UNUSED(speed_mode);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
#if defined(RTEMS_BOARD_ESP32)
    WRITE_PERI_REG(ESP_LEDC_CH_CONF1_REG(channel), 0);
#endif
    return 0;
}

int ledc_set_fade_time_and_start(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, int max_fade_time_ms, int fade_mode)
{
    int res = ledc_set_fade_with_time(speed_mode, channel, target_duty, max_fade_time_ms);
    if (res != 0) {
        return res;
    }
    return ledc_fade_start(speed_mode, channel, fade_mode);
}

int ledc_set_fade_step_and_start(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, uint32_t scale, uint32_t cycle_num, int fade_mode)
{
    int res = ledc_set_fade_with_step(speed_mode, channel, target_duty, scale, cycle_num);
    if (res != 0) {
        return res;
    }
    return ledc_fade_start(speed_mode, channel, fade_mode);
}

uint32_t ledc_get_duty(ledc_mode_t speed_mode, ledc_channel_t channel)
{
    UNUSED(speed_mode);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return LEDC_ERR_DUTY;
    }
#if defined(RTEMS_BOARD_ESP32)
    return READ_PERI_REG(ESP_LEDC_CH_DUTY_R_REG(channel)) >> 4;
#else
    return s_ledc.channels[channel].duty;
#endif
}

int ledc_set_duty(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t duty)
{
    UNUSED(speed_mode);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT || speed_mode != LEDC_LOW_SPEED_MODE
        || !s_ledc.channels[channel].configured
        || duty > (1U << s_ledc.timers[s_ledc.channels[channel].timer_sel].duty_resolution)) {
        return -1;
    }
    s_ledc.channels[channel].duty = duty;
#if defined(RTEMS_BOARD_ESP32)
    WRITE_PERI_REG(ESP_LEDC_CH_DUTY_REG(channel), (duty & ESP_LEDC_DUTY_MASK) << 4);
#endif
    return 0;
}

int ledc_update_duty(ledc_mode_t speed_mode, ledc_channel_t channel)
{
    UNUSED(speed_mode);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
#if defined(RTEMS_BOARD_ESP32)
    if (!s_ledc.channels[channel].configured
        || !bsp_pin_claim((uint32_t) s_ledc.channels[channel].gpio_num, ledc_pin_owner[channel])) {
        return -1;
    }
    s_ledc.channels[channel].claimed = true;
    uint32_t pin = (uint32_t) s_ledc.channels[channel].gpio_num;
    uintptr_t mux = ESP_LEDC_IOMUX0 + pin * 4U;
    WRITE_PERI_REG(mux, (READ_PERI_REG(mux) & ~(7U << 12)) | (1U << 12));
    WRITE_PERI_REG(ESP_LEDC_GPIO_OUT_SEL + pin * 4U, ESP_LEDC_SIGNAL0 + channel);
    WRITE_PERI_REG(ESP_LEDC_GPIO_ENABLE_SET + (pin >= 32 ? 12U : 0U), 1U << (pin % 32));
    uint32_t conf0 = READ_PERI_REG(ESP_LEDC_CH_CONF0_REG(channel));
    WRITE_PERI_REG(ESP_LEDC_CH_CONF0_REG(channel), conf0 | (1U << 4) | (1U << 2)); // PARA_UP, SIG_OUT_EN
    WRITE_PERI_REG(ESP_LEDC_CH_CONF1_REG(channel), 1U << 31); // DUTY_START
#endif
    return 0;
}

int ledc_set_duty_and_update(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t duty, uint32_t hpoint)
{
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    s_ledc.channels[channel].hpoint = (int) hpoint;
#if defined(RTEMS_BOARD_ESP32)
    WRITE_PERI_REG(ESP_LEDC_CH_HPOINT_REG(channel), hpoint & ESP_LEDC_HPOINT_MASK);
#endif
    int res = ledc_set_duty(speed_mode, channel, duty);
    if (res != 0) {
        return res;
    }
    return ledc_update_duty(speed_mode, channel);
}

uint32_t ledc_get_freq(ledc_mode_t speed_mode, ledc_timer_t timer_num)
{
    UNUSED(speed_mode);
    if ((unsigned) timer_num >= LEDC_TIMERS_COUNT) {
        return 0;
    }
#if defined(RTEMS_BOARD_ESP32)
    uint32_t conf = READ_PERI_REG(ESP_LEDC_TIMER_REG(timer_num));
    uint32_t resolution = conf & ((1U << ESP_LEDC_RES_BITS) - 1);
    uint32_t divider = (conf >> ESP_LEDC_RES_BITS) & 0x3FFFFU;
    return divider && resolution && resolution <= ESP_LEDC_MAX_BITS ? (uint32_t) (((uint64_t) ledc_source_clock_hz() * 256ULL) / ((uint64_t) divider << resolution)) : 0;
#else
    return s_ledc.timers[timer_num].freq_hz;
#endif
}

int ledc_set_freq(ledc_mode_t speed_mode, ledc_timer_t timer_num, uint32_t freq_hz)
{
    if ((unsigned) timer_num >= LEDC_TIMERS_COUNT || freq_hz == 0) {
        return -1;
    }
    ledc_timer_config_t conf = {
        .duty_resolution = s_ledc.timers[timer_num].duty_resolution,
        .freq_hz = freq_hz,
        .speed_mode = speed_mode,
        .timer_num = timer_num
    };
    return ledc_timer_config(&conf);
}

int ledc_stop(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t idle_level)
{
    UNUSED(speed_mode);
    if ((unsigned) channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
#if defined(RTEMS_BOARD_ESP32)
    uint32_t conf0 = READ_PERI_REG(ESP_LEDC_CH_CONF0_REG(channel));
    conf0 &= ~(1U << 2); // Disable SIG_OUT_EN
    if (idle_level) {
        conf0 |= (1U << 3); // IDLE_LV = 1
    } else {
        conf0 &= ~(1U << 3); // IDLE_LV = 0
    }
    conf0 |= (1U << 4); // PARA_UP
    WRITE_PERI_REG(ESP_LEDC_CH_CONF0_REG(channel), conf0);
    if (s_ledc.channels[channel].claimed) {
        bsp_pin_release((uint32_t) s_ledc.channels[channel].gpio_num);
        s_ledc.channels[channel].claimed = false;
    }
#else
    UNUSED(idle_level);
#endif
    return 0;
}

/*
 * AtomVM NIF implementations
 */

static term nif_ledc_timer_config(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term config = argv[0];
    VALIDATE_VALUE(config, term_is_list);
    GlobalContext *glb = ctx->global;

    term speed_mode = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_speed_mode));
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term duty_resolution = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_duty_resolution));
    VALIDATE_VALUE(duty_resolution, term_is_integer);
    term freq_hz = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_freq_hz));
    VALIDATE_VALUE(freq_hz, term_is_integer);
    term timer_num = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_timer_num));
    VALIDATE_VALUE(timer_num, term_is_integer);

    ledc_timer_config_t ledc_timer = {
        .duty_resolution = (uint32_t) term_to_int(duty_resolution),
        .freq_hz = (uint32_t) term_to_int(freq_hz),
        .speed_mode = (ledc_mode_t) term_to_int(speed_mode),
        .timer_num = (ledc_timer_t) term_to_int(timer_num)
    };
    int err = ledc_timer_config(&ledc_timer);
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_channel_config(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term config = argv[0];
    VALIDATE_VALUE(config, term_is_list);
    GlobalContext *glb = ctx->global;

    term gpio_num = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_gpio_num));
    VALIDATE_VALUE(gpio_num, term_is_integer);
    term speed_mode = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_speed_mode));
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_channel));
    VALIDATE_VALUE(channel, term_is_integer);
    term timer_sel = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_timer_sel));
    VALIDATE_VALUE(timer_sel, term_is_integer);
    term duty = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_duty));
    VALIDATE_VALUE(duty, term_is_integer);
    term hpoint = interop_proplist_get_value(config, globalcontext_make_atom(glb, ledc_hpoint));
    VALIDATE_VALUE(hpoint, term_is_integer);

    ledc_channel_config_t ledc_channel = {
        .gpio_num = term_to_int(gpio_num),
        .speed_mode = (ledc_mode_t) term_to_int(speed_mode),
        .channel = (ledc_channel_t) term_to_int(channel),
        .timer_sel = (ledc_timer_t) term_to_int(timer_sel),
        .duty = (uint32_t) term_to_int(duty),
        .hpoint = term_to_int(hpoint)
    };
    int err = ledc_channel_config(&ledc_channel);
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_fade_func_install(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term flags = argv[0];
    VALIDATE_VALUE(flags, term_is_integer);

    int err = ledc_fade_func_install(term_to_int(flags));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_fade_func_uninstall(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    ledc_fade_func_uninstall();
    return OK_ATOM;
}

static term nif_ledc_set_fade_with_time(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);
    term fade_time = argv[3];
    VALIDATE_VALUE(fade_time, term_is_integer);

    int err = ledc_set_fade_with_time(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        (uint32_t) term_to_int(duty),
        term_to_int(fade_time));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_set_fade_with_step(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);
    term scale = argv[3];
    VALIDATE_VALUE(scale, term_is_integer);
    term cycle_num = argv[4];
    VALIDATE_VALUE(cycle_num, term_is_integer);

    int err = ledc_set_fade_with_step(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        (uint32_t) term_to_int(duty),
        (uint32_t) term_to_int(scale),
        (uint32_t) term_to_int(cycle_num));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_fade_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term fade_no_wait = argv[2];
    VALIDATE_VALUE(fade_no_wait, term_is_integer);

    int err = ledc_fade_start(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        term_to_int(fade_no_wait));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_fade_stop(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);

    int err = ledc_fade_stop(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_set_fade_time_and_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);
    term fade_time = argv[3];
    VALIDATE_VALUE(fade_time, term_is_integer);
    term fade_mode = argv[4];
    VALIDATE_VALUE(fade_mode, term_is_integer);

    int err = ledc_set_fade_time_and_start(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        (uint32_t) term_to_int(duty),
        term_to_int(fade_time),
        term_to_int(fade_mode));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_set_fade_step_and_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);
    term scale = argv[3];
    VALIDATE_VALUE(scale, term_is_integer);
    term cycle_num = argv[4];
    VALIDATE_VALUE(cycle_num, term_is_integer);
    term fade_mode = argv[5];
    VALIDATE_VALUE(fade_mode, term_is_integer);

    int err = ledc_set_fade_step_and_start(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        (uint32_t) term_to_int(duty),
        (uint32_t) term_to_int(scale),
        (uint32_t) term_to_int(cycle_num),
        term_to_int(fade_mode));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_get_duty(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);

    uint32_t duty = ledc_get_duty(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel));
    if (duty == LEDC_ERR_DUTY) {
        RAISE_ERROR(term_from_int(LEDC_ERR_DUTY));
    }
    return term_from_int(duty);
}

static term nif_ledc_set_duty(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);

    int err = ledc_set_duty(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        (uint32_t) term_to_int(duty));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_update_duty(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);

    int err = ledc_update_duty(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_set_duty_and_update(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);
    term hpoint = argv[3];
    VALIDATE_VALUE(hpoint, term_is_integer);

    int err = ledc_set_duty_and_update(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        (uint32_t) term_to_int(duty),
        (uint32_t) term_to_int(hpoint));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_get_freq(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term timer_num = argv[1];
    VALIDATE_VALUE(timer_num, term_is_integer);

    uint32_t freq_hz = ledc_get_freq(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_timer_t) term_to_int(timer_num));
    if (freq_hz == 0) {
        RAISE_ERROR(term_from_int(0));
    }
    return term_from_int(freq_hz);
}

static term nif_ledc_set_freq(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term timer_num = argv[1];
    VALIDATE_VALUE(timer_num, term_is_integer);
    term freq_hz = argv[2];
    VALIDATE_VALUE(freq_hz, term_is_integer);

    int err = ledc_set_freq(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_timer_t) term_to_int(timer_num),
        (uint32_t) term_to_int(freq_hz));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static term nif_ledc_stop(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term idle_level = argv[2];
    VALIDATE_VALUE(idle_level, term_is_integer);

    int err = ledc_stop(
        (ledc_mode_t) term_to_int(speed_mode),
        (ledc_channel_t) term_to_int(channel),
        (uint32_t) term_to_int(idle_level));
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(term_from_int(err));
    }
    return OK_ATOM;
}

static const struct Nif ledc_timer_config_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_timer_config
};

static const struct Nif ledc_channel_config_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_channel_config
};

static const struct Nif ledc_fade_func_install_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_fade_func_install
};

static const struct Nif ledc_fade_func_uninstall_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_fade_func_uninstall
};

static const struct Nif ledc_set_fade_with_time_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_fade_with_time
};

static const struct Nif ledc_set_fade_with_step_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_fade_with_step
};

static const struct Nif ledc_fade_start_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_fade_start
};

static const struct Nif ledc_fade_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_fade_stop
};

static const struct Nif ledc_set_fade_time_and_start_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_fade_time_and_start
};

static const struct Nif ledc_set_fade_step_and_start_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_fade_step_and_start
};

static const struct Nif ledc_get_duty_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_get_duty
};

static const struct Nif ledc_set_duty_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_duty
};

static const struct Nif ledc_update_duty_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_update_duty
};

static const struct Nif ledc_set_duty_and_update_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_duty_and_update
};

static const struct Nif ledc_get_freq_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_get_freq
};

static const struct Nif ledc_set_freq_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_freq
};

static const struct Nif ledc_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_stop
};

static const struct Nif *ledc_nif_get_nif(const char *nifname)
{
    if (strcmp("ledc:timer_config/1", nifname) == 0 || strcmp("Elixir.LEDC:timer_config/1", nifname) == 0 || strcmp("atomvm_rtems:ledc_timer_config/1", nifname) == 0) {
        return &ledc_timer_config_nif;
    }
    if (strcmp("ledc:channel_config/1", nifname) == 0 || strcmp("Elixir.LEDC:channel_config/1", nifname) == 0 || strcmp("atomvm_rtems:ledc_channel_config/1", nifname) == 0) {
        return &ledc_channel_config_nif;
    }
    if (strcmp("ledc:fade_func_install/1", nifname) == 0 || strcmp("Elixir.LEDC:fade_func_install/1", nifname) == 0 || strcmp("atomvm_rtems:ledc_fade_func_install/1", nifname) == 0) {
        return &ledc_fade_func_install_nif;
    }
    if (strcmp("ledc:fade_func_uninstall/0", nifname) == 0 || strcmp("Elixir.LEDC:fade_func_uninstall/0", nifname) == 0 || strcmp("atomvm_rtems:ledc_fade_func_uninstall/0", nifname) == 0) {
        return &ledc_fade_func_uninstall_nif;
    }
    if (strcmp("ledc:set_fade_with_time/4", nifname) == 0 || strcmp("Elixir.LEDC:set_fade_with_time/4", nifname) == 0 || strcmp("atomvm_rtems:ledc_set_fade_with_time/4", nifname) == 0) {
        return &ledc_set_fade_with_time_nif;
    }
    if (strcmp("ledc:set_fade_with_step/5", nifname) == 0 || strcmp("Elixir.LEDC:set_fade_with_step/5", nifname) == 0 || strcmp("atomvm_rtems:ledc_set_fade_with_step/5", nifname) == 0) {
        return &ledc_set_fade_with_step_nif;
    }
    if (strcmp("ledc:fade_start/3", nifname) == 0 || strcmp("Elixir.LEDC:fade_start/3", nifname) == 0 || strcmp("atomvm_rtems:ledc_fade_start/3", nifname) == 0) {
        return &ledc_fade_start_nif;
    }
    if (strcmp("ledc:fade_stop/2", nifname) == 0 || strcmp("Elixir.LEDC:fade_stop/2", nifname) == 0 || strcmp("atomvm_rtems:ledc_fade_stop/2", nifname) == 0) {
        return &ledc_fade_stop_nif;
    }
    if (strcmp("ledc:set_fade_time_and_start/5", nifname) == 0 || strcmp("Elixir.LEDC:set_fade_time_and_start/5", nifname) == 0 || strcmp("atomvm_rtems:ledc_set_fade_time_and_start/5", nifname) == 0) {
        return &ledc_set_fade_time_and_start_nif;
    }
    if (strcmp("ledc:set_fade_step_and_start/6", nifname) == 0 || strcmp("Elixir.LEDC:set_fade_step_and_start/6", nifname) == 0 || strcmp("atomvm_rtems:ledc_set_fade_step_and_start/6", nifname) == 0) {
        return &ledc_set_fade_step_and_start_nif;
    }
    if (strcmp("ledc:get_duty/2", nifname) == 0 || strcmp("Elixir.LEDC:get_duty/2", nifname) == 0 || strcmp("atomvm_rtems:ledc_get_duty/2", nifname) == 0) {
        return &ledc_get_duty_nif;
    }
    if (strcmp("ledc:set_duty/3", nifname) == 0 || strcmp("Elixir.LEDC:set_duty/3", nifname) == 0 || strcmp("atomvm_rtems:ledc_set_duty/3", nifname) == 0) {
        return &ledc_set_duty_nif;
    }
    if (strcmp("ledc:update_duty/2", nifname) == 0 || strcmp("Elixir.LEDC:update_duty/2", nifname) == 0 || strcmp("atomvm_rtems:ledc_update_duty/2", nifname) == 0) {
        return &ledc_update_duty_nif;
    }
    if (strcmp("ledc:set_duty_and_update/4", nifname) == 0 || strcmp("Elixir.LEDC:set_duty_and_update/4", nifname) == 0 || strcmp("atomvm_rtems:ledc_set_duty_and_update/4", nifname) == 0) {
        return &ledc_set_duty_and_update_nif;
    }
    if (strcmp("ledc:get_freq/2", nifname) == 0 || strcmp("Elixir.LEDC:get_freq/2", nifname) == 0 || strcmp("atomvm_rtems:ledc_get_freq/2", nifname) == 0) {
        return &ledc_get_freq_nif;
    }
    if (strcmp("ledc:set_freq/3", nifname) == 0 || strcmp("Elixir.LEDC:set_freq/3", nifname) == 0 || strcmp("atomvm_rtems:ledc_set_freq/3", nifname) == 0) {
        return &ledc_set_freq_nif;
    }
    if (strcmp("ledc:stop/3", nifname) == 0 || strcmp("Elixir.LEDC:stop/3", nifname) == 0 || strcmp("atomvm_rtems:ledc_stop/3", nifname) == 0) {
        return &ledc_stop_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(ledc, NULL, NULL, ledc_nif_get_nif)
