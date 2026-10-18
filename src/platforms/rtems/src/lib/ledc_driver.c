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

#if defined(RTEMS_BOARD_ESP32C3)
#include <bsp/gpio.h>
#include <bsp/pin.h>

#define WRITE_PERI_REG(addr, val) (*((volatile uint32_t *) (addr)) = (uint32_t) (val))
#define READ_PERI_REG(addr) (*((volatile uint32_t *) (addr)))

#define ESP32C3_LEDC_BASE 0x60019000U
#define ESP32C3_LEDC_CH_CONF0_REG(n) (ESP32C3_LEDC_BASE + (n) * 0x14U + 0x00U)
#define ESP32C3_LEDC_CH_HPOINT_REG(n) (ESP32C3_LEDC_BASE + (n) * 0x14U + 0x04U)
#define ESP32C3_LEDC_CH_DUTY_REG(n) (ESP32C3_LEDC_BASE + (n) * 0x14U + 0x08U)
#define ESP32C3_LEDC_CH_CONF1_REG(n) (ESP32C3_LEDC_BASE + (n) * 0x14U + 0x0CU)
#define ESP32C3_LEDC_CH_DUTY_R_REG(n) (ESP32C3_LEDC_BASE + (n) * 0x14U + 0x10U)

#define ESP32C3_LEDC_TIMER_CONF_REG(m) (ESP32C3_LEDC_BASE + 0xA0U + (m) * 0x08U)
#define ESP32C3_LEDC_TIMER_VAL_REG(m) (ESP32C3_LEDC_BASE + 0xA4U + (m) * 0x08U)
#define ESP32C3_LEDC_CONF_REG (ESP32C3_LEDC_BASE + 0xC0U)

#define ESP32C3_SYSREG_BASE 0x600C0000U
#define ESP32C3_SYSREG_CLK_EN0_REG (ESP32C3_SYSREG_BASE + 0x0010U)
#define ESP32C3_SYSREG_RST_EN0_REG (ESP32C3_SYSREG_BASE + 0x0018U)

#define ESP32C3_GPIO_OUT_SEL_REG(pin) (0x60004554U + (pin) * 4U)
#define ESP32C3_LEDC_SIG_OUT0_IDX 45U
#endif

#define LEDC_TIMERS_COUNT 4
#define LEDC_CHANNELS_COUNT 6

struct ledc_timer_info
{
    bool configured;
    uint32_t duty_resolution;
    uint32_t freq_hz;
};

struct ledc_channel_info
{
    bool configured;
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

int ledc_timer_config(const ledc_timer_config_t *timer_conf)
{
    if (timer_conf == NULL) {
        return -1;
    }
    if (timer_conf->timer_num >= LEDC_TIMERS_COUNT) {
        return -1;
    }
    if (timer_conf->duty_resolution < 1 || timer_conf->duty_resolution > 14) {
        return -1;
    }
    if (timer_conf->freq_hz == 0) {
        return -1;
    }

    uint32_t timer_num = timer_conf->timer_num;
    s_ledc.timers[timer_num].configured = true;
    s_ledc.timers[timer_num].duty_resolution = timer_conf->duty_resolution;
    s_ledc.timers[timer_num].freq_hz = timer_conf->freq_hz;

#if defined(RTEMS_BOARD_ESP32C3)
    // Enable LEDC peripheral clock in SYSREG
    WRITE_PERI_REG(ESP32C3_SYSREG_CLK_EN0_REG, READ_PERI_REG(ESP32C3_SYSREG_CLK_EN0_REG) | (1U << 11));
    WRITE_PERI_REG(ESP32C3_SYSREG_RST_EN0_REG, READ_PERI_REG(ESP32C3_SYSREG_RST_EN0_REG) & ~(1U << 11));

    // Select APB clock (80 MHz)
    WRITE_PERI_REG(ESP32C3_LEDC_CONF_REG, 1U);

    // Calculate clock divider: 80 MHz APB clock, fixed-point with 8 fractional bits
    uint64_t precision_ticks = 1ULL << timer_conf->duty_resolution;
    uint64_t div_scaled = (80000000ULL * 256ULL) / ((uint64_t) timer_conf->freq_hz * precision_ticks);
    if (div_scaled == 0) {
        div_scaled = 256;
    } else if (div_scaled > 0x3FFFF) {
        div_scaled = 0x3FFFF;
    }

    uint32_t conf = (timer_conf->duty_resolution & 0x0FU) | (((uint32_t) div_scaled & 0x3FFFFU) << 4) | (1U << 24); // tick_sel = 1 (APB)
    WRITE_PERI_REG(ESP32C3_LEDC_TIMER_CONF_REG(timer_num), conf | (1U << 23)); // Reset timer
    WRITE_PERI_REG(ESP32C3_LEDC_TIMER_CONF_REG(timer_num), conf); // Run timer
#endif

    return 0;
}

int ledc_channel_config(const ledc_channel_config_t *chan_conf)
{
    if (chan_conf == NULL) {
        return -1;
    }
    if (chan_conf->channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    if (chan_conf->timer_sel >= LEDC_TIMERS_COUNT) {
        return -1;
    }

    uint32_t ch = chan_conf->channel;
    s_ledc.channels[ch].configured = true;
    s_ledc.channels[ch].gpio_num = chan_conf->gpio_num;
    s_ledc.channels[ch].timer_sel = chan_conf->timer_sel;
    s_ledc.channels[ch].duty = chan_conf->duty;
    s_ledc.channels[ch].target_duty = chan_conf->duty;
    s_ledc.channels[ch].hpoint = chan_conf->hpoint;

#if defined(RTEMS_BOARD_ESP32C3)
    WRITE_PERI_REG(ESP32C3_LEDC_CH_HPOINT_REG(ch), (uint32_t) chan_conf->hpoint & 0x3FFFU);
    WRITE_PERI_REG(ESP32C3_LEDC_CH_DUTY_REG(ch), ((uint32_t) chan_conf->duty & 0x3FFFU) << 4);
    // timer_sel, sig_out_en, para_up
    uint32_t conf0 = ((uint32_t) chan_conf->timer_sel & 0x03U) | (1U << 2) | (1U << 4);
    WRITE_PERI_REG(ESP32C3_LEDC_CH_CONF0_REG(ch), conf0);

    // Route pin through GPIO matrix if valid
    if (chan_conf->gpio_num >= 0 && chan_conf->gpio_num < (int) BSP_PIN_COUNT) {
        bsp_pin_claim((uint32_t) chan_conf->gpio_num, "AtomVM LEDC");
        rtems_gpio_bsp_select_output(0, (uint32_t) chan_conf->gpio_num, NULL);
        WRITE_PERI_REG(ESP32C3_GPIO_OUT_SEL_REG((uint32_t) chan_conf->gpio_num), ESP32C3_LEDC_SIG_OUT0_IDX + ch);
    }
#endif

    return 0;
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
    if (channel >= LEDC_CHANNELS_COUNT) {
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
    if (channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    s_ledc.channels[channel].target_duty = target_duty;
    return 0;
}

int ledc_fade_start(ledc_mode_t speed_mode, ledc_channel_t channel, int fade_mode)
{
    UNUSED(speed_mode);
    UNUSED(fade_mode);
    if (channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    return ledc_set_duty_and_update(speed_mode, channel, s_ledc.channels[channel].target_duty, s_ledc.channels[channel].hpoint);
}

int ledc_fade_stop(ledc_mode_t speed_mode, ledc_channel_t channel)
{
    UNUSED(speed_mode);
    if (channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
#if defined(RTEMS_BOARD_ESP32C3)
    WRITE_PERI_REG(ESP32C3_LEDC_CH_CONF1_REG(channel), 0);
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
    if (channel >= LEDC_CHANNELS_COUNT) {
        return LEDC_ERR_DUTY;
    }
#if defined(RTEMS_BOARD_ESP32C3)
    uint32_t val = READ_PERI_REG(ESP32C3_LEDC_CH_DUTY_R_REG(channel));
    if (val != 0) {
        return val >> 4;
    }
#endif
    return s_ledc.channels[channel].duty;
}

int ledc_set_duty(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t duty)
{
    UNUSED(speed_mode);
    if (channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    s_ledc.channels[channel].duty = duty;
#if defined(RTEMS_BOARD_ESP32C3)
    WRITE_PERI_REG(ESP32C3_LEDC_CH_DUTY_REG(channel), (duty & 0x3FFFU) << 4);
#endif
    return 0;
}

int ledc_update_duty(ledc_mode_t speed_mode, ledc_channel_t channel)
{
    UNUSED(speed_mode);
    if (channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
#if defined(RTEMS_BOARD_ESP32C3)
    uint32_t conf0 = READ_PERI_REG(ESP32C3_LEDC_CH_CONF0_REG(channel));
    WRITE_PERI_REG(ESP32C3_LEDC_CH_CONF0_REG(channel), conf0 | (1U << 4)); // PARA_UP
    WRITE_PERI_REG(ESP32C3_LEDC_CH_CONF1_REG(channel), 1U << 31); // DUTY_START
#endif
    return 0;
}

int ledc_set_duty_and_update(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t duty, uint32_t hpoint)
{
    if (channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
    s_ledc.channels[channel].hpoint = (int) hpoint;
#if defined(RTEMS_BOARD_ESP32C3)
    WRITE_PERI_REG(ESP32C3_LEDC_CH_HPOINT_REG(channel), hpoint & 0x3FFFU);
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
    if (timer_num >= LEDC_TIMERS_COUNT) {
        return 0;
    }
    return s_ledc.timers[timer_num].freq_hz;
}

int ledc_set_freq(ledc_mode_t speed_mode, ledc_timer_t timer_num, uint32_t freq_hz)
{
    if (timer_num >= LEDC_TIMERS_COUNT || freq_hz == 0) {
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
    if (channel >= LEDC_CHANNELS_COUNT) {
        return -1;
    }
#if defined(RTEMS_BOARD_ESP32C3)
    uint32_t conf0 = READ_PERI_REG(ESP32C3_LEDC_CH_CONF0_REG(channel));
    conf0 &= ~(1U << 2); // Disable SIG_OUT_EN
    if (idle_level) {
        conf0 |= (1U << 3); // IDLE_LV = 1
    } else {
        conf0 &= ~(1U << 3); // IDLE_LV = 0
    }
    conf0 |= (1U << 4); // PARA_UP
    WRITE_PERI_REG(ESP32C3_LEDC_CH_CONF0_REG(channel), conf0);
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
