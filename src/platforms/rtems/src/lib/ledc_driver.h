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

#ifndef _ATOMVM_RTEMS_LEDC_DRIVER_H_
#define _ATOMVM_RTEMS_LEDC_DRIVER_H_

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum
{
    LEDC_HIGH_SPEED_MODE = 0,
    LEDC_LOW_SPEED_MODE = 1,
    LEDC_SPEED_MODE_MAX
} ledc_mode_t;

typedef enum
{
    LEDC_TIMER_0 = 0,
    LEDC_TIMER_1,
    LEDC_TIMER_2,
    LEDC_TIMER_3,
    LEDC_TIMER_MAX
} ledc_timer_t;

typedef enum
{
    LEDC_CHANNEL_0 = 0,
    LEDC_CHANNEL_1,
    LEDC_CHANNEL_2,
    LEDC_CHANNEL_3,
    LEDC_CHANNEL_4,
    LEDC_CHANNEL_5,
    LEDC_CHANNEL_MAX
} ledc_channel_t;

#define LEDC_ERR_DUTY 0xFFFFFFFFU

typedef struct
{
    uint32_t duty_resolution;
    uint32_t freq_hz;
    ledc_mode_t speed_mode;
    ledc_timer_t timer_num;
} ledc_timer_config_t;

typedef struct
{
    int gpio_num;
    ledc_mode_t speed_mode;
    ledc_channel_t channel;
    ledc_timer_t timer_sel;
    uint32_t duty;
    int hpoint;
} ledc_channel_config_t;

int ledc_timer_config(const ledc_timer_config_t *timer_conf);
int ledc_channel_config(const ledc_channel_config_t *chan_conf);
int ledc_fade_func_install(int flags);
void ledc_fade_func_uninstall(void);
int ledc_set_fade_with_time(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, int max_fade_time_ms);
int ledc_set_fade_with_step(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, uint32_t scale, uint32_t cycle_num);
int ledc_fade_start(ledc_mode_t speed_mode, ledc_channel_t channel, int fade_mode);
int ledc_fade_stop(ledc_mode_t speed_mode, ledc_channel_t channel);
int ledc_set_fade_time_and_start(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, int max_fade_time_ms, int fade_mode);
int ledc_set_fade_step_and_start(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t target_duty, uint32_t scale, uint32_t cycle_num, int fade_mode);
uint32_t ledc_get_duty(ledc_mode_t speed_mode, ledc_channel_t channel);
int ledc_set_duty(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t duty);
int ledc_update_duty(ledc_mode_t speed_mode, ledc_channel_t channel);
int ledc_set_duty_and_update(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t duty, uint32_t hpoint);
uint32_t ledc_get_freq(ledc_mode_t speed_mode, ledc_timer_t timer_num);
int ledc_set_freq(ledc_mode_t speed_mode, ledc_timer_t timer_num, uint32_t freq_hz);
int ledc_stop(ledc_mode_t speed_mode, ledc_channel_t channel, uint32_t idle_level);

#ifdef __cplusplus
}
#endif

#endif /* _ATOMVM_RTEMS_LEDC_DRIVER_H_ */
