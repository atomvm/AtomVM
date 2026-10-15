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

#ifndef ADC_DRIVER_H
#define ADC_DRIVER_H

#include <globalcontext.h>
#include <nifs.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum
{
    ADC_UNIT_1 = 0,
    ADC_UNIT_2 = 1,
} adc_unit_t;

typedef enum
{
    ADC_CHANNEL_0 = 0,
    ADC_CHANNEL_1 = 1,
    ADC_CHANNEL_2 = 2,
    ADC_CHANNEL_3 = 3,
    ADC_CHANNEL_4 = 4,
} adc_channel_t;

typedef enum
{
    ADC_ATTEN_DB_0   = 0,
    ADC_ATTEN_DB_2_5 = 1,
    ADC_ATTEN_DB_6   = 2,
    ADC_ATTEN_DB_11  = 3,
    ADC_ATTEN_DB_12  = 3,
} adc_atten_t;

typedef enum
{
    ADC_BITWIDTH_DEFAULT = 12,
    ADC_BITWIDTH_9       = 9,
    ADC_BITWIDTH_10      = 10,
    ADC_BITWIDTH_11      = 11,
    ADC_BITWIDTH_12      = 12,
} adc_bitwidth_t;

void adc_driver_init(GlobalContext *global);
const struct Nif *adc_driver_get_nif(const char *nifname);

void adc_nif(void);

#ifdef __cplusplus
}
#endif

#endif /* ADC_DRIVER_H */
