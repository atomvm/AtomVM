/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

#include "stm32l4xx_hal.h"

/**
 * System Clock Configuration for STM32L4 family.
 *
 * Configures MSI -> PLL -> SYSCLK at 80 MHz (standard L4)
 * or 120 MHz (L4R/L4S L4+ devices).
 *
 * PLL formula: SYSCLK = (source / PLLM) * PLLN / PLLR
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

#if AVM_CLOCK_HZ == 120000000
    /* L4+ Range 1 Boost for 120 MHz */
    if (HAL_PWREx_ControlVoltageScaling(PWR_REGULATOR_VOLTAGE_SCALE1_BOOST) != HAL_OK) {
        while (1) {
        }
    }
#else
    /* Range 1 for up to 80 MHz */
    if (HAL_PWREx_ControlVoltageScaling(PWR_REGULATOR_VOLTAGE_SCALE1) != HAL_OK) {
        while (1) {
        }
    }
#endif

#if AVM_CLOCK_HZ == 80000000
    /* Configure MSI at 4 MHz -> PLL -> 80 MHz
     * MSI 4 MHz / 1 * 40 = 160 MHz VCO / 2 = 80 MHz */
    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_MSI;
    rcc_osc_init.MSIState = RCC_MSI_ON;
    rcc_osc_init.MSICalibrationValue = RCC_MSICALIBRATION_DEFAULT;
    rcc_osc_init.MSIClockRange = RCC_MSIRANGE_6; /* 4 MHz */
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_MSI;
    rcc_osc_init.PLL.PLLM = 1;
    rcc_osc_init.PLL.PLLN = 40;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV7;
    rcc_osc_init.PLL.PLLQ = RCC_PLLQ_DIV2;
    rcc_osc_init.PLL.PLLR = RCC_PLLR_DIV2;
#elif AVM_CLOCK_HZ == 120000000
    /* L4+ (L4R/L4S): MSI 4 MHz / 1 * 60 = 240 MHz VCO / 2 = 120 MHz */
    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_MSI;
    rcc_osc_init.MSIState = RCC_MSI_ON;
    rcc_osc_init.MSICalibrationValue = RCC_MSICALIBRATION_DEFAULT;
    rcc_osc_init.MSIClockRange = RCC_MSIRANGE_6; /* 4 MHz */
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_MSI;
    rcc_osc_init.PLL.PLLM = 1;
    rcc_osc_init.PLL.PLLN = 60;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV7;
    rcc_osc_init.PLL.PLLQ = RCC_PLLQ_DIV2;
    rcc_osc_init.PLL.PLLR = RCC_PLLR_DIV2;
#else
#error "Unsupported clock frequency for STM32L4"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV1;

#if AVM_CLOCK_HZ == 80000000
    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_4) != HAL_OK) {
        while (1) {
        }
    }
#elif AVM_CLOCK_HZ == 120000000
    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_5) != HAL_OK) {
        while (1) {
        }
    }
#endif
}
