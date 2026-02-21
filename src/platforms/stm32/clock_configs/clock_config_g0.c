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

#include "stm32g0xx_hal.h"

/**
 * System Clock Configuration for STM32G0 family.
 *
 * Configures HSE -> PLL -> SYSCLK at 64 MHz.
 *
 * PLL formula: SYSCLK = (HSE / PLLM) * PLLN / PLLR
 *   8 / 1 * 16 / 2 = 64 MHz
 *
 * Flash latency: 2 wait states at 64 MHz.
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    rcc_osc_init.HSEState = RCC_HSE_ON;
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_HSE;

#if AVM_CLOCK_HZ == 64000000 && (HSE_VALUE == 8000000U)
    /* 8 MHz / 1 = 8 MHz * 16 = 128 MHz VCO / 2 = 64 MHz */
    rcc_osc_init.PLL.PLLM = RCC_PLLM_DIV1;
    rcc_osc_init.PLL.PLLN = 16;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = RCC_PLLQ_DIV2;
    rcc_osc_init.PLL.PLLR = RCC_PLLR_DIV2;
#else
#error "Unsupported clock frequency / HSE combination for STM32G0"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    /*
     * Configure bus clocks:
     *   SYSCLK = 64 MHz
     *   AHB    = 64 MHz
     *   APB1   = 64 MHz
     * Flash latency: 2 wait states at 64 MHz.
     */
    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV1;

    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_2) != HAL_OK) {
        while (1) {
        }
    }
}
