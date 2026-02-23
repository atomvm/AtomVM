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

#include "stm32f7xx_hal.h"

/**
 * System Clock Configuration for STM32F7 family.
 *
 * Configures HSE -> PLL -> SYSCLK at 216 MHz.
 *
 * PLL formula: SYSCLK = (HSE / PLLM) * PLLN / PLLP
 * USB requires (HSE / PLLM) * PLLN / PLLQ = 48 MHz
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    __HAL_RCC_PWR_CLK_ENABLE();

    /* Voltage scaling: Scale 1 with over-drive for 216 MHz */
    __HAL_PWR_VOLTAGESCALING_CONFIG(PWR_REGULATOR_VOLTAGE_SCALE1);

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    rcc_osc_init.HSEState = RCC_HSE_ON;
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_HSE;

#if AVM_CLOCK_HZ == 216000000 && (HSE_VALUE == 8000000U)
    /* Nucleo F746/F767: 8MHz / 8 = 1 MHz * 432 = 432 MHz VCO / 2 = 216 MHz
     * USB: 432/9 = 48 MHz */
    rcc_osc_init.PLL.PLLM = 8;
    rcc_osc_init.PLL.PLLN = 432;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = 9;
#elif AVM_CLOCK_HZ == 216000000 && (HSE_VALUE == 25000000U)
    /* 25MHz / 25 = 1 MHz * 432 = 432 MHz VCO / 2 = 216 MHz
     * USB: 432/9 = 48 MHz */
    rcc_osc_init.PLL.PLLM = 25;
    rcc_osc_init.PLL.PLLN = 432;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = 9;
#else
#error "Unsupported clock frequency / HSE combination for STM32F7"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    if (HAL_PWREx_EnableOverDrive() != HAL_OK) {
        while (1) {
        }
    }

    /* Select PLL as system clock source and configure HCLK, PCLK1, PCLK2
     *   SYSCLK = 216 MHz
     *   AHB    = 216 MHz (HPRE = /1)
     *   APB1   = 54 MHz  (PPRE1 = /4, max 54 MHz)
     *   APB2   = 108 MHz (PPRE2 = /2, max 108 MHz)
     */
    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV4;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV2;

    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_7) != HAL_OK) {
        while (1) {
        }
    }
}
