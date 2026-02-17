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

#include "stm32g4xx_hal.h"

/**
 * System Clock Configuration for STM32G4 family.
 *
 * Configures HSE -> PLL -> SYSCLK at 170 MHz.
 *
 * PLL formula: SYSCLK = (HSE / PLLM) * PLLN / PLLR
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    /* Voltage scaling: Range 1 boost mode for 170 MHz */
    if (HAL_PWREx_ControlVoltageScaling(PWR_REGULATOR_VOLTAGE_SCALE1_BOOST) != HAL_OK) {
        while (1) {
        }
    }

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    rcc_osc_init.HSEState = RCC_HSE_ON;
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_HSE;

#if AVM_CLOCK_HZ == 170000000 && (HSE_VALUE == 8000000U)
    /* 8MHz / 2 = 4 MHz * 85 = 340 MHz VCO / 2 = 170 MHz */
    rcc_osc_init.PLL.PLLM = RCC_PLLM_DIV2;
    rcc_osc_init.PLL.PLLN = 85;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = RCC_PLLQ_DIV2;
    rcc_osc_init.PLL.PLLR = RCC_PLLR_DIV2;
#elif AVM_CLOCK_HZ == 170000000 && (HSE_VALUE == 24000000U)
    /* 24MHz / 6 = 4 MHz * 85 = 340 MHz VCO / 2 = 170 MHz */
    rcc_osc_init.PLL.PLLM = RCC_PLLM_DIV6;
    rcc_osc_init.PLL.PLLN = 85;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = RCC_PLLQ_DIV2;
    rcc_osc_init.PLL.PLLR = RCC_PLLR_DIV2;
#else
#error "Unsupported clock frequency / HSE combination for STM32G4"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    /*
     * Configure bus clocks:
     *   SYSCLK = 170 MHz
     *   AHB    = 170 MHz
     *   APB1   = 170 MHz (G4 APBs can run at full speed)
     *   APB2   = 170 MHz
     * Flash latency: 4 wait states at 170 MHz (boost mode).
     */
    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV1;

    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_4) != HAL_OK) {
        while (1) {
        }
    }
}
