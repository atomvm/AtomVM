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

#include "stm32wbxx_hal.h"

/**
 * System Clock Configuration for STM32WB family.
 *
 * Configures HSE (32 MHz, required for BLE radio) -> PLL -> SYSCLK at 64 MHz.
 * The WB has two CPU cores: M4 (CPU1) and M0+ (CPU2, radio stack).
 *
 * PLL formula: SYSCLK = (HSE / PLLM) * PLLN / PLLR
 *   32 / 2 * 8 / 2 = 64 MHz
 *
 * CPU2 (M0+) runs at HCLK2 = SYSCLK / 2 = 32 MHz.
 * Shared SRAM clock (HCLK4) = SYSCLK / 1 = 64 MHz.
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    /* Voltage scaling: Range 1 for 64 MHz */
    if (HAL_PWREx_ControlVoltageScaling(PWR_REGULATOR_VOLTAGE_SCALE1) != HAL_OK) {
        while (1) {
        }
    }

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    rcc_osc_init.HSEState = RCC_HSE_ON;
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_HSE;

#if AVM_CLOCK_HZ == 64000000 && (HSE_VALUE == 32000000U)
    /* 32MHz / 2 = 16 MHz * 8 = 128 MHz VCO / 2 = 64 MHz */
    rcc_osc_init.PLL.PLLM = 2;
    rcc_osc_init.PLL.PLLN = 8;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = RCC_PLLQ_DIV2;
    rcc_osc_init.PLL.PLLR = RCC_PLLR_DIV2;
#else
#error "Unsupported clock frequency / HSE combination for STM32WB (HSE must be 32 MHz)"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    /*
     * Configure bus clocks:
     *   HCLK1 (M4 AHB)  = SYSCLK / 1 = 64 MHz
     *   HCLK2 (M0+ AHB)  = SYSCLK / 2 = 32 MHz
     *   HCLK4 (shared)   = SYSCLK / 1 = 64 MHz
     *   PCLK1 (APB1)     = HCLK / 1   = 64 MHz
     *   PCLK2 (APB2)     = HCLK / 1   = 64 MHz
     * Flash latency: 3 wait states at 64 MHz.
     */
    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2
        | RCC_CLOCKTYPE_HCLK2 | RCC_CLOCKTYPE_HCLK4;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.AHBCLK2Divider = RCC_SYSCLK_DIV2; /* CPU2 M0+ at 32 MHz */
    rcc_clk_init.AHBCLK4Divider = RCC_SYSCLK_DIV1; /* Shared SRAM at 64 MHz */

    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_3) != HAL_OK) {
        while (1) {
        }
    }
}
