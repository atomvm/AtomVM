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

#include "stm32u3xx_hal.h"

/**
 * System Clock Configuration for STM32U3 family.
 *
 * Configures MSIS (MSIRC0) at 96 MHz as SYSCLK directly.
 * STM32U3 has no PLL. MSIRC0 provides 96 MHz with DIV1.
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    __HAL_RCC_PWR_CLK_ENABLE();

    /* Voltage scaling: Range 1 for up to 96 MHz */
    if (HAL_PWREx_ControlVoltageScaling(PWR_REGULATOR_VOLTAGE_SCALE1) != HAL_OK) {
        while (1) {
        }
    }

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_MSIS;
    rcc_osc_init.MSISState = RCC_MSI_ON;
    rcc_osc_init.MSISSource = RCC_MSI_RC0; /* MSIRC0: 96 MHz base */
    rcc_osc_init.MSISDiv = RCC_MSI_DIV1; /* 96 / 1 = 96 MHz */

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    /*
     * Configure bus clocks:
     *   SYSCLK = MSIS = 96 MHz
     *   AHB    = 96 MHz
     *   APB1   = 96 MHz
     *   APB2   = 96 MHz
     *   APB3   = 96 MHz
     * Flash latency: 2 wait states at 96 MHz.
     */
    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2
        | RCC_CLOCKTYPE_PCLK3;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_MSIS;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.APB3CLKDivider = RCC_HCLK_DIV1;

    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_2) != HAL_OK) {
        while (1) {
        }
    }
}
