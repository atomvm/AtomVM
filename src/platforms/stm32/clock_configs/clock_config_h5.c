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

#include "stm32h5xx_hal.h"

/**
 * System Clock Configuration for STM32H5 family.
 *
 * Configures HSE -> PLL1 -> SYSCLK at 250 MHz.
 * All bus clocks (AHB, APB1, APB2, APB3) can run at 250 MHz.
 *
 * PLL1 formula: SYSCLK = (HSE / PLLM) * PLLN / PLLP
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    /* Voltage scaling: Scale 0 for maximum frequency (250 MHz) */
    if (HAL_PWREx_ControlVoltageScaling(PWR_REGULATOR_VOLTAGE_SCALE0) != HAL_OK) {
        while (1) {
        }
    }

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    rcc_osc_init.HSEState = RCC_HSE_ON;
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_HSE;
    rcc_osc_init.PLL.PLLFRACN = 0;

    /*
     * PLL1: HSE / PLLM * PLLN => VCO (192-836 MHz wide), / PLLP => SYSCLK
     */
#if AVM_CLOCK_HZ == 250000000 && (HSE_VALUE == 8000000U)
    /* Nucleo H562: 8MHz / 2 = 4 MHz * 125 = 500 MHz VCO / 2 = 250 MHz */
    rcc_osc_init.PLL.PLLM = 2;
    rcc_osc_init.PLL.PLLN = 125;
    rcc_osc_init.PLL.PLLP = 2;
    rcc_osc_init.PLL.PLLQ = 2;
    rcc_osc_init.PLL.PLLR = 2;
    rcc_osc_init.PLL.PLLRGE = RCC_PLL1_VCIRANGE_1; /* 2-4 MHz input */
    rcc_osc_init.PLL.PLLVCOSEL = RCC_PLL1_VCORANGE_WIDE;
#elif AVM_CLOCK_HZ == 250000000 && (HSE_VALUE == 25000000U)
    /* 25MHz / 5 = 5 MHz * 100 = 500 MHz VCO / 2 = 250 MHz */
    rcc_osc_init.PLL.PLLM = 5;
    rcc_osc_init.PLL.PLLN = 100;
    rcc_osc_init.PLL.PLLP = 2;
    rcc_osc_init.PLL.PLLQ = 2;
    rcc_osc_init.PLL.PLLR = 2;
    rcc_osc_init.PLL.PLLRGE = RCC_PLL1_VCIRANGE_2; /* 4-8 MHz input */
    rcc_osc_init.PLL.PLLVCOSEL = RCC_PLL1_VCORANGE_WIDE;
#elif AVM_CLOCK_HZ == 250000000 && (HSE_VALUE == 24000000U)
    /* 24MHz / 6 = 4 MHz * 125 = 500 MHz VCO / 2 = 250 MHz */
    rcc_osc_init.PLL.PLLM = 6;
    rcc_osc_init.PLL.PLLN = 125;
    rcc_osc_init.PLL.PLLP = 2;
    rcc_osc_init.PLL.PLLQ = 2;
    rcc_osc_init.PLL.PLLR = 2;
    rcc_osc_init.PLL.PLLRGE = RCC_PLL1_VCIRANGE_1; /* 2-4 MHz input */
    rcc_osc_init.PLL.PLLVCOSEL = RCC_PLL1_VCORANGE_WIDE;
#else
#error "Unsupported clock frequency / HSE combination for STM32H5"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    /*
     * Configure bus clocks: all at 250 MHz (no prescaling).
     * Flash latency: 5 wait states at 250 MHz.
     */
    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2
        | RCC_CLOCKTYPE_PCLK3;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV1;
    rcc_clk_init.APB3CLKDivider = RCC_HCLK_DIV1;

    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_5) != HAL_OK) {
        while (1) {
        }
    }
}
