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

#include "stm32h7xx_hal.h"

/**
 * System Clock Configuration for STM32H7 family.
 *
 * Configures HSE -> PLL1 -> SYSCLK at the target frequency.
 * The H7 has multiple clock domains (D1/D2/D3) and requires
 * power supply and voltage scaling configuration.
 *
 * Target: 480 MHz SYSCLK, 240 MHz AHB, 120 MHz APBx.
 *
 * PLL1 formula: SYSCLK = (HSE / PLLM) * PLLN / PLLP
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    /* Configure power supply: LDO mode */
    if (HAL_PWREx_ConfigSupply(PWR_LDO_SUPPLY) != HAL_OK) {
        while (1) {
        }
    }

    /* Voltage scaling: Scale 0 for maximum frequency (480 MHz) */
    __HAL_PWR_VOLTAGESCALING_CONFIG(PWR_REGULATOR_VOLTAGE_SCALE0);
    while (!__HAL_PWR_GET_FLAG(PWR_FLAG_VOSRDY)) {
    }

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    rcc_osc_init.HSEState = RCC_HSE_ON;
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_HSE;

    /*
     * PLL1: HSE / PLLM * PLLN => VCO, then / PLLP => SYSCLK
     * VCO must be 192-960 MHz (wide range)
     * VCO input (HSE/PLLM) must be 1-16 MHz
     */
#if AVM_CLOCK_HZ == 480000000 && (HSE_VALUE == 8000000U)
    /* Nucleo H743: 8MHz / 4 = 2 MHz * 480 = 960 MHz VCO / 2 = 480 MHz */
    rcc_osc_init.PLL.PLLM = 4;
    rcc_osc_init.PLL.PLLN = 480;
    rcc_osc_init.PLL.PLLP = 2;
    rcc_osc_init.PLL.PLLQ = 20; /* 960/20 = 48 MHz for USB */
    rcc_osc_init.PLL.PLLR = 2;
    rcc_osc_init.PLL.PLLRGE = RCC_PLL1VCIRANGE_1; /* 2-4 MHz input */
    rcc_osc_init.PLL.PLLVCOSEL = RCC_PLL1VCOWIDE;
    rcc_osc_init.PLL.PLLFRACN = 0;
#elif AVM_CLOCK_HZ == 480000000 && (HSE_VALUE == 25000000U)
    /* 25MHz / 5 = 5 MHz * 192 = 960 MHz VCO / 2 = 480 MHz */
    rcc_osc_init.PLL.PLLM = 5;
    rcc_osc_init.PLL.PLLN = 192;
    rcc_osc_init.PLL.PLLP = 2;
    rcc_osc_init.PLL.PLLQ = 20; /* 960/20 = 48 MHz for USB */
    rcc_osc_init.PLL.PLLR = 2;
    rcc_osc_init.PLL.PLLRGE = RCC_PLL1VCIRANGE_2; /* 4-8 MHz input */
    rcc_osc_init.PLL.PLLVCOSEL = RCC_PLL1VCOWIDE;
    rcc_osc_init.PLL.PLLFRACN = 0;
#else
#error "Unsupported clock frequency / HSE combination for STM32H7"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

    /*
     * Configure bus clocks:
     *   SYSCLK = 480 MHz (D1CPRE = /1)
     *   AHB    = 240 MHz (HPRE = /2)
     *   APB3   = 120 MHz (D1PPRE = /2)
     *   APB1   = 120 MHz (D2PPRE1 = /2)
     *   APB2   = 120 MHz (D2PPRE2 = /2)
     *   APB4   = 120 MHz (D3PPRE = /2)
     */
    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2
        | RCC_CLOCKTYPE_D1PCLK1 | RCC_CLOCKTYPE_D3PCLK1;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.SYSCLKDivider = RCC_SYSCLK_DIV1;
    rcc_clk_init.AHBCLKDivider = RCC_HCLK_DIV2;
    rcc_clk_init.APB3CLKDivider = RCC_APB3_DIV2;
    rcc_clk_init.APB1CLKDivider = RCC_APB1_DIV2;
    rcc_clk_init.APB2CLKDivider = RCC_APB2_DIV2;
    rcc_clk_init.APB4CLKDivider = RCC_APB4_DIV2;

    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_4) != HAL_OK) {
        while (1) {
        }
    }
}
