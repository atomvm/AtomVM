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

#include "stm32f4xx_hal.h"
#include <utils.h>

/**
 * System Clock Configuration for STM32F4 family.
 *
 * Configures HSE -> PLL -> SYSCLK at the target frequency.
 * HSE_VALUE is defined in stm32f4xx_hal_conf.h (default 8MHz for
 * Discovery/Nucleo boards, 25MHz for Blackpill boards).
 *
 * The PLL configuration depends on the target clock frequency:
 * - 84 MHz:  for STM32F401 (Blackpill with 25MHz HSE)
 * - 100 MHz: for STM32F411 (Blackpill with 25MHz HSE)
 * - 168 MHz: for STM32F407 (Discovery with 8MHz HSE)
 * - 180 MHz: for STM32F4x9 (with 8MHz HSE)
 */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef rcc_osc_init = { 0 };
    RCC_ClkInitTypeDef rcc_clk_init = { 0 };

    __HAL_RCC_PWR_CLK_ENABLE();

    /* The voltage scaling allows optimizing the power consumption when the
     * device is clocked below the maximum system frequency. */
    __HAL_PWR_VOLTAGESCALING_CONFIG(PWR_REGULATOR_VOLTAGE_SCALE1);

    rcc_osc_init.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    rcc_osc_init.HSEState = RCC_HSE_ON;
    rcc_osc_init.PLL.PLLState = RCC_PLL_ON;
    rcc_osc_init.PLL.PLLSource = RCC_PLLSOURCE_HSE;

    /*
     * PLL formula: SYSCLK = (HSE / PLLM) * PLLN / PLLP
     * USB requires (HSE / PLLM) * PLLN / PLLQ = 48 MHz
     */
#if AVM_CLOCK_HZ == 84000000 && HSE_VALUE == 25000000U
    /* Blackpill F401: 25MHz / 25 * 336 / 4 = 84 MHz, USB: 336/7 = 48 MHz */
    rcc_osc_init.PLL.PLLM = 25;
    rcc_osc_init.PLL.PLLN = 336;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV4;
    rcc_osc_init.PLL.PLLQ = 7;
#elif AVM_CLOCK_HZ == 100000000 && HSE_VALUE == 25000000U
    /* Blackpill F411: 25MHz / 25 * 400 / 4 = 100 MHz
     * Note: USB clock = 400/9 = 44.4 MHz (not exactly 48 MHz).
     * Exact 48 MHz is impossible at 100 MHz SYSCLK with integer dividers. */
    rcc_osc_init.PLL.PLLM = 25;
    rcc_osc_init.PLL.PLLN = 400;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV4;
    rcc_osc_init.PLL.PLLQ = 9;
#elif AVM_CLOCK_HZ == 168000000 && HSE_VALUE == 8000000U
    /* Discovery F407: 8MHz / 8 * 336 / 2 = 168 MHz, USB: 336/7 = 48 MHz */
    rcc_osc_init.PLL.PLLM = 8;
    rcc_osc_init.PLL.PLLN = 336;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = 7;
#elif AVM_CLOCK_HZ == 180000000 && HSE_VALUE == 8000000U
    /* F4x9: 8MHz / 8 * 360 / 2 = 180 MHz
     * Note: USB clock = 360/8 = 45 MHz (not 48 MHz). USB may not work reliably. */
    rcc_osc_init.PLL.PLLM = 8;
    rcc_osc_init.PLL.PLLN = 360;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = 8;
#elif AVM_CLOCK_HZ == 84000000 && HSE_VALUE == 8000000U
    /* F401 with 8MHz HSE: 8MHz / 8 * 336 / 4 = 84 MHz */
    rcc_osc_init.PLL.PLLM = 8;
    rcc_osc_init.PLL.PLLN = 336;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV4;
    rcc_osc_init.PLL.PLLQ = 7;
#elif AVM_CLOCK_HZ == 100000000 && HSE_VALUE == 8000000U
    /* F411 with 8MHz HSE: 8MHz / 8 * 400 / 4 = 100 MHz
     * Note: USB clock = 400/9 = 44.4 MHz (not exactly 48 MHz).
     * Exact 48 MHz is impossible at 100 MHz SYSCLK with integer dividers. */
    rcc_osc_init.PLL.PLLM = 8;
    rcc_osc_init.PLL.PLLN = 400;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV4;
    rcc_osc_init.PLL.PLLQ = 9;
#elif AVM_CLOCK_HZ == 168000000 && HSE_VALUE == 16000000U
    /* F407 with 16MHz HSE: 16MHz / 8 * 168 / 2 = 168 MHz, USB: 336/7 = 48 MHz */
    rcc_osc_init.PLL.PLLM = 8;
    rcc_osc_init.PLL.PLLN = 168;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = 7;
#elif AVM_CLOCK_HZ == 180000000 && HSE_VALUE == 16000000U
    /* F4x9 with 16MHz HSE: 16MHz / 8 * 180 / 2 = 180 MHz
     * Note: USB clock = 360/8 = 45 MHz (not 48 MHz). USB may not work reliably. */
    rcc_osc_init.PLL.PLLM = 8;
    rcc_osc_init.PLL.PLLN = 180;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = 8;
#elif AVM_CLOCK_HZ == 168000000 && HSE_VALUE == 25000000U
    /* F407 with 25MHz HSE: 25MHz / 25 * 336 / 2 = 168 MHz */
    rcc_osc_init.PLL.PLLM = 25;
    rcc_osc_init.PLL.PLLN = 336;
    rcc_osc_init.PLL.PLLP = RCC_PLLP_DIV2;
    rcc_osc_init.PLL.PLLQ = 7;
#else
#error "Unsupported clock frequency / HSE combination for STM32F4"
#endif

    if (HAL_RCC_OscConfig(&rcc_osc_init) != HAL_OK) {
        while (1) {
        }
    }

#if AVM_CLOCK_HZ == 180000000
    if (HAL_PWREx_EnableOverDrive() != HAL_OK) {
        while (1) {
        }
    }
#endif

    rcc_clk_init.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK
        | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2;
    rcc_clk_init.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    rcc_clk_init.AHBCLKDivider = RCC_SYSCLK_DIV1;

#if AVM_CLOCK_HZ == 84000000 || AVM_CLOCK_HZ == 100000000
    /* APB1 max 50 MHz, APB2 max 100 MHz for F401/F411 */
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV2;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV1;
    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_3) != HAL_OK) {
        while (1) {
        }
    }
#elif AVM_CLOCK_HZ == 168000000
    /* APB1 max 42 MHz, APB2 max 84 MHz */
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV4;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV2;
    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_5) != HAL_OK) {
        while (1) {
        }
    }
#elif AVM_CLOCK_HZ == 180000000
    /* APB1 max 45 MHz, APB2 max 90 MHz */
    rcc_clk_init.APB1CLKDivider = RCC_HCLK_DIV4;
    rcc_clk_init.APB2CLKDivider = RCC_HCLK_DIV2;
    if (HAL_RCC_ClockConfig(&rcc_clk_init, FLASH_LATENCY_5) != HAL_OK) {
        while (1) {
        }
    }
#endif
}
