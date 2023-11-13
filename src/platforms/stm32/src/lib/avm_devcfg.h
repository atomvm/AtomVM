/* This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

#ifndef _AVM_DEVCFG_H_
#define _AVM_DEVCFG_H_

#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/usart.h>

#if (defined(STM32F4) || defined(STM32F7))
#define FLASH_START_ADDRESS (0x08000000U)
#endif

#if defined(ROM_512K)
#define CFG_FLASH_END (FLASH_START_ADDRESS + 0x80000U)
#define AVM_APP_ADDRESS (FLASH_START_ADDRESS + 0x60000U)
#elif defined(ROM_768K)
#define CFG_FLASH_END (FLASH_START_ADDRESS + 0xC0000U)
#elif defined(ROM_1024K)
#define CFG_FLASH_END (FLASH_START_ADDRESS + 0x100000U)
#elif defined(ROM_1536K)
#define CFG_FLASH_END (FLASH_START_ADDRESS + 0x180000U)
#elif defined(ROM_2048K)
#define CFG_FLASH_END (FLASH_START_ADDRESS + 0x200000U)
#elif defined(ROM_4096K)
#define CFG_FLASH_END (FLASH_START_ADDRESS + 0x400000U)
#endif

#ifndef AVM_APP_ADDRESS
#define AVM_APP_ADDRESS (FLASH_START_ADDRESS + 0x80000U)
#endif

#if defined(MHZ_84)
#define AVM_CLOCK_HZ (84000000U)
#define AVM_CLOCK_CONFIGURATION (&rcc_hse_25mhz_3v3[(RCC_CLOCK_3V3_84MHZ)])
#elif defined(MHZ_100)
#define AVM_CLOCK_HZ (100000000U)
#define AVM_CLOCK_CONFIGURATION (&rcc_hse_25mhz_3v3[(RCC_CLOCK_3V3_84MHZ)])
#elif defined(MHZ_168)
#define AVM_CLOCK_HZ (168000000U)
#define AVM_CLOCK_CONFIGURATION (&rcc_hse_8mhz_3v3[(RCC_CLOCK_3V3_168MHZ)])
#elif defined(MHZ_180)
#define AVM_CLOCK_HZ (180000000U)
#define AVM_CLOCK_CONFIGURATION (&rcc_hse_8mhz_3v3[(RCC_CLOCK_3V3_180MHZ)])
#elif defined(MHZ_216)
#define AVM_CLOCK_HZ (216000000U)
#define AVM_CLOCK_CONFIGURATION (&rcc_hse_8mhz_3v3[(RCC_CLOCK_3V3_216MHZ)])
#endif

#if defined(CONSOLE_1)
#define AVM_CONSOLE (USART1)
#define AVM_CONSOLE_TX (GPIO9)
#define AVM_CONSOLE_GPIO (GPIOA)
#define AVM_CONSOLE_RCC (RCC_USART1)
#elif defined(CONSOLE_2)
#define AVM_CONSOLE (USART2)
#define AVM_CONSOLE_TX (GPIO2)
#define AVM_CONSOLE_GPIO (GPIOA)
#define AVM_CONSOLE_RCC (RCC_USART2)
#elif defined(CONSOLE_3)
#define AVM_CONSOLE (USART3)
#define AVM_CONSOLE_TX (GPIO8)
#define AVM_CONSOLE_GPIO (GPIOD)
#define AVM_CONSOLE_RCC (RCC_USART3)
#elif defined(CONSOLE_4)
#define AVM_CONSOLE (UART4)
#define AVM_CONSOLE_TX (GPIO10)
#define AVM_CONSOLE_GPIO (GPIOC)
#define AVM_CONSOLE_RCC (RCC_UART4)
#elif defined(CONSOLE_5)
#define AVM_CONSOLE (UART5)
#define AVM_CONSOLE_TX (GPIO12)
#define AVM_CONSOLE_GPIO (GPIOC)
#define AVM_CONSOLE_RCC (RCC_UART5)
#ifdef LIBOPENCM3_USART_COMMON_F24_H
#elif defined(CONSOLE_6)
#define AVM_CONSOLE (USART6)
#define AVM_CONSOLE_TX (GPIO6)
#define AVM_CONSOLE_GPIO (GPIOC)
#define AVM_CONSOLE_RCC (RCC_USART6)
#elif defined(CONSOLE_7)
#define AVM_CONSOLE (UART7)
#define AVM_CONSOLE_TX (GPIO7)
#define AVM_CONSOLE_GPIO (GPIOF)
#define AVM_CONSOLE_RCC (RCC_UART7)
#elif defined(CONSOLE_8)
#define AVM_CONSOLE (UART8)
#define AVM_CONSOLE_TX (GPIO8)
#define AVM_CONSOLE_GPIO (GPIOJ)
#define AVM_CONSOLE_RCC (RCC_UART8)
#endif /* LIBOPENCM3_USART_COMMON_F24_H */
#endif

#endif /* _AVM_DEVCFG_H_ */
