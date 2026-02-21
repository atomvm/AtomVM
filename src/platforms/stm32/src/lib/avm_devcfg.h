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

#include "stm32_hal_platform.h"

#define FLASH_START_ADDRESS (0x08000000U)

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

/* USART console configuration using HAL peripheral defines. */
#if defined(CONSOLE_1)
#define AVM_CONSOLE_USART USART1
#define AVM_CONSOLE_TX_PIN GPIO_PIN_9
#define AVM_CONSOLE_TX_PORT GPIOA
#if defined(STM32G0XX)
#define AVM_CONSOLE_TX_AF GPIO_AF1_USART1
#else
#define AVM_CONSOLE_TX_AF GPIO_AF7_USART1
#endif
#elif defined(CONSOLE_2)
#define AVM_CONSOLE_USART USART2
#define AVM_CONSOLE_TX_PIN GPIO_PIN_2
#define AVM_CONSOLE_TX_PORT GPIOA
#if defined(STM32G0XX)
#define AVM_CONSOLE_TX_AF GPIO_AF1_USART2
#else
#define AVM_CONSOLE_TX_AF GPIO_AF7_USART2
#endif
#elif defined(CONSOLE_3)
#define AVM_CONSOLE_USART USART3
#define AVM_CONSOLE_TX_PIN GPIO_PIN_8
#define AVM_CONSOLE_TX_PORT GPIOD
#if defined(STM32G0XX)
#define AVM_CONSOLE_TX_AF GPIO_AF4_USART3
#else
#define AVM_CONSOLE_TX_AF GPIO_AF7_USART3
#endif
#elif defined(CONSOLE_4)
#define AVM_CONSOLE_USART UART4
#define AVM_CONSOLE_TX_PIN GPIO_PIN_10
#define AVM_CONSOLE_TX_PORT GPIOC
#define AVM_CONSOLE_TX_AF GPIO_AF8_UART4
#elif defined(CONSOLE_5)
#define AVM_CONSOLE_USART UART5
#define AVM_CONSOLE_TX_PIN GPIO_PIN_12
#define AVM_CONSOLE_TX_PORT GPIOC
#define AVM_CONSOLE_TX_AF GPIO_AF8_UART5
#elif defined(CONSOLE_6)
#define AVM_CONSOLE_USART USART6
#define AVM_CONSOLE_TX_PIN GPIO_PIN_6
#define AVM_CONSOLE_TX_PORT GPIOC
#define AVM_CONSOLE_TX_AF GPIO_AF8_USART6
#endif

#endif /* _AVM_DEVCFG_H_ */
