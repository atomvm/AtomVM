/* This file is part of AtomVM.
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

#ifndef _STM32_HAL_PLATFORM_H_
#define _STM32_HAL_PLATFORM_H_

/*
 * Single include point for STM32 HAL headers.
 * The correct family-specific hal.h is selected via the family define
 * (e.g. STM32F4XX) which is set by cmake/stm32_device.cmake.
 */

#if defined(STM32F4XX)
#include "stm32f4xx_hal.h"
#include "stm32f4xx_ll_exti.h"
#include "stm32f4xx_ll_gpio.h"

#elif defined(STM32H7XX)
#include "stm32h7xx_hal.h"
#include "stm32h7xx_ll_exti.h"
#include "stm32h7xx_ll_gpio.h"

#elif defined(STM32U5XX)
#include "stm32u5xx_hal.h"
#include "stm32u5xx_ll_exti.h"
#include "stm32u5xx_ll_gpio.h"

#elif defined(STM32WBXX)
#include "stm32wbxx_hal.h"
#include "stm32wbxx_ll_exti.h"
#include "stm32wbxx_ll_gpio.h"

#elif defined(STM32H5XX)
#include "stm32h5xx_hal.h"
#include "stm32h5xx_ll_exti.h"
#include "stm32h5xx_ll_gpio.h"

#elif defined(STM32F7XX)
#include "stm32f7xx_hal.h"
#include "stm32f7xx_ll_exti.h"
#include "stm32f7xx_ll_gpio.h"

#elif defined(STM32G0XX)
#include "stm32g0xx_hal.h"
#include "stm32g0xx_ll_exti.h"
#include "stm32g0xx_ll_gpio.h"

#elif defined(STM32G4XX)
#include "stm32g4xx_hal.h"
#include "stm32g4xx_ll_exti.h"
#include "stm32g4xx_ll_gpio.h"

#elif defined(STM32L4XX)
#include "stm32l4xx_hal.h"
#include "stm32l4xx_ll_exti.h"
#include "stm32l4xx_ll_gpio.h"

#elif defined(STM32L5XX)
#include "stm32l5xx_hal.h"
#include "stm32l5xx_ll_exti.h"
#include "stm32l5xx_ll_gpio.h"

#elif defined(STM32F2XX)
#include "stm32f2xx_hal.h"
#include "stm32f2xx_ll_exti.h"
#include "stm32f2xx_ll_gpio.h"

#elif defined(STM32U3XX)
#include "stm32u3xx_hal.h"
#include "stm32u3xx_ll_exti.h"
#include "stm32u3xx_ll_gpio.h"

#else
#error "Unsupported STM32 family. Define the appropriate STM32 family macro (e.g. STM32F4XX)."
#endif

#endif /* _STM32_HAL_PLATFORM_H_ */
