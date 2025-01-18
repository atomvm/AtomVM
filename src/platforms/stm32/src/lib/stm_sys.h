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

#ifndef _STM_SYS_H_
#define _STM_SYS_H_

#include <interop.h>
#include <portnifloader.h>
#include <sys.h>

#define STM32_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "stm32"))

/*  Only on ARMv7EM and above
 *  TODO: These definitions are back-ported from libopencm3 `master`, if they ever release a new version this section should
 *  be removed, along with the included headers and replaced with only the following inclusion:
 *  `#include <libopencm3/cm3/scb.h>`
 */
#if defined(__ARM_ARCH_7EM__)

#include <libopencm3/cm3/common.h>
#include <libopencm3/cm3/memorymap.h>
#include <libopencm3/stm32/rcc.h>

/** ICIALLU: I-cache invalidate all to Point of Unification */
#define SCB_ICIALLU MMIO32(SCB_BASE + 0x250)
/** BPIALL: Branch predictor invalidate all */
#define SCB_BPIALL MMIO32(SCB_BASE + 0x278)

#endif /* __ARM_ARCH_7EM__ */

/* Define macros for data and instruction barriers for sys_init_icache()
 * See: ARM V7-M Architecture Reference Manual :: https://static.docs.arm.com/ddi0403/eb/DDI0403E_B_armv7m_arm.pdf */
// Ensure write is visible
#define __dsb asm __volatile__("dsb" :: \
                                   : "memory");
// Synchronize fetched instruction stream
#define __isb asm __volatile__("isb" :: \
                                   : "memory");

#ifdef LIBOPENCM3_GPIO_COMMON_F24_H
#define GPIO_CLOCK_LIST                                                                                                         \
    {                                                                                                                           \
        RCC_GPIOA, RCC_GPIOB, RCC_GPIOC, RCC_GPIOD, RCC_GPIOE, RCC_GPIOF, RCC_GPIOG, RCC_GPIOH, RCC_GPIOI, RCC_GPIOJ, RCC_GPIOK \
    }
#else
#define GPIO_CLOCK_LIST                                                                        \
    {                                                                                          \
        RCC_GPIOA, RCC_GPIOB, RCC_GPIOC, RCC_GPIOD, RCC_GPIOE, RCC_GPIOF, RCC_GPIOG, RCC_GPIOH \
    }
#endif

struct LockedPin
{
    struct ListHead locked_pins_list_head;
    uint32_t gpio_bank;
    uint16_t pin_num;
};

struct STM32PlatformData
{
    struct ListHead locked_pins;
};

void sys_init_icache(void);
void sys_enable_flash_cache(void);
void *_sbrk_r(struct _reent *, ptrdiff_t);
// This function may be defined to relocate the heap.
void local_heap_setup(uint8_t **start, uint8_t **end);
void sys_enable_core_periph_clocks();
bool sys_lock_pin(GlobalContext *glb, uint32_t gpio_bank, uint16_t pin_num);

#endif /* _STM_SYS_H_ */
