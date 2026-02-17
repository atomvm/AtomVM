/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Riccardo Binetti <rbino@gmx.com>
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
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include <avmpack.h>
#include <defaultatoms.h>
#include <scheduler.h>
#include <sys.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "stm32_hal_platform.h"

#include "avm_log.h"
#include "stm_sys.h"

#define TAG "sys"
#define RESERVE_STACK_SIZE 4096U

/* These functions are needed to fix a hard fault when using malloc in devices with sram spanning
 * multiple chips
 */
#pragma weak local_heap_setup = __local_ram

/* these are defined by the linker script */
extern uint8_t _ebss, _stack;

static uint8_t *_cur_brk = NULL;
static uint8_t *_heap_end = NULL;

/*
 * If not overridden, this puts the heap into the left
 * over ram between the BSS section and the stack while
 * preserving RESERVE_STACK_SIZE bytes for the stack itself.
 * This may be overridden by defining the function
 * `local_heap_setup` (exported in `stm_sys.h`).
 */
static void __local_ram(uint8_t **start, uint8_t **end)
{
    *start = &_ebss;
    *end = (uint8_t *) (((uintptr_t) &_stack - RESERVE_STACK_SIZE));
}

void *sbrk(ptrdiff_t diff)
{
    uint8_t *_old_brk;

    if (_heap_end == NULL) {
        local_heap_setup(&_cur_brk, &_heap_end);
    }

    _old_brk = _cur_brk;
    if (_cur_brk + diff > _heap_end) {
        errno = ENOMEM;
        return (void *) -1;
    }
    _cur_brk += diff;
    return _old_brk;
}

static volatile uint64_t system_millis;

void SysTick_Handler(void)
{
    HAL_IncTick();
    system_millis++;
}

static inline void sys_clock_gettime(struct timespec *t)
{
    uint64_t now = sys_monotonic_time_u64();
    t->tv_sec = (time_t) now / 1000;
    t->tv_nsec = ((int32_t) now % 1000) * 1000000;
}

/* TODO: Needed because `defaultatoms_init` in libAtomVM/defaultatoms.c calls this function.
 * We should be able to remove this after `platform_defaulatoms.{c,h}` are removed on all platforms
 * and `defaultatoms_init` is no longer called.
 */
void platform_defaultatoms_init(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_enable_core_periph_clocks(void)
{
    __HAL_RCC_GPIOA_CLK_ENABLE();
    __HAL_RCC_GPIOB_CLK_ENABLE();
    __HAL_RCC_GPIOC_CLK_ENABLE();
    __HAL_RCC_GPIOD_CLK_ENABLE();
    __HAL_RCC_GPIOE_CLK_ENABLE();
#ifdef __HAL_RCC_GPIOF_CLK_ENABLE
    __HAL_RCC_GPIOF_CLK_ENABLE();
#endif
#ifdef __HAL_RCC_GPIOG_CLK_ENABLE
    __HAL_RCC_GPIOG_CLK_ENABLE();
#endif
#ifdef __HAL_RCC_GPIOH_CLK_ENABLE
    __HAL_RCC_GPIOH_CLK_ENABLE();
#endif
#ifdef __HAL_RCC_GPIOI_CLK_ENABLE
    __HAL_RCC_GPIOI_CLK_ENABLE();
#endif
#ifdef __HAL_RCC_GPIOJ_CLK_ENABLE
    __HAL_RCC_GPIOJ_CLK_ENABLE();
#endif
#ifdef __HAL_RCC_GPIOK_CLK_ENABLE
    __HAL_RCC_GPIOK_CLK_ENABLE();
#endif

#ifndef AVM_DISABLE_GPIO_PORT_DRIVER
    /* Enable SYSCFG clock for EXTI configuration (not all families need it) */
#ifdef __HAL_RCC_SYSCFG_CLK_ENABLE
    __HAL_RCC_SYSCFG_CLK_ENABLE();
#endif
#endif
}

bool sys_lock_pin(GlobalContext *glb, uint32_t gpio_bank, uint16_t pin_num)
{
    struct STM32PlatformData *platform_data = glb->platform_data;
    struct ListHead *item;
    struct ListHead *tmp;
    if (!list_is_empty(&platform_data->locked_pins)) {
        MUTABLE_LIST_FOR_EACH (item, tmp, &platform_data->locked_pins) {
            struct LockedPin *gpio_pin = GET_LIST_ENTRY(item, struct LockedPin, locked_pins_list_head);
            if ((gpio_pin->gpio_bank == gpio_bank) && (gpio_pin->pin_num == pin_num)) {
                AVM_LOGW(TAG, "Pin is already reserved by the system!");
                return false;
            }
        }
    }

    struct LockedPin *data = malloc(sizeof(struct LockedPin));
    if (IS_NULL_PTR(data)) {
        AVM_LOGE(TAG, "Out of memory!");
        AVM_ABORT();
    }
    list_append(&platform_data->locked_pins, &data->locked_pins_list_head);
    data->gpio_bank = gpio_bank;
    data->pin_num = pin_num;

    return true;
}

bool sys_unlock_pin(GlobalContext *glb, uint32_t gpio_bank, uint16_t pin_num)
{
    struct STM32PlatformData *platform_data = glb->platform_data;
    struct ListHead *item;
    struct ListHead *tmp;
    if (!list_is_empty(&platform_data->locked_pins)) {
        MUTABLE_LIST_FOR_EACH (item, tmp, &platform_data->locked_pins) {
            struct LockedPin *gpio_pin = GET_LIST_ENTRY(item, struct LockedPin, locked_pins_list_head);
            if ((gpio_pin->gpio_bank == gpio_bank) && (gpio_pin->pin_num == pin_num)) {
                list_remove(item);
                free(gpio_pin);
                return true;
            }
        }
    }

    return false;
}

void sys_init_platform(GlobalContext *glb)
{
    struct STM32PlatformData *platform = malloc(sizeof(struct STM32PlatformData));
    if (IS_NULL_PTR(platform)) {
        AVM_LOGE(TAG, "Out of memory!");
        AVM_ABORT();
    }
    glb->platform_data = platform;
    list_init(&platform->locked_pins);
}

void sys_free_platform(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    UNUSED(glb);
    UNUSED(timeout_ms);
}

void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_time(struct timespec *t)
{
    sys_clock_gettime(t);
}

void sys_monotonic_time(struct timespec *t)
{
    sys_clock_gettime(t);
}

uint64_t sys_monotonic_time_u64(void)
{
    /* 64-bit read is not atomic on 32-bit ARM; disable interrupts briefly
     * to prevent SysTick_Handler from updating system_millis mid-read.
     * Save/restore PRIMASK so this is safe if called with IRQs already disabled. */
    uint32_t primask = __get_PRIMASK();
    __disable_irq();
    uint64_t val = system_millis;
    __set_PRIMASK(primask);
    return val;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t;
}

enum OpenAVMResult sys_open_avm_from_file(GlobalContext *global, const char *path, struct AVMPackData **data)
{
    UNUSED(global);
    UNUSED(path);
    UNUSED(data);
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);

    // TODO
    AVM_LOGW(TAG, "Open from file not supported on this platform.");
    return AVM_OPEN_NOT_SUPPORTED;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    UNUSED(global);
    UNUSED(path);
    // TODO
    return NULL;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    Context *new_ctx = port_driver_create_port(driver_name, glb, opts);
    if (IS_NULL_PTR(new_ctx)) {
        AVM_LOGE(TAG, "Failed to load port \"%s\".  Ensure the port is configured properly in the build.", driver_name);
        new_ctx = NULL;
    }
    return new_ctx;
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}

void sys_enable_flash_cache(void)
{
    /* Enable flash prefetch and set appropriate wait states.
     * The exact wait states depend on clock frequency and voltage range,
     * but HAL_Init() and SystemClock_Config() will set them correctly.
     * Here we just enable prefetch and instruction cache early. */
#ifdef __HAL_FLASH_PREFETCH_BUFFER_ENABLE
    __HAL_FLASH_PREFETCH_BUFFER_ENABLE();
#endif
#ifdef __HAL_FLASH_INSTRUCTION_CACHE_ENABLE
    __HAL_FLASH_INSTRUCTION_CACHE_ENABLE();
#endif
#ifdef __HAL_FLASH_DATA_CACHE_ENABLE
    __HAL_FLASH_DATA_CACHE_ENABLE();
#endif
}

/* See: ARM V7-M Architecture Reference Manual :: https://static.docs.arm.com/ddi0403/eb/DDI0403E_B_armv7m_arm.pdf */
void sys_init_icache(void)
{
    __DSB();
    __ISB();
#if defined(__ICACHE_PRESENT) && (__ICACHE_PRESENT == 1U)
    SCB_EnableICache();
#endif
#if defined(__DCACHE_PRESENT) && (__DCACHE_PRESENT == 1U)
    SCB_EnableDCache();
#endif
    /* STM32H5 and STM32U5 (Cortex-M33) have a dedicated ICACHE peripheral.
     * Invalidate then enable after SystemClock_Config() has set flash latency. */
#ifdef HAL_ICACHE_MODULE_ENABLED
    HAL_ICACHE_Invalidate();
    HAL_ICACHE_Enable();
#endif
    __DSB();
    __ISB();
}

/* Empty _init/_fini stubs required by __libc_init_array when
 * linking with -nostartfiles (which skips crti.o/crtn.o). */
void _init(void)
{
}

void _fini(void)
{
}

#ifndef AVM_NO_JIT
ModuleNativeEntryPoint sys_map_native_code(const uint8_t *native_code, size_t size, size_t offset)
{
    UNUSED(size);
    // We need to set the Thumb bit
    return (ModuleNativeEntryPoint) ((uintptr_t) (native_code + offset) | 1);
}
#endif
