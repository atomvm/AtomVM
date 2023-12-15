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
#include <malloc.h>
#include <stdint.h>
#include <time.h>

#include <avmpack.h>
#include <defaultatoms.h>
#include <scheduler.h>
#include <sys.h>

// #define ENABLE_TRACE
#include <trace.h>

#include <libopencm3/cm3/nvic.h>
#include <libopencm3/cm3/scb.h>
#include <libopencm3/stm32/flash.h>
#include <libopencm3/stm32/rcc.h>

#include "avm_log.h"
#include "stm_sys.h"

#define TAG "sys"
#define RESERVE_STACK_SIZE 4096U

static Context *port_driver_create_port(const char *port_name, GlobalContext *global, term opts);

struct PortDriverDefListItem *port_driver_list;
struct NifCollectionDefListItem *nif_collection_list;

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

void *_sbrk_r(struct _reent *reent, ptrdiff_t diff)
{
    uint8_t *_old_brk;

    if (_heap_end == NULL) {
        local_heap_setup(&_cur_brk, &_heap_end);
    }

    _old_brk = _cur_brk;
    if (_cur_brk + diff > _heap_end) {
        reent->_errno = ENOMEM;
        return (void *) -1;
    }
    _cur_brk += diff;
    return _old_brk;
}

// Monotonically increasing number of milliseconds from reset
static volatile uint64_t system_millis;

// Called when systick fires
void sys_tick_handler()
{
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

void sys_enable_core_periph_clocks()
{
    uint32_t list[] = GPIO_CLOCK_LIST;
    for (size_t i = 0; i < sizeof(list) / sizeof(list[0]); i++) {
        rcc_periph_clock_enable((enum rcc_periph_clken) list[i]);
    }
#ifndef AVM_DISABLE_GPIO_PORT_DRIVER
    // This clock enables the syscfg manger for external gpio interupts & ethernet PHY interface.
    rcc_periph_clock_enable(RCC_SYSCFG);
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

uint64_t sys_monotonic_time_u64()
{
    return system_millis;
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

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    struct ListHead *avmpack_data_list = synclist_rdlock(&global->avmpack_data);
    struct ListHead *item;
    LIST_FOR_EACH (item, avmpack_data_list) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        avmpack_data->in_use = true;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    if (IS_NULL_PTR(beam_module)) {
        AVM_LOGE(TAG, "Failed to open module: %s.", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
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

void sys_enable_flash_cache()
{
    flash_unlock_option_bytes();
    flash_set_ws(FLASH_ACR_LATENCY_5WS);
    flash_prefetch_enable();
}

/* See: ARM V7-M Architecture Reference Manual :: https://static.docs.arm.com/ddi0403/eb/DDI0403E_B_armv7m_arm.pdf */
void sys_init_icache()
{
    // Synchronize data and instruction barriers
    __dsb;
    __isb;
    // Invalidate the instruction cache
    SCB_ICIALLU = 0UL;
    // Invalidate all branch predictors
    SCB_BPIALL = 0UL;
    // Re-synchronize
    __dsb;
    __isb;
    // Enable the I-cache
    SCB_CCR |= (1 << 17);
    // Enable branch prediction
    SCB_CCR |= (1 << 18);
    // Force a final resync and clear of the instruction pipeline
    __dsb;
    __isb;
}

void port_driver_init_all(GlobalContext *global)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (item->def->port_driver_init_cb) {
            item->def->port_driver_init_cb(global);
        }
    }
}

void port_driver_destroy_all(GlobalContext *global)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (item->def->port_driver_destroy_cb) {
            item->def->port_driver_destroy_cb(global);
        }
    }
}

static Context *port_driver_create_port(const char *port_name, GlobalContext *global, term opts)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (strcmp(port_name, item->def->port_driver_name) == 0) {
            return item->def->port_driver_create_port_cb(global, opts);
        }
    }

    return NULL;
}

void nif_collection_init_all(GlobalContext *global)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        if (item->def->nif_collection_init_cb) {
            item->def->nif_collection_init_cb(global);
        }
    }
}

void nif_collection_destroy_all(GlobalContext *global)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        if (item->def->nif_collection_destroy_cb) {
            item->def->nif_collection_destroy_cb(global);
        }
    }
}

const struct Nif *nif_collection_resolve_nif(const char *name)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        const struct Nif *res = item->def->nif_collection_resolve_nif_cb(name);
        if (res) {
            return res;
        }
    }

    return NULL;
}
