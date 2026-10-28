/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#include "rtc.h"
#include "rtems_sys.h"

#ifdef RTEMS_HAS_GPIO
#include <bsp.h>
#include <dev/gpio/gpio.h>
#include <fcntl.h>
#include <rtems.h>
#include <unistd.h>

#if defined(RTEMS_HAS_IMX_GPIO)
#include "imx_gpio_device.h"
#include <bsp/fdt.h>
#include <libfdt.h>
#define GPIO_PIN_COUNT IMX_GPIO_DEVICE_PIN_COUNT
#else
#define GPIO_PIN_COUNT BSP_GPIO_PIN_COUNT
#endif
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <interop.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>
#include <utils.h>

static const char *const enotsup_atom = ATOM_STR("\x7", "enotsup");

static term error_atom(Context *ctx, AtomString reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, ERROR_ATOM);
    term_put_tuple_element(result, 1, globalcontext_make_atom(ctx->global, reason));
    return result;
}

#ifndef RTEMS_HAS_GPIO
void gpio_interrupt_drain(GlobalContext *glb)
{
    UNUSED(glb);
}

static term nif_gpio_unsupported(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return error_atom(ctx, enotsup_atom);
}
#else

static const char *const high_atom = ATOM_STR("\x4", "high");
static const char *const low_atom = ATOM_STR("\x3", "low");
static const char *const input_atom = ATOM_STR("\x5", "input");
static const char *const output_atom = ATOM_STR("\x6", "output");
static const char *const output_od_atom = ATOM_STR("\x9", "output_od");
static const char *const rising_atom = ATOM_STR("\x6", "rising");
static const char *const falling_atom = ATOM_STR("\x7", "falling");
static const char *const both_atom = ATOM_STR("\x4", "both");
static const char *const none_atom = ATOM_STR("\x4", "none");
static const char *const up_atom = ATOM_STR("\x2", "up");
static const char *const down_atom = ATOM_STR("\x4", "down");
static const char *const up_down_atom = ATOM_STR("\x7", "up_down");
static const char *const floating_atom = ATOM_STR("\x8", "floating");
static const char *const gpio_interrupt_atom = ATOM_STR("\xE", "gpio_interrupt");

#define MAX_GPIO_LISTENERS 32
#define GPIO_EVENT_QUEUE_SIZE 64

struct GPIOPin
{
    uint32_t id;
    bool is_int;
    bool active_low;
};

struct GPIOListener
{
    bool active;
    struct GPIOPin pin;
    int32_t target_local_pid;
};

static struct
{
    uint32_t head;
    uint32_t tail;
    uint32_t pins[GPIO_EVENT_QUEUE_SIZE];
} gpio_event_queue;
static struct GPIOListener gpio_listeners[MAX_GPIO_LISTENERS];
static bool gpio_owned[GPIO_PIN_COUNT];
static int gpio_fd = -1;
static rtems_id atomvm_main_task_id;
RTEMS_INTERRUPT_LOCK_DEFINE(static, gpio_lock, "gpio_lock")

static bool gpio_open(void)
{
    if (gpio_fd < 0) {
#if defined(RTEMS_HAS_IMX_GPIO)
        if (!imx_gpio_device_initialize()) {
            return false;
        }
#endif
        gpio_fd = open("/dev/gpio", O_RDWR);
    }
    return gpio_fd >= 0;
}

static bool parse_pin(Context *ctx, term pin_term, struct GPIOPin *pin)
{
    pin->is_int = false;
    pin->active_low = false;
#if defined(RTEMS_HAS_ESP32C3_GPIO)
    UNUSED(ctx);
    if (term_is_integer(pin_term)) {
        avm_int_t number = term_to_int(pin_term);
        if (number < 0 || number >= GPIO_PIN_COUNT) {
            return false;
        }
        pin->id = (uint32_t) number;
        pin->is_int = true;
        return true;
    }
#endif
    if (term_is_tuple(pin_term) && term_get_tuple_arity(pin_term) == 2) {
        term bank_term = term_get_tuple_element(pin_term, 0);
        term number_term = term_get_tuple_element(pin_term, 1);
        if (!term_is_integer(bank_term) || !term_is_integer(number_term)) {
            return false;
        }
        avm_int_t bank = term_to_int(bank_term);
        avm_int_t number = term_to_int(number_term);
#if defined(RTEMS_HAS_IMX_GPIO)
        if (bank < 1 || bank > 7 || number < 0 || number > 31) {
            return false;
        }
        pin->id = (uint32_t) ((bank - 1) * 32 + number);
#else
        if ((bank != 0 && bank != 1) || number < 0 || number >= GPIO_PIN_COUNT) {
            return false;
        }
        pin->id = (uint32_t) number;
#endif
        return true;
    }
#if defined(RTEMS_HAS_IMX_GPIO)
    if (term_is_map(pin_term)) {
        term path_term = interop_kv_get_value(pin_term, ATOM_STR("\x4", "path"), ctx->global);
        term property_term = interop_kv_get_value(pin_term, ATOM_STR("\x8", "property"), ctx->global);
        term index_term = interop_kv_get_value(pin_term, ATOM_STR("\x5", "index"), ctx->global);
        if (term_is_invalid_term(path_term) || term_is_invalid_term(property_term)
            || term_is_invalid_term(index_term) || !term_is_integer(index_term)) {
            return false;
        }
        avm_int_t index = term_to_int(index_term);
        if (index < 0) {
            return false;
        }
        int ok = 0;
        char *path = interop_term_to_string(path_term, &ok);
        if (!ok || IS_NULL_PTR(path)) {
            return false;
        }
        char *property = interop_term_to_string(property_term, &ok);
        if (!ok || IS_NULL_PTR(property)) {
            free(path);
            return false;
        }
        const void *fdt = bsp_fdt_get();
        int node = fdt == NULL ? -1 : fdt_path_offset(fdt, path);
        free(path);
        bool resolved = node >= 0 && gpio_open()
            && imx_gpio_resolve_fdt(node, property, (size_t) index, &pin->id, &pin->active_low);
        free(property);
        return resolved;
    }
#endif
    return false;
}

static void gpio_isr(uint32_t pin, void *arg)
{
    UNUSED(arg);
    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
    uint32_t next_head = (gpio_event_queue.head + 1) % GPIO_EVENT_QUEUE_SIZE;
    if (next_head != gpio_event_queue.tail) {
        gpio_event_queue.pins[gpio_event_queue.head] = pin;
        gpio_event_queue.head = next_head;
    }
    rtems_interrupt_lock_release(&gpio_lock, &lock_context);
    if (atomvm_main_task_id != 0) {
        rtems_event_send(atomvm_main_task_id, RTEMS_EVENT_0);
    }
}

static bool gpio_get_config(const struct GPIOPin *pin, rtems_gpio_config *config)
{
    return gpio_open() && rtems_gpio_pin_get_configuration(gpio_fd, pin->id, config) == 0;
}

/* Release/configure is required by the device API. Keep the physical latch
 * unchanged even when a device-tree alias changes the logical polarity. */
static bool gpio_configure(const struct GPIOPin *pin, rtems_gpio_config *config)
{
    rtems_gpio_config previous;
    if (!gpio_get_config(pin, &previous)) {
        return false;
    }
    bool configured = previous.direction != RTEMS_GPIO_DIRECTION_NONE;
    if (configured && rtems_gpio_pin_release(gpio_fd, pin->id) != 0) {
        return false;
    }
    if (config->direction == RTEMS_GPIO_DIRECTION_NONE) {
        config->direction = RTEMS_GPIO_DIRECTION_INPUT;
    }
    bool was_active_low = (config->flags & RTEMS_GPIO_FLAG_ACTIVE_LOW) != 0;
    if (was_active_low != pin->active_low) {
        config->initial_value = !config->initial_value;
        config->flags ^= RTEMS_GPIO_FLAG_ACTIVE_LOW;
    }
    bool success = rtems_gpio_pin_configure(gpio_fd, pin->id, config) == 0;
    if (!success && (!configured || rtems_gpio_pin_configure(gpio_fd, pin->id, &previous) != 0)) {
        gpio_owned[pin->id] = false;
        return false;
    }
    gpio_owned[pin->id] = true;
    const rtems_gpio_config *applied = success ? config : &previous;
    /* initial_value only applies to outputs in the device API. Explicitly
     * restore an input's preload after release/configure (or rollback). */
    if (applied->direction == RTEMS_GPIO_DIRECTION_INPUT
        && rtems_gpio_pin_set(gpio_fd, pin->id, applied->initial_value) != 0) {
        return false;
    }
    if (applied->trigger != RTEMS_GPIO_TRIGGER_NONE) {
        for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
            if (gpio_listeners[i].active && gpio_listeners[i].pin.id == pin->id) {
                if (rtems_gpio_pin_irq_enable(gpio_fd, pin->id, RTEMS_GPIO_IRQ_DEFAULT, gpio_isr, NULL) != 0) {
                    return false;
                }
                break;
            }
        }
    }
    return success;
}

static void gpio_nif_destroy(GlobalContext *global)
{
    UNUSED(global);
    if (gpio_fd >= 0) {
        for (unsigned pin = 0; pin < GPIO_PIN_COUNT; pin++) {
            if (gpio_owned[pin]) {
                rtems_gpio_pin_release(gpio_fd, pin);
                gpio_owned[pin] = false;
            }
        }
        close(gpio_fd);
        gpio_fd = -1;
    }
    memset(gpio_listeners, 0, sizeof(gpio_listeners));
    memset(&gpio_event_queue, 0, sizeof(gpio_event_queue));
    atomvm_main_task_id = 0;
}

void gpio_interrupt_drain(GlobalContext *glb)
{
    while (1) {
        rtems_interrupt_lock_context lock_context;
        rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
        if (gpio_event_queue.head == gpio_event_queue.tail) {
            rtems_interrupt_lock_release(&gpio_lock, &lock_context);
            break;
        }
        uint32_t pin = gpio_event_queue.pins[gpio_event_queue.tail];
        gpio_event_queue.tail = (gpio_event_queue.tail + 1) % GPIO_EVENT_QUEUE_SIZE;
        struct GPIOListener matched[MAX_GPIO_LISTENERS];
        size_t matched_count = 0;
        for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
            if (gpio_listeners[i].active && gpio_listeners[i].pin.id == pin) {
                matched[matched_count++] = gpio_listeners[i];
            }
        }
        rtems_interrupt_lock_release(&gpio_lock, &lock_context);
        for (size_t i = 0; i < matched_count; i++) {
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + TUPLE_SIZE(2), heap);
            term pin_term;
            if (matched[i].pin.is_int) {
                pin_term = term_from_int(pin);
            } else {
#if defined(RTEMS_HAS_IMX_GPIO)
                unsigned bank = pin / 32 + 1;
                unsigned number = pin % 32;
#else
                unsigned bank = 1;
                unsigned number = pin;
#endif
                pin_term = term_alloc_tuple(2, &heap);
                term_put_tuple_element(pin_term, 0, term_from_int11(bank));
                term_put_tuple_element(pin_term, 1, term_from_int11(number));
            }
            term message = term_alloc_tuple(2, &heap);
            term_put_tuple_element(message, 0, globalcontext_make_atom(glb, gpio_interrupt_atom));
            term_put_tuple_element(message, 1, pin_term);
            globalcontext_send_message(glb, matched[i].target_local_pid, message);
            END_WITH_STACK_HEAP(heap, glb);
        }
    }
}

static term nif_gpio_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin pin;
    rtems_gpio_config config;
    if (!parse_pin(ctx, argv[0], &pin) || !gpio_get_config(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    config.direction = RTEMS_GPIO_DIRECTION_INPUT;
    if (!gpio_configure(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    return OK_ATOM;
}

static term nif_gpio_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin pin;
    rtems_gpio_config config;
    if (!parse_pin(ctx, argv[0], &pin) || !gpio_get_config(&pin, &config)
        || (config.direction != RTEMS_GPIO_DIRECTION_NONE && rtems_gpio_pin_release(gpio_fd, pin.id) != 0)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    gpio_owned[pin.id] = false;
    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
    for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
        if (gpio_listeners[i].active && gpio_listeners[i].pin.id == pin.id) {
            gpio_listeners[i].active = false;
        }
    }
    rtems_interrupt_lock_release(&gpio_lock, &lock_context);
    return OK_ATOM;
}

static term nif_gpio_set_pin_mode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin pin;
    rtems_gpio_config config;
    if (!parse_pin(ctx, argv[0], &pin) || !gpio_get_config(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    config.drive = RTEMS_GPIO_DRIVE_PUSH_PULL;
    if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], input_atom)) {
        config.direction = RTEMS_GPIO_DIRECTION_INPUT;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], output_atom)) {
        config.direction = RTEMS_GPIO_DIRECTION_OUTPUT;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], output_od_atom)) {
        config.direction = RTEMS_GPIO_DIRECTION_OUTPUT;
        config.drive = RTEMS_GPIO_DRIVE_OPEN_DRAIN;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (!gpio_configure(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    return OK_ATOM;
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], up_down_atom)) {
        return error_atom(ctx, enotsup_atom);
    }
    struct GPIOPin pin;
    rtems_gpio_config config;
    if (!parse_pin(ctx, argv[0], &pin)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    if (!gpio_get_config(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\x7", "einval"));
    }
    if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], up_atom)) {
        config.bias = RTEMS_GPIO_BIAS_PULL_UP;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], down_atom)) {
        config.bias = RTEMS_GPIO_BIAS_PULL_DOWN;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], floating_atom)) {
        config.bias = RTEMS_GPIO_BIAS_NONE;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (!gpio_configure(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\x7", "einval"));
    }
    return OK_ATOM;
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    int value;
    if (term_is_integer(argv[1]) && (term_to_int(argv[1]) == 0 || term_to_int(argv[1]) == 1)) {
        value = term_to_int(argv[1]);
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], low_atom)) {
        value = 0;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], high_atom)) {
        value = 1;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct GPIOPin pin;
    rtems_gpio_config config;
    if (!parse_pin(ctx, argv[0], &pin) || !gpio_get_config(&pin, &config)
        || (config.direction == RTEMS_GPIO_DIRECTION_NONE && !gpio_configure(&pin, &config))) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    value ^= ((config.flags & RTEMS_GPIO_FLAG_ACTIVE_LOW) != 0) != pin.active_low;
    if (rtems_gpio_pin_set(gpio_fd, pin.id, value) != 0) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    return OK_ATOM;
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin pin;
    rtems_gpio_config config;
    int value;
    if (!parse_pin(ctx, argv[0], &pin) || !gpio_get_config(&pin, &config)
        || (config.direction == RTEMS_GPIO_DIRECTION_NONE && !gpio_configure(&pin, &config))
        || rtems_gpio_pin_get(gpio_fd, pin.id, &value) != 0) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    value ^= ((config.flags & RTEMS_GPIO_FLAG_ACTIVE_LOW) != 0) != pin.active_low;
    return globalcontext_make_atom(ctx->global, value ? high_atom : low_atom);
}

static term nif_gpio_remove_int(Context *ctx, int argc, term argv[]);

static term nif_gpio_set_int(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_pid(argv[2])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct GPIOPin pin;
    if (!parse_pin(ctx, argv[0], &pin)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    rtems_gpio_trigger trigger;
    if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], rising_atom)) {
        trigger = RTEMS_GPIO_TRIGGER_EDGE_RISING;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], falling_atom)) {
        trigger = RTEMS_GPIO_TRIGGER_EDGE_FALLING;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], both_atom)) {
        trigger = RTEMS_GPIO_TRIGGER_EDGE_BOTH;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], low_atom)) {
        trigger = RTEMS_GPIO_TRIGGER_LEVEL_LOW;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], high_atom)) {
        trigger = RTEMS_GPIO_TRIGGER_LEVEL_HIGH;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], none_atom)) {
        return nif_gpio_remove_int(ctx, 1, argv);
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    atomvm_main_task_id = rtems_task_self();
    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
    int free_slot = -1;
    for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
        if (gpio_listeners[i].active && gpio_listeners[i].pin.id == pin.id) {
            free_slot = (int) i;
            break;
        }
        if (!gpio_listeners[i].active && free_slot < 0) {
            free_slot = (int) i;
        }
    }
    rtems_interrupt_lock_release(&gpio_lock, &lock_context);
    if (free_slot < 0) {
        return error_atom(ctx, ATOM_STR("\x10", "listeners_full"));
    }
    rtems_gpio_config config;
    if (!gpio_get_config(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
    }
#if defined(RTEMS_HAS_IMX_GPIO)
    config.direction = RTEMS_GPIO_DIRECTION_INPUT;
#endif
    config.trigger = trigger;
    if (!gpio_configure(&pin, &config)) {
        return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
    }
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
    gpio_listeners[free_slot].active = true;
    gpio_listeners[free_slot].pin = pin;
    gpio_listeners[free_slot].target_local_pid = term_to_local_process_id(argv[2]);
    rtems_interrupt_lock_release(&gpio_lock, &lock_context);
    if (rtems_gpio_pin_irq_enable(gpio_fd, pin.id, RTEMS_GPIO_IRQ_DEFAULT, gpio_isr, NULL) != 0) {
        rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
        gpio_listeners[free_slot].active = false;
        rtems_interrupt_lock_release(&gpio_lock, &lock_context);
        return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
    }
    return OK_ATOM;
}

static term nif_gpio_remove_int(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin pin;
    rtems_gpio_config config;
    if (!parse_pin(ctx, argv[0], &pin)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    if (!gpio_get_config(&pin, &config)
        || (config.direction != RTEMS_GPIO_DIRECTION_NONE && rtems_gpio_pin_irq_disable(gpio_fd, pin.id) != 0)) {
        return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
    }
    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
    for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
        if (gpio_listeners[i].active && gpio_listeners[i].pin.id == pin.id) {
            gpio_listeners[i].active = false;
        }
    }
    rtems_interrupt_lock_release(&gpio_lock, &lock_context);
    return OK_ATOM;
}

static term nif_gpio_set_pin_mux(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin pin;
    if (!parse_pin(ctx, argv[0], &pin)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
#if defined(RTEMS_HAS_IMX_GPIO)
    if (!term_is_integer(argv[1]) || term_to_int(argv[1]) < 0 || term_to_int(argv[1]) > 7) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (!gpio_open() || !imx_gpio_set_mux(pin.id, (unsigned) term_to_int(argv[1]))) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
#endif
    return OK_ATOM;
}

static const struct Nif gpio_init_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_init };
static const struct Nif gpio_deinit_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_deinit };
static const struct Nif gpio_set_pin_mode_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_set_pin_mode };
static const struct Nif gpio_set_pin_pull_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_set_pin_pull };
static const struct Nif gpio_digital_write_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_digital_write };
static const struct Nif gpio_digital_read_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_digital_read };
static const struct Nif gpio_set_int_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_set_int };
static const struct Nif gpio_remove_int_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_remove_int };
static const struct Nif gpio_set_pin_mux_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_set_pin_mux };

#endif /* RTEMS_HAS_GPIO */

static term nif_gpio_deep_sleep_hold_en(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
#if defined(RTEMS_BOARD_ESP32)
    if (rtems_esp_deep_sleep_hold_en() != 0) {
        return error_atom(ctx, enotsup_atom);
    }
#endif
    return OK_ATOM;
}

static term nif_gpio_deep_sleep_hold_dis(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
#if defined(RTEMS_BOARD_ESP32)
    if (rtems_esp_deep_sleep_hold_dis() != 0) {
        return error_atom(ctx, enotsup_atom);
    }
#endif
    return OK_ATOM;
}

static const struct Nif gpio_deep_sleep_hold_en_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_deep_sleep_hold_en
};

static const struct Nif gpio_deep_sleep_hold_dis_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_deep_sleep_hold_dis
};

static const struct Nif *gpio_nif_get_nif(const char *nifname)
{
    if (strcmp("gpio:deep_sleep_hold_en/0", nifname) == 0
        || strcmp("Elixir.GPIO:deep_sleep_hold_en/0", nifname) == 0) {
        return &gpio_deep_sleep_hold_en_nif;
    }
    if (strcmp("gpio:deep_sleep_hold_dis/0", nifname) == 0
        || strcmp("Elixir.GPIO:deep_sleep_hold_dis/0", nifname) == 0) {
        return &gpio_deep_sleep_hold_dis_nif;
    }

#ifndef RTEMS_HAS_GPIO
    static const struct Nif unsupported = {
        .base.type = NIFFunctionType,
        .nif_ptr = nif_gpio_unsupported
    };
    if (strcmp("gpio:init/1", nifname) == 0
        || strcmp("gpio:deinit/1", nifname) == 0
        || strcmp("gpio:set_pin_mode/2", nifname) == 0
        || strcmp("gpio:set_pin_pull/2", nifname) == 0
        || strcmp("gpio:digital_write/2", nifname) == 0
        || strcmp("gpio:digital_read/1", nifname) == 0
        || strcmp("gpio:set_int_nif/3", nifname) == 0
        || strcmp("gpio:remove_int_nif/1", nifname) == 0
        || strcmp("gpio:set_pin_mux/2", nifname) == 0) {
        return &unsupported;
    }
    return NULL;
#else
    if (strcmp("gpio:init/1", nifname) == 0) {
        return &gpio_init_nif;
    }
    if (strcmp("gpio:deinit/1", nifname) == 0) {
        return &gpio_deinit_nif;
    }
    if (strcmp("gpio:set_pin_mode/2", nifname) == 0) {
        return &gpio_set_pin_mode_nif;
    }
    if (strcmp("gpio:set_pin_pull/2", nifname) == 0) {
        return &gpio_set_pin_pull_nif;
    }
    if (strcmp("gpio:digital_write/2", nifname) == 0) {
        return &gpio_digital_write_nif;
    }
    if (strcmp("gpio:digital_read/1", nifname) == 0) {
        return &gpio_digital_read_nif;
    }
    if (strcmp("gpio:set_int_nif/3", nifname) == 0) {
        return &gpio_set_int_nif;
    }
    if (strcmp("gpio:remove_int_nif/1", nifname) == 0) {
        return &gpio_remove_int_nif;
    }
    if (strcmp("gpio:set_pin_mux/2", nifname) == 0) {
        return &gpio_set_pin_mux_nif;
    }
    return NULL;
#endif
}

#ifdef RTEMS_HAS_GPIO
REGISTER_NIF_COLLECTION(gpio, NULL, gpio_nif_destroy, gpio_nif_get_nif)
#else
REGISTER_NIF_COLLECTION(gpio, NULL, NULL, gpio_nif_get_nif)
#endif
