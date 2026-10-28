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
#include <rtems.h>
#include <rtems/irq-extension.h>
#include <stdio.h>

#if defined(RTEMS_HAS_IMX_GPIO)
#include <bsp/fdt.h>
#include <bsp/imx-gpio.h>
#include <bsp/irq.h>
#include <libfdt.h>
#elif defined(RTEMS_HAS_ESP32C3_GPIO)
#include <dev/gpio/gpio.h>
#include <fcntl.h>
#include <unistd.h>
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

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static term error_atom(Context *ctx, AtomString reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, reason));
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
#if defined(RTEMS_HAS_IMX_GPIO)
static const char *const path_atom = ATOM_STR("\x4", "path");
static const char *const property_atom = ATOM_STR("\x8", "property");
static const char *const index_atom = ATOM_STR("\x5", "index");
#endif
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

struct GPIOEvent
{
    uint8_t bank;
    uint8_t pin;
};

struct GPIOEventQueue
{
    volatile uint32_t head;
    volatile uint32_t tail;
    struct GPIOEvent events[GPIO_EVENT_QUEUE_SIZE];
};

struct GPIOListener
{
    bool active;
    uint8_t bank;
    uint8_t pin;
    bool is_int;
    int32_t target_local_pid;
};

static struct GPIOEventQueue gpio_event_queue;
static struct GPIOListener gpio_listeners[MAX_GPIO_LISTENERS];
static rtems_id atomvm_main_task_id = 0;
RTEMS_INTERRUPT_LOCK_DEFINE(static, gpio_lock, "gpio_lock")
#if defined(RTEMS_HAS_IMX_GPIO)
static bool gpio_bank_irq_installed[7][2];
static uint32_t open_drain_mask[7];
#elif defined(RTEMS_HAS_ESP32C3_GPIO)
static int esp32_gpio_fd = -1;
static bool esp32_gpio_owned[BSP_GPIO_PIN_COUNT];

static void esp32_gpio_isr(uint32_t pin, void *arg)
{
    UNUSED(arg);

    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);

    uint32_t head = gpio_event_queue.head;
    uint32_t next_head = (head + 1) % GPIO_EVENT_QUEUE_SIZE;
    if (next_head != gpio_event_queue.tail) {
        gpio_event_queue.events[head].bank = 0;
        gpio_event_queue.events[head].pin = (uint8_t) pin;
        gpio_event_queue.head = next_head;
    }

    rtems_interrupt_lock_release(&gpio_lock, &lock_context);

    if (atomvm_main_task_id != 0) {
        rtems_event_send(atomvm_main_task_id, RTEMS_EVENT_0);
    }
}

static bool esp32_gpio_get_config(unsigned pin, rtems_gpio_config *config)
{
    if (esp32_gpio_fd < 0) {
        esp32_gpio_fd = open("/dev/gpio", O_RDWR);
    }
    return esp32_gpio_fd >= 0
        && rtems_gpio_pin_get_configuration(esp32_gpio_fd, pin, config) == 0;
}

/* The device API configures only free pins. Preserve the other settings and
 * restore the previous configuration if a mode/pull/trigger change fails. */
static bool esp32_gpio_configure(unsigned pin, rtems_gpio_config *config)
{
    rtems_gpio_config previous;
    if (!esp32_gpio_get_config(pin, &previous)) {
        return false;
    }
    bool configured = previous.direction != RTEMS_GPIO_DIRECTION_NONE;
    if (configured && rtems_gpio_pin_release(esp32_gpio_fd, pin) != 0) {
        return false;
    }
    if (config->direction == RTEMS_GPIO_DIRECTION_NONE) {
        config->direction = RTEMS_GPIO_DIRECTION_INPUT;
    }
    bool success = rtems_gpio_pin_configure(esp32_gpio_fd, pin, config) == 0;
    if (!success) {
        if (!configured || rtems_gpio_pin_configure(esp32_gpio_fd, pin, &previous) != 0) {
            esp32_gpio_owned[pin] = false;
            return false;
        }
    }
    esp32_gpio_owned[pin] = true;
    if ((success ? config->trigger : previous.trigger) != RTEMS_GPIO_TRIGGER_NONE) {
        for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
            if (gpio_listeners[i].active && gpio_listeners[i].bank == 0 && gpio_listeners[i].pin == pin) {
                if (rtems_gpio_pin_irq_enable(esp32_gpio_fd, pin, RTEMS_GPIO_IRQ_DEFAULT, esp32_gpio_isr, NULL) != 0) {
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
    if (esp32_gpio_fd >= 0) {
        for (unsigned pin = 0; pin < BSP_GPIO_PIN_COUNT; pin++) {
            if (esp32_gpio_owned[pin]) {
                rtems_gpio_pin_release(esp32_gpio_fd, pin);
                esp32_gpio_owned[pin] = false;
            }
        }
        close(esp32_gpio_fd);
        esp32_gpio_fd = -1;
    }
    memset(gpio_listeners, 0, sizeof(gpio_listeners));
    memset(&gpio_event_queue, 0, sizeof(gpio_event_queue));
    atomvm_main_task_id = 0;
}

static bool parse_esp32c3_pin(term pin_term, unsigned *pin_num, bool *is_int)
{
    if (term_is_integer(pin_term)) {
        avm_int_t val = term_to_int(pin_term);
        if (val >= 0 && val < BSP_GPIO_PIN_COUNT) {
            *pin_num = (unsigned) val;
            *is_int = true;
            return true;
        }
        return false;
    }
    if (term_is_tuple(pin_term) && term_get_tuple_arity(pin_term) == 2) {
        term bank_term = term_get_tuple_element(pin_term, 0);
        term pin_t = term_get_tuple_element(pin_term, 1);
        if (term_is_integer(bank_term) && term_is_integer(pin_t)) {
            avm_int_t b = term_to_int(bank_term);
            avm_int_t p = term_to_int(pin_t);
            if ((b == 0 || b == 1) && p >= 0 && p < BSP_GPIO_PIN_COUNT) {
                *pin_num = (unsigned) p;
                *is_int = false;
                return true;
            }
        }
    }
    return false;
}
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
static bool parse_bank_pin(term pin_term, unsigned *bank, unsigned *pin)
{
    if (!term_is_tuple(pin_term) || term_get_tuple_arity(pin_term) != 2) {
        return false;
    }
    term bank_term = term_get_tuple_element(pin_term, 0);
    term pin_num_term = term_get_tuple_element(pin_term, 1);
    if (!term_is_integer(bank_term) || !term_is_integer(pin_num_term)) {
        return false;
    }
    avm_int_t bank_val = term_to_int(bank_term);
    avm_int_t pin_val = term_to_int(pin_num_term);
    if (bank_val < 1 || bank_val > 7 || pin_val < 0 || pin_val > 31) {
        return false;
    }
    *bank = (unsigned) bank_val;
    *pin = (unsigned) pin_val;
    return true;
}

static bool parse_fdt_pin(Context *ctx, term pin_term, int *node_offset, char **property, size_t *index)
{
    if (!term_is_map(pin_term)) {
        return false;
    }

    GlobalContext *glb = ctx->global;
    term path_term = interop_kv_get_value(pin_term, path_atom, glb);
    term property_term = interop_kv_get_value(pin_term, property_atom, glb);
    term index_term = interop_kv_get_value(pin_term, index_atom, glb);
    if (term_is_invalid_term(path_term) || term_is_invalid_term(property_term) || term_is_invalid_term(index_term)) {
        return false;
    }
    if (!term_is_integer(index_term)) {
        return false;
    }
    avm_int_t idx = term_to_int(index_term);
    if (idx < 0) {
        return false;
    }

    int ok = 0;
    char *path = interop_term_to_string(path_term, &ok);
    if (!ok || IS_NULL_PTR(path)) {
        return false;
    }
    ok = 0;
    char *prop = interop_term_to_string(property_term, &ok);
    if (!ok || IS_NULL_PTR(prop)) {
        free(path);
        return false;
    }

    const void *fdt = bsp_fdt_get();
    int node = fdt_path_offset(fdt, path);
    free(path);
    if (node < 0) {
        free(prop);
        return false;
    }

    *node_offset = node;
    *property = prop;
    *index = (size_t) idx;
    return true;
}

static bool parse_direction(GlobalContext *glb, term dir_term, enum imx_gpio_mode *mode)
{
    if (globalcontext_is_term_equal_to_atom_string(glb, dir_term, input_atom)) {
        *mode = IMX_GPIO_MODE_INPUT;
        return true;
    }
    if (globalcontext_is_term_equal_to_atom_string(glb, dir_term, output_atom)) {
        *mode = IMX_GPIO_MODE_OUTPUT;
        return true;
    }
    return false;
}
#endif

static bool parse_level(GlobalContext *glb, term level_term, uint32_t *value)
{
    if (term_is_integer(level_term)) {
        avm_int_t v = term_to_int(level_term);
        if (v == 0) {
            *value = 0;
            return true;
        }
        if (v == 1) {
            *value = 1;
            return true;
        }
        return false;
    }
    if (globalcontext_is_term_equal_to_atom_string(glb, level_term, low_atom)) {
        *value = 0;
        return true;
    }
    if (globalcontext_is_term_equal_to_atom_string(glb, level_term, high_atom)) {
        *value = 1;
        return true;
    }
    return false;
}

#if defined(RTEMS_HAS_IMX_GPIO)
static bool fill_bank_pin(unsigned bank, unsigned pin_num, enum imx_gpio_mode mode, struct imx_gpio_pin *out)
{
    struct imx_gpio *gpio = imx_gpio_get_by_index(bank - 1);
    if (IS_NULL_PTR(gpio)) {
        return false;
    }
    out->gpio = gpio;
    out->mask = 1u << pin_num;
    out->shift = pin_num;
    out->mode = mode;
    out->is_active_low = false;
    return true;
}

static bool fill_pin(Context *ctx, term pin_term, enum imx_gpio_mode mode, struct imx_gpio_pin *out, char **owned_prop)
{
    *owned_prop = NULL;
    unsigned bank;
    unsigned pin_num;
    if (parse_bank_pin(pin_term, &bank, &pin_num)) {
        return fill_bank_pin(bank, pin_num, mode, out);
    }

    int node;
    char *property;
    size_t index;
    if (!parse_fdt_pin(ctx, pin_term, &node, &property, &index)) {
        return false;
    }
    rtems_status_code sc = imx_gpio_init_from_fdt_property(out, node, property, mode, index);
    *owned_prop = property;
    return sc == RTEMS_SUCCESSFUL;
}

static bool get_pin_bank_and_num(const struct imx_gpio_pin *pin, unsigned *bank, unsigned *pin_num)
{
    *pin_num = pin->shift;
    for (unsigned i = 0; i < 7; i++) {
        if (pin->gpio == imx_gpio_get_by_index(i)) {
            *bank = i + 1;
            return true;
        }
    }
    return false;
}

static rtems_vector_number get_gpio_irq(unsigned bank, unsigned pin_num)
{
    const void *fdt = bsp_fdt_get();
    if (fdt != NULL) {
        char alias[16];
        snprintf(alias, sizeof(alias), "gpio%u", bank - 1);
        const char *path = fdt_get_alias(fdt, alias);
        int node = -1;
        if (path != NULL) {
            node = fdt_path_offset(fdt, path);
        }
        if (node >= 0) {
            int len = 0;
            const uint32_t *prop = fdt_getprop(fdt, node, "interrupts", &len);
            if (prop != NULL && len >= 12) {
                int entry_idx = (pin_num >= 16 && len >= 24) ? 1 : 0;
                uint32_t spi_num = fdt32_to_cpu(prop[entry_idx * 3 + 1]);
                return 32 + spi_num;
            }
        }
    }
    bool is_imx6 = false;
    if (fdt != NULL) {
        if (fdt_node_check_compatible(fdt, 0, "fsl,imx6ul") == 0
            || fdt_node_check_compatible(fdt, 0, "fsl,imx6ull") == 0
            || fdt_node_check_compatible(fdt, 0, "embeddedbrains,grisp2") == 0) {
            is_imx6 = true;
        }
    }
    uint32_t base_spi = is_imx6 ? 66 : 64;
    return 32 + base_spi + (bank - 1) * 2 + (pin_num >= 16 ? 1 : 0);
}

static void imx_gpio_isr(void *arg)
{
    uintptr_t bank = (uintptr_t) arg;
    if (bank < 1 || bank > 7) {
        return;
    }
    struct imx_gpio *gpio = imx_gpio_get_by_index(bank - 1);
    if (IS_NULL_PTR(gpio)) {
        return;
    }
    volatile uint32_t *regs = (volatile uint32_t *) gpio;
    uint32_t isr_status = regs[6]; /* 0x18: ISR */
    uint32_t imr_mask = regs[5]; /* 0x14: IMR */
    uint32_t pending = isr_status & imr_mask;
    if (pending == 0) {
        return;
    }
    regs[6] = pending; /* Write 1 to clear pending flags */

    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);

    while (pending != 0) {
        unsigned pin = (unsigned) __builtin_ctz(pending);
        pending &= ~(1u << pin);

        uint32_t head = gpio_event_queue.head;
        uint32_t next_head = (head + 1) % GPIO_EVENT_QUEUE_SIZE;
        if (next_head != gpio_event_queue.tail) {
            gpio_event_queue.events[head].bank = (uint8_t) bank;
            gpio_event_queue.events[head].pin = (uint8_t) pin;
            gpio_event_queue.head = next_head;
        }
    }

    rtems_interrupt_lock_release(&gpio_lock, &lock_context);

    if (atomvm_main_task_id != 0) {
        rtems_event_send(atomvm_main_task_id, RTEMS_EVENT_0);
    }
}
#endif

void gpio_interrupt_drain(GlobalContext *glb)
{
    while (1) {
        rtems_interrupt_lock_context lock_context;
        rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
        if (gpio_event_queue.head == gpio_event_queue.tail) {
            rtems_interrupt_lock_release(&gpio_lock, &lock_context);
            break;
        }

        uint32_t tail = gpio_event_queue.tail;
        struct GPIOEvent ev = gpio_event_queue.events[tail];
        gpio_event_queue.tail = (tail + 1) % GPIO_EVENT_QUEUE_SIZE;

        struct GPIOListener matched[MAX_GPIO_LISTENERS];
        size_t matched_count = 0;
        for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
            if (gpio_listeners[i].active
                && gpio_listeners[i].bank == ev.bank
                && gpio_listeners[i].pin == ev.pin) {
                matched[matched_count++] = gpio_listeners[i];
            }
        }
        rtems_interrupt_lock_release(&gpio_lock, &lock_context);

        for (size_t i = 0; i < matched_count; i++) {
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + TUPLE_SIZE(2), heap);
            term pin_term;
            if (matched[i].is_int) {
                pin_term = term_from_int(ev.pin);
            } else {
                term pin_tuple = term_alloc_tuple(2, &heap);
                term_put_tuple_element(pin_tuple, 0, term_from_int11(ev.bank == 0 ? 1 : ev.bank));
                term_put_tuple_element(pin_tuple, 1, term_from_int11(ev.pin));
                pin_term = pin_tuple;
            }
            term int_msg = term_alloc_tuple(2, &heap);
            term_put_tuple_element(int_msg, 0, globalcontext_make_atom(glb, gpio_interrupt_atom));
            term_put_tuple_element(int_msg, 1, pin_term);

            globalcontext_send_message(glb, matched[i].target_local_pid, int_msg);
            END_WITH_STACK_HEAP(heap, glb);
        }
    }
}

#if defined(RTEMS_HAS_IMX_GPIO)
static volatile uint32_t *get_iomuxc_base(void)
{
    static volatile uint32_t *base = NULL;
    if (base != NULL) {
        return base;
    }
    const void *fdt = bsp_fdt_get();
    if (fdt != NULL) {
        int node = fdt_node_offset_by_compatible(fdt, -1, "fsl,imx6ul-iomuxc");
        if (node < 0) {
            node = fdt_node_offset_by_compatible(fdt, -1, "fsl,imx7d-iomuxc");
        }
        if (node >= 0) {
            int len = 0;
            const uint32_t *reg = fdt_getprop(fdt, node, "reg", &len);
            if (reg != NULL && len >= 8) {
                base = (volatile uint32_t *) (uintptr_t) fdt32_to_cpu(reg[0]);
                return base;
            }
        }
    }
    base = (volatile uint32_t *) 0x020E0000;
    return base;
}

static bool get_pin_pad_mux(Context *ctx, term pin_term, volatile uint32_t **pad_reg, volatile uint32_t **mux_reg)
{
    if (pad_reg) {
        *pad_reg = NULL;
    }
    if (mux_reg) {
        *mux_reg = NULL;
    }

    unsigned bank, pin_num;
    if (parse_bank_pin(pin_term, &bank, &pin_num)) {
        volatile uint8_t *base = (volatile uint8_t *) get_iomuxc_base();
        if (base == NULL) {
            return false;
        }
        uint32_t pad_off = 0;
        uint32_t mux_off = 0;
        if (bank == 1) {
            if (pin_num <= 9) {
                pad_off = 0x2E8 + pin_num * 4;
                mux_off = 0x05C + pin_num * 4;
            } else if (pin_num <= 15) {
                pad_off = 0x310 + (pin_num - 10) * 4;
                mux_off = 0x084 + (pin_num - 10) * 4;
            } else {
                pad_off = 0x328 + (pin_num - 16) * 4;
                mux_off = 0x09C + (pin_num - 16) * 4;
            }
        } else if (bank == 3) {
            if (pin_num == 8) {
                pad_off = 0x3B0;
                mux_off = 0x124;
            } else if (pin_num == 9) {
                pad_off = 0x3B4;
                mux_off = 0x128;
            } else if (pin_num == 13) {
                pad_off = 0x3C4;
                mux_off = 0x138;
            } else if (pin_num == 14) {
                pad_off = 0x3C8;
                mux_off = 0x13C;
            } else if (pin_num == 15) {
                pad_off = 0x3CC;
                mux_off = 0x140;
            }
        } else if (bank == 4) {
            if (pin_num <= 6) {
                pad_off = 0x2A8 + pin_num * 4;
                mux_off = 0x01C + pin_num * 4;
            }
        }
        if (pad_off != 0) {
            if (pad_reg) {
                *pad_reg = (volatile uint32_t *) (base + pad_off);
            }
            if (mux_reg) {
                *mux_reg = (volatile uint32_t *) (base + mux_off);
            }
            return true;
        }
        return false;
    }

    int node;
    char *prop;
    size_t index;
    if (parse_fdt_pin(ctx, pin_term, &node, &prop, &index)) {
        free(prop);
        const void *fdt = bsp_fdt_get();
        if (fdt == NULL) {
            return false;
        }
        const uint32_t *pinctrl_prop = fdt_getprop(fdt, node, "pinctrl-0", NULL);
        if (pinctrl_prop != NULL) {
            uint32_t phandle = fdt32_to_cpu(*pinctrl_prop);
            int pnode = fdt_node_offset_by_phandle(fdt, phandle);
            if (pnode >= 0) {
                int len = 0;
                const uint32_t *pins = fdt_getprop(fdt, pnode, "fsl,pins", &len);
                if (pins != NULL && (int) ((index + 1) * 24) <= len) {
                    uint32_t mux_off = fdt32_to_cpu(pins[index * 6 + 0]);
                    uint32_t pad_off = fdt32_to_cpu(pins[index * 6 + 1]);
                    volatile uint8_t *base = (volatile uint8_t *) get_iomuxc_base();
                    if (pad_reg) {
                        *pad_reg = (volatile uint32_t *) (base + pad_off);
                    }
                    if (mux_reg) {
                        *mux_reg = (volatile uint32_t *) (base + mux_off);
                    }
                    return true;
                }
            }
        }
    }
    return false;
}
#endif

static term nif_gpio_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(argv[0], &pin_num, &is_int)) {
        rtems_gpio_config config;
        if (!esp32_gpio_get_config(pin_num, &config)) {
            return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
        }
        config.direction = RTEMS_GPIO_DIRECTION_INPUT;
        if (!esp32_gpio_configure(pin_num, &config)) {
            return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
        }
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, argv[0], IMX_GPIO_MODE_INPUT, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    imx_gpio_init(&pin);
    free(owned_prop);
    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(argv[0], &pin_num, &is_int)) {
        rtems_gpio_config config;
        if (!esp32_gpio_get_config(pin_num, &config)
            || (config.direction != RTEMS_GPIO_DIRECTION_NONE
                && rtems_gpio_pin_release(esp32_gpio_fd, pin_num) != 0)) {
            return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
        }
        esp32_gpio_owned[pin_num] = false;
        rtems_interrupt_lock_context lock_context;
        rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
        for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
            if (gpio_listeners[i].active && gpio_listeners[i].bank == 0 && gpio_listeners[i].pin == pin_num) {
                gpio_listeners[i].active = false;
            }
        }
        rtems_interrupt_lock_release(&gpio_lock, &lock_context);
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    UNUSED(ctx);
    UNUSED(argv);
    return OK_ATOM;
#else
    UNUSED(ctx);
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_set_pin_mode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(argv[0], &pin_num, &is_int)) {
        rtems_gpio_config config;
        if (!esp32_gpio_get_config(pin_num, &config)) {
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
        if (!esp32_gpio_configure(pin_num, &config)) {
            return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
        }
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    enum imx_gpio_mode mode;
    bool is_od = false;

    if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], output_od_atom)) {
        mode = IMX_GPIO_MODE_OUTPUT;
        is_od = true;
    } else if (!parse_direction(ctx->global, argv[1], &mode)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, argv[0], mode, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    imx_gpio_init(&pin);
    free(owned_prop);

    unsigned bank, pin_num;
    if (get_pin_bank_and_num(&pin, &bank, &pin_num)) {
        volatile uint32_t *pad_reg = NULL;
        get_pin_pad_mux(ctx, argv[0], &pad_reg, NULL);
        if (is_od) {
            open_drain_mask[bank - 1] |= (1u << pin_num);
            if (pad_reg != NULL) {
                *pad_reg |= (1u << 11); /* ODE */
            }
        } else {
            open_drain_mask[bank - 1] &= ~(1u << pin_num);
            if (pad_reg != NULL) {
                *pad_reg &= ~(1u << 11);
            }
        }
    }
    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term pin_term = argv[0];
    term pull_term = argv[1];

    if (globalcontext_is_term_equal_to_atom_string(ctx->global, pull_term, up_down_atom)) {
        return error_atom(ctx, enotsup_atom);
    }

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(pin_term, &pin_num, &is_int)) {
        rtems_gpio_config config;
        if (!esp32_gpio_get_config(pin_num, &config)) {
            return error_atom(ctx, ATOM_STR("\x7", "einval"));
        }
        if (globalcontext_is_term_equal_to_atom_string(ctx->global, pull_term, up_atom)) {
            config.bias = RTEMS_GPIO_BIAS_PULL_UP;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, pull_term, down_atom)) {
            config.bias = RTEMS_GPIO_BIAS_PULL_DOWN;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, pull_term, floating_atom)) {
            config.bias = RTEMS_GPIO_BIAS_NONE;
        } else {
            RAISE_ERROR(BADARG_ATOM);
        }
        if (!esp32_gpio_configure(pin_num, &config)) {
            return error_atom(ctx, ATOM_STR("\x7", "einval"));
        }
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    volatile uint32_t *pad_reg = NULL;
    if (!get_pin_pad_mux(ctx, pin_term, &pad_reg, NULL) || pad_reg == NULL) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }

    uint32_t val = *pad_reg;
    val &= ~(0x0000F000u); /* Clear PKE (12), PUE (13), PUS (15:14) */

    if (globalcontext_is_term_equal_to_atom_string(ctx->global, pull_term, floating_atom)) {
        /* PKE = 0 */
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, pull_term, down_atom)) {
        val |= (1u << 12) | (1u << 13); /* PKE=1, PUE=1, PUS=00 */
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, pull_term, up_atom)) {
        val |= (1u << 12) | (1u << 13) | (2u << 14); /* PKE=1, PUE=1, PUS=10 */
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    *pad_reg = val;
    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    uint32_t value;
    if (!parse_level(ctx->global, argv[1], &value)) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(argv[0], &pin_num, &is_int)) {
        rtems_gpio_config config;
        if (!esp32_gpio_get_config(pin_num, &config)
            || (config.direction == RTEMS_GPIO_DIRECTION_NONE && !esp32_gpio_configure(pin_num, &config))
            || rtems_gpio_pin_set(esp32_gpio_fd, pin_num, value) != 0) {
            return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
        }
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, argv[0], IMX_GPIO_MODE_OUTPUT, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    free(owned_prop);

    unsigned bank, pin_num;
    if (get_pin_bank_and_num(&pin, &bank, &pin_num)
        && (open_drain_mask[bank - 1] & (1u << pin_num))) {
        uint32_t output = pin.is_active_low ? !value : value;
        if (output == 0) {
            pin.mode = IMX_GPIO_MODE_OUTPUT;
            imx_gpio_init(&pin);
            imx_gpio_set_output(&pin, 0);
        } else {
            /* High-Z emulation for open drain */
            pin.mode = IMX_GPIO_MODE_INPUT;
            imx_gpio_init(&pin);
        }
        return OK_ATOM;
    }

    imx_gpio_init(&pin);
    uint32_t output = pin.is_active_low ? !value : value;
    imx_gpio_set_output(&pin, output);
    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(argv[0], &pin_num, &is_int)) {
        rtems_gpio_config config;
        int value;
        if (!esp32_gpio_get_config(pin_num, &config)
            || (config.direction == RTEMS_GPIO_DIRECTION_NONE && !esp32_gpio_configure(pin_num, &config))
            || rtems_gpio_pin_get(esp32_gpio_fd, pin_num, &value) != 0) {
            return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
        }
        return globalcontext_make_atom(ctx->global, value ? high_atom : low_atom);
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, argv[0], IMX_GPIO_MODE_INPUT, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    imx_gpio_init(&pin);
    uint32_t value = imx_gpio_get_input(&pin);
    if (pin.is_active_low) {
        value = !value;
    }
    free(owned_prop);
    return globalcontext_make_atom(ctx->global, value ? high_atom : low_atom);
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_remove_int(Context *ctx, int argc, term argv[]);

static term nif_gpio_set_int(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term pin_term = argv[0];
    term trigger_term = argv[1];
    term pid_term = argv[2];

    if (!term_is_pid(pid_term)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int32_t target_local_pid = term_to_local_process_id(pid_term);

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(pin_term, &pin_num, &is_int)) {
        rtems_gpio_trigger trigger;
        if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, rising_atom)) {
            trigger = RTEMS_GPIO_TRIGGER_EDGE_RISING;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, falling_atom)) {
            trigger = RTEMS_GPIO_TRIGGER_EDGE_FALLING;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, both_atom)) {
            trigger = RTEMS_GPIO_TRIGGER_EDGE_BOTH;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, low_atom)) {
            trigger = RTEMS_GPIO_TRIGGER_LEVEL_LOW;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, high_atom)) {
            trigger = RTEMS_GPIO_TRIGGER_LEVEL_HIGH;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, none_atom)) {
            return nif_gpio_remove_int(ctx, 1, argv);
        } else {
            RAISE_ERROR(BADARG_ATOM);
        }

        atomvm_main_task_id = rtems_task_self();

        rtems_interrupt_lock_context lock_context;
        rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);

        int free_slot = -1;
        for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
            if (gpio_listeners[i].active
                && gpio_listeners[i].bank == 0
                && gpio_listeners[i].pin == pin_num) {
                free_slot = (int) i;
                break;
            }
            if (!gpio_listeners[i].active && free_slot < 0) {
                free_slot = (int) i;
            }
        }
        if (free_slot < 0) {
            rtems_interrupt_lock_release(&gpio_lock, &lock_context);
            return error_atom(ctx, ATOM_STR("\x10", "listeners_full"));
        }
        rtems_interrupt_lock_release(&gpio_lock, &lock_context);

        rtems_gpio_config config;
        if (!esp32_gpio_get_config(pin_num, &config)) {
            return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
        }
        config.trigger = trigger;
        if (!esp32_gpio_configure(pin_num, &config)) {
            return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
        }

        rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
        gpio_listeners[free_slot].active = true;
        gpio_listeners[free_slot].bank = 0;
        gpio_listeners[free_slot].pin = (uint8_t) pin_num;
        gpio_listeners[free_slot].is_int = is_int;
        gpio_listeners[free_slot].target_local_pid = target_local_pid;

        rtems_interrupt_lock_release(&gpio_lock, &lock_context);

        if (rtems_gpio_pin_irq_enable(esp32_gpio_fd, pin_num, RTEMS_GPIO_IRQ_DEFAULT, esp32_gpio_isr, NULL) != 0) {
            rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
            gpio_listeners[free_slot].active = false;
            rtems_interrupt_lock_release(&gpio_lock, &lock_context);
            return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
        }
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    enum imx_gpio_mode imx_mode;
    if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, rising_atom)) {
        imx_mode = IMX_GPIO_MODE_INTERRUPT_RISING;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, falling_atom)) {
        imx_mode = IMX_GPIO_MODE_INTERRUPT_FALLING;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, both_atom)) {
        imx_mode = IMX_GPIO_MODE_INTERRUPT_ANY_EDGE;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, low_atom)) {
        imx_mode = IMX_GPIO_MODE_INTERRUPT_LOW;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, high_atom)) {
        imx_mode = IMX_GPIO_MODE_INTERRUPT_HIGH;
    } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, trigger_term, none_atom)) {
        return nif_gpio_remove_int(ctx, 1, argv);
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, pin_term, imx_mode, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    free(owned_prop);

    unsigned bank, pin_num;
    if (!get_pin_bank_and_num(&pin, &bank, &pin_num)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }

    atomvm_main_task_id = rtems_task_self();

    unsigned half = (pin_num >= 16) ? 1 : 0;
    if (!gpio_bank_irq_installed[bank - 1][half]) {
        rtems_vector_number vec = get_gpio_irq(bank, pin_num);
        rtems_status_code sc = rtems_interrupt_handler_install(
            vec,
            "atomvm_gpio",
            RTEMS_INTERRUPT_SHARED,
            imx_gpio_isr,
            (void *) (uintptr_t) bank);
        if (sc != RTEMS_SUCCESSFUL) {
            return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
        }
        gpio_bank_irq_installed[bank - 1][half] = true;
    }

    imx_gpio_init(&pin);

    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);

    int free_slot = -1;
    for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
        if (gpio_listeners[i].active
            && gpio_listeners[i].bank == bank
            && gpio_listeners[i].pin == pin_num) {
            free_slot = (int) i;
            break;
        }
        if (!gpio_listeners[i].active && free_slot < 0) {
            free_slot = (int) i;
        }
    }
    if (free_slot < 0) {
        rtems_interrupt_lock_release(&gpio_lock, &lock_context);
        return error_atom(ctx, ATOM_STR("\x10", "listeners_full"));
    }

    gpio_listeners[free_slot].active = true;
    gpio_listeners[free_slot].bank = (uint8_t) bank;
    gpio_listeners[free_slot].pin = (uint8_t) pin_num;
    gpio_listeners[free_slot].is_int = false;
    gpio_listeners[free_slot].target_local_pid = target_local_pid;

    rtems_interrupt_lock_release(&gpio_lock, &lock_context);

    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_remove_int(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(argv[0], &pin_num, &is_int)) {
        rtems_gpio_config config;
        if (!esp32_gpio_get_config(pin_num, &config)
            || (config.direction != RTEMS_GPIO_DIRECTION_NONE
                && rtems_gpio_pin_irq_disable(esp32_gpio_fd, pin_num) != 0)) {
            return error_atom(ctx, ATOM_STR("\xE", "irq_install_err"));
        }

        rtems_interrupt_lock_context lock_context;
        rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
        for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
            if (gpio_listeners[i].active
                && gpio_listeners[i].bank == 0
                && gpio_listeners[i].pin == pin_num) {
                gpio_listeners[i].active = false;
            }
        }
        rtems_interrupt_lock_release(&gpio_lock, &lock_context);
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, argv[0], IMX_GPIO_MODE_INPUT, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    free(owned_prop);

    unsigned bank, pin_num;
    if (!get_pin_bank_and_num(&pin, &bank, &pin_num)) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }

    /* Mask pin in IMR and set mode to standard input */
    imx_gpio_init(&pin);

    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&gpio_lock, &lock_context);
    for (size_t i = 0; i < MAX_GPIO_LISTENERS; i++) {
        if (gpio_listeners[i].active
            && gpio_listeners[i].bank == bank
            && gpio_listeners[i].pin == pin_num) {
            gpio_listeners[i].active = false;
        }
    }
    rtems_interrupt_lock_release(&gpio_lock, &lock_context);
    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static term nif_gpio_set_pin_mux(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#if defined(RTEMS_HAS_ESP32C3_GPIO)
    unsigned pin_num;
    bool is_int;
    if (parse_esp32c3_pin(argv[0], &pin_num, &is_int)) {
        return OK_ATOM;
    }
#endif

#if defined(RTEMS_HAS_IMX_GPIO)
    term pin_term = argv[0];
    term mux_term = argv[1];

    if (!term_is_integer(mux_term)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t mux_val = term_to_int(mux_term);
    if (mux_val < 0 || mux_val > 7) {
        RAISE_ERROR(BADARG_ATOM);
    }

    volatile uint32_t *mux_reg = NULL;
    if (!get_pin_pad_mux(ctx, pin_term, NULL, &mux_reg) || mux_reg == NULL) {
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }

    uint32_t val = *mux_reg;
    val = (val & ~0x0Fu) | ((uint32_t) mux_val & 0x0Fu);
    *mux_reg = val;
    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
#endif
}

static const struct Nif gpio_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_init
};

static const struct Nif gpio_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_deinit
};

static const struct Nif gpio_set_pin_mode_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_set_pin_mode
};

static const struct Nif gpio_set_pin_pull_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_set_pin_pull
};

static const struct Nif gpio_digital_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_digital_write
};

static const struct Nif gpio_digital_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_digital_read
};

static const struct Nif gpio_set_int_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_set_int
};

static const struct Nif gpio_remove_int_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_remove_int
};

static const struct Nif gpio_set_pin_mux_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_set_pin_mux
};

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

#if defined(RTEMS_HAS_ESP32C3_GPIO)
REGISTER_NIF_COLLECTION(gpio, NULL, gpio_nif_destroy, gpio_nif_get_nif)
#else
REGISTER_NIF_COLLECTION(gpio, NULL, NULL, gpio_nif_get_nif)
#endif
