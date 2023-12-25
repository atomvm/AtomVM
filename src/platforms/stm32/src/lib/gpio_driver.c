/*
 * This file is part of AtomVM.
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

#if !(defined(AVM_DISABLE_GPIO_PORT_DRIVER) && defined(AVM_DISABLE_GPIO_NIFS))

#include <stdbool.h>
#include <string.h>

#include <libopencm3/cm3/nvic.h>
#include <libopencm3/stm32/exti.h>
#include <libopencm3/stm32/gpio.h>

#include <atom.h>
#include <bif.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <nifs.h>
#include <port.h>
#include <scheduler.h>
#include <sys.h>
#include <term.h>
// #define ENABLE_TRACE
#include <trace.h>

#include "avm_log.h"
#include "gpio_driver.h"
#include "stm_sys.h"

#define TAG "gpio_driver"

#define GPIO_MODE_OUTPUT_OD 0x4
// Error that cannot be used for these registers
#define GPIOInvalidBank 0x0000U
#define GPIO_INVALID_MODE 0xE
#define INVALID_GPIO_OSPEED 0xE

static const char *const high_atom = ATOM_STR("\x4", "high");
static const char *const low_atom = ATOM_STR("\x3", "low");
static const char *const invalid_bank_atom = ATOM_STR("\xC", "invalid_bank");
static const char *const invalid_pin_atom = ATOM_STR("\xB", "invalid_pin");
static const char *const invalid_mode_atom = ATOM_STR("\xC", "invalid_mode");
static const char *const invalid_pull_atom = ATOM_STR("\xC", "invalid_pull");
static const char *const invalid_rate_atom = ATOM_STR("\xC", "invalid_rate");
static const char *const invalid_level_atom = ATOM_STR("\xD", "invalid_level");
static const char *const invalid_irq_atom = ATOM_STR("\xB", "invalid_irq");

// Port driver specific  data structures and definitions
#ifndef AVM_DISABLE_GPIO_PORT_DRIVER

static NativeHandlerResult consume_gpio_mailbox(Context *ctx);

static const char *const gpio_atom = ATOM_STR("\x4", "gpio");
static const char *const gpio_interrupt_atom = ATOM_STR("\xE", "gpio_interrupt");
static const char *const invalid_trigger_atom = ATOM_STR("\xF", "invalid_trigger");
static const char *const invalid_listener_atom = ATOM_STR("\x10", "invalid_listener");

#define INVALID_EXTI_TRIGGER 0xEE

struct GPIOListenerData
{
    struct ListHead gpio_listener_list_head;
    int32_t target_local_pid;
    term bank_atom;
    uint16_t gpio_pin;
    uint32_t exti;
    uint8_t exti_irq;
};

struct GPIOData
{
    struct ListHead gpio_listeners;
};

enum gpio_cmd
{
    GPIOInvalidCmd = 0,
    GPIOSetLevelCmd,
    GPIOReadCmd,
    GPIOSetDirectionCmd,
    GPIOSetIntCmd,
    GPIORemoveIntCmd,
    GPIOCloseCmd
};

static const AtomStringIntPair exti_trigger_table[] = {
    { ATOM_STR("\x6", "rising"), EXTI_TRIGGER_RISING },
    { ATOM_STR("\x7", "falling"), EXTI_TRIGGER_FALLING },
    { ATOM_STR("\x4", "both"), EXTI_TRIGGER_BOTH },
    SELECT_INT_DEFAULT(INVALID_EXTI_TRIGGER)
};

static const AtomStringIntPair gpio_cmd_table[] = {
    { ATOM_STR("\x9", "set_level"), GPIOSetLevelCmd },
    { ATOM_STR("\x4", "read"), GPIOReadCmd },
    { ATOM_STR("\xD", "set_direction"), GPIOSetDirectionCmd },
    { ATOM_STR("\x7", "set_int"), GPIOSetIntCmd },
    { ATOM_STR("\xA", "remove_int"), GPIORemoveIntCmd },
    { ATOM_STR("\x5", "close"), GPIOCloseCmd },
    SELECT_INT_DEFAULT(GPIOInvalidCmd)
};

#endif /* NOT defined AVM_DISABLE_GPIO_PORT_DRIVER */

enum gpio_pin_state
{
    GPIOPinInvalid = -1,
    GPIOPinLow = 0,
    GPIOPinHigh = 1
};

static const AtomStringIntPair gpio_bank_table[] = {
    { ATOM_STR("\x1", "a"), GPIOA },
    { ATOM_STR("\x1", "b"), GPIOB },
    { ATOM_STR("\x1", "c"), GPIOC },
    { ATOM_STR("\x1", "d"), GPIOD },
    { ATOM_STR("\x1", "e"), GPIOE },
    { ATOM_STR("\x1", "f"), GPIOF },
    { ATOM_STR("\x1", "g"), GPIOG },
    { ATOM_STR("\x1", "h"), GPIOH },
#ifdef LIBOPENCM3_GPIO_COMMON_F24_H
    { ATOM_STR("\x1", "i"), GPIOI },
    { ATOM_STR("\x1", "j"), GPIOJ },
    { ATOM_STR("\x1", "k"), GPIOK },
#endif /* defined LIBOPENCM3_GPIO_COMMON_F24_H */
    SELECT_INT_DEFAULT(GPIOInvalidBank)
};

static const AtomStringIntPair output_mhz_table[] = {
    { ATOM_STR("\x5", "mhz_2"), GPIO_OSPEED_2MHZ },
    { ATOM_STR("\x6", "mhz_25"), GPIO_OSPEED_25MHZ },
    { ATOM_STR("\x6", "mhz_50"), GPIO_OSPEED_50MHZ },
    { ATOM_STR("\x7", "mhz_100"), GPIO_OSPEED_100MHZ },
    SELECT_INT_DEFAULT(INVALID_GPIO_OSPEED)
};

static const AtomStringIntPair pin_level_table[] = {
    { ATOM_STR("\x3", "low"), GPIOPinLow },
    { ATOM_STR("\x4", "high"), GPIOPinHigh },
    SELECT_INT_DEFAULT(GPIOPinInvalid)
};

static const AtomStringIntPair pin_mode_table[] = {
    { ATOM_STR("\x5", "input"), GPIO_MODE_INPUT },
    { ATOM_STR("\x6", "output"), GPIO_MODE_OUTPUT },
    { ATOM_STR("\x9", "output_od"), GPIO_MODE_OUTPUT_OD },
    { ATOM_STR("\x2", "af"), GPIO_MODE_AF },
    { ATOM_STR("\x6", "analog"), GPIO_MODE_ANALOG },
    SELECT_INT_DEFAULT(GPIO_INVALID_MODE)
};

static const AtomStringIntPair pull_mode_table[] = {
    { ATOM_STR("\x2", "up"), GPIO_PUPD_PULLUP },
    { ATOM_STR("\x4", "down"), GPIO_PUPD_PULLDOWN },
    { ATOM_STR("\x8", "floating"), GPIO_PUPD_NONE },
    SELECT_INT_DEFAULT(GPIO_PUPD_NONE)
};

#ifndef AVM_DISABLE_GPIO_PORT_DRIVER
// Obtain the IRQ interrupt associated with a pin number
static uint8_t pin_num_to_exti_irq(uint16_t pin_num)
{
    switch (pin_num) {
        case 0:
            return NVIC_EXTI0_IRQ;
        case 1:
            return NVIC_EXTI1_IRQ;
        case 2:
            return NVIC_EXTI2_IRQ;
        case 3:
            return NVIC_EXTI3_IRQ;
        case 4:
            return NVIC_EXTI4_IRQ;
        case 5:
            return NVIC_EXTI9_5_IRQ;
        case 6:
            return NVIC_EXTI9_5_IRQ;
        case 7:
            return NVIC_EXTI9_5_IRQ;
        case 8:
            return NVIC_EXTI9_5_IRQ;
        case 9:
            return NVIC_EXTI9_5_IRQ;
        case 10:
            return NVIC_EXTI15_10_IRQ;
        case 11:
            return NVIC_EXTI15_10_IRQ;
        case 12:
            return NVIC_EXTI15_10_IRQ;
        case 13:
            return NVIC_EXTI15_10_IRQ;
        case 14:
            return NVIC_EXTI15_10_IRQ;
        case 15:
            return NVIC_EXTI15_10_IRQ;
        default:
            return 0;
    }
}

void gpio_interrupt_callback(Context *ctx, uint32_t exti);
void isr_handler(Context *ctx, uint32_t exti);
void isr_error_handler(const char *isr_name);
#endif /* NOT defined AVM_DISABLE_GPIO_PORT_DRIVER */

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static term error_tuple_maybe_gc(Context *ctx, term reason_atom)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }

    return create_pair(ctx, ERROR_ATOM, reason_atom);
}

static term error_tuple_str_maybe_gc(Context *ctx, AtomString reason_str)
{
    term reason = globalcontext_make_atom(ctx->global, reason_str);
    return error_tuple_maybe_gc(ctx, reason);
}

static inline term level_to_atom(Context *ctx, uint16_t level)
{
    term level_atom;
    if (level != 0) {
        level_atom = globalcontext_make_atom(ctx->global, high_atom);
    } else {
        level_atom = globalcontext_make_atom(ctx->global, low_atom);
    }
    return level_atom;
}

static term get_error_type(term error_tuple)
{
    if ((term_is_tuple(error_tuple)) && (term_get_tuple_element(error_tuple, 0) == ERROR_ATOM)) {
        return term_get_tuple_element(error_tuple, 1);
    }
    return OK_ATOM;
}

// Common setup function used by nif and port driver
static term setup_gpio_pin(Context *ctx, term gpio_pin_tuple, term mode_term)
{
    if (UNLIKELY(!term_is_tuple(gpio_pin_tuple))) {
        AVM_LOGE(TAG, "Invalid GPIO Pin tuple, expect {Bank, Pin}.");
        return error_tuple_maybe_gc(ctx, BADARG_ATOM);
    }
    term gpio_bank_atom = term_get_tuple_element(gpio_pin_tuple, 0);
    if (UNLIKELY(!term_is_atom(gpio_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom! (a...h|k depending on board)");
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }

    uint32_t gpio_bank = ((uint32_t) interop_atom_term_select_int(gpio_bank_table, gpio_bank_atom, ctx->global));
    if (UNLIKELY(gpio_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO Bank '%s' in pin tuple", bank_string);
        free(bank_string);
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }

    term pin_term = term_get_tuple_element(gpio_pin_tuple, 1);
    uint16_t gpio_pin_mask = 0x0000U;
    if (term_is_list(pin_term)) {
        if (UNLIKELY(!term_is_nonempty_list(pin_term))) {
            AVM_LOGE(TAG, "Pin list parameter contains no pin numbers!");
            return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
        }
        while (term_is_nonempty_list(pin_term)) {
            term gpio_pin_term = term_get_list_head(pin_term);
            if (UNLIKELY(!term_is_any_integer(gpio_pin_term))) {
                AVM_LOGE(TAG, "Pin numbers must be between 0 and 15!");
                return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
            }
            uint16_t gpio_pin_num = ((uint16_t) term_to_int32(gpio_pin_term));
            if (UNLIKELY(gpio_pin_num > 15)) {
                AVM_LOGE(TAG, "Pin numbers must be between 0 and 15!");
                return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
            }
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
            pin_term = term_get_list_tail(pin_term);
        }
    } else if (term_is_integer(pin_term)) {
        uint16_t gpio_pin_num = ((uint16_t) term_to_int32(pin_term));
        if (UNLIKELY(gpio_pin_num > 15)) {
            AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
            return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
        } else {
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
        }
    } else if (term_is_atom(pin_term)) {
        if (pin_term != ALL_ATOM) {
            return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
        }
        gpio_pin_mask = GPIO_ALL;
    } else {
        return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
    }

    term mode_atom;
    uint8_t gpio_mode;
    bool setup_output = false;
    uint8_t pull_up_down;
    uint8_t out_type;
    uint8_t output_speed;
    term mhz_atom = term_invalid_term();
    term pull_atom = term_invalid_term();
    if (term_is_tuple(mode_term)) {
        mode_atom = term_get_tuple_element(mode_term, 0);
        if (UNLIKELY(!term_is_atom(mode_atom))) {
            AVM_LOGE(TAG, "GPIO Mode must be an atom ('input', 'output', 'output_od').");
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        gpio_mode = ((uint8_t) interop_atom_term_select_int(pin_mode_table, mode_atom, ctx->global));
        if (UNLIKELY(gpio_mode == GPIO_INVALID_MODE)) {
            char *mode_string = interop_atom_to_string(ctx, mode_atom);
            AVM_LOGE(TAG, "Invalid gpio mode: %s", mode_string);
            free(mode_string);
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        if ((gpio_mode == GPIO_MODE_OUTPUT) || (gpio_mode == GPIO_MODE_OUTPUT_OD)) {
            if (gpio_mode == GPIO_MODE_OUTPUT_OD) {
                gpio_mode = GPIO_MODE_OUTPUT;
                out_type = GPIO_OTYPE_OD;
            } else {
                out_type = GPIO_OTYPE_PP;
            }
            setup_output = true;
        }

        pull_atom = term_get_tuple_element(mode_term, 1);
        if (UNLIKELY(!term_is_atom(pull_atom))) {
            AVM_LOGE(TAG, "GPIO pull direction must be one of the following atoms: up | down | floating");
            return error_tuple_str_maybe_gc(ctx, invalid_pull_atom);
        }

        pull_up_down = ((uint8_t) interop_atom_term_select_int(pull_mode_table, pull_atom, ctx->global));
        if ((setup_output) && (term_get_tuple_arity(mode_term) == 3)) {
            mhz_atom = term_get_tuple_element(mode_term, 2);
            if (UNLIKELY(!term_is_atom(mhz_atom))) {
                AVM_LOGE(TAG, "GPIO output speed must be one of the following atoms: mhz_2 | mhz_25 | mhz_50 | mhz_100");
                error_tuple_str_maybe_gc(ctx, invalid_rate_atom);
            }

            output_speed = (uint8_t) interop_atom_term_select_int(output_mhz_table, mhz_atom, ctx->global);
            if (output_speed == INVALID_GPIO_OSPEED) {
                output_speed = GPIO_OSPEED_2MHZ;
                char *mhz_string = interop_atom_to_string(ctx, mhz_atom);
                AVM_LOGW(TAG, "Invalid output speed '%s' given, falling back to 2 Mhz default.", mhz_string);
                free(mhz_string);
            }
        } else if (setup_output) {
            output_speed = GPIO_OSPEED_2MHZ;
            AVM_LOGW(TAG, "No output speed given, falling back to 2 Mhz default.");
        }
    } else {
        mode_atom = mode_term;
        if (UNLIKELY(!term_is_atom(mode_atom))) {
            AVM_LOGE(TAG, "GPIO Mode must be an atom ('input', 'output', 'output_od').");
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        gpio_mode = ((uint8_t) interop_atom_term_select_int(pin_mode_table, mode_atom, ctx->global));
        if (UNLIKELY(gpio_mode == GPIO_INVALID_MODE)) {
            char *mode_string = interop_atom_to_string(ctx, mode_atom);
            AVM_LOGE(TAG, "Invalid gpio mode: %s", mode_string);
            free(mode_string);
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        pull_up_down = GPIO_PUPD_NONE;
        if ((gpio_mode == GPIO_MODE_OUTPUT) || (gpio_mode == GPIO_MODE_OUTPUT_OD)) {
            if (gpio_mode == GPIO_MODE_OUTPUT_OD) {
                gpio_mode = GPIO_MODE_OUTPUT;
                out_type = GPIO_OTYPE_OD;
            } else {
                out_type = GPIO_OTYPE_PP;
            }
            output_speed = GPIO_OSPEED_2MHZ;
            setup_output = true;
        }
    }

    gpio_mode_setup(gpio_bank, gpio_mode, pull_up_down, gpio_pin_mask);
    if (setup_output) {
        gpio_set_output_options(gpio_bank, out_type, output_speed, gpio_pin_mask);
        AVM_LOGD(TAG, "Setup: Pin bank 0x%08lX pin bitmask 0x%04X output mode 0x%02X, output speed 0x%04X, pull mode 0x%02X", gpio_bank, gpio_pin_mask, gpio_mode, output_speed, pull_up_down);
    } else {
        AVM_LOGD(TAG, "Setup: Pin bank 0x%08lX pin bitmask 0x%04X input mode 0x%02X, pull mode 0x%02X", gpio_bank, gpio_pin_mask, gpio_mode, pull_up_down);
    }
    return OK_ATOM;
}

// Common write function used by nif and port driver
static term gpio_digital_write(Context *ctx, term gpio_pin_tuple, term level_term)
{
    if (UNLIKELY(!term_is_tuple(gpio_pin_tuple))) {
        AVM_LOGE(TAG, "Invalid GPIO Pin tuple, expect {Bank, Pin}.");
        return error_tuple_maybe_gc(ctx, BADARG_ATOM);
    }
    term gpio_bank_atom = term_get_tuple_element(gpio_pin_tuple, 0);
    if (UNLIKELY(!term_is_atom(gpio_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom!");
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }

    uint32_t gpio_bank = ((uint32_t) interop_atom_term_select_int(gpio_bank_table, gpio_bank_atom, ctx->global));
    if (UNLIKELY(gpio_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO Bank '%s' in pin tuple", bank_string);
        free(bank_string);
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }

    term pin_term = term_get_tuple_element(gpio_pin_tuple, 1);
    uint16_t gpio_pin_mask = 0x0000U;
    if (term_is_list(pin_term)) {
        if (UNLIKELY(!term_is_nonempty_list(pin_term))) {
            AVM_LOGE(TAG, "Pin list parameter contains no pin numbers!");
            return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
        }
        while (term_is_nonempty_list(pin_term)) {
            term gpio_pin_term = term_get_list_head(pin_term);
            if (UNLIKELY(!term_is_integer(gpio_pin_term))) {
                return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
            }
            uint16_t gpio_pin_num = ((uint16_t) term_to_int32(gpio_pin_term));
            if (UNLIKELY(gpio_pin_num > 15)) {
                AVM_LOGE(TAG, "Pin numbers must be between 0 and 15!");
                return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
            }
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
            pin_term = term_get_list_tail(pin_term);
        }
    } else if (term_is_integer(pin_term)) {
        uint16_t gpio_pin_num = ((uint16_t) term_to_int32(pin_term));
        if (UNLIKELY(gpio_pin_num > 15)) {
            AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
            return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
        } else {
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
        }
    } else if (term_is_atom(pin_term)) {
        if (UNLIKELY(pin_term != ALL_ATOM)) {

            AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
            return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
        }
        gpio_pin_mask = GPIO_ALL;
    } else {
        return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
    }

    int level;
    if (term_is_integer(level_term)) {
        level = term_to_int(level_term);
        if (UNLIKELY((level != 0) && (level != 1))) {
            return error_tuple_str_maybe_gc(ctx, invalid_level_atom);
        }
    } else {
        if (UNLIKELY(!term_is_atom(level_term))) {
            AVM_LOGE(TAG, "GPIO level must be 0 or 1, or an atom ('high' or 'low').");
            return error_tuple_str_maybe_gc(ctx, invalid_level_atom);
        }
        level = interop_atom_term_select_int(pin_level_table, level_term, ctx->global);
        if (UNLIKELY(level < 0)) {
            AVM_LOGE(TAG, "GPIO level atom must be 'high' or 'low'.");
            return error_tuple_str_maybe_gc(ctx, invalid_level_atom);
        }
    }

    if (level != 0) {
        gpio_set(gpio_bank, gpio_pin_mask);
    } else {
        gpio_clear(gpio_bank, gpio_pin_mask);
    }
    TRACE("Write: bank: 0x%08lX, pin mask: 0x%04X, level: %i\n", gpio_bank, gpio_pin_mask, level);
    return OK_ATOM;
}

// Common read function used by nif and port driver
static term gpio_digital_read(Context *ctx, term gpio_pin_tuple)
{
    if (UNLIKELY(!term_is_tuple(gpio_pin_tuple))) {
        AVM_LOGE(TAG, "Invalid GPIO Pin tuple, expect {Bank, Pin}.");
        return error_tuple_maybe_gc(ctx, BADARG_ATOM);
    }
    term gpio_bank_atom = term_get_tuple_element(gpio_pin_tuple, 0);
    if (UNLIKELY(!term_is_atom(gpio_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom!");
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }
    uint32_t gpio_bank = ((uint32_t) interop_atom_term_select_int(gpio_bank_table, gpio_bank_atom, ctx->global));

    if (UNLIKELY(gpio_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO Bank '%s' in pin tuple", bank_string);
        free(bank_string);
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }
    // TODO: Add support for reading list, or all input pins on port?
    uint16_t gpio_pin_num = ((uint16_t) term_to_int32(term_get_tuple_element(gpio_pin_tuple, 1)));
    if (UNLIKELY(gpio_pin_num > 15)) {
        AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
        return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
    }

    uint16_t pin_levels = gpio_get(gpio_bank, (1U << gpio_pin_num));
    uint16_t level = (pin_levels >> gpio_pin_num);
    TRACE("Read: Bank 0x%08lX Pin %u. RESULT: %u\n", gpio_bank, gpio_pin_num, level);

    return level_to_atom(ctx, level);
}

#ifndef AVM_DISABLE_GPIO_PORT_DRIVER

void gpiodriver_init(GlobalContext *glb)
{
    UNUSED(glb);
}

static Context *gpio_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);
    Context *ctx = context_new(global);

    struct GPIOData *gpio_data = malloc(sizeof(struct GPIOData));
    list_init(&gpio_data->gpio_listeners);

    ctx->native_handler = consume_gpio_mailbox;
    ctx->platform_data = gpio_data;

    term reg_name_term = globalcontext_make_atom(global, gpio_atom);
    int atom_index = term_to_atom_index(reg_name_term);

    if (UNLIKELY(!globalcontext_register_process(ctx->global, atom_index, ctx->process_id))) {
        scheduler_terminate(ctx);
        AVM_LOGE(TAG, "Only a single GPIO driver can be opened.");
        return NULL;
    }

    return ctx;
}

static term gpiodriver_close(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    term gpio_atom_term = globalcontext_make_atom(glb, gpio_atom);
    int gpio_atom_index = term_to_atom_index(gpio_atom_term);
    if (UNLIKELY(!globalcontext_get_registered_process(glb, gpio_atom_index))) {
        AVM_LOGE(TAG, "No active GPIO driver can be found.");
        return error_tuple_maybe_gc(ctx, NOPROC_ATOM);
    }

    struct GPIOData *gpio_data = ctx->platform_data;
    struct ListHead *item;
    struct ListHead *tmp;
    if (!list_is_empty(&gpio_data->gpio_listeners)) {
        MUTABLE_LIST_FOR_EACH (item, tmp, &gpio_data->gpio_listeners) {
            struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
            uint32_t exti = gpio_listener->exti;
            uint8_t irqn = pin_num_to_exti_irq(gpio_listener->gpio_pin);
            nvic_disable_irq(irqn);
            exti_disable_request(exti);
            list_remove(&gpio_listener->gpio_listener_list_head);
            free(gpio_listener);
        }
    }

    globalcontext_unregister_process(glb, gpio_atom_index);
    free(gpio_data);

    return OK_ATOM;
}

void gpio_interrupt_callback(Context *ctx, uint32_t exti)
{
    int32_t listening_pid;
    term gpio_bank;
    uint16_t gpio_pin;

    struct GPIOData *gpio_data = ctx->platform_data;
    struct ListHead *item;
    LIST_FOR_EACH (item, &gpio_data->gpio_listeners) {
        struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
        if (gpio_listener->exti == exti) {
            listening_pid = gpio_listener->target_local_pid;
            gpio_bank = gpio_listener->bank_atom;
            gpio_pin = gpio_listener->gpio_pin;

            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + TUPLE_SIZE(2), heap);

            term int_msg = term_alloc_tuple(2, &heap);
            term gpio_tuple = term_alloc_tuple(2, &heap);
            term_put_tuple_element(int_msg, 0, globalcontext_make_atom(ctx->global, gpio_interrupt_atom));
            term_put_tuple_element(gpio_tuple, 0, gpio_bank);
            term_put_tuple_element(gpio_tuple, 1, term_from_int32((int32_t) gpio_pin));
            term_put_tuple_element(int_msg, 1, gpio_tuple);

            globalcontext_send_message(ctx->global, listening_pid, int_msg);

            END_WITH_STACK_HEAP(heap, ctx->global);
        }
    }
}

// This function is used to store the local Context pointer during gpiodriver_set_int and to relay messages to the interrupt_callback
// from the exti#_isr interrupt handlers (defined in libopencm3/stm32/CHIP-SERIES/nvic.h) that have no access to the context.
void isr_handler(Context *ctx, uint32_t exti)
{
    static Context *local_ctx;
    if (ctx != NULL) {
        UNUSED(exti);
        local_ctx = ctx;
        TRACE("gpio_driver: GlobalContext stored in isr_handler\n");
    } else {
        UNUSED(ctx);
        TRACE("gpio_driver: isr_handler relaying exti 0x%08lX to gpio_interrupt_callback\n", exti);
        gpio_interrupt_callback(local_ctx, exti);
    }
}

void isr_error_handler(const char *isr_name)
{
    AVM_LOGE(TAG, "%s triggered, but no match found!", isr_name);
}

void exti0_isr()
{
    exti_reset_request(EXTI0);
    isr_handler(NULL, EXTI0);
}

void exti1_isr()
{
    exti_reset_request(EXTI1);
    isr_handler(NULL, EXTI1);
}

void exti2_isr()
{
    exti_reset_request(EXTI2);
    isr_handler(NULL, EXTI2);
}

void exti3_isr()
{
    exti_reset_request(EXTI3);
    isr_handler(NULL, EXTI3);
}

void exti4_isr()
{
    exti_reset_request(EXTI4);
    isr_handler(NULL, EXTI4);
}

void exti9_5_isr()
{
    if (exti_get_flag_status(EXTI5) == EXTI5) {
        exti_reset_request(EXTI5);
        isr_handler(NULL, EXTI5);
    } else if (exti_get_flag_status(EXTI6) == EXTI6) {
        exti_reset_request(EXTI6);
        isr_handler(NULL, EXTI6);
    } else if (exti_get_flag_status(EXTI7) == EXTI7) {
        exti_reset_request(EXTI7);
        isr_handler(NULL, EXTI7);
    } else if (exti_get_flag_status(EXTI8) == EXTI8) {
        exti_reset_request(EXTI8);
        isr_handler(NULL, EXTI8);
    } else if (exti_get_flag_status(EXTI9) == EXTI9) {
        exti_reset_request(EXTI9);
        isr_handler(NULL, EXTI9);
    } else {
        static const char *const isr_name = "exti9_5_isr";
        isr_error_handler(isr_name);
    }
}

void exti15_10_isr()
{
    if (exti_get_flag_status(EXTI10)) {
        exti_reset_request(EXTI10);
        isr_handler(NULL, EXTI10);
    } else if (exti_get_flag_status(EXTI11)) {
        exti_reset_request(EXTI11);
        isr_handler(NULL, EXTI11);
    } else if (exti_get_flag_status(EXTI12)) {
        exti_reset_request(EXTI12);
        isr_handler(NULL, EXTI12);
    } else if (exti_get_flag_status(EXTI13)) {
        exti_reset_request(EXTI13);
        isr_handler(NULL, EXTI13);
    } else if (exti_get_flag_status(EXTI14)) {
        exti_reset_request(EXTI14);
        isr_handler(NULL, EXTI14);
    } else if (exti_get_flag_status(EXTI15)) {
        exti_reset_request(EXTI15);
        isr_handler(NULL, EXTI15);
    } else {
        static const char *const isr_name = "exti15_10_isr";
        isr_error_handler(isr_name);
    }
}

static term gpiodriver_set_level(Context *ctx, term cmd)
{
    term gpio_pin_tuple = term_get_tuple_element(cmd, 1);
    term level = term_get_tuple_element(cmd, 2);

    return gpio_digital_write(ctx, gpio_pin_tuple, level);
}

static term gpiodriver_set_direction(Context *ctx, term cmd)
{
    term gpio_tuple = term_get_tuple_element(cmd, 1);
    term direction = term_get_tuple_element(cmd, 2);

    return setup_gpio_pin(ctx, gpio_tuple, direction);
}

static term gpiodriver_read(Context *ctx, term cmd)
{
    term gpio_pin_tuple = term_get_tuple_element(cmd, 1);
    return gpio_digital_read(ctx, gpio_pin_tuple);
}

static bool gpiodriver_is_gpio_attached(struct GPIOData *gpio_data, term gpio_bank_atom, uint16_t pin)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &gpio_data->gpio_listeners) {
        struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
        if ((gpio_listener->gpio_pin == pin) && (gpio_listener->bank_atom == gpio_bank_atom)) {
            return true;
        }
    }
    return false;
}

static term gpiodriver_set_int(Context *ctx, int32_t target_pid, term cmd)
{
    int32_t target_local_pid;

    term gpio_tuple = term_get_tuple_element(cmd, 1);
    if (UNLIKELY(!term_is_tuple(gpio_tuple))) {
        AVM_LOGE(TAG, "Invalid GPIO Pin tuple, expect {Bank, Pin}.");
        return error_tuple_maybe_gc(ctx, BADARG_ATOM);
    }
    term gpio_bank_atom = term_get_tuple_element(gpio_tuple, 0);
    if (UNLIKELY(!term_is_atom(gpio_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom!");
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }
    uint32_t gpio_bank = (uint32_t) interop_atom_term_select_int(gpio_bank_table, gpio_bank_atom, ctx->global);
    if (UNLIKELY(gpio_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO bank '%s' in pin tuple", bank_string);
        free(bank_string);
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }

    uint16_t gpio_pin = (uint16_t) term_to_int32(term_get_tuple_element(gpio_tuple, 1));

    struct GPIOData *gpio_data = ctx->platform_data;
    if (gpiodriver_is_gpio_attached(gpio_data, gpio_bank_atom, gpio_pin)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGW(TAG, "GPIO pin %s%u interrupt already attached.", bank_string, gpio_pin);
        free(bank_string);
        return OK_ATOM;
    }

    term trigger = term_get_tuple_element(cmd, 2);
    if (UNLIKELY(!term_is_atom(trigger))) {
        AVM_LOGE(TAG, "GPIO interrupt trigger must be an atom ('rising', 'falling', or 'both').");
        return error_tuple_str_maybe_gc(ctx, invalid_trigger_atom);
    }
    enum exti_trigger_type interrupt_type = interop_atom_term_select_int(exti_trigger_table, trigger, ctx->global);
    if (UNLIKELY(interrupt_type == INVALID_EXTI_TRIGGER)) {
        char *trigger_string = interop_atom_to_string(ctx, trigger);
        AVM_LOGE(TAG, "Interrupt type %s not supported on stm32 platform.", trigger_string);
        free(trigger_string);
        return error_tuple_str_maybe_gc(ctx, invalid_trigger_atom);
    }

    if (term_get_tuple_arity(cmd) == 4) {
        term pid = term_get_tuple_element(cmd, 3);
        if (UNLIKELY(!term_is_pid(pid) && !term_is_atom(pid))) {
            AVM_LOGE(TAG, "Invalid listener parameter, must be a pid() or registered process!");
            return create_pair(ctx, ERROR_ATOM, invalid_listener_atom);
        }
        if (term_is_pid(pid)) {
            target_local_pid = term_to_local_process_id(pid);
        } else {
            int pid_atom_index = term_to_atom_index(pid);
            int32_t registered_process = (int32_t) globalcontext_get_registered_process(ctx->global, pid_atom_index);
            if (UNLIKELY(registered_process == 0)) {
                AVM_LOGE(TAG, "Invalid listener parameter, atom() is not a registered process name!");
                return create_pair(ctx, ERROR_ATOM, NOPROC_ATOM);
            }
            target_local_pid = registered_process;
        }
    } else {
        target_local_pid = target_pid;
    }

    uint32_t exti = 1U << gpio_pin;

    if (!list_is_empty(&gpio_data->gpio_listeners)) {
        struct ListHead *item;
        LIST_FOR_EACH (item, &gpio_data->gpio_listeners) {
            struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
            if (gpio_listener->exti == exti) {
                char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
                AVM_LOGE(TAG, "Cannot set interrupt for pin %s%u, exti%u device already in use!", bank_string, gpio_pin, gpio_pin);
                free(bank_string);
                return error_tuple_str_maybe_gc(ctx, invalid_irq_atom);
            }
        }
    }

    uint8_t exti_irq = pin_num_to_exti_irq(gpio_pin);
    if (UNLIKELY(exti_irq == 0)) {
        AVM_LOGE(TAG, "BUG: No valid exti irq found!");
        return error_tuple_str_maybe_gc(ctx, invalid_irq_atom);
    }

    struct GPIOListenerData *data = malloc(sizeof(struct GPIOListenerData));
    if (IS_NULL_PTR(data)) {
        AVM_LOGE(TAG, "Out of memory!");
        AVM_ABORT();
    }
    list_append(&gpio_data->gpio_listeners, &data->gpio_listener_list_head);
    data->target_local_pid = target_local_pid;
    data->bank_atom = gpio_bank_atom;
    data->gpio_pin = gpio_pin;
    data->exti = exti;
    data->exti_irq = exti_irq;

    AVM_LOGD(TAG, "Installing interrupt type 0x%02X with exti%u for bank 0x%08lX pin %u.", interrupt_type, gpio_pin, gpio_bank, gpio_pin);

    exti_disable_request(exti);
    exti_select_source(exti, gpio_bank);
    exti_set_trigger(exti, interrupt_type);
    // Store the Context pointer in the isr_handler
    isr_handler(ctx, 0x0000U);
    exti_enable_request(exti);
    if (!nvic_get_irq_enabled(exti_irq)) {
        nvic_enable_irq(exti_irq);
    }
    nvic_set_priority(exti_irq, 1);

    return OK_ATOM;
}

static term gpiodriver_remove_int(Context *ctx, term cmd)
{
    term gpio_tuple = term_get_tuple_element(cmd, 1);
    if (UNLIKELY(!term_is_tuple(gpio_tuple))) {
        AVM_LOGE(TAG, "Invalid GPIO Pin tuple, expect {Bank, Pin}.");
        return error_tuple_maybe_gc(ctx, BADARG_ATOM);
    }
    term target_bank_atom = term_get_tuple_element(gpio_tuple, 0);
    if (UNLIKELY(!term_is_atom(target_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom!");
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }
    uint32_t target_bank = (uint32_t) interop_atom_term_select_int(gpio_bank_table, target_bank_atom, ctx->global);
    if (UNLIKELY(target_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, target_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO bank %s in pin tuple", bank_string);
        free(bank_string);
        return error_tuple_str_maybe_gc(ctx, invalid_bank_atom);
    }
    uint16_t target_num = (uint16_t) term_to_int32(term_get_tuple_element(gpio_tuple, 1));
    uint8_t target_irq = pin_num_to_exti_irq(target_num);

    bool stop_irq = true;
    bool int_removed = false;
    struct GPIOData *gpio_data = ctx->platform_data;
    struct ListHead *item;
    struct ListHead *tmp;
    if (!list_is_empty(&gpio_data->gpio_listeners)) {
        MUTABLE_LIST_FOR_EACH (item, tmp, &gpio_data->gpio_listeners) {
            struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
            if ((gpio_listener->gpio_pin == target_num) && (gpio_listener->bank_atom == target_bank_atom)) {
                uint16_t exti = gpio_listener->exti;
                uint8_t irqn = pin_num_to_exti_irq(gpio_listener->gpio_pin);
                nvic_disable_irq(irqn);
                list_remove(&gpio_listener->gpio_listener_list_head);
                exti_disable_request(exti);
                free(gpio_listener);
                int_removed = true;
                // some pins share irqs - don't stop the irq if another pin is still using it.
            } else if (gpio_listener->exti_irq == target_irq) {
                stop_irq = false;
            }
        }
        if (stop_irq) {
            nvic_disable_irq(target_irq);
        }
        if (int_removed == false) {
            AVM_LOGW(TAG, "No interrupt removed, match not found for bank 0x%08lX pin %u.", target_bank, target_num);
        }
    } else {
        AVM_LOGW(TAG, "No interrupts have been previously set.");
    }

    return OK_ATOM;
}

static NativeHandlerResult consume_gpio_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    GenMessage gen_message;
    if (UNLIKELY(port_parse_gen_message(message->message, &gen_message) != GenCallMessage)
        || !term_is_tuple(gen_message.req) || term_get_tuple_arity(gen_message.req) < 1) {
        AVM_LOGW(TAG, "Received invalid message.");
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
        return NativeContinue;
    }
    term req = gen_message.req;
    term cmd_term = term_get_tuple_element(req, 0);

    int local_process_id = term_to_local_process_id(gen_message.pid);

    term ret;

    enum gpio_cmd cmd = interop_atom_term_select_int(gpio_cmd_table, cmd_term, ctx->global);
    switch (cmd) {
        case GPIOSetLevelCmd:
            ret = gpiodriver_set_level(ctx, req);
            break;

        case GPIOSetDirectionCmd:
            ret = gpiodriver_set_direction(ctx, req);
            break;

        case GPIOReadCmd:
            ret = gpiodriver_read(ctx, req);
            break;

        case GPIOSetIntCmd:
            ret = gpiodriver_set_int(ctx, local_process_id, req);
            break;

        case GPIORemoveIntCmd:
            ret = gpiodriver_remove_int(ctx, req);
            break;

        case GPIOCloseCmd:
            ret = gpiodriver_close(ctx);
            break;

        case GPIOInvalidCmd: {
            char *invalid_name = interop_atom_to_string(ctx, cmd_term);
            AVM_LOGE(TAG, "Invalid command: %s", invalid_name);
            free(invalid_name);
            ret = error_tuple_maybe_gc(ctx, UNDEFINED_ATOM);
            break;
        }

        default: {
            char *cmd_name = interop_atom_to_string(ctx, cmd_term);
            AVM_LOGE(TAG, "Unhandled error processing command: %s", cmd_name);
            free(cmd_name);
            ret = error_tuple_maybe_gc(ctx, BADMATCH_ATOM);
        }
    }

    term ret_msg;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ret, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ret_msg = OUT_OF_MEMORY_ATOM;
    } else {
        ret_msg = create_pair(ctx, gen_message.ref, ret);
    }

    globalcontext_send_message(ctx->global, local_process_id, ret_msg);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return cmd == GPIOCloseCmd ? NativeTerminate : NativeContinue;
}

REGISTER_PORT_DRIVER(gpio, gpiodriver_init, NULL, gpio_driver_create_port)

#endif /* NOT defined AVM_DISABLE_GPIO_PORT_DRIVER */

//
// Nif implementation
//

#ifndef AVM_DISABLE_GPIO_NIFS

static term nif_gpio_set_pin_mode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term ret = setup_gpio_pin(ctx, argv[0], argv[1]);
    term error = get_error_type(ret);
    if (UNLIKELY(error != OK_ATOM)) {
        RAISE_ERROR(error);
    } else {
        if (UNLIKELY(ret == OUT_OF_MEMORY_ATOM)) {
            RAISE_ERROR(ret);
        }
    }
    return ret;
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    AVM_LOGW(TAG, "Pull mode must be set using `gpio:set_pin_mode/2` arg #2 i.e. {Mode,PullMode}");
    return UNDEF_ATOM;
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term ret = gpio_digital_write(ctx, argv[0], argv[1]);
    term error = get_error_type(ret);
    if (UNLIKELY(error != OK_ATOM)) {
        RAISE_ERROR(error);
    } else {
        if (UNLIKELY(ret == OUT_OF_MEMORY_ATOM)) {
            RAISE_ERROR(ret);
        }
    }
    return ret;
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term ret = gpio_digital_read(ctx, argv[0]);
    term error = get_error_type(ret);
    if (UNLIKELY(error != OK_ATOM)) {
        RAISE_ERROR(error);
    } else {
        if (UNLIKELY(ret == OUT_OF_MEMORY_ATOM)) {
            RAISE_ERROR(ret);
        }
    }
    return ret;
}

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

const struct Nif *gpio_nif_get_nif(const char *nifname)
{
    if (strcmp("gpio:set_pin_mode/2", nifname) == 0 || strcmp("Elixir.GPIO:set_pin_mode/2", nifname) == 0) {
        AVM_LOGD(TAG, "Resolved platform nif %s ...", nifname);
        return &gpio_set_pin_mode_nif;
    }

    if (strcmp("gpio:set_pin_pull/2", nifname) == 0 || strcmp("Elixir.GPIO:set_pin_pull/2", nifname) == 0) {
        AVM_LOGD(TAG, "Resolved platform nif %s ...", nifname);
        return &gpio_set_pin_pull_nif;
    }

    if (strcmp("gpio:digital_write/2", nifname) == 0 || strcmp("Elixir.GPIO:digital_write/2", nifname) == 0) {
        AVM_LOGD(TAG, "Resolved platform nif %s ...", nifname);
        return &gpio_digital_write_nif;
    }

    if (strcmp("gpio:digital_read/1", nifname) == 0 || strcmp("Elixir.GPIO:digital_read/1", nifname) == 0) {
        AVM_LOGD(TAG, "Resolved platform nif %s ...", nifname);
        return &gpio_digital_read_nif;
    }

    return NULL;
}

REGISTER_NIF_COLLECTION(gpio, NULL, NULL, gpio_nif_get_nif)
#endif /* NOT defined AVM_DISABLE_GPIO_NIFS */

#endif /* NOT defined AVM_DISABLE_GPIO_PORT_DRIVER AND AVM_DISABLE_GPIO_NIFS */
