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

#include "stm32_hal_platform.h"

/* Normalize GPIO_PIN_All vs GPIO_PIN_ALL across HAL versions */
#if !defined(GPIO_PIN_All) && defined(GPIO_PIN_ALL)
#define GPIO_PIN_All GPIO_PIN_ALL
#endif

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
    { ATOM_STR("\x6", "rising"), GPIO_MODE_IT_RISING },
    { ATOM_STR("\x7", "falling"), GPIO_MODE_IT_FALLING },
    { ATOM_STR("\x4", "both"), GPIO_MODE_IT_RISING_FALLING },
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
    { ATOM_STR("\x1", "a"), (int) GPIOA },
    { ATOM_STR("\x1", "b"), (int) GPIOB },
    { ATOM_STR("\x1", "c"), (int) GPIOC },
    { ATOM_STR("\x1", "d"), (int) GPIOD },
    { ATOM_STR("\x1", "e"), (int) GPIOE },
#ifdef GPIOF
    { ATOM_STR("\x1", "f"), (int) GPIOF },
#endif
#ifdef GPIOG
    { ATOM_STR("\x1", "g"), (int) GPIOG },
#endif
#ifdef GPIOH
    { ATOM_STR("\x1", "h"), (int) GPIOH },
#endif
#ifdef GPIOI
    { ATOM_STR("\x1", "i"), (int) GPIOI },
#endif
#ifdef GPIOJ
    { ATOM_STR("\x1", "j"), (int) GPIOJ },
#endif
#ifdef GPIOK
    { ATOM_STR("\x1", "k"), (int) GPIOK },
#endif
    SELECT_INT_DEFAULT(GPIOInvalidBank)
};

static const AtomStringIntPair output_mhz_table[] = {
    { ATOM_STR("\x5", "mhz_2"), GPIO_SPEED_FREQ_LOW },
    { ATOM_STR("\x6", "mhz_25"), GPIO_SPEED_FREQ_MEDIUM },
    { ATOM_STR("\x6", "mhz_50"), GPIO_SPEED_FREQ_HIGH },
    { ATOM_STR("\x7", "mhz_100"), GPIO_SPEED_FREQ_VERY_HIGH },
    SELECT_INT_DEFAULT(INVALID_GPIO_OSPEED)
};

static const AtomStringIntPair pin_level_table[] = {
    { ATOM_STR("\x3", "low"), GPIOPinLow },
    { ATOM_STR("\x4", "high"), GPIOPinHigh },
    SELECT_INT_DEFAULT(GPIOPinInvalid)
};

static const AtomStringIntPair pin_mode_table[] = {
    { ATOM_STR("\x5", "input"), GPIO_MODE_INPUT },
    { ATOM_STR("\x6", "output"), GPIO_MODE_OUTPUT_PP },
    { ATOM_STR("\x9", "output_od"), GPIO_MODE_OUTPUT_OD },
    { ATOM_STR("\x2", "af"), GPIO_MODE_AF_PP },
    { ATOM_STR("\x6", "analog"), GPIO_MODE_ANALOG },
    SELECT_INT_DEFAULT(GPIO_INVALID_MODE)
};

static const AtomStringIntPair pull_mode_table[] = {
    { ATOM_STR("\x2", "up"), GPIO_PULLUP },
    { ATOM_STR("\x4", "down"), GPIO_PULLDOWN },
    { ATOM_STR("\x8", "floating"), GPIO_NOPULL },
    SELECT_INT_DEFAULT(GPIO_NOPULL)
};

#ifndef AVM_DISABLE_GPIO_PORT_DRIVER
// Obtain the IRQ interrupt associated with a pin number
static IRQn_Type pin_num_to_exti_irq(uint16_t pin_num)
{
    switch (pin_num) {
#if defined(STM32G0XX)
        /* G0: grouped IRQs for lines 0-1, 2-3, 4-15 */
        case 0:
        case 1:
            return EXTI0_1_IRQn;
        case 2:
        case 3:
            return EXTI2_3_IRQn;
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
            return EXTI4_15_IRQn;
#else
        case 0:
            return EXTI0_IRQn;
        case 1:
            return EXTI1_IRQn;
        case 2:
            return EXTI2_IRQn;
        case 3:
            return EXTI3_IRQn;
        case 4:
            return EXTI4_IRQn;
#if defined(STM32F4XX) || defined(STM32H7XX) || defined(STM32WBXX) || defined(STM32F7XX) \
    || defined(STM32G4XX) || defined(STM32L4XX) || defined(STM32F2XX)
        /* Classic EXTI: lines 5-9 and 10-15 share IRQs */
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
            return EXTI9_5_IRQn;
        case 10:
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
            return EXTI15_10_IRQn;
#else
        /* Per-line EXTI: each line has its own IRQ (U5, H5, L5, U3) */
        case 5:
            return EXTI5_IRQn;
        case 6:
            return EXTI6_IRQn;
        case 7:
            return EXTI7_IRQn;
        case 8:
            return EXTI8_IRQn;
        case 9:
            return EXTI9_IRQn;
        case 10:
            return EXTI10_IRQn;
        case 11:
            return EXTI11_IRQn;
        case 12:
            return EXTI12_IRQn;
        case 13:
            return EXTI13_IRQn;
        case 14:
            return EXTI14_IRQn;
        case 15:
            return EXTI15_IRQn;
#endif
#endif /* STM32G0XX */
        default:
            return (IRQn_Type) -1;
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
    GPIO_TypeDef *gpio_port = (GPIO_TypeDef *) gpio_bank;

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
        gpio_pin_mask = GPIO_PIN_All;
    } else {
        return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
    }

    term mode_atom;
    uint32_t gpio_mode;
    bool setup_output = false;
    uint32_t pull_up_down;
    uint32_t output_speed;
    term mhz_atom = term_invalid_term();
    term pull_atom = term_invalid_term();
    if (term_is_tuple(mode_term)) {
        mode_atom = term_get_tuple_element(mode_term, 0);
        if (UNLIKELY(!term_is_atom(mode_atom))) {
            AVM_LOGE(TAG, "GPIO Mode must be an atom ('input', 'output', 'output_od').");
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        gpio_mode = ((uint32_t) interop_atom_term_select_int(pin_mode_table, mode_atom, ctx->global));
        if (UNLIKELY(gpio_mode == GPIO_INVALID_MODE)) {
            char *mode_string = interop_atom_to_string(ctx, mode_atom);
            AVM_LOGE(TAG, "Invalid gpio mode: %s", mode_string);
            free(mode_string);
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        if ((gpio_mode == GPIO_MODE_OUTPUT_PP) || (gpio_mode == GPIO_MODE_OUTPUT_OD)) {
            setup_output = true;
        }

        pull_atom = term_get_tuple_element(mode_term, 1);
        if (UNLIKELY(!term_is_atom(pull_atom))) {
            AVM_LOGE(TAG, "GPIO pull direction must be one of the following atoms: up | down | floating");
            return error_tuple_str_maybe_gc(ctx, invalid_pull_atom);
        }

        pull_up_down = ((uint32_t) interop_atom_term_select_int(pull_mode_table, pull_atom, ctx->global));
        if ((setup_output) && (term_get_tuple_arity(mode_term) == 3)) {
            mhz_atom = term_get_tuple_element(mode_term, 2);
            if (UNLIKELY(!term_is_atom(mhz_atom))) {
                AVM_LOGE(TAG, "GPIO output speed must be one of the following atoms: mhz_2 | mhz_25 | mhz_50 | mhz_100");
                error_tuple_str_maybe_gc(ctx, invalid_rate_atom);
            }

            output_speed = (uint32_t) interop_atom_term_select_int(output_mhz_table, mhz_atom, ctx->global);
            if (output_speed == INVALID_GPIO_OSPEED) {
                output_speed = GPIO_SPEED_FREQ_LOW;
                char *mhz_string = interop_atom_to_string(ctx, mhz_atom);
                AVM_LOGW(TAG, "Invalid output speed '%s' given, falling back to 2 Mhz default.", mhz_string);
                free(mhz_string);
            }
        } else if (setup_output) {
            output_speed = GPIO_SPEED_FREQ_LOW;
            AVM_LOGW(TAG, "No output speed given, falling back to 2 Mhz default.");
        }
    } else {
        mode_atom = mode_term;
        if (UNLIKELY(!term_is_atom(mode_atom))) {
            AVM_LOGE(TAG, "GPIO Mode must be an atom ('input', 'output', 'output_od').");
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        gpio_mode = ((uint32_t) interop_atom_term_select_int(pin_mode_table, mode_atom, ctx->global));
        if (UNLIKELY(gpio_mode == GPIO_INVALID_MODE)) {
            char *mode_string = interop_atom_to_string(ctx, mode_atom);
            AVM_LOGE(TAG, "Invalid gpio mode: %s", mode_string);
            free(mode_string);
            return error_tuple_str_maybe_gc(ctx, invalid_mode_atom);
        }
        pull_up_down = GPIO_NOPULL;
        if ((gpio_mode == GPIO_MODE_OUTPUT_PP) || (gpio_mode == GPIO_MODE_OUTPUT_OD)) {
            output_speed = GPIO_SPEED_FREQ_LOW;
            setup_output = true;
        }
    }

    GPIO_InitTypeDef gpio_init = { 0 };
    gpio_init.Pin = gpio_pin_mask;
    gpio_init.Pull = pull_up_down;

    gpio_init.Mode = gpio_mode;
    if (setup_output) {
        gpio_init.Speed = output_speed;
        AVM_LOGD(TAG, "Setup: Pin bank 0x%08lX pin bitmask 0x%04X output mode 0x%02lX, output speed 0x%04lX, pull mode 0x%02lX", gpio_bank, gpio_pin_mask, gpio_mode, output_speed, pull_up_down);
    } else if (gpio_mode == GPIO_MODE_INPUT) {
        AVM_LOGD(TAG, "Setup: Pin bank 0x%08lX pin bitmask 0x%04X input mode 0x%02lX, pull mode 0x%02lX", gpio_bank, gpio_pin_mask, gpio_mode, pull_up_down);
    } else if (gpio_mode == GPIO_MODE_AF_PP) {
        gpio_init.Speed = GPIO_SPEED_FREQ_HIGH;
        AVM_LOGD(TAG, "Setup: Pin bank 0x%08lX pin bitmask 0x%04X AF mode", gpio_bank, gpio_pin_mask);
    } else if (gpio_mode == GPIO_MODE_ANALOG) {
        AVM_LOGD(TAG, "Setup: Pin bank 0x%08lX pin bitmask 0x%04X analog mode", gpio_bank, gpio_pin_mask);
    }

    HAL_GPIO_Init(gpio_port, &gpio_init);
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
    GPIO_TypeDef *gpio_port = (GPIO_TypeDef *) gpio_bank;

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
        gpio_pin_mask = GPIO_PIN_All;
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

    HAL_GPIO_WritePin(gpio_port, gpio_pin_mask, level ? GPIO_PIN_SET : GPIO_PIN_RESET);
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
    GPIO_TypeDef *gpio_port = (GPIO_TypeDef *) gpio_bank;

    // TODO: Add support for reading list, or all input pins on port?
    uint16_t gpio_pin_num = ((uint16_t) term_to_int32(term_get_tuple_element(gpio_pin_tuple, 1)));
    if (UNLIKELY(gpio_pin_num > 15)) {
        AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
        return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
    }

    GPIO_PinState state = HAL_GPIO_ReadPin(gpio_port, (uint16_t) (1U << gpio_pin_num));
    uint16_t level = (state == GPIO_PIN_SET) ? 1 : 0;
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

    term local_port_id = term_port_from_local_process_id(ctx->process_id);
    if (UNLIKELY(!globalcontext_register_process(ctx->global, atom_index, local_port_id))) {
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
    if (UNLIKELY(globalcontext_get_registered_process(glb, gpio_atom_index) == UNDEFINED_ATOM)) {
        AVM_LOGE(TAG, "No active GPIO driver can be found.");
        return error_tuple_maybe_gc(ctx, NOPROC_ATOM);
    }

    struct GPIOData *gpio_data = ctx->platform_data;
    struct ListHead *item;
    struct ListHead *tmp;
    if (!list_is_empty(&gpio_data->gpio_listeners)) {
        MUTABLE_LIST_FOR_EACH (item, tmp, &gpio_data->gpio_listeners) {
            struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
            uint32_t exti_line = gpio_listener->exti;
            IRQn_Type irqn = pin_num_to_exti_irq(gpio_listener->gpio_pin);
            HAL_NVIC_DisableIRQ(irqn);
            __HAL_GPIO_EXTI_CLEAR_IT(exti_line);
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
// from the exti#_isr interrupt handlers that have no access to the context.
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

/* Compatibility macros for families with separate rising/falling EXTI
 * registers (U5, H5) where __HAL_GPIO_EXTI_GET_IT may not be defined */
#ifndef __HAL_GPIO_EXTI_GET_IT
#define __HAL_GPIO_EXTI_GET_IT(__EXTI_LINE__) \
    (__HAL_GPIO_EXTI_GET_RISING_IT(__EXTI_LINE__) || __HAL_GPIO_EXTI_GET_FALLING_IT(__EXTI_LINE__))
#define __HAL_GPIO_EXTI_CLEAR_IT(__EXTI_LINE__)          \
    do {                                                 \
        __HAL_GPIO_EXTI_CLEAR_RISING_IT(__EXTI_LINE__);  \
        __HAL_GPIO_EXTI_CLEAR_FALLING_IT(__EXTI_LINE__); \
    } while (0)
#endif

/* EXTI ISR handlers - HAL provides HAL_GPIO_EXTI_Callback but we handle
 * them directly for more control over the interrupt handling */

#if defined(STM32G0XX)
/* G0: grouped handlers for lines 0-1, 2-3, 4-15 */
void EXTI0_1_IRQHandler(void)
{
    for (uint16_t pin = 0; pin <= 1; pin++) {
        uint16_t pin_mask = (uint16_t) (1U << pin);
        if (__HAL_GPIO_EXTI_GET_IT(pin_mask)) {
            __HAL_GPIO_EXTI_CLEAR_IT(pin_mask);
            isr_handler(NULL, pin_mask);
        }
    }
}

void EXTI2_3_IRQHandler(void)
{
    for (uint16_t pin = 2; pin <= 3; pin++) {
        uint16_t pin_mask = (uint16_t) (1U << pin);
        if (__HAL_GPIO_EXTI_GET_IT(pin_mask)) {
            __HAL_GPIO_EXTI_CLEAR_IT(pin_mask);
            isr_handler(NULL, pin_mask);
        }
    }
}

void EXTI4_15_IRQHandler(void)
{
    for (uint16_t pin = 4; pin <= 15; pin++) {
        uint16_t pin_mask = (uint16_t) (1U << pin);
        if (__HAL_GPIO_EXTI_GET_IT(pin_mask)) {
            __HAL_GPIO_EXTI_CLEAR_IT(pin_mask);
            isr_handler(NULL, pin_mask);
        }
    }
}

#else /* !STM32G0XX */

void EXTI0_IRQHandler(void)
{
    if (__HAL_GPIO_EXTI_GET_IT(GPIO_PIN_0)) {
        __HAL_GPIO_EXTI_CLEAR_IT(GPIO_PIN_0);
        isr_handler(NULL, GPIO_PIN_0);
    }
}

void EXTI1_IRQHandler(void)
{
    if (__HAL_GPIO_EXTI_GET_IT(GPIO_PIN_1)) {
        __HAL_GPIO_EXTI_CLEAR_IT(GPIO_PIN_1);
        isr_handler(NULL, GPIO_PIN_1);
    }
}

void EXTI2_IRQHandler(void)
{
    if (__HAL_GPIO_EXTI_GET_IT(GPIO_PIN_2)) {
        __HAL_GPIO_EXTI_CLEAR_IT(GPIO_PIN_2);
        isr_handler(NULL, GPIO_PIN_2);
    }
}

void EXTI3_IRQHandler(void)
{
    if (__HAL_GPIO_EXTI_GET_IT(GPIO_PIN_3)) {
        __HAL_GPIO_EXTI_CLEAR_IT(GPIO_PIN_3);
        isr_handler(NULL, GPIO_PIN_3);
    }
}

void EXTI4_IRQHandler(void)
{
    if (__HAL_GPIO_EXTI_GET_IT(GPIO_PIN_4)) {
        __HAL_GPIO_EXTI_CLEAR_IT(GPIO_PIN_4);
        isr_handler(NULL, GPIO_PIN_4);
    }
}

#if defined(STM32F4XX) || defined(STM32H7XX) || defined(STM32WBXX) || defined(STM32F7XX) \
    || defined(STM32G4XX) || defined(STM32L4XX) || defined(STM32F2XX)
/* Classic EXTI: shared handlers for lines 5-9 and 10-15 */
void EXTI9_5_IRQHandler(void)
{
    bool handled = false;
    for (uint16_t pin = 5; pin <= 9; pin++) {
        uint16_t pin_mask = (uint16_t) (1U << pin);
        if (__HAL_GPIO_EXTI_GET_IT(pin_mask)) {
            __HAL_GPIO_EXTI_CLEAR_IT(pin_mask);
            isr_handler(NULL, pin_mask);
            handled = true;
        }
    }
    if (!handled) {
        isr_error_handler("EXTI9_5_IRQHandler");
    }
}

void EXTI15_10_IRQHandler(void)
{
    bool handled = false;
    for (uint16_t pin = 10; pin <= 15; pin++) {
        uint16_t pin_mask = (uint16_t) (1U << pin);
        if (__HAL_GPIO_EXTI_GET_IT(pin_mask)) {
            __HAL_GPIO_EXTI_CLEAR_IT(pin_mask);
            isr_handler(NULL, pin_mask);
            handled = true;
        }
    }
    if (!handled) {
        isr_error_handler("EXTI15_10_IRQHandler");
    }
}
#else
/* Per-line EXTI: each line has its own handler (U5, H5, L5, U3) */
#define DEFINE_EXTI_HANDLER(n)                      \
    void EXTI##n##_IRQHandler(void)                 \
    {                                               \
        if (__HAL_GPIO_EXTI_GET_IT(GPIO_PIN_##n)) { \
            __HAL_GPIO_EXTI_CLEAR_IT(GPIO_PIN_##n); \
            isr_handler(NULL, GPIO_PIN_##n);        \
        }                                           \
    }

DEFINE_EXTI_HANDLER(5)
DEFINE_EXTI_HANDLER(6)
DEFINE_EXTI_HANDLER(7)
DEFINE_EXTI_HANDLER(8)
DEFINE_EXTI_HANDLER(9)
DEFINE_EXTI_HANDLER(10)
DEFINE_EXTI_HANDLER(11)
DEFINE_EXTI_HANDLER(12)
DEFINE_EXTI_HANDLER(13)
DEFINE_EXTI_HANDLER(14)
DEFINE_EXTI_HANDLER(15)

#undef DEFINE_EXTI_HANDLER
#endif
#endif /* !STM32G0XX */

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
    int interrupt_type = interop_atom_term_select_int(exti_trigger_table, trigger, ctx->global);
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
            return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, invalid_listener_atom));
        }
        if (term_is_atom(pid)) {
            int pid_atom_index = term_to_atom_index(pid);
            pid = globalcontext_get_registered_process(ctx->global, pid_atom_index);
        }
        if (term_is_local_pid(pid)) {
            target_local_pid = term_to_local_process_id(pid);
        } else {
            AVM_LOGE(TAG, "Invalid listener parameter, must be a pid() or registered process!");
            return create_pair(ctx, ERROR_ATOM, NOPROC_ATOM);
        }
    } else {
        target_local_pid = target_pid;
    }

    uint32_t exti_line = 1U << gpio_pin;

    if (!list_is_empty(&gpio_data->gpio_listeners)) {
        struct ListHead *item;
        LIST_FOR_EACH (item, &gpio_data->gpio_listeners) {
            struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
            if (gpio_listener->exti == exti_line) {
                char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
                AVM_LOGE(TAG, "Cannot set interrupt for pin %s%u, exti%u device already in use!", bank_string, gpio_pin, gpio_pin);
                free(bank_string);
                return error_tuple_str_maybe_gc(ctx, invalid_irq_atom);
            }
        }
    }

    IRQn_Type exti_irq = pin_num_to_exti_irq(gpio_pin);
    if (UNLIKELY(exti_irq == (IRQn_Type) -1)) {
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
    data->exti = exti_line;
    data->exti_irq = (uint8_t) exti_irq;

    AVM_LOGD(TAG, "Installing interrupt type 0x%02X with exti%u for bank 0x%08lX pin %u.", interrupt_type, gpio_pin, gpio_bank, gpio_pin);

    GPIO_TypeDef *gpio_port = (GPIO_TypeDef *) gpio_bank;
    GPIO_InitTypeDef gpio_init = { 0 };
    gpio_init.Pin = exti_line;
    gpio_init.Pull = GPIO_NOPULL;
    gpio_init.Mode = interrupt_type;
    HAL_GPIO_Init(gpio_port, &gpio_init);

    // Store the Context pointer in the isr_handler
    isr_handler(ctx, 0x0000U);

    HAL_NVIC_SetPriority(exti_irq, 1, 0);
    HAL_NVIC_EnableIRQ(exti_irq);

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
    if (UNLIKELY(target_num > 15)) {
        AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
        return error_tuple_str_maybe_gc(ctx, invalid_pin_atom);
    }
    IRQn_Type target_irq = pin_num_to_exti_irq(target_num);

    bool stop_irq = true;
    bool int_removed = false;
    struct GPIOData *gpio_data = ctx->platform_data;
    struct ListHead *item;
    struct ListHead *tmp;
    if (!list_is_empty(&gpio_data->gpio_listeners)) {
        MUTABLE_LIST_FOR_EACH (item, tmp, &gpio_data->gpio_listeners) {
            struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
            if ((gpio_listener->gpio_pin == target_num) && (gpio_listener->bank_atom == target_bank_atom)) {
                uint32_t exti_line = gpio_listener->exti;
                IRQn_Type irqn = pin_num_to_exti_irq(gpio_listener->gpio_pin);
                HAL_NVIC_DisableIRQ(irqn);
                list_remove(&gpio_listener->gpio_listener_list_head);
                __HAL_GPIO_EXTI_CLEAR_IT(exti_line);
                free(gpio_listener);
                int_removed = true;
                // some pins share irqs - don't stop the irq if another pin is still using it.
            } else if (gpio_listener->exti_irq == (uint8_t) target_irq) {
                stop_irq = false;
            }
        }
        if (stop_irq) {
            HAL_NVIC_DisableIRQ(target_irq);
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

static term nif_gpio_init(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    /* GPIO clocks are already enabled at startup in sys_enable_core_periph_clocks() */
    return OK_ATOM;
}

static term nif_gpio_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return OK_ATOM;
}

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

const struct Nif *gpio_nif_get_nif(const char *nifname)
{
    if (strcmp("gpio:init/1", nifname) == 0 || strcmp("Elixir.GPIO:init/1", nifname) == 0) {
        AVM_LOGD(TAG, "Resolved platform nif %s ...", nifname);
        return &gpio_init_nif;
    }

    if (strcmp("gpio:deinit/1", nifname) == 0 || strcmp("Elixir.GPIO:deinit/1", nifname) == 0) {
        AVM_LOGD(TAG, "Resolved platform nif %s ...", nifname);
        return &gpio_deinit_nif;
    }

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
