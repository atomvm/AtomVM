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

#include <libopencm3/stm32/gpio.h>

#include <atom.h>
#include <bif.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <nifs.h>
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

#define HIGH_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "high"))
#define LOW_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "low"))

#define INVALID_BANK_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "invalid_bank"))
#define INVALID_PIN_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "invalid_pin"))
#define INVALID_MODE_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "invalid_mode"))
#define INVALID_PULL_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "invalid_pull"))
#define INVALID_RATE_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "invalid_rate"))
#define INVALID_LEVEL_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\xD", "invalid_level"))

// Port driver specific  data structures and definitions
#ifndef AVM_DISABLE_GPIO_PORT_DRIVER
static NativeHandlerResult consume_gpio_mailbox(Context *ctx);

static const char *const gpio_atom = ATOM_STR("\x4", "gpio");
static const char *const gpio_driver_atom = ATOM_STR("\xB", "gpio_driver");

static term gpio_driver;

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

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

// Common setup function used by nif and port driver
static term setup_gpio_pin(Context *ctx, term gpio_pin_tuple, term mode_term)
{
    bool setup_output = false;
    uint16_t gpio_pin_mask = 0x0000U;
    uint8_t gpio_mode;
    uint8_t pull_up_down;
    uint8_t out_type;
    uint8_t output_speed;
    term mhz_atom = term_invalid_term();
    term pull_atom = term_invalid_term();

    if (UNLIKELY(!term_is_tuple(gpio_pin_tuple))) {
        AVM_LOGE(TAG, "Invalid GPIO Pin tuple, expect {Bank, Pin}.");
        return create_pair(ctx, ERROR_ATOM, BADARG_ATOM);
    }
    term gpio_bank_atom = term_get_tuple_element(gpio_pin_tuple, 0);
    if (UNLIKELY(!term_is_atom(gpio_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom! (a...h|k depending on board)");
        return create_pair(ctx, ERROR_ATOM, INVALID_BANK_ATOM);
    }

    uint32_t gpio_bank = ((uint32_t) interop_atom_term_select_int(gpio_bank_table, gpio_bank_atom, ctx->global));
    if (UNLIKELY(gpio_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO Bank '%s' in pin tuple", bank_string);
        free(bank_string);
        return create_pair(ctx, ERROR_ATOM, INVALID_BANK_ATOM);
    }

    term pin_term = term_get_tuple_element(gpio_pin_tuple, 1);
    if (term_is_list(pin_term)) {
        if (UNLIKELY(!term_is_nonempty_list(pin_term))) {
            AVM_LOGE(TAG, "Pin list parameter contains no pin numbers!");
            return create_pair(ctx, ERROR_ATOM, INVALID_PIN_ATOM);
        }
        while (term_is_nonempty_list(pin_term)) {
            term gpio_pin_term = term_get_list_head(pin_term);
            uint16_t gpio_pin_num = ((uint16_t) term_to_int32(gpio_pin_term));
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
            pin_term = term_get_list_tail(pin_term);
        }
    } else if (term_is_integer(pin_term)) {
        uint16_t gpio_pin_num = ((uint16_t) term_to_int32(pin_term));
        if (UNLIKELY(gpio_pin_num > 15)) {
            AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
            return create_pair(ctx, ERROR_ATOM, INVALID_PIN_ATOM);
        } else {
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
        }
    } else if (term_is_atom(pin_term)) {
        if (pin_term == ALL_ATOM) {
            gpio_pin_mask = GPIO_ALL;
        }
    } else {
        return create_pair(ctx, ERROR_ATOM, INVALID_PIN_ATOM);
    }

    term mode_atom;
    if (term_is_tuple(mode_term)) {
        mode_atom = term_get_tuple_element(mode_term, 0);
        if (UNLIKELY(!term_is_atom(mode_atom))) {
            AVM_LOGE(TAG, "GPIO Mode must be an atom ('input', 'output', 'output_od').");
            return create_pair(ctx, ERROR_ATOM, INVALID_MODE_ATOM);
        }
        gpio_mode = ((uint8_t) interop_atom_term_select_int(pin_mode_table, mode_atom, ctx->global));
        if (UNLIKELY(gpio_mode == GPIO_INVALID_MODE)) {
            char *mode_string = interop_atom_to_string(ctx, mode_atom);
            AVM_LOGE(TAG, "Invalid gpio mode: %s", mode_string);
            free(mode_string);
            return create_pair(ctx, ERROR_ATOM, INVALID_MODE_ATOM);
        }
        if ((gpio_mode == GPIO_MODE_OUTPUT) || (gpio_mode == GPIO_MODE_OUTPUT_OD)) {
            if (gpio_mode == GPIO_MODE_OUTPUT_OD) {
                gpio_mode = GPIO_MODE_OUTPUT;
                out_type = GPIO_OTYPE_OD;
                setup_output = true;
            } else {
                out_type = GPIO_OTYPE_PP;
                setup_output = true;
            }
        }

        pull_atom = term_get_tuple_element(mode_term, 1);
        if (UNLIKELY(!term_is_atom(pull_atom))) {
            AVM_LOGE(TAG, "GPIO pull direction must be one of the following atoms: up | down | floating");
            return create_pair(ctx, ERROR_ATOM, INVALID_PULL_ATOM);
        }

        pull_up_down = ((uint8_t) interop_atom_term_select_int(pull_mode_table, pull_atom, ctx->global));
        if ((setup_output) && (term_get_tuple_arity(mode_term) == 3)) {
            mhz_atom = term_get_tuple_element(mode_term, 2);
            if (UNLIKELY(!term_is_atom(mhz_atom))) {
                AVM_LOGE(TAG, "GPIO output speed must be one of the following atoms: mhz_2 | mhz_25 | mhz_50 | mhz_100");
                return create_pair(ctx, ERROR_ATOM, INVALID_RATE_ATOM);
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
            return create_pair(ctx, ERROR_ATOM, INVALID_MODE_ATOM);
        }
        gpio_mode = ((uint8_t) interop_atom_term_select_int(pin_mode_table, mode_atom, ctx->global));
        if (UNLIKELY(gpio_mode == GPIO_INVALID_MODE)) {
            char *mode_string = interop_atom_to_string(ctx, mode_atom);
            AVM_LOGE(TAG, "Invalid gpio mode: %s", mode_string);
            free(mode_string);
            return create_pair(ctx, ERROR_ATOM, INVALID_MODE_ATOM);
        }
        pull_up_down = GPIO_PUPD_NONE;
        if (setup_output) {
            output_speed = GPIO_OSPEED_2MHZ;
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
    uint16_t gpio_pin_mask = 0x0000U;
    int level;
    uint32_t gpio_bank;

    if (UNLIKELY(!term_is_tuple(gpio_pin_tuple))) {
        AVM_LOGE(TAG, "Invalid GPIO Pin tuple, expect {Bank, Pin}.");
        return create_pair(ctx, ERROR_ATOM, BADARG_ATOM);
    }
    term gpio_bank_atom = term_get_tuple_element(gpio_pin_tuple, 0);
    if (UNLIKELY(!term_is_atom(gpio_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom!");
        return create_pair(ctx, ERROR_ATOM, INVALID_BANK_ATOM);
    }
    gpio_bank = ((uint32_t) interop_atom_term_select_int(gpio_bank_table, gpio_bank_atom, ctx->global));

    if (UNLIKELY(gpio_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO Bank '%s' in pin tuple", bank_string);
        free(bank_string);
        return create_pair(ctx, ERROR_ATOM, INVALID_BANK_ATOM);
    }

    term pin_term = term_get_tuple_element(gpio_pin_tuple, 1);
    if (term_is_list(pin_term)) {
        if (UNLIKELY(!term_is_nonempty_list(pin_term))) {
            AVM_LOGE(TAG, "Pin list parameter contains no pin numbers!");
            return create_pair(ctx, ERROR_ATOM, INVALID_PIN_ATOM);
        }
        while (term_is_nonempty_list(pin_term)) {
            term gpio_pin_term = term_get_list_head(pin_term);
            uint16_t gpio_pin_num = ((uint16_t) term_to_int32(gpio_pin_term));
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
            pin_term = term_get_list_tail(pin_term);
        }
    } else if (term_is_integer(pin_term)) {
        uint16_t gpio_pin_num = ((uint16_t) term_to_int32(pin_term));
        if (UNLIKELY(gpio_pin_num > 15)) {
            AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
            return create_pair(ctx, ERROR_ATOM, INVALID_PIN_ATOM);
        } else {
            gpio_pin_mask = 1U << gpio_pin_num | gpio_pin_mask;
        }
    } else if (term_is_atom(pin_term)) {
        if (pin_term == ALL_ATOM) {
            gpio_pin_mask = GPIO_ALL;
        }
    } else {
        return create_pair(ctx, ERROR_ATOM, INVALID_PIN_ATOM);
    }

    if (term_is_integer(level_term)) {
        level = term_to_int(level_term);
        if (UNLIKELY((level != 0) && (level != 1))) {
            return create_pair(ctx, ERROR_ATOM, INVALID_LEVEL_ATOM);
        }
    } else {
        if (UNLIKELY(!term_is_atom(level_term))) {
            AVM_LOGE(TAG, "GPIO level must be 0 or 1, or an atom ('high' or 'low').");
            return create_pair(ctx, ERROR_ATOM, INVALID_LEVEL_ATOM);
        }
        level = interop_atom_term_select_int(pin_level_table, level_term, ctx->global);
        if (UNLIKELY(level < 0)) {
            AVM_LOGE(TAG, "GPIO level atom must be 'high' or 'low'.");
            return create_pair(ctx, ERROR_ATOM, INVALID_LEVEL_ATOM);
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
        return create_pair(ctx, ERROR_ATOM, BADARG_ATOM);
    }
    term gpio_bank_atom = term_get_tuple_element(gpio_pin_tuple, 0);
    if (UNLIKELY(!term_is_atom(gpio_bank_atom))) {
        AVM_LOGE(TAG, "Bank parameter of pin tuple must be an atom!");
        return create_pair(ctx, ERROR_ATOM, INVALID_BANK_ATOM);
    }
    uint32_t gpio_bank = ((uint32_t) interop_atom_term_select_int(gpio_bank_table, gpio_bank_atom, ctx->global));

    if (UNLIKELY(gpio_bank == GPIOInvalidBank)) {
        char *bank_string = interop_atom_to_string(ctx, gpio_bank_atom);
        AVM_LOGE(TAG, "Invalid GPIO Bank '%s' in pin tuple", bank_string);
        free(bank_string);
        return create_pair(ctx, ERROR_ATOM, INVALID_BANK_ATOM);
    }
    // TODO: Add support for reading list, or all input pins on port?
    uint16_t gpio_pin_num = ((uint16_t) term_to_int32(term_get_tuple_element(gpio_pin_tuple, 1)));
    if (UNLIKELY(gpio_pin_num > 15)) {
        AVM_LOGE(TAG, "Pin number must be between 0 and 15!");
        return create_pair(ctx, ERROR_ATOM, INVALID_PIN_ATOM);
    }

    uint16_t pin_levels = gpio_get(gpio_bank, (1U << gpio_pin_num));
    uint16_t level = (pin_levels >> gpio_pin_num);
    TRACE("Read: Bank 0x%08lX Pin %u. RESULT: %u\n", gpio_bank, gpio_pin_num, level);

    return level ? HIGH_ATOM : LOW_ATOM;
}

#ifndef AVM_DISABLE_GPIO_PORT_DRIVER

void gpiodriver_init(GlobalContext *glb)
{
    int index = globalcontext_insert_atom(glb, gpio_driver_atom);
    gpio_driver = term_from_atom_index(index);
}

static Context *gpio_driver_create_port(GlobalContext *global, term opts)
{
    Context *ctx = context_new(global);

    ctx->native_handler = consume_gpio_mailbox;
    ctx->platform_data = NULL;

    term reg_name_term = globalcontext_make_atom(global, gpio_atom);
    int atom_index = term_to_atom_index(reg_name_term);

    if (UNLIKELY(!globalcontext_register_process(ctx->global, atom_index, ctx->process_id))) {
        scheduler_terminate(ctx);
        AVM_LOGE(TAG, "Only a single GPIO driver can be opened.");
        return create_pair(ctx, ERROR_ATOM, USED_ATOM);
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
        return create_pair(ctx, ERROR_ATOM, NOPROC_ATOM);
    }

    ctx->platform_data = NULL;

    globalcontext_unregister_process(glb, gpio_atom_index);

    return OK_ATOM;
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

static NativeHandlerResult consume_gpio_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term req = term_get_tuple_element(msg, 2);
    term cmd_term = term_get_tuple_element(req, 0);

    int local_process_id = term_to_local_process_id(pid);

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
            AVM_LOGE(TAG, "set_int not yet supported on stm32");
            ret = create_pair(ctx, ERROR_ATOM, UNSUPPORTED_ATOM);
            break;

        case GPIORemoveIntCmd:
            AVM_LOGE(TAG, "remove_int not yet supported on stm32");
            ret = create_pair(ctx, ERROR_ATOM, UNSUPPORTED_ATOM);
            break;

        case GPIOCloseCmd:
            ret = gpiodriver_close(ctx);
            break;

        case GPIOInvalidCmd:
            char *invalid_name = interop_atom_to_string(ctx, cmd_term);
            AVM_LOGE(TAG, "Invalid command: %s", invalid_name);
            free(invalid_name);
            ret = create_pair(ctx, ERROR_ATOM, UNDEFINED_ATOM);
            break;

        default:
            char *cmd_name = interop_atom_to_string(ctx, cmd_term);
            AVM_LOGE(TAG, "Unhandled error processing command: %s", cmd_name);
            free(cmd_name);
            ret = create_pair(ctx, ERROR_ATOM, BADMATCH_ATOM);
    }

    term ret_msg;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &ret, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ret_msg = create_pair(ctx, ERROR_ATOM, OUT_OF_MEMORY_ATOM);
    } else {
        term ref = term_get_tuple_element(msg, 1);
        ret_msg = create_pair(ctx, ref, ret);
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
    return setup_gpio_pin(ctx, argv[0], argv[1]);
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    AVM_LOGW(TAG, "Pull mode must be set using `gpio:set_pin_mode/2` arg #2 i.e. {Mode,PullMode}");
    return ERROR_ATOM;
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return gpio_digital_write(ctx, argv[0], argv[1]);
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return gpio_digital_read(ctx, argv[0]);
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
