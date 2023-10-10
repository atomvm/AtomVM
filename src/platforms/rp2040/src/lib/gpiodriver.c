/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#include "gpiodriver.h"

#include <stdbool.h>
#include <string.h>

#include <hardware/gpio.h>

#include "defaultatoms.h"
#include "interop.h"
#include "rp2040_sys.h"
#include "trace.h"

#define WL_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "wl"))

static const struct Nif *gpio_nif_get_nif(const char *nifname);

static const AtomStringIntPair pin_mode_table[] = {
    { ATOM_STR("\x5", "input"), GPIO_IN },
    { ATOM_STR("\x6", "output"), GPIO_OUT },
    SELECT_INT_DEFAULT(-1)
};

enum
{
    AtomVMRP2040GPIOFloating = 0,
    AtomVMRP2040GPIOPullUp = 1,
    AtomVMRP2040GPIOPullDown = 2,
};

static const AtomStringIntPair pull_mode_table[] = {
    { ATOM_STR("\x2", "up"), AtomVMRP2040GPIOPullUp },
    { ATOM_STR("\x4", "down"), AtomVMRP2040GPIOPullDown },
    { ATOM_STR("\x7", "up_down"), AtomVMRP2040GPIOPullUp | AtomVMRP2040GPIOPullDown },
    { ATOM_STR("\x8", "floating"), AtomVMRP2040GPIOFloating },
    SELECT_INT_DEFAULT(AtomVMRP2040GPIOFloating)
};

enum gpio_pin_level
{
    AtomVMRP2040GPIOInvalid = -1,
    AtomVMRP2040GPIOLow = 0,
    AtomVMRP2040GPIOHigh = 1
};

static const AtomStringIntPair pin_level_table[] = {
    { ATOM_STR("\x3", "low"), AtomVMRP2040GPIOLow },
    { ATOM_STR("\x4", "high"), AtomVMRP2040GPIOHigh },
    SELECT_INT_DEFAULT(AtomVMRP2040GPIOInvalid)
};

static term nif_gpio_init(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    uint pin = term_to_int(argv[0]);
    if (UNLIKELY(pin >= NUM_BANK0_GPIOS)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    gpio_init(pin);
    return OK_ATOM;
}

static term nif_gpio_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    uint gpio_num = term_to_int(argv[0]);
    if (UNLIKELY(gpio_num >= NUM_BANK0_GPIOS)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    gpio_deinit(gpio_num);
    return OK_ATOM;
}

static term nif_gpio_set_pin_mode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    uint gpio_num = term_to_int(argv[0]);
    if (UNLIKELY(gpio_num >= NUM_BANK0_GPIOS)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int mode = interop_atom_term_select_int(pin_mode_table, argv[1], ctx->global);
    if (UNLIKELY(mode < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    gpio_set_dir(gpio_num, mode);
    return OK_ATOM;
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    uint gpio_num = term_to_int(argv[0]);
    if (UNLIKELY(gpio_num >= NUM_BANK0_GPIOS)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int pull_mode = interop_atom_term_select_int(pull_mode_table, argv[1], ctx->global);
    gpio_set_pulls(gpio_num, pull_mode & AtomVMRP2040GPIOPullUp, pull_mode & AtomVMRP2040GPIOPullDown);
    return OK_ATOM;
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term gpio_pin = argv[0];
    term level_term = argv[1];

    int level;
    if (term_is_integer(level_term)) {
        level = term_to_int32(level_term);
        if (UNLIKELY((level != 0) && (level != 1))) {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        level = interop_atom_term_select_int(pin_level_table, level_term, ctx->global);
        if (UNLIKELY(level < 0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    uint gpio_num;
    if (term_is_integer(gpio_pin)) {
        gpio_num = (uint) term_to_int32(gpio_pin);
        if (UNLIKELY(gpio_num >= NUM_BANK0_GPIOS)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        gpio_put(gpio_num, level);
#ifdef LIB_PICO_CYW43_ARCH
    } else if (term_is_tuple(gpio_pin)) {
        term gpio_bank_atom = term_get_tuple_element(gpio_pin, 0);
        VALIDATE_VALUE(gpio_bank_atom, term_is_atom);
        if (UNLIKELY(gpio_bank_atom != WL_ATOM)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        term pin_term = term_get_tuple_element(gpio_pin, 1);
        VALIDATE_VALUE(pin_term, term_is_integer);
        gpio_num = (uint) term_to_int32(pin_term);
        if (UNLIKELY((gpio_num == -1) || (gpio_num > 1))) {
            RAISE_ERROR(BADARG_ATOM);
        }
        cyw43_arch_gpio_put(gpio_num, level);
#endif
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term gpio_pin = argv[0];
    uint gpio_num;
    bool level;

    if (term_is_integer(gpio_pin)) {
        gpio_num = (uint) term_to_int32(gpio_pin);
        if (UNLIKELY(gpio_num >= NUM_BANK0_GPIOS)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        level = gpio_get(gpio_num);
#ifdef LIB_PICO_CYW43_ARCH
    } else if (term_is_tuple(gpio_pin)) {
        term gpio_bank_atom = term_get_tuple_element(gpio_pin, 0);
        VALIDATE_VALUE(gpio_bank_atom, term_is_atom);
        if (UNLIKELY(gpio_bank_atom != WL_ATOM)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        term pin_term = term_get_tuple_element(gpio_pin, 1);
        VALIDATE_VALUE(pin_term, term_is_integer);
        gpio_num = (uint) term_to_int32(pin_term);
        if (UNLIKELY(gpio_num != 2)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        level = cyw43_arch_gpio_get(gpio_num);
#endif
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    return level ? globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "high")) : globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "low"));
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
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_init_nif;
    }
    if (strcmp("gpio:deinit/1", nifname) == 0 || strcmp("Elixir.GPIO:deinit/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_deinit_nif;
    }
    if (strcmp("gpio:set_pin_mode/2", nifname) == 0 || strcmp("Elixir.GPIO:set_pin_mode/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_set_pin_mode_nif;
    }
    if (strcmp("gpio:set_pin_pull/2", nifname) == 0 || strcmp("Elixir.GPIO:set_pin_pull/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_set_pin_pull_nif;
    }
    if (strcmp("gpio:digital_write/2", nifname) == 0 || strcmp("Elixir.GPIO:digital_write/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_digital_write_nif;
    }
    if (strcmp("gpio:digital_read/1", nifname) == 0 || strcmp("Elixir.GPIO:digital_read/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_digital_read_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(gpio, NULL, NULL, gpio_nif_get_nif)
