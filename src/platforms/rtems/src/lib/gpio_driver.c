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

#ifdef RTEMS_HAS_IMX_GPIO
#include <bsp/fdt.h>
#include <bsp/imx-gpio.h>
#include <libfdt.h>
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
#ifdef RTEMS_HAS_IMX_GPIO
static const char *const high_atom = ATOM_STR("\x4", "high");
static const char *const low_atom = ATOM_STR("\x3", "low");
static const char *const input_atom = ATOM_STR("\x5", "input");
static const char *const output_atom = ATOM_STR("\x6", "output");
static const char *const output_od_atom = ATOM_STR("\x9", "output_od");
static const char *const path_atom = ATOM_STR("\x4", "path");
static const char *const property_atom = ATOM_STR("\x8", "property");
static const char *const index_atom = ATOM_STR("\x5", "index");
#endif

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

#ifndef RTEMS_HAS_IMX_GPIO
static term nif_gpio_unsupported(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return error_atom(ctx, enotsup_atom);
}
#else

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

static term nif_gpio_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, argv[0], IMX_GPIO_MODE_INPUT, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    imx_gpio_init(&pin);
    free(owned_prop);
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
    if (globalcontext_is_term_equal_to_atom_string(ctx->global, argv[1], output_od_atom)) {
        return error_atom(ctx, enotsup_atom);
    }
    enum imx_gpio_mode mode;
    if (!parse_direction(ctx->global, argv[1], &mode)) {
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
    return OK_ATOM;
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return error_atom(ctx, enotsup_atom);
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    uint32_t value;
    if (!parse_level(ctx->global, argv[1], &value)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct imx_gpio_pin pin;
    char *owned_prop;
    if (!fill_pin(ctx, argv[0], IMX_GPIO_MODE_OUTPUT, &pin, &owned_prop)) {
        free(owned_prop);
        return error_atom(ctx, ATOM_STR("\xB", "invalid_pin"));
    }
    imx_gpio_init(&pin);
    uint32_t output = pin.is_active_low ? !value : value;
    imx_gpio_set_output(&pin, output);
    free(owned_prop);
    return OK_ATOM;
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
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

#endif /* RTEMS_HAS_IMX_GPIO */

static const struct Nif *gpio_nif_get_nif(const char *nifname)
{
#ifndef RTEMS_HAS_IMX_GPIO
    static const struct Nif unsupported = {
        .base.type = NIFFunctionType,
        .nif_ptr = nif_gpio_unsupported
    };
    if (strcmp("gpio:init/1", nifname) == 0
        || strcmp("gpio:deinit/1", nifname) == 0
        || strcmp("gpio:set_pin_mode/2", nifname) == 0
        || strcmp("gpio:set_pin_pull/2", nifname) == 0
        || strcmp("gpio:digital_write/2", nifname) == 0
        || strcmp("gpio:digital_read/1", nifname) == 0) {
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
    return NULL;
#endif
}

REGISTER_NIF_COLLECTION(gpio, NULL, NULL, gpio_nif_get_nif)
