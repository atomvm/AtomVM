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

#include "gpiodriver.h"

#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/rcc.h>

#include <context.h>
#include <mailbox.h>
#include <term.h>

#include <trace.h>

#include "platform_defaultatoms.h"

static NativeHandlerResult consume_gpio_mailbox(Context *ctx);
static uint32_t port_atom_to_gpio_port(Context *ctx, term port_atom);
static uint16_t gpio_port_to_rcc_port(uint32_t gpio_port);
static char gpio_port_to_name(uint32_t gpio_port);

void gpiodriver_init(Context *ctx)
{
    ctx->native_handler = consume_gpio_mailbox;
    ctx->platform_data = NULL;
}

static NativeHandlerResult consume_gpio_mailbox(Context *ctx)
{
    term ret;

    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term cmd = term_get_tuple_element(msg, 1);

    int local_process_id = term_to_local_process_id(pid);

    if (cmd == SET_LEVEL_ATOM) {
        term gpio_tuple = term_get_tuple_element(msg, 2);
        term gpio_port_atom = term_get_tuple_element(gpio_tuple, 0);
        uint32_t gpio_port = port_atom_to_gpio_port(ctx, gpio_port_atom);
        int32_t gpio_pin_num = term_to_int32(term_get_tuple_element(gpio_tuple, 1));
        int32_t level = term_to_int32(term_get_tuple_element(msg, 3));

        if (level != 0) {
            gpio_set(gpio_port, 1 << gpio_pin_num);
        } else {
            gpio_clear(gpio_port, 1 << gpio_pin_num);
        }
        TRACE("gpio: set_level: %c%i %i\n", gpio_port_to_name(gpio_port), gpio_pin_num, level != 0);
        ret = OK_ATOM;

    } else if (cmd == SET_DIRECTION_ATOM) {
        term gpio_tuple = term_get_tuple_element(msg, 2);
        term gpio_port_atom = term_get_tuple_element(gpio_tuple, 0);
        uint32_t gpio_port = port_atom_to_gpio_port(ctx, gpio_port_atom);
        int32_t gpio_pin_num = term_to_int32(term_get_tuple_element(gpio_tuple, 1));
        term direction = term_get_tuple_element(msg, 3);

        uint16_t rcc_port = gpio_port_to_rcc_port(gpio_port);
        // Set direction implicitly enables the port of the GPIO
        rcc_periph_clock_enable(rcc_port);

        if (direction == INPUT_ATOM) {
            gpio_mode_setup(gpio_port, GPIO_MODE_INPUT, GPIO_PUPD_NONE, 1 << gpio_pin_num);
            TRACE("gpio: set_direction: %c%i INPUT\n", gpio_port_to_name(gpio_port), gpio_pin_num);
            ret = OK_ATOM;

        } else if (direction == OUTPUT_ATOM) {
            gpio_mode_setup(gpio_port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, 1 << gpio_pin_num);
            TRACE("gpio: set_direction: %c%i OUTPUT\n", gpio_port_to_name(gpio_port), gpio_pin_num);
            ret = OK_ATOM;

        } else {
            TRACE("gpio: unrecognized direction\n");
            ret = ERROR_ATOM;
        }

    } else {
        TRACE("gpio: unrecognized command\n");
        ret = ERROR_ATOM;
    }

    globalcontext_send_message(ctx->global, local_process_id, ret);

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return NativeContinue;
}

static uint32_t port_atom_to_gpio_port(Context *ctx, term port_atom)
{
    if (port_atom == A_ATOM) {
        return GPIOA;
    } else if (port_atom == B_ATOM) {
        return GPIOB;
    } else if (port_atom == C_ATOM) {
        return GPIOC;
    } else if (port_atom == D_ATOM) {
        return GPIOD;
    } else if (port_atom == E_ATOM) {
        return GPIOE;
    } else if (port_atom == F_ATOM) {
        return GPIOF;
    } else {
        return 0;
    }
}

static uint16_t gpio_port_to_rcc_port(uint32_t gpio_port)
{
    switch (gpio_port) {
        case GPIOA:
            return RCC_GPIOA;
        case GPIOB:
            return RCC_GPIOB;
        case GPIOC:
            return RCC_GPIOC;
        case GPIOD:
            return RCC_GPIOD;
        case GPIOE:
            return RCC_GPIOE;
        case GPIOF:
            return RCC_GPIOF;
        default:
            return 0;
    }
}

static char gpio_port_to_name(uint32_t gpio_port)
{
    switch (gpio_port) {
        case GPIOA:
            return 'A';
        case GPIOB:
            return 'B';
        case GPIOC:
            return 'C';
        case GPIOD:
            return 'D';
        case GPIOE:
            return 'E';
        case GPIOF:
            return 'F';
        default:
            return 0;
    }
}
