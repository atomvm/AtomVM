/***************************************************************************
 *   Copyright 2018 by Riccardo Binetti <rbino@gmx.com>                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "gpiodriver.h"

#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/rcc.h>

#include <context.h>
#include <mailbox.h>
#include <term.h>

#include <trace.h>

static void consume_gpio_mailbox(Context *ctx);
static uint32_t port_atom_to_gpio_port(Context *ctx, term port_atom);
static uint16_t gpio_port_to_rcc_port(uint32_t gpio_port);
static char gpio_port_to_name(uint32_t gpio_port);

static const char *const ok_a = "\x2" "ok";
static const char *const error_a = "\x5" "error";
static const char *const set_level_a = "\x9" "set_level";
static const char *const input_a = "\x5" "input";
static const char *const output_a = "\x6" "output";
static const char *const set_direction_a ="\xD" "set_direction";
static const char *const a_a = "\x01" "a";
static const char *const b_a = "\x01" "b";
static const char *const c_a = "\x01" "c";
static const char *const d_a = "\x01" "d";
static const char *const e_a = "\x01" "e";
static const char *const f_a = "\x01" "f";


void gpiodriver_init(Context *ctx)
{
    ctx->native_handler = consume_gpio_mailbox;
    ctx->platform_data = NULL;
}

static void consume_gpio_mailbox(Context *ctx)
{
    term ret;

    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term cmd = term_get_tuple_element(msg, 1);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    if (cmd == context_make_atom(ctx, set_level_a)) {
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
        ret = context_make_atom(ctx, ok_a);

    } else if (cmd == context_make_atom(ctx, set_direction_a)) {
        term gpio_tuple = term_get_tuple_element(msg, 2);
        term gpio_port_atom = term_get_tuple_element(gpio_tuple, 0);
        uint32_t gpio_port = port_atom_to_gpio_port(ctx, gpio_port_atom);
        int32_t gpio_pin_num = term_to_int32(term_get_tuple_element(gpio_tuple, 1));
        term direction = term_get_tuple_element(msg, 3);

        uint16_t rcc_port = gpio_port_to_rcc_port(gpio_port);
        // Set direction implicitly enables the port of the GPIO
        rcc_periph_clock_enable(rcc_port);

        if (direction == context_make_atom(ctx, input_a)) {
            gpio_mode_setup(gpio_port, GPIO_MODE_INPUT, GPIO_PUPD_NONE, 1 << gpio_pin_num);
            TRACE("gpio: set_direction: %c%i INPUT\n", gpio_port_to_name(gpio_port), gpio_pin_num);
            ret = context_make_atom(ctx, ok_a);

        } else if (direction == context_make_atom(ctx, output_a)) {
            gpio_mode_setup(gpio_port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, 1 << gpio_pin_num);
            TRACE("gpio: set_direction: %c%i OUTPUT\n", gpio_port_to_name(gpio_port), gpio_pin_num);
            ret = context_make_atom(ctx, ok_a);

        } else {
            TRACE("gpio: unrecognized direction\n");
            ret = context_make_atom(ctx, error_a);
        }

    } else {
        TRACE("gpio: unrecognized command\n");
        ret = context_make_atom(ctx, error_a);
    }

    free(message);

    mailbox_send(target, ret);
}

static uint32_t port_atom_to_gpio_port(Context *ctx, term port_atom)
{
    if (port_atom == context_make_atom(ctx, a_a)) {
        return GPIOA;
    } else if (port_atom == context_make_atom(ctx, b_a)) {
        return GPIOB;
    } else if (port_atom == context_make_atom(ctx, c_a)) {
        return GPIOC;
    } else if (port_atom == context_make_atom(ctx, d_a)) {
        return GPIOD;
    } else if (port_atom == context_make_atom(ctx, e_a)) {
        return GPIOE;
    } else if (port_atom == context_make_atom(ctx, f_a)) {
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
