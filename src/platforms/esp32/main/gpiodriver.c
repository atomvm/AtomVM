/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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

#include <string.h>

#include "driver/gpio.h"

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "module.h"
#include "utils.h"
#include "term.h"

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

static void consume_gpio_mailbox(Context *ctx);

static const char *const ok_a = "\x2ok";
static const char *const error_a = "\x5error";
static const char *const set_level_a = "\x9set_level";
static const char *const input_a = "\x5input";
static const char *const output_a = "\x6output";
static const char *const set_direction_a ="\xDset_direction";

native_handler platform_open_port(const char *driver_name)
{
    if (!strcmp(driver_name, "gpio")) {
        return consume_gpio_mailbox;
    } else {
        return NULL;
    }
}

static inline term term_from_atom_string(GlobalContext *glb, AtomString string)
{
    int global_atom_index = globalcontext_insert_atom(glb, string);
    return term_from_atom_index(global_atom_index);
}

static void consume_gpio_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;

    term ret;

    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term cmd = term_get_tuple_element(msg, 1);

    if (cmd == term_from_atom_string(glb, set_level_a)) {
        int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
        int32_t level = term_to_int32(term_get_tuple_element(msg, 3));
        gpio_set_level(gpio_num, level != 0);
        TRACE("gpio: set_level: %i %i\n", gpio_num, level != 0);
        ret = term_from_atom_string(glb, ok_a);

    } else if (cmd == term_from_atom_string(glb, set_direction_a)) {
        int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
        term direction = term_get_tuple_element(msg, 3);

        if (direction == term_from_atom_string(glb, input_a)) {
            gpio_set_direction(gpio_num, GPIO_MODE_INPUT);
            TRACE("gpio: set_direction: %i INPUT\n", gpio_num);
            ret = term_from_atom_string(glb, ok_a);

        } else if (direction == term_from_atom_string(glb, output_a)) {
            gpio_set_direction(gpio_num, GPIO_MODE_OUTPUT);
            TRACE("gpio: set_direction: %i OUTPUT\n", gpio_num);
            ret = term_from_atom_string(glb, ok_a);

        } else {
            TRACE("gpio: unrecognized direction\n");
            ret = term_from_atom_string(glb, error_a);
        }

    } else {
        TRACE("gpio: unrecognized command\n");
        ret = term_from_atom_string(glb, error_a);
    }

    free(message);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, ret);
}
