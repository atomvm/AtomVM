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

#include "gpio_driver.h"

#include <string.h>

#include <driver/gpio.h>

#include <freertos/FreeRTOS.h>
#include <freertos/task.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "platform_defaultatoms.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "module.h"
#include "nifs.h"
#include "utils.h"
#include "term.h"

#include "trace.h"

#include "sys.h"
#include "esp32_sys.h"

static const char *const gpio_atom = "\x4" "gpio";

static Context *global_gpio_ctx = NULL;

static void consume_gpio_mailbox(Context *ctx);
static void IRAM_ATTR gpio_isr_handler(void *arg);

struct GPIOListenerData
{
    Context *target_context;
    int gpio;
};

static inline term gpio_set_pin_mode(term gpio_num_term, term mode)
{
    int gpio_num = term_to_int(gpio_num_term);

    esp_err_t result;
    switch (mode) {
        case INPUT_ATOM:
            result = gpio_set_direction(gpio_num, GPIO_MODE_INPUT);
            break;

        case OUTPUT_ATOM:
            result = gpio_set_direction(gpio_num, GPIO_MODE_OUTPUT);
            break;

        default:
            return ERROR_ATOM;
    }

    if (UNLIKELY(result != ESP_OK)) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static inline term gpio_digital_write(term gpio_num_term, term level_term)
{
    int gpio_num = term_to_int(gpio_num_term);

    esp_err_t result;
    if ((level_term == LOW_ATOM) || (level_term == term_from_int(0))) {
        result = gpio_set_level(gpio_num, 0);

    } else if ((level_term == HIGH_ATOM) || (level_term == term_from_int(1))) {
        result = gpio_set_level(gpio_num, 1);

    } else {
        return ERROR_ATOM;
    }

    if (UNLIKELY(result != ESP_OK)) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static inline term gpio_digital_read(term gpio_num_term)
{
    avm_int_t gpio_num = term_to_int(gpio_num_term);
    avm_int_t level = gpio_get_level(gpio_num);

    return level ? HIGH_ATOM : LOW_ATOM;
}

void gpiodriver_init(Context *ctx)
{
    if (LIKELY(!global_gpio_ctx)) {
        global_gpio_ctx = ctx;

        ctx->native_handler = consume_gpio_mailbox;
        ctx->platform_data = NULL;

        term reg_name_term = context_make_atom(ctx, gpio_atom);
        int atom_index = term_to_atom_index(reg_name_term);

        globalcontext_register_process(ctx->global, atom_index, ctx->process_id);
    } else {
        fprintf(stderr, "Only a single GPIO driver can be opened.\n");
        abort();
    }
}

void gpio_interrupt_callback(EventListener *listener)
{
    struct GPIOListenerData *data = listener->data;
    Context *listening_ctx = data->target_context;
    int gpio_num = data->gpio;

    // 1 header + 2 elements
    if (UNLIKELY(memory_ensure_free(global_gpio_ctx, 3) != MEMORY_GC_OK)) {
        //TODO: it must not fail
        abort();
    }

    term int_msg = term_alloc_tuple(2, global_gpio_ctx);
    term_put_tuple_element(int_msg, 0, GPIO_INTERRUPT_ATOM);
    term_put_tuple_element(int_msg, 1, term_from_int32(gpio_num));

    mailbox_send(listening_ctx, int_msg);
}

static term gpiodriver_set_level(term msg)
{
    int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
    int32_t level = term_to_int32(term_get_tuple_element(msg, 3));
    gpio_set_level(gpio_num, level != 0);
    TRACE("gpio: set_level: %i %i\n", gpio_num, level != 0);

    return OK_ATOM;
}

static term gpiodriver_set_direction(term msg)
{
    int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
    term direction = term_get_tuple_element(msg, 3);

    if (direction == INPUT_ATOM) {
        gpio_set_direction(gpio_num, GPIO_MODE_INPUT);
        TRACE("gpio: set_direction: %i INPUT\n", gpio_num);
        return OK_ATOM;

    } else if (direction == OUTPUT_ATOM) {
        gpio_set_direction(gpio_num, GPIO_MODE_OUTPUT);
        TRACE("gpio: set_direction: %i OUTPUT\n", gpio_num);
        return OK_ATOM;

    } else {
        TRACE("gpio: unrecognized direction\n");
        return ERROR_ATOM;
    }
}

static term gpiodriver_read(term msg)
{
    int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
    int level = gpio_get_level(gpio_num);
    return term_from_int11(level);
}

static term gpiodriver_set_int(Context *ctx, Context *target, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
    term trigger = term_get_tuple_element(msg, 3);

    gpio_int_type_t interrupt_type;
    switch (trigger) {
        case NONE_ATOM:
            interrupt_type = GPIO_INTR_DISABLE;
            break;

        case RISING_ATOM:
            interrupt_type = GPIO_INTR_POSEDGE;
            break;

        case FALLING_ATOM:
            interrupt_type = GPIO_INTR_NEGEDGE;
            break;

        case BOTH_ATOM:
            interrupt_type = GPIO_INTR_ANYEDGE;
            break;

        case LOW_ATOM:
            interrupt_type = GPIO_INTR_LOW_LEVEL;
            break;

        case HIGH_ATOM:
            interrupt_type = GPIO_INTR_HIGH_LEVEL;
            break;

        default:
            return ERROR_ATOM;
    }

    TRACE("going to install interrupt for %i.\n", gpio_num);

    //TODO: ugly workaround here, write a real implementation
    gpio_install_isr_service(0);
    TRACE("installed ISR service 0.\n");

    struct GPIOListenerData *data = malloc(sizeof(struct GPIOListenerData));
    if (IS_NULL_PTR(data)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    data->gpio = gpio_num;
    data->target_context = target;

    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    list_append(&platform->listeners, &listener->listeners_list_head);
    listener->sender = data;
    listener->data = data;
    listener->handler = gpio_interrupt_callback;

    gpio_set_direction(gpio_num, GPIO_MODE_INPUT);
    gpio_set_intr_type(gpio_num, interrupt_type);

    gpio_isr_handler_add(gpio_num, gpio_isr_handler, data);

    return OK_ATOM;
}

static void consume_gpio_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term cmd = term_get_tuple_element(msg, 1);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    term ret;

    switch (cmd) {
        case SET_LEVEL_ATOM:
            ret = gpiodriver_set_level(msg);
            break;

        case SET_DIRECTION_ATOM:
            ret = gpiodriver_set_direction(msg);
            break;

        case READ_ATOM:
            ret = gpiodriver_read(msg);
            break;

        case SET_INT_ATOM:
            ret = gpiodriver_set_int(ctx, target, msg);
            break;

        default:
            TRACE("gpio: unrecognized command\n");
            ret = ERROR_ATOM;
    }

    free(message);

    mailbox_send(target, ret);
}

static void IRAM_ATTR gpio_isr_handler(void *arg)
{
    xQueueSendFromISR(event_queue, &arg, NULL);
}

static term nif_gpio_set_pin_mode(Context *ctx, int argc, term argv[])
{
    return gpio_set_pin_mode(argv[0], argv[1]);
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    return gpio_digital_write(argv[0], argv[1]);
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    return gpio_digital_read(argv[0]);
}

static const struct Nif gpio_set_pin_mode_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_set_pin_mode
};

static const struct Nif gpio_digital_write_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_digital_write
};

static const struct Nif gpio_digital_read_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_digital_read
};

const struct Nif *gpio_nifs_get_nif(const char *nifname)
{
    if (strcmp("gpio:set_pin_mode/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_set_pin_mode_nif;
    }
    if (strcmp("Elixir.GPIO:set_pin_mode/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_set_pin_mode_nif;
    }

    if (strcmp("gpio:digital_write/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_digital_write_nif;
    }
    if (strcmp("Elixir.GPIO:digital_write/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_digital_write_nif;
    }

    if (strcmp("gpio:digital_read/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_digital_read_nif;
    }
    if (strcmp("Elixir.GPIO:digital_read/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_digital_read_nif;
    }

    return NULL;
}
