/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

#include <sdkconfig.h>
#if defined(CONFIG_AVM_ENABLE_GPIO_PORT_DRIVER) || defined(CONFIG_AVM_ENABLE_GPIO_NIFS)

#include <stdbool.h>
#include <string.h>

#include <driver/gpio.h>

#include <freertos/FreeRTOS.h>
#include <freertos/task.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "nifs.h"
#include "platform_defaultatoms.h"
#include "port.h"
#include "scheduler.h"
#include "term.h"
#include "utils.h"

#include "trace.h"

#include "esp32_sys.h"
#include "esp_log.h"
#include "sys.h"

#define TAG "gpio_driver"

static const char *const gpio_atom = "\x4" "gpio";

#ifdef CONFIG_AVM_ENABLE_GPIO_NIFS
static const struct Nif *gpio_nif_get_nif(const char *nifname);
#endif

static void gpio_driver_init(GlobalContext *global);

#ifdef CONFIG_AVM_ENABLE_GPIO_PORT_DRIVER
static NativeHandlerResult consume_gpio_mailbox(Context *ctx);
static void IRAM_ATTR gpio_isr_handler(void *arg);

static Context *gpio_driver_create_port(GlobalContext *global, term opts);
#endif

static const char *const gpio_driver_atom = "\xB" "gpio_driver";

static const AtomStringIntPair pin_mode_table[] = {
    { ATOM_STR("\x5", "input"), GPIO_MODE_INPUT },
    { ATOM_STR("\x6", "output"), GPIO_MODE_OUTPUT },
    { ATOM_STR("\x9", "output_od"), GPIO_MODE_OUTPUT_OD },
    SELECT_INT_DEFAULT(-1)
};

static const AtomStringIntPair pull_mode_table[] = {
    { ATOM_STR("\x2", "up"), GPIO_PULLUP_ONLY },
    { ATOM_STR("\x4", "down"), GPIO_PULLDOWN_ONLY },
    { ATOM_STR("\x7", "up_down"), GPIO_PULLUP_PULLDOWN },
    { ATOM_STR("\x8", "floating"), GPIO_FLOATING },
    SELECT_INT_DEFAULT(GPIO_FLOATING)
};

enum gpio_pin_level
{
    GPIOPinInvalid = -1,
    GPIOPinLow = 0,
    GPIOPinHigh = 1
};

static const AtomStringIntPair pin_level_table[] = {
    { ATOM_STR("\x3", "low"), GPIOPinLow },
    { ATOM_STR("\x4", "high"), GPIOPinHigh },
    SELECT_INT_DEFAULT(GPIOPinInvalid)
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

static const AtomStringIntPair gpio_cmd_table[] = {
    { ATOM_STR("\x9", "set_level"), GPIOSetLevelCmd },
    { ATOM_STR("\x4", "read"), GPIOReadCmd },
    { ATOM_STR("\xD", "set_direction"), GPIOSetDirectionCmd },
    { ATOM_STR("\x7", "set_int"), GPIOSetIntCmd },
    { ATOM_STR("\xA", "remove_int"), GPIORemoveIntCmd },
    { ATOM_STR("\x5", "close"), GPIOCloseCmd },
    SELECT_INT_DEFAULT(GPIOInvalidCmd)
};

static term gpio_driver;

struct GPIOListenerData
{
    struct ListHead gpio_listener_list_head;
    EventListener listener;
    int32_t target_local_pid;
    int gpio;
};

struct GPIOData
{
    struct ListHead gpio_listeners;
};

/* TODO: Change error returns to {error, Reason} (See: https://github.com/atomvm/AtomVM/issues/894) */

static inline term gpio_set_pin_mode(Context *ctx, term gpio_num_term, term mode_term)
{
    int gpio_num = term_to_int(gpio_num_term);

    gpio_mode_t mode = interop_atom_term_select_int(pin_mode_table, mode_term, ctx->global);
    if (UNLIKELY(mode < 0)) {
        return ERROR_ATOM;
    }
    esp_err_t result = gpio_set_direction(gpio_num, mode);

    if (UNLIKELY(result != ESP_OK)) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static gpio_pull_mode_t get_pull_mode(Context *ctx, term pull)
{
    return interop_atom_term_select_int(pull_mode_table, pull, ctx->global);
}

static inline term set_pin_pull_mode(Context *ctx, term gpio_num_term, term pull)
{
    avm_int_t gpio_num = term_to_int(gpio_num_term);
    gpio_pull_mode_t pull_mode = get_pull_mode(ctx, pull);
    esp_err_t result = gpio_set_pull_mode(gpio_num, pull_mode);
    if (UNLIKELY(result != ESP_OK)) {
        return ERROR_ATOM;
    }
    return OK_ATOM;
}

static inline term hold_en(term gpio_num_term)
{
    int gpio_num = term_to_int(gpio_num_term);
    esp_err_t result = gpio_hold_en(gpio_num);
    if (UNLIKELY(result != ESP_OK)) {
        return ERROR_ATOM;
    }
    return OK_ATOM;
}

static inline term hold_dis(term gpio_num_term)
{
    int gpio_num = term_to_int(gpio_num_term);
    esp_err_t result = gpio_hold_dis(gpio_num);
    if (UNLIKELY(result != ESP_OK)) {
        return ERROR_ATOM;
    }
    return OK_ATOM;
}

static inline term gpio_digital_write(Context *ctx, term gpio_num_term, term level_term)
{
    int gpio_num = term_to_int(gpio_num_term);

    int level;
    if (term_is_integer(level_term)) {
        level = term_to_int32(level_term);
        if (UNLIKELY((level != 0) && (level != 1))) {
            return ERROR_ATOM;
        }
    } else {
        level = interop_atom_term_select_int(pin_level_table, level_term, ctx->global);
        if (UNLIKELY(level < 0)) {
            return ERROR_ATOM;
        }
    }

    esp_err_t result = gpio_set_level(gpio_num, level);
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

#ifdef CONFIG_AVM_ENABLE_GPIO_PORT_DRIVER

void gpio_driver_init(GlobalContext *global)
{
    int index = globalcontext_insert_atom(global, gpio_driver_atom);
    gpio_driver = term_from_atom_index(index);
}

Context *gpio_driver_create_port(GlobalContext *global, term opts)
{
    Context *ctx = context_new(global);

    struct GPIOData *gpio_data = malloc(sizeof(struct GPIOData));
    list_init(&gpio_data->gpio_listeners);

    ctx->native_handler = consume_gpio_mailbox;
    ctx->platform_data = gpio_data;

    term reg_name_term = globalcontext_make_atom(global, gpio_atom);
    int atom_index = term_to_atom_index(reg_name_term);

    if (UNLIKELY(!globalcontext_register_process(ctx->global, atom_index, ctx->process_id))) {
        scheduler_terminate(ctx);
        ESP_LOGE(TAG, "Only a single GPIO driver can be opened.");
        return NULL;
    }

    return ctx;
}

static term gpiodriver_close(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    int gpio_atom_index = atom_table_ensure_atom(glb->atom_table, gpio_atom, AtomTableNoOpts);
    if (UNLIKELY(!globalcontext_get_registered_process(glb, gpio_atom_index))) {
        return ERROR_ATOM;
    }

    struct GPIOData *gpio_data = ctx->platform_data;

    struct ListHead *item;
    struct ListHead *tmp;
    int32_t gpio_num;
    if (!list_is_empty(&gpio_data->gpio_listeners)) {
        MUTABLE_LIST_FOR_EACH (item, tmp, &gpio_data->gpio_listeners) {
            struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
            gpio_num = gpio_listener->gpio;
            list_remove(&gpio_listener->gpio_listener_list_head);
            list_remove(&gpio_listener->listener.listeners_list_head);
            free(gpio_listener);

            gpio_set_intr_type(gpio_num, GPIO_INTR_DISABLE);
            gpio_isr_handler_remove(gpio_num);

            if (list_is_empty(&gpio_data->gpio_listeners)) {
                gpio_uninstall_isr_service();
            }
        }
    }

    globalcontext_unregister_process(glb, gpio_atom_index);
    free(gpio_data);

    return OK_ATOM;
}

EventListener *gpio_interrupt_callback(GlobalContext *glb, EventListener *listener)
{
    struct GPIOListenerData *data = GET_LIST_ENTRY(listener, struct GPIOListenerData, listener);
    int32_t listening_pid = data->target_local_pid;
    int gpio_num = data->gpio;

    // 1 header + 2 elements
    BEGIN_WITH_STACK_HEAP(1 + 2, heap);

    term int_msg = term_alloc_tuple(2, &heap);
    term_put_tuple_element(int_msg, 0, GPIO_INTERRUPT_ATOM);
    term_put_tuple_element(int_msg, 1, term_from_int32(gpio_num));

    globalcontext_send_message(glb, listening_pid, int_msg);

    END_WITH_STACK_HEAP(heap, glb);

    return listener;
}

static term gpiodriver_set_level(Context *ctx, term cmd)
{
    term gpio_num = term_get_tuple_element(cmd, 1);
    term level = term_get_tuple_element(cmd, 2);

    return gpio_digital_write(ctx, gpio_num, level);
}

static term gpiodriver_set_direction(Context *ctx, term cmd)
{
    term gpio_num = term_get_tuple_element(cmd, 1);
    term direction = term_get_tuple_element(cmd, 2);

    return gpio_set_pin_mode(ctx, gpio_num, direction);
}

static term gpiodriver_read(term cmd)
{
    term gpio_num = term_get_tuple_element(cmd, 1);
    return gpio_digital_read(gpio_num);
}

static bool gpiodriver_is_gpio_attached(struct GPIOData *gpio_data, int gpio_num)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &gpio_data->gpio_listeners) {
        struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
        if (gpio_listener->gpio == gpio_num) {
            return true;
        }
    }
    return false;
}

static term gpiodriver_set_int(Context *ctx, int32_t target_pid, term cmd)
{
    GlobalContext *glb = ctx->global;
    int32_t target_local_pid;

    struct GPIOData *gpio_data = ctx->platform_data;

    int32_t gpio_num = term_to_int32(term_get_tuple_element(cmd, 1));
    term trigger = term_get_tuple_element(cmd, 2);
    if (term_get_tuple_arity(cmd) == 4) {
        term pid = term_get_tuple_element(cmd, 3);
        if (UNLIKELY(!term_is_pid(pid) && !term_is_atom(pid))) {
            ESP_LOGE(TAG, "Invalid listener parameter, must be a pid() or registered process!");
            return ERROR_ATOM;
        }
        if (term_is_pid(pid)) {
            target_local_pid = term_to_local_process_id(pid);
        } else {
            int pid_atom_index = term_to_atom_index(pid);
            int32_t registered_process = (int32_t) globalcontext_get_registered_process(ctx->global, pid_atom_index);
            if (UNLIKELY(registered_process == 0)) {
                ESP_LOGE(TAG, "Invalid listener parameter, atom() is not a registered process name!");
                return ERROR_ATOM;
            }
            target_local_pid = registered_process;
        }
    } else {
        target_local_pid = target_pid;
    }

    if (gpiodriver_is_gpio_attached(gpio_data, gpio_num)) {
        return ERROR_ATOM;
    }

    /* TODO: GPIO specific atoms should be removed from platform_defaultatoms and constructed within this driver */
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

    if (list_is_empty(&gpio_data->gpio_listeners)) {
        gpio_install_isr_service(0);
        TRACE("installed ISR service 0.\n");
    }

    struct GPIOListenerData *data = malloc(sizeof(struct GPIOListenerData));
    if (IS_NULL_PTR(data)) {
        ESP_LOGE(TAG, "gpiodriver_set_int: Failed to ensure free heap space.");
        AVM_ABORT();
    }
    list_append(&gpio_data->gpio_listeners, &data->gpio_listener_list_head);
    data->gpio = gpio_num;
    data->target_local_pid = target_local_pid;
    sys_register_listener(glb, &data->listener);
    data->listener.sender = data;
    data->listener.handler = gpio_interrupt_callback;

    gpio_set_direction(gpio_num, GPIO_MODE_INPUT);
    gpio_set_intr_type(gpio_num, interrupt_type);

    esp_err_t ret = gpio_isr_handler_add(gpio_num, gpio_isr_handler, data);
    if (UNLIKELY(ret != ESP_OK)) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term gpiodriver_remove_int(Context *ctx, term cmd)
{
    struct GPIOData *gpio_data = ctx->platform_data;

    int32_t gpio_num = term_to_int32(term_get_tuple_element(cmd, 1));

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &gpio_data->gpio_listeners) {
        struct GPIOListenerData *gpio_listener = GET_LIST_ENTRY(item, struct GPIOListenerData, gpio_listener_list_head);
        if (gpio_listener->gpio == gpio_num) {
            list_remove(&gpio_listener->gpio_listener_list_head);
            sys_unregister_listener(ctx->global, &gpio_listener->listener);
            free(gpio_listener);

            gpio_set_intr_type(gpio_num, GPIO_INTR_DISABLE);
            gpio_isr_handler_remove(gpio_num);

            if (list_is_empty(&gpio_data->gpio_listeners)) {
                gpio_uninstall_isr_service();
            }

            return OK_ATOM;
        }
    }

    return ERROR_ATOM;
}

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static NativeHandlerResult consume_gpio_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    GenMessage gen_message;
    if (UNLIKELY(port_parse_gen_message(message->message, &gen_message) != GenCallMessage)) {
        ESP_LOGW(TAG, "Received invalid message.");
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
        return NativeContinue;
    }

    term cmd_term = term_get_tuple_element(gen_message.req, 0);

    int local_process_id = term_to_local_process_id(gen_message.pid);

    term ret;

    enum gpio_cmd cmd = interop_atom_term_select_int(gpio_cmd_table, cmd_term, ctx->global);
    switch (cmd) {
        case GPIOSetLevelCmd:
            ret = gpiodriver_set_level(ctx, gen_message.req);
            break;

        case GPIOSetDirectionCmd:
            ret = gpiodriver_set_direction(ctx, gen_message.req);
            break;

        case GPIOReadCmd:
            ret = gpiodriver_read(gen_message.req);
            break;

        case GPIOSetIntCmd:
            ret = gpiodriver_set_int(ctx, local_process_id, gen_message.req);
            break;

        case GPIORemoveIntCmd:
            ret = gpiodriver_remove_int(ctx, gen_message.req);
            break;

        case GPIOCloseCmd:
            ret = gpiodriver_close(ctx);
            break;

        default:
            ESP_LOGW(TAG, "Unrecognized command");
            ret = ERROR_ATOM;
    }

    term ret_msg;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &ret, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ret_msg = OUT_OF_MEMORY_ATOM;
    } else {
        ret_msg = create_pair(ctx, gen_message.ref, ret);
    }

    globalcontext_send_message(ctx->global, local_process_id, ret_msg);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return cmd == GPIOCloseCmd ? NativeTerminate : NativeContinue;
}

static void IRAM_ATTR gpio_isr_handler(void *arg)
{
    xQueueSendFromISR(event_queue, &arg, NULL);
}

REGISTER_PORT_DRIVER(gpio, gpio_driver_init, NULL, gpio_driver_create_port)

#endif

//
// Nif implementation
//

#ifdef CONFIG_AVM_ENABLE_GPIO_NIFS

/* TODO: in the case of {error, Return} we should RAISE_ERROR(Reason) */

static term nif_gpio_set_pin_mode(Context *ctx, int argc, term argv[])
{
    return gpio_set_pin_mode(ctx, argv[0], argv[1]);
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    return set_pin_pull_mode(ctx, argv[0], argv[1]);
}

static term nif_gpio_hold_en(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    return hold_en(argv[0]);
}

static term nif_gpio_hold_dis(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    return hold_dis(argv[0]);
}

#if !SOC_GPIO_SUPPORT_HOLD_SINGLE_IO_IN_DSLP
static term nif_gpio_deep_sleep_hold_en(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    gpio_deep_sleep_hold_en();
    return OK_ATOM;
}

static term nif_gpio_deep_sleep_hold_dis(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    gpio_deep_sleep_hold_dis();
    return OK_ATOM;
}
#endif

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    return gpio_digital_write(ctx, argv[0], argv[1]);
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

static const struct Nif gpio_set_pin_pull_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_set_pin_pull
};

static const struct Nif gpio_hold_en_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_hold_en
};

static const struct Nif gpio_hold_dis_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_hold_dis
};

#if !SOC_GPIO_SUPPORT_HOLD_SINGLE_IO_IN_DSLP
static const struct Nif gpio_deep_sleep_hold_en_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_deep_sleep_hold_en
};

static const struct Nif gpio_deep_sleep_hold_dis_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_deep_sleep_hold_dis
};
#endif

static const struct Nif gpio_digital_write_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_digital_write
};

static const struct Nif gpio_digital_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_digital_read
};

const struct Nif *gpio_nif_get_nif(const char *nifname)
{
    if (strcmp("gpio:set_pin_mode/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_set_pin_mode_nif;
    }
    if (strcmp("Elixir.GPIO:set_pin_mode/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_set_pin_mode_nif;
    }

    if (strcmp("gpio:set_pin_pull/2", nifname) == 0 || strcmp("Elixir.GPIO:set_pin_pull/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_set_pin_pull_nif;
    }

    if (strcmp("gpio:hold_en/1", nifname) == 0 || strcmp("Elixir.GPIO:hold_en/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_hold_en_nif;
    }

    if (strcmp("gpio:hold_dis/1", nifname) == 0 || strcmp("Elixir.GPIO:hold_dis/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_hold_dis_nif;
    }

#if !SOC_GPIO_SUPPORT_HOLD_SINGLE_IO_IN_DSLP
    if (strcmp("gpio:deep_sleep_hold_en/0", nifname) == 0 || strcmp("Elixir.GPIO:deep_sleep_hold_en/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_deep_sleep_hold_en_nif;
    }

    if (strcmp("gpio:deep_sleep_hold_dis/0", nifname) == 0 || strcmp("Elixir.GPIO:deep_sleep_hold_dis/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_deep_sleep_hold_dis_nif;
    }
#endif

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

REGISTER_NIF_COLLECTION(gpio, NULL, NULL, gpio_nif_get_nif)
#endif

#endif
