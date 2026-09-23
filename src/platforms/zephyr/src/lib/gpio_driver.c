/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/drivers/gpio.h>
#include <zephyr/kernel.h>
#include <zephyr/sys/atomic.h>

#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <port.h>
#include <portnifloader.h>
#include <term.h>

struct GPIOPin
{
    const struct device *dev;
    gpio_pin_t pin;
};

#define MAX_GPIO_INTERRUPTS 16

struct GPIOInterrupt
{
    atomic_t state;
    const struct device *dev;
    gpio_pin_t pin;
    struct gpio_callback callback;
    struct k_work work;
    GlobalContext *global;
    int32_t target_process_id;
    Heap pin_heap;
    term pin_term;
};

static struct GPIOInterrupt gpio_interrupts[MAX_GPIO_INTERRUPTS];

static term create_pair(Context *ctx, term first, term second)
{
    term pair = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(pair, 0, first);
    term_put_tuple_element(pair, 1, second);
    return pair;
}

static term make_error(Context *ctx, AtomString reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, reason));
}

static term gpio_error_to_term(Context *ctx, int err)
{
    int normalized = err < 0 ? -err : err;
    if (normalized == ENODEV) {
        return make_error(ctx, ATOM_STR("\x6", "enodev"));
    }
    if (normalized == ENOTSUP) {
        return make_error(ctx, ATOM_STR("\x7", "enotsup"));
    }
    if (normalized == EBUSY) {
        return make_error(ctx, ATOM_STR("\x4", "busy"));
    }
    return make_error(ctx, ATOM_STR("\x3", "eio"));
}

static const struct device *get_gpio_device_by_index(int index)
{
    switch (index) {
#if defined(DT_N_NODELABEL_gpio0) && DT_NODE_HAS_STATUS(DT_NODELABEL(gpio0), okay)
        case 0: return DEVICE_DT_GET(DT_NODELABEL(gpio0));
#endif
#if defined(DT_N_NODELABEL_gpio1) && DT_NODE_HAS_STATUS(DT_NODELABEL(gpio1), okay)
        case 1: return DEVICE_DT_GET(DT_NODELABEL(gpio1));
#endif
#if defined(DT_N_NODELABEL_gpio2) && DT_NODE_HAS_STATUS(DT_NODELABEL(gpio2), okay)
        case 2: return DEVICE_DT_GET(DT_NODELABEL(gpio2));
#endif
#if defined(DT_N_NODELABEL_gpio3) && DT_NODE_HAS_STATUS(DT_NODELABEL(gpio3), okay)
        case 3: return DEVICE_DT_GET(DT_NODELABEL(gpio3));
#endif
        default: return NULL;
    }
}

static const struct device *get_default_gpio_device(void)
{
#if DT_HAS_CHOSEN(atomvm_gpio)
    return DEVICE_DT_GET(DT_CHOSEN(atomvm_gpio));
#else
    return get_gpio_device_by_index(0);
#endif
}

static const struct device *get_gpio_device(term controller)
{
    if (term_is_integer(controller)) {
        avm_int_t index = term_to_int(controller);
        return index >= 0 ? get_gpio_device_by_index((int) index) : NULL;
    }
    int ok;
    char *name = interop_term_to_string(controller, &ok);
    if (!ok) {
        return NULL;
    }
    const struct device *dev = device_get_binding(name);
    free(name);
    return dev;
}

static bool get_gpio_pin(term pin_term, struct GPIOPin *out)
{
    term number_term = pin_term;
    const struct device *dev = get_default_gpio_device();
    if (term_is_tuple(pin_term) && term_get_tuple_arity(pin_term) == 2) {
        dev = get_gpio_device(term_get_tuple_element(pin_term, 0));
        number_term = term_get_tuple_element(pin_term, 1);
    }
    if (IS_NULL_PTR(dev) || !term_is_integer(number_term)) {
        return false;
    }
    avm_int_t pin = term_to_int(number_term);
    if (pin < 0 || pin > UINT8_MAX) {
        return false;
    }
    out->dev = dev;
    out->pin = (gpio_pin_t) pin;
    return true;
}

static bool get_ready_gpio_pin(term pin_term, struct GPIOPin *out)
{
    return get_gpio_pin(pin_term, out) && device_is_ready(out->dev);
}

static struct GPIOInterrupt *find_interrupt(const struct GPIOPin *gpio)
{
    for (size_t i = 0; i < MAX_GPIO_INTERRUPTS; i++) {
        struct GPIOInterrupt *interrupt = &gpio_interrupts[i];
        if (atomic_get(&interrupt->state) == 1 && interrupt->dev == gpio->dev && interrupt->pin == gpio->pin) {
            return interrupt;
        }
    }
    return NULL;
}

static void gpio_interrupt_work_handler(struct k_work *work)
{
    struct GPIOInterrupt *interrupt = CONTAINER_OF(work, struct GPIOInterrupt, work);
    if (atomic_get(&interrupt->state) != 1) {
        return;
    }
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), heap);
    term message = port_heap_create_tuple2(
        &heap,
        globalcontext_make_atom(interrupt->global, ATOM_STR("\xE", "gpio_interrupt")),
        interrupt->pin_term);
    globalcontext_send_message_from_task(interrupt->global, interrupt->target_process_id, NormalMessage, message);
    END_WITH_STACK_HEAP(heap, interrupt->global);
}

static void gpio_interrupt_callback(const struct device *dev, struct gpio_callback *callback, gpio_port_pins_t pins)
{
    UNUSED(dev);
    UNUSED(pins);
    struct GPIOInterrupt *interrupt = CONTAINER_OF(callback, struct GPIOInterrupt, callback);
    if (atomic_get(&interrupt->state) == 1) {
        k_work_submit(&interrupt->work);
    }
}

static void destroy_interrupt(struct GPIOInterrupt *interrupt)
{
    atomic_set(&interrupt->state, 2);
    gpio_pin_interrupt_configure(interrupt->dev, interrupt->pin, GPIO_INT_DISABLE);
    gpio_remove_callback(interrupt->dev, &interrupt->callback);
    struct k_work_sync sync;
    k_work_cancel_sync(&interrupt->work, &sync);
    memory_destroy_heap(&interrupt->pin_heap, interrupt->global);
    interrupt->dev = NULL;
    atomic_set(&interrupt->state, 0);
}

static bool trigger_to_flags(GlobalContext *global, term trigger, gpio_flags_t *flags)
{
    if (!term_is_atom(trigger)) {
        return false;
    }
    if (trigger == globalcontext_make_atom(global, ATOM_STR("\x6", "rising"))) {
        *flags = GPIO_INT_EDGE_RISING;
    } else if (trigger == globalcontext_make_atom(global, ATOM_STR("\x7", "falling"))) {
        *flags = GPIO_INT_EDGE_FALLING;
    } else if (trigger == globalcontext_make_atom(global, ATOM_STR("\x4", "both"))) {
        *flags = GPIO_INT_EDGE_BOTH;
    } else if (trigger == globalcontext_make_atom(global, ATOM_STR("\x3", "low"))) {
        *flags = GPIO_INT_LEVEL_LOW;
    } else if (trigger == globalcontext_make_atom(global, ATOM_STR("\x4", "high"))) {
        *flags = GPIO_INT_LEVEL_HIGH;
    } else {
        return false;
    }
    return true;
}

static void gpio_nif_destroy(GlobalContext *global)
{
    UNUSED(global);
    for (size_t i = 0; i < MAX_GPIO_INTERRUPTS; i++) {
        if (atomic_get(&gpio_interrupts[i].state) == 1) {
            destroy_interrupt(&gpio_interrupts[i]);
        }
    }
}

static term configure_pin(Context *ctx, term pin_term, gpio_flags_t flags)
{
    struct GPIOPin gpio;
    if (!get_ready_gpio_pin(pin_term, &gpio)) {
        return make_error(ctx, ATOM_STR("\x6", "enodev"));
    }
    int err = gpio_pin_configure(gpio.dev, gpio.pin, flags);
    return err == 0 ? OK_ATOM : gpio_error_to_term(ctx, err);
}

static term nif_gpio_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin gpio;
    return get_ready_gpio_pin(argv[0], &gpio) ? OK_ATOM : make_error(ctx, ATOM_STR("\x6", "enodev"));
}

static term nif_gpio_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return configure_pin(ctx, argv[0], GPIO_DISCONNECTED);
}

static term nif_gpio_set_pin_mode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_atom(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    gpio_flags_t flags;
    if (argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "input"))) {
        flags = GPIO_INPUT;
    } else if (argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "output"))) {
        flags = GPIO_OUTPUT;
    } else if (argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "output_od"))) {
        flags = GPIO_OUTPUT | GPIO_OPEN_DRAIN;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    return configure_pin(ctx, argv[0], flags);
}

static term nif_gpio_set_pin_pull(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_atom(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    gpio_flags_t flags = GPIO_INPUT;
    if (argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "up"))) {
        flags |= GPIO_PULL_UP;
    } else if (argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "down"))) {
        flags |= GPIO_PULL_DOWN;
    } else if (argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "up_down"))) {
        flags |= GPIO_PULL_UP | GPIO_PULL_DOWN;
    } else if (argv[1] != globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "floating"))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return configure_pin(ctx, argv[0], flags);
}

static term nif_gpio_digital_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin gpio;
    if (!get_ready_gpio_pin(argv[0], &gpio)) {
        return make_error(ctx, ATOM_STR("\x6", "enodev"));
    }
    int value;
    if ((term_is_integer(argv[1]) && term_to_int(argv[1]) == 0)
        || argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "low"))) {
        value = 0;
    } else if ((term_is_integer(argv[1]) && term_to_int(argv[1]) == 1)
        || argv[1] == globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "high"))) {
        value = 1;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    int err = gpio_pin_set(gpio.dev, gpio.pin, value);
    return err == 0 ? OK_ATOM : gpio_error_to_term(ctx, err);
}

static term nif_gpio_digital_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin gpio;
    if (!get_ready_gpio_pin(argv[0], &gpio)) {
        return make_error(ctx, ATOM_STR("\x6", "enodev"));
    }
    int value = gpio_pin_get(gpio.dev, gpio.pin);
    if (value < 0) {
        return gpio_error_to_term(ctx, value);
    }
    return globalcontext_make_atom(ctx->global, value == 0 ? ATOM_STR("\x3", "low") : ATOM_STR("\x4", "high"));
}

static term nif_gpio_attach_interrupt(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin gpio;
    gpio_flags_t flags;
    if (!get_ready_gpio_pin(argv[0], &gpio)
        || gpio.pin >= sizeof(gpio_port_pins_t) * 8
        || !trigger_to_flags(ctx->global, argv[1], &flags)
        || !term_is_pid(argv[2])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct GPIOInterrupt *existing = find_interrupt(&gpio);
    if (!IS_NULL_PTR(existing)) {
        destroy_interrupt(existing);
    }

    struct GPIOInterrupt *interrupt = NULL;
    for (size_t i = 0; i < MAX_GPIO_INTERRUPTS; i++) {
        if (atomic_cas(&gpio_interrupts[i].state, 0, 2)) {
            interrupt = &gpio_interrupts[i];
            break;
        }
    }
    if (IS_NULL_PTR(interrupt)) {
        return make_error(ctx, ATOM_STR("\xC", "too_many_ints"));
    }

    size_t pin_size = memory_estimate_usage(argv[0]);
    if (memory_init_heap(&interrupt->pin_heap, pin_size) != MEMORY_GC_OK) {
        atomic_set(&interrupt->state, 0);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    interrupt->pin_term = memory_copy_term_tree(&interrupt->pin_heap, argv[0]);
    interrupt->dev = gpio.dev;
    interrupt->pin = gpio.pin;
    interrupt->global = ctx->global;
    interrupt->target_process_id = term_to_local_process_id(argv[2]);
    k_work_init(&interrupt->work, gpio_interrupt_work_handler);
    gpio_init_callback(&interrupt->callback, gpio_interrupt_callback, BIT(gpio.pin));

    int err = gpio_add_callback(gpio.dev, &interrupt->callback);
    if (err == 0) {
        err = gpio_pin_interrupt_configure(gpio.dev, gpio.pin, flags);
    }
    if (err != 0) {
        gpio_remove_callback(gpio.dev, &interrupt->callback);
        memory_destroy_heap(&interrupt->pin_heap, ctx->global);
        interrupt->dev = NULL;
        atomic_set(&interrupt->state, 0);
        return gpio_error_to_term(ctx, err);
    }
    atomic_set(&interrupt->state, 1);
    return OK_ATOM;
}

static term nif_gpio_detach_interrupt(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct GPIOPin gpio;
    if (!get_gpio_pin(argv[0], &gpio)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct GPIOInterrupt *interrupt = find_interrupt(&gpio);
    if (IS_NULL_PTR(interrupt)) {
        return OK_ATOM;
    }
    destroy_interrupt(interrupt);
    return OK_ATOM;
}

static const struct Nif gpio_init_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_init };
static const struct Nif gpio_deinit_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_deinit };
static const struct Nif gpio_set_pin_mode_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_set_pin_mode };
static const struct Nif gpio_set_pin_pull_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_set_pin_pull };
static const struct Nif gpio_digital_write_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_digital_write };
static const struct Nif gpio_digital_read_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_digital_read };
static const struct Nif gpio_attach_interrupt_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_attach_interrupt };
static const struct Nif gpio_detach_interrupt_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_gpio_detach_interrupt };

static const struct Nif *gpio_nif_get_nif(const char *nifname)
{
    if (strncmp("gpio:", nifname, 5) != 0) {
        return NULL;
    }
    const char *rest = nifname + 5;
    if (strcmp("init/1", rest) == 0) return &gpio_init_nif;
    if (strcmp("deinit/1", rest) == 0) return &gpio_deinit_nif;
    if (strcmp("set_pin_mode/2", rest) == 0) return &gpio_set_pin_mode_nif;
    if (strcmp("set_pin_pull/2", rest) == 0) return &gpio_set_pin_pull_nif;
    if (strcmp("digital_write/2", rest) == 0) return &gpio_digital_write_nif;
    if (strcmp("digital_read/1", rest) == 0) return &gpio_digital_read_nif;
    if (strcmp("attach_interrupt/3", rest) == 0) return &gpio_attach_interrupt_nif;
    if (strcmp("detach_interrupt/1", rest) == 0) return &gpio_detach_interrupt_nif;
    return NULL;
}

REGISTER_NIF_COLLECTION(gpio, NULL, gpio_nif_destroy, gpio_nif_get_nif)
