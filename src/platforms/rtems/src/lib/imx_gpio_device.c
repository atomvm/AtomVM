/*
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include "imx_gpio_device.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <bsp.h>
#include <bsp/fdt.h>
#include <dev/gpio/gpio.h>
#include <libfdt.h>
#include <rtems/irq-extension.h>

/* GPIO register layout is shared by i.MX6UL/ULL and i.MX7D. The opaque BSP
 * struct imx_gpio is NOT this register block, and is deliberately not used. */
struct GPIORegisters
{
    uint32_t dr;
    uint32_t gdir;
    uint32_t psr;
    uint32_t icr[2];
    uint32_t imr;
    uint32_t isr;
    uint32_t edge_sel;
};

struct GPIOBank
{
    volatile struct GPIORegisters *regs;
    int node;
    unsigned index;
    rtems_vector_number irq[2];
    bool installed[2];
    uint32_t enabled;
    uint32_t latch;
    uint32_t tracked;
};

struct GPIOPin
{
    rtems_gpio_config config;
    rtems_gpio_irq_handler handler;
    void *arg;
    volatile uint32_t *pad;
    volatile uint32_t *mux;
    uint32_t bias_mask;
    uint32_t pull_up;
    uint32_t pull_down;
    uint32_t od_mask;
    uint32_t saved_pad;
    uint32_t saved_dr;
    uint32_t saved_gdir;
    uint32_t saved_icr;
    uint32_t saved_edge;
};

static struct GPIOBank banks[7];
static struct GPIOPin pins[IMX_GPIO_DEVICE_PIN_COUNT];
static uint32_t active_low[7];
static uint32_t scratch[7];
static bool initialized;
static bool registered;
static rtems_mutex initialize_mutex = RTEMS_MUTEX_INITIALIZER("imx gpio init");
/* Lock order: generic controller mutex, then this interrupt lock. The ISR
 * holds this lock THROUGH the callback: disable/release cannot return with
 * a callback in flight. Callbacks must not call back into the GPIO API. */
RTEMS_INTERRUPT_LOCK_DEFINE(static, device_lock, "imx gpio")

static bool node_enabled(const void *fdt, int node)
{
    int len;
    const char *status = fdt_getprop(fdt, node, "status", &len);
    return status == NULL || (len == 5 && memcmp(status, "okay", 5) == 0)
        || (len == 3 && memcmp(status, "ok", 3) == 0);
}

/* The supported BSPs use identity-mapped 32-bit simple buses. Refuse other
 * address formats instead of truncating an address or guessing a base. */
static bool register_region(const void *fdt, int node, uintptr_t *base, uint32_t *size)
{
    int parent = fdt_parent_offset(fdt, node);
    int len;
    if (parent < 0 || fdt_address_cells(fdt, parent) != 1
        || fdt_size_cells(fdt, parent) != 1) {
        return false;
    }
    const fdt32_t *reg = fdt_getprop(fdt, node, "reg", &len);
    if (reg == NULL || len < 8) {
        return false;
    }
    *base = fdt32_to_cpu(reg[0]);
    *size = fdt32_to_cpu(reg[1]);
    return *base != 0 && (*base & 3) == 0 && *size >= 4
        && *base <= UINTPTR_MAX - *size;
}

static void gpio_isr(void *arg)
{
    struct GPIOBank *bank = arg;
    rtems_interrupt_lock_context context;
    rtems_interrupt_lock_acquire(&device_lock, &context);
    uint32_t pending = bank->regs->isr & bank->regs->imr & bank->enabled;
    if (pending != 0) {
        /* W1C only our bits, never another peripheral driver's pending IRQ. */
        bank->regs->isr = pending;
    }
    while (pending != 0) {
        unsigned bit = (unsigned) __builtin_ctz(pending);
        pending &= ~(UINT32_C(1) << bit);
        uint32_t id = bank->index * 32 + bit;
        if (pins[id].handler != NULL) {
            pins[id].handler(id, pins[id].arg);
        }
    }
    rtems_interrupt_lock_release(&device_lock, &context);
}

static int pin_info(rtems_gpio_drv_ctrl *ctrl, uint32_t id, rtems_gpio_pin_info *info)
{
    (void) ctrl;
    struct GPIOBank *bank = &banks[id / 32];
    struct GPIOPin *pin = &pins[id];
    info->kind = RTEMS_GPIO_PIN_PHYSICAL;
    info->flags = 0;
    info->capabilities = 0;
    info->name[0] = '\0';
    info->owner[0] = '\0';
    if (bank->regs == NULL) {
        info->flags = RTEMS_GPIO_PIN_NO_PAD;
        return 0;
    }
    info->capabilities = RTEMS_GPIO_CAP_INPUT | RTEMS_GPIO_CAP_OUTPUT
        | RTEMS_GPIO_CAP_OPEN_DRAIN;
    if (bank->irq[(id % 32) / 16] != UINT32_MAX) {
        info->capabilities |= RTEMS_GPIO_CAP_EDGE_RISING | RTEMS_GPIO_CAP_EDGE_FALLING
            | RTEMS_GPIO_CAP_EDGE_BOTH | RTEMS_GPIO_CAP_LEVEL_HIGH | RTEMS_GPIO_CAP_LEVEL_LOW;
    }
    if (pin->pad != NULL) {
        info->capabilities |= RTEMS_GPIO_CAP_PULL_UP | RTEMS_GPIO_CAP_PULL_DOWN;
    }
    if (pin->config.direction != RTEMS_GPIO_DIRECTION_NONE) {
        info->flags |= RTEMS_GPIO_PIN_IN_USE;
    }
    snprintf(info->name, sizeof(info->name), "GPIO%u_IO%u", id / 32 + 1, id % 32);
    return 0;
}

/* DR reads may return pad levels for GDIR=0 (as QEMU models), not the hidden
 * output latch. Overlay every controller-owned latch before any bank RMW so
 * writing one pin cannot discard another input's preload or software-OD high.
 * Unowned bits use the register read: pre-existing hidden input latches cannot
 * be recovered through this register interface without changing direction. */
static void write_latch(struct GPIOBank *bank, uint32_t mask, uint32_t value)
{
    uint32_t dr = (bank->regs->dr & ~bank->tracked) | (bank->latch & bank->tracked);
    bank->latch = (bank->latch & ~mask) | (value & mask);
    bank->regs->dr = (dr & ~mask) | (value & mask);
}

static void set_output(struct GPIOBank *bank, struct GPIOPin *pin, uint32_t mask, int value)
{
    bool software_od = pin->config.drive == RTEMS_GPIO_DRIVE_OPEN_DRAIN
        && (pin->pad == NULL || pin->od_mask == 0);
    if (software_od && value) {
        bank->regs->gdir &= ~mask;
    }
    write_latch(bank, mask, value ? mask : 0);
    if (!software_od || !value) {
        bank->regs->gdir |= mask;
    }
}

static int pin_configure(rtems_gpio_drv_ctrl *ctrl, uint32_t id, rtems_gpio_config *config)
{
    (void) ctrl;
    struct GPIOBank *bank = &banks[id / 32];
    struct GPIOPin *pin = &pins[id];
    uint32_t mask = UINT32_C(1) << (id % 32);
    unsigned shift = (id % 16) * 2;
    volatile uint32_t *icr = &bank->regs->icr[(id % 32) / 16];
    if ((config->direction != RTEMS_GPIO_DIRECTION_INPUT
            && config->direction != RTEMS_GPIO_DIRECTION_OUTPUT)
        || (unsigned) config->bias > RTEMS_GPIO_BIAS_PULL_DOWN
        || (unsigned) config->drive > RTEMS_GPIO_DRIVE_OPEN_SOURCE
        || (unsigned) config->trigger > RTEMS_GPIO_TRIGGER_LEVEL_LOW
        || (config->flags & ~RTEMS_GPIO_FLAG_ACTIVE_LOW) != 0) {
        return EINVAL;
    }
    if (config->trigger != RTEMS_GPIO_TRIGGER_NONE
        && config->direction != RTEMS_GPIO_DIRECTION_INPUT) {
        return ENOTSUP;
    }
    rtems_interrupt_lock_context context;
    rtems_interrupt_lock_acquire(&device_lock, &context);
    /* A live interrupt not enabled by us belongs to someone else. Do not
     * claim all pins named in FDT as reserved: that is not ownership data. */
    if ((bank->regs->imr & mask) != 0) {
        rtems_interrupt_lock_release(&device_lock, &context);
        return EBUSY;
    }
    pin->saved_dr = bank->regs->dr & mask;
    pin->saved_gdir = bank->regs->gdir & mask;
    pin->saved_icr = *icr & (3U << shift);
    pin->saved_edge = bank->regs->edge_sel & mask;
    bank->latch = (bank->latch & ~mask) | pin->saved_dr;
    bank->tracked |= mask;
    pin->config = *config;
    bank->regs->gdir &= ~mask;
    if (pin->pad != NULL) {
        uint32_t pad_mask = pin->bias_mask | pin->od_mask;
        uint32_t value = config->bias == RTEMS_GPIO_BIAS_PULL_UP ? pin->pull_up
            : config->bias == RTEMS_GPIO_BIAS_PULL_DOWN          ? pin->pull_down
                                                                 : 0;
        if (config->direction == RTEMS_GPIO_DIRECTION_OUTPUT
            && config->drive == RTEMS_GPIO_DRIVE_OPEN_DRAIN) {
            value |= pin->od_mask;
        }
        pin->saved_pad = *pin->pad & pad_mask;
        *pin->pad = (*pin->pad & ~pad_mask) | value;
    }
    if (config->direction == RTEMS_GPIO_DIRECTION_OUTPUT) {
        set_output(bank, pin, mask, config->initial_value);
    }
    uint32_t mode = 0;
    switch (config->trigger) {
        case RTEMS_GPIO_TRIGGER_EDGE_RISING:
            mode = 2;
            break;
        case RTEMS_GPIO_TRIGGER_EDGE_FALLING:
            mode = 3;
            break;
        case RTEMS_GPIO_TRIGGER_LEVEL_HIGH:
            mode = 1;
            break;
        default:
            break;
    }
    *icr = (*icr & ~(3U << shift)) | (mode << shift);
    bank->regs->edge_sel = (bank->regs->edge_sel & ~mask)
        | (config->trigger == RTEMS_GPIO_TRIGGER_EDGE_BOTH ? mask : 0);
    rtems_interrupt_lock_release(&device_lock, &context);
    return 0;
}

static int pin_get_config(rtems_gpio_drv_ctrl *ctrl, uint32_t id, rtems_gpio_config *config)
{
    (void) ctrl;
    *config = pins[id].config;
    /* The core converts the tracked physical latch back to a logical level.
     * Reading DR here would lose input preloads and software-OD high values. */
    config->initial_value = (banks[id / 32].latch >> (id % 32)) & 1;
    return 0;
}

static void disable_locked(uint32_t id)
{
    struct GPIOBank *bank = &banks[id / 32];
    uint32_t mask = UINT32_C(1) << (id % 32);
    if ((bank->enabled & mask) != 0) {
        bank->regs->imr &= ~mask;
        bank->regs->isr = mask;
    }
    bank->enabled &= ~mask;
    pins[id].handler = NULL;
    pins[id].arg = NULL;
}

static int pin_irq_disable(rtems_gpio_drv_ctrl *ctrl, uint32_t id)
{
    (void) ctrl;
    rtems_interrupt_lock_context context;
    rtems_interrupt_lock_acquire(&device_lock, &context);
    disable_locked(id);
    rtems_interrupt_lock_release(&device_lock, &context);
    return 0;
}

static int pin_release(rtems_gpio_drv_ctrl *ctrl, uint32_t id)
{
    (void) ctrl;
    struct GPIOBank *bank = &banks[id / 32];
    struct GPIOPin *pin = &pins[id];
    uint32_t mask = UINT32_C(1) << (id % 32);
    unsigned shift = (id % 16) * 2;
    volatile uint32_t *icr = &bank->regs->icr[(id % 32) / 16];
    rtems_interrupt_lock_context context;
    rtems_interrupt_lock_acquire(&device_lock, &context);
    disable_locked(id);
    bank->regs->gdir &= ~mask;
    write_latch(bank, mask, pin->saved_dr);
    bank->tracked &= ~mask;
    *icr = (*icr & ~(3U << shift)) | pin->saved_icr;
    bank->regs->edge_sel = (bank->regs->edge_sel & ~mask) | pin->saved_edge;
    if (pin->pad != NULL) {
        *pin->pad = (*pin->pad & ~(pin->bias_mask | pin->od_mask)) | pin->saved_pad;
    }
    bank->regs->gdir = (bank->regs->gdir & ~mask) | pin->saved_gdir;
    memset(&pin->config, 0, sizeof(pin->config));
    rtems_interrupt_lock_release(&device_lock, &context);
    return 0;
}

static int pin_get(rtems_gpio_drv_ctrl *ctrl, uint32_t id, int *value)
{
    (void) ctrl;
    struct GPIOBank *bank = &banks[id / 32];
    uint32_t bits = pins[id].config.direction == RTEMS_GPIO_DIRECTION_OUTPUT
        ? bank->latch
        : bank->regs->psr;
    *value = (bits >> (id % 32)) & 1;
    return 0;
}

static int pin_set(rtems_gpio_drv_ctrl *ctrl, uint32_t id, int value)
{
    (void) ctrl;
    rtems_interrupt_lock_context context;
    rtems_interrupt_lock_acquire(&device_lock, &context);
    struct GPIOBank *bank = &banks[id / 32];
    uint32_t mask = UINT32_C(1) << (id % 32);
    if (pins[id].config.direction == RTEMS_GPIO_DIRECTION_OUTPUT) {
        set_output(bank, &pins[id], mask, value);
    } else {
        /* Legacy i.MX writes can preload an input's output latch, but must
         * never turn that input into an output (including software OD). */
        write_latch(bank, mask, value ? mask : 0);
    }
    rtems_interrupt_lock_release(&device_lock, &context);
    return 0;
}

static int pin_irq_enable(rtems_gpio_drv_ctrl *ctrl, uint32_t id, rtems_gpio_irq_flags flags,
    rtems_gpio_irq_handler handler, void *arg)
{
    (void) ctrl;
    if (flags != RTEMS_GPIO_IRQ_DEFAULT) {
        return ENOTSUP;
    }
    if (pins[id].config.direction != RTEMS_GPIO_DIRECTION_INPUT
        || pins[id].config.trigger == RTEMS_GPIO_TRIGGER_NONE) {
        return EINVAL;
    }
    struct GPIOBank *bank = &banks[id / 32];
    unsigned half = (id % 32) / 16;
    if (bank->irq[half] == UINT32_MAX) {
        return ENOTSUP;
    }
    if (!bank->installed[half]) {
        if (rtems_interrupt_handler_install(bank->irq[half], "AtomVM GPIO", RTEMS_INTERRUPT_SHARED,
                gpio_isr, bank)
            != RTEMS_SUCCESSFUL) {
            return EIO;
        }
        bank->installed[half] = true;
        if (bank->irq[0] == bank->irq[1]) {
            bank->installed[1 - half] = true;
        }
    }
    rtems_interrupt_lock_context context;
    rtems_interrupt_lock_acquire(&device_lock, &context);
    uint32_t mask = UINT32_C(1) << (id % 32);
    bank->regs->imr &= ~mask;
    bank->regs->isr = mask;
    pins[id].handler = handler;
    pins[id].arg = arg;
    bank->enabled |= mask;
    bank->regs->imr |= mask;
    rtems_interrupt_lock_release(&device_lock, &context);
    return 0;
}

static const rtems_gpio_drv_handlers handlers = {
    .pin_get_info = pin_info,
    .pin_configure = pin_configure,
    .pin_get_config = pin_get_config,
    .pin_release = pin_release,
    .pin_get = pin_get,
    .pin_set = pin_set,
    .pin_irq_enable = pin_irq_enable,
    .pin_irq_disable = pin_irq_disable
};

static rtems_gpio_drv_ctrl controller = {
    .handlers = &handlers,
    .pin_count = IMX_GPIO_DEVICE_PIN_COUNT,
    .name = "i.MX GPIO",
    .can_block = false,
    .active_low = active_low,
    .scratch = scratch
};

/* Physical offsets from the i.MX6UL and i.MX7D pin-function tables. In
 * particular GPIO1_IO10..15 are JTAG pads on 6UL, NOT the UART pads, and
 * GPIO4_IO0..6 are NAND pads, NOT the SNVS pads. */
static bool pad_offsets(unsigned id, bool imx6, bool lpsr, uint32_t *mux, uint32_t *pad)
{
    unsigned bank = id / 32 + 1;
    unsigned bit = id % 32;
    if (imx6) {
        switch (bank) {
            case 1:
                *mux = bit < 10 ? 0x05C + bit * 4
                    : bit < 16  ? 0x044 + (bit - 10) * 4
                                : 0x084 + (bit - 16) * 4;
                break;
            case 2:
                if (bit > 21) {
                    return false;
                }
                *mux = bit < 16 ? 0x0C4 + bit * 4 : 0x1BC + (bit - 16) * 4;
                break;
            case 3:
                if (bit > 28) {
                    return false;
                }
                *mux = 0x104 + bit * 4;
                break;
            case 4:
                if (bit > 28) {
                    return false;
                }
                *mux = bit < 17 ? 0x178 + bit * 4 : 0x1D4 + (bit - 17) * 4;
                break;
            case 5:
                if (bit > 11) {
                    return false;
                }
                *mux = bit < 10 ? 0x01C + bit * 4 : 0x014 + (bit - 10) * 4;
                break;
            default:
                return false;
        }
        *pad = *mux + 0x28C;
        return true;
    }
    if (lpsr) {
        if (bank != 1 || bit > 7) {
            return false;
        }
        *mux = bit * 4;
        *pad = 0x030 + bit * 4;
        return true;
    }
    if (bank == 1) {
        if (bit < 8 || bit > 15) {
            return false;
        }
        *mux = 0x014 + (bit - 8) * 4;
        *pad = 0x26C + (bit - 8) * 4;
        return true;
    }
    static const uint16_t starts[] = { 0x034, 0x0B4, 0x128, 0x188, 0x1D0, 0x22C };
    static const uint8_t counts[] = { 32, 29, 24, 18, 23, 16 };
    if (bank < 2 || bank > 7 || bit >= counts[bank - 2]) {
        return false;
    }
    *mux = starts[bank - 2] + bit * 4;
    *pad = *mux + 0x270;
    return true;
}

static void discover_pads(const void *fdt)
{
    static const char *const compatibles[] = {
        "fsl,imx6ul-iomuxc", "fsl,imx7d-iomuxc", "fsl,imx7d-iomuxc-lpsr"
    };
    for (unsigned type = 0; type < 3; type++) {
        int node = fdt_node_offset_by_compatible(fdt, -1, compatibles[type]);
        uintptr_t base;
        uint32_t size;
        if (node < 0 || !node_enabled(fdt, node) || !register_region(fdt, node, &base, &size)) {
            continue;
        }
        for (unsigned id = 0; id < IMX_GPIO_DEVICE_PIN_COUNT; id++) {
            uint32_t mux;
            uint32_t pad;
            if (!pad_offsets(id, type == 0, type == 2, &mux, &pad)
                || mux > size - 4 || pad > size - 4) {
                continue;
            }
            pins[id].mux = (volatile uint32_t *) (base + mux);
            pins[id].pad = (volatile uint32_t *) (base + pad);
            pins[id].bias_mask = type == 0 ? (3U << 14) | (1U << 13) | (1U << 12) : 0x70;
            pins[id].pull_up = type == 0 ? (2U << 14) | (1U << 13) | (1U << 12) : 0x70;
            pins[id].pull_down = type == 0 ? (1U << 13) | (1U << 12) : 0x10;
            pins[id].od_mask = type == 0 ? 1U << 11 : 0;
        }
    }
}

bool imx_gpio_device_initialize(void)
{
    rtems_mutex_lock(&initialize_mutex);
    if (!initialized) {
        initialized = true;
        const void *fdt = bsp_fdt_get();
        if (fdt != NULL && fdt_check_header(fdt) == 0) {
            for (unsigned i = 0; i < 7; i++) {
                struct GPIOBank *bank = &banks[i];
                bank->node = -1;
                bank->index = i;
                bank->irq[0] = UINT32_MAX;
                bank->irq[1] = UINT32_MAX;
                char alias[8];
                snprintf(alias, sizeof(alias), "gpio%u", i);
                const char *path = fdt_get_alias(fdt, alias);
                int node = path == NULL ? -1 : fdt_path_offset(fdt, path);
                uintptr_t base;
                uint32_t size;
                if (node < 0 || !node_enabled(fdt, node)
                    || !register_region(fdt, node, &base, &size)
                    || size < sizeof(struct GPIORegisters)) {
                    continue;
                }
                bank->node = node;
                bank->regs = (volatile struct GPIORegisters *) base;
                int len;
                const fdt32_t *irq = fdt_getprop(fdt, node, "interrupts", &len);
                /* i.MX6/7 GPIO interrupts are two GIC SPI specifiers. No
                 * hardcoded IRQ fallback when FDT does not describe them. */
                if (irq != NULL && len == 24) {
                    for (unsigned half = 0; half < 2; half++) {
                        uint32_t spi = fdt32_to_cpu(irq[half * 3 + 1]);
                        if (fdt32_to_cpu(irq[half * 3]) == 0 && spi < 988) {
                            bank->irq[half] = spi + 32;
                        }
                    }
                }
            }
            discover_pads(fdt);
            registered = rtems_gpio_drv_ctrl_init(&controller) == 0
                && rtems_gpio_drv_ctrl_register(&controller, "/dev/gpio") == 0;
        }
    }
    bool result = registered;
    rtems_mutex_unlock(&initialize_mutex);
    return result;
}

static bool associate_pad(const void *fdt, int node, uint32_t id)
{
    int len;
    const fdt32_t *pinctrl = fdt_getprop(fdt, node, "pinctrl-0", &len);
    if (pinctrl == NULL) {
        return true;
    }
    if (len <= 0 || len % 4 != 0) {
        return false;
    }
    size_t groups = (size_t) len / 4;
    for (size_t group = 0; group < groups; group++) {
        int pnode = fdt_node_offset_by_phandle(fdt, fdt32_to_cpu(pinctrl[group]));
        const fdt32_t *entries = fdt_getprop(fdt, pnode, "fsl,pins", &len);
        if (entries == NULL || len <= 0 || len % 24 != 0) {
            return false;
        }
        size_t count = (size_t) len / 24;
        int parent = fdt_parent_offset(fdt, pnode);
        while (parent >= 0) {
            bool imx6 = fdt_node_check_compatible(fdt, parent, "fsl,imx6ul-iomuxc") == 0;
            bool imx7 = fdt_node_check_compatible(fdt, parent, "fsl,imx7d-iomuxc") == 0
                || fdt_node_check_compatible(fdt, parent, "fsl,imx7d-iomuxc-lpsr") == 0;
            if (imx6 || imx7) {
                uintptr_t base;
                uint32_t size;
                if (!register_region(fdt, parent, &base, &size)) {
                    return false;
                }
                uint32_t expected_mux;
                uint32_t expected_pad;
                bool lpsr = fdt_node_check_compatible(fdt, parent, "fsl,imx7d-iomuxc-lpsr") == 0;
                if (!pad_offsets(id, imx6, lpsr, &expected_mux, &expected_pad)) {
                    break;
                }
                /* A GPIO property's index need not be its pad's index in
                 * pinctrl-0. Match the physical pad, never another entry. */
                for (size_t entry = 0; entry < count; entry++) {
                    uint32_t mux = fdt32_to_cpu(entries[entry * 6]);
                    uint32_t pad = fdt32_to_cpu(entries[entry * 6 + 1]);
                    if ((mux & 3) != 0 || (pad & 3) != 0
                        || mux > size - 4 || pad > size - 4) {
                        return false;
                    }
                    if (mux == expected_mux && pad == expected_pad) {
                        return pins[id].mux == (volatile uint32_t *) (base + mux)
                            && pins[id].pad == (volatile uint32_t *) (base + pad);
                    }
                }
                break;
            }
            parent = fdt_parent_offset(fdt, parent);
        }
    }
    /* pinctrl-0 often describes a peripheral rather than its reset GPIO.
     * In that case use the SoC mapping, not the unrelated peripheral pad. */
    return true;
}

bool imx_gpio_resolve_fdt(int node, const char *property, size_t index,
    uint32_t *pin_id, bool *is_active_low)
{
    if (!registered || node < 0 || property == NULL || pin_id == NULL || is_active_low == NULL) {
        return false;
    }
    const void *fdt = bsp_fdt_get();
    int len;
    const fdt32_t *prop = fdt_getprop(fdt, node, property, &len);
    if (prop == NULL || len <= 0 || len % 12 != 0 || index >= (size_t) len / 12) {
        return false;
    }
    int gpio_node = fdt_node_offset_by_phandle(fdt, fdt32_to_cpu(prop[index * 3]));
    const fdt32_t *cells = fdt_getprop(fdt, gpio_node, "#gpio-cells", &len);
    if (cells == NULL || len != 4 || fdt32_to_cpu(*cells) != 2) {
        return false;
    }
    uint32_t bit = fdt32_to_cpu(prop[index * 3 + 1]);
    uint32_t flags = fdt32_to_cpu(prop[index * 3 + 2]);
    if (bit >= 32 || (flags & ~UINT32_C(1)) != 0) {
        return false;
    }
    for (unsigned bank = 0; bank < 7; bank++) {
        if (banks[bank].regs != NULL && banks[bank].node == gpio_node) {
            uint32_t id = bank * 32 + bit;
            rtems_mutex_lock(&controller.mutex);
            bool result = associate_pad(fdt, node, id);
            rtems_mutex_unlock(&controller.mutex);
            if (result) {
                *pin_id = id;
                *is_active_low = (flags & 1) != 0;
            }
            return result;
        }
    }
    return false;
}

bool imx_gpio_set_mux(uint32_t id, unsigned mux)
{
    if (!registered || id >= IMX_GPIO_DEVICE_PIN_COUNT || mux > 7) {
        return false;
    }
    rtems_mutex_lock(&controller.mutex);
    bool result = banks[id / 32].regs != NULL && pins[id].mux != NULL;
    if (result) {
        rtems_interrupt_lock_context context;
        rtems_interrupt_lock_acquire(&device_lock, &context);
        *pins[id].mux = (*pins[id].mux & ~UINT32_C(7)) | mux;
        rtems_interrupt_lock_release(&device_lock, &context);
    }
    rtems_mutex_unlock(&controller.mutex);
    return result;
}
