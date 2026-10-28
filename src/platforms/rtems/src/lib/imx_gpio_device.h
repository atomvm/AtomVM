/*
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef IMX_GPIO_DEVICE_H
#define IMX_GPIO_DEVICE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define IMX_GPIO_DEVICE_PIN_COUNT 224

/** @brief Lazily register the i.MX controller at /dev/gpio, once, thread-safely. */
bool imx_gpio_device_initialize(void);

/**
 * @brief Resolve a three-cell GPIO specifier after controller initialization.
 *
 * Matches pinctrl-0 fsl,pins pad/mux entries by physical pin, not list order.
 * Pad/mux registers use the SoC pin-function mapping and FDT IOMUXC base.
 * Does not configure hardware. Outputs are unchanged on failure; polarity is
 * returned per call, not stored as a property of the physical pin.
 * @param[out] pin_id Logical ID, (bank - 1) * 32 + pin.
 * @param[out] active_low Whether the specifier requests active-low operation.
 */
bool imx_gpio_resolve_fdt(int node, const char *property, size_t index,
    uint32_t *pin_id, bool *active_low);

/**
 * @brief Change only the mux mode bits (0..7), preserving SION and other bits.
 * Returns false when the pin has no known mux register. No GPIO ownership or
 * peripheral reservation is inferred from a pinctrl reference.
 */
bool imx_gpio_set_mux(uint32_t pin_id, unsigned mux);

#endif
