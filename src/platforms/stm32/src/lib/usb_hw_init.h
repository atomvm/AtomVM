/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef _ATOMVM_USB_HW_INIT_H
#define _ATOMVM_USB_HW_INIT_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Family-specific USB peripheral hardware initialization.
 * @returns true on success, false if HAL clock configuration failed.
 */
bool usb_cdc_hw_init(void);

#ifdef __cplusplus
}
#endif

#endif
