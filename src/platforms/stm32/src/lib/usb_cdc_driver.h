/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef ATOMVM_USB_CDC_DRIVER_H
#define ATOMVM_USB_CDC_DRIVER_H

#ifdef __cplusplus
extern "C" {
#endif

/* Drive TinyUSB's task pump and deliver any newly-arrived bytes to a
 * parked reader. Safe to call when no port is open. */
void usb_cdc_driver_poll(void);

#ifdef __cplusplus
}
#endif

#endif
