/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifdef AVM_USB_CDC_PORT_DRIVER_ENABLED

#include <tusb.h>

/* TinyUSB DCD interrupt entry point. The exact ISR symbol is family-specific:
 * - H7 / F2 / F4 / F7: OTG_FS_IRQHandler (synopsys dwc2 OTG controller)
 * - H5: USB_DRD_FS_IRQHandler (STM32 USB_DRD_FS device IP)
 *
 * Both forward to dcd_int_handler(0). For H7 with OTG_HS in use, also wire
 * OTG_HS_IRQHandler -> dcd_int_handler(1) (not enabled here by default).
 */

#if defined(STM32H7XX) || defined(STM32F4XX) || defined(STM32F7XX) || defined(STM32F2XX)

void OTG_FS_IRQHandler(void);
void OTG_FS_IRQHandler(void)
{
    tud_int_handler(0);
}

#elif defined(STM32H5XX)

void USB_DRD_FS_IRQHandler(void);
void USB_DRD_FS_IRQHandler(void)
{
    tud_int_handler(0);
}

#else

#error "AVM_USB_CDC_PORT_DRIVER_ENABLED: no IRQ handler wiring for this STM32 family"

#endif

#endif
