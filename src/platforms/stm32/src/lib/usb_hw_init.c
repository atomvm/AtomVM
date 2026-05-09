/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/* Family-specific USB peripheral hardware initialization (clocks, GPIO, NVIC).
 * Called once from usb_cdc_driver_create_port before tusb_init().
 */

#ifdef AVM_USB_CDC_PORT_DRIVER_ENABLED

#include "usb_hw_init.h"
#include "stm32_hal_platform.h"

#include <stdio.h>

#if defined(STM32H7XX) || defined(STM32H5XX)

bool usb_cdc_hw_init(void)
{
    RCC_OscInitTypeDef osc = { 0 };
    RCC_PeriphCLKInitTypeDef pclk = { 0 };

    osc.OscillatorType = RCC_OSCILLATORTYPE_HSI48;
    osc.HSI48State = RCC_HSI48_ON;
    osc.PLL.PLLState = RCC_PLL_NONE;
    if (HAL_RCC_OscConfig(&osc) != HAL_OK) {
        fprintf(stderr, "usb_cdc: HAL_RCC_OscConfig failed (HSI48 unavailable)\n");
        return false;
    }

    pclk.PeriphClockSelection = RCC_PERIPHCLK_USB;
    pclk.UsbClockSelection = RCC_USBCLKSOURCE_HSI48;
    if (HAL_RCCEx_PeriphCLKConfig(&pclk) != HAL_OK) {
        fprintf(stderr, "usb_cdc: HAL_RCCEx_PeriphCLKConfig failed\n");
        return false;
    }

#if defined(STM32H7XX)
    if (HAL_PWREx_EnableUSBVoltageDetector() != HAL_OK) {
        fprintf(stderr, "usb_cdc: HAL_PWREx_EnableUSBVoltageDetector failed\n");
        return false;
    }

    __HAL_RCC_USB2_OTG_FS_CLK_ENABLE();
#else
    HAL_PWREx_EnableVddUSB();

    __HAL_RCC_USB_CLK_ENABLE();
#endif

    __HAL_RCC_GPIOA_CLK_ENABLE();
    GPIO_InitTypeDef gpio = { 0 };
    gpio.Pin = GPIO_PIN_11 | GPIO_PIN_12;
    gpio.Mode = GPIO_MODE_AF_PP;
    gpio.Pull = GPIO_NOPULL;
    gpio.Speed = GPIO_SPEED_FREQ_VERY_HIGH;

#if defined(STM32H7XX)
    gpio.Alternate = GPIO_AF10_OTG1_FS;
#else
    gpio.Alternate = GPIO_AF10_USB;
#endif
    HAL_GPIO_Init(GPIOA, &gpio);

#if defined(STM32H7XX)
    HAL_NVIC_SetPriority(OTG_FS_IRQn, 6, 0);
    HAL_NVIC_EnableIRQ(OTG_FS_IRQn);
#else
    HAL_NVIC_SetPriority(USB_DRD_FS_IRQn, 6, 0);
    HAL_NVIC_EnableIRQ(USB_DRD_FS_IRQn);
#endif
    return true;
}

#else

#error AVM_USB_CDC_PORT_DRIVER_ENABLED: usb_cdc_hw_init not implemented for this STM32 family. Add the family-specific clock/GPIO/NVIC setup (see the H7/H5 branch above) or disable the option.

#endif

#endif
