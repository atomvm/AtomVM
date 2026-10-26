/*
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-FileCopyrightText: 2025 Espressif Systems (Shanghai) CO LTD
 * SPDX-License-Identifier: Apache-2.0
 */

#include "esp_sleep_console.h"
#include "esp_sleep_regs.h"

/* ESP-IDF 9a97f6c54ec638111ce55cd36581b3c192f15207:
 * sleep_uart.c, sleep_console.c and each chip's uart/usb_serial_jtag LL.
 * The baseline retains TOP in light sleep: suspend every enabled HP UART,
 * not just the console. Deep sleep drains output before the ROM reset.
 */
extern void uart_tx_wait_idle(uint8_t uart);

#if defined(RTEMS_BOARD_ESP32P4)
#define SLEEP_UART_COUNT 5
#define SLEEP_UART_BASE 0x500CA000U
#define SLEEP_UART_STRIDE 0x1000U
#define USJ_CONF0 0x500D2018U
#elif defined(RTEMS_BOARD_ESP32S31)
#define SLEEP_UART_COUNT 4
#define SLEEP_UART_BASE 0x2038A000U
#define SLEEP_UART_STRIDE 0x1000U
#define USJ_CONF0 0x20391018U
#elif defined(RTEMS_BOARD_ESP32C3)
#define SLEEP_UART_COUNT 2
#define SLEEP_UART_BASE 0x60000000U
#define SLEEP_UART_STRIDE 0x10000U
#define USJ_CONF0 0x60043018U
#else
#define SLEEP_UART_COUNT 2
#define SLEEP_UART_BASE 0x60000000U
#define SLEEP_UART_STRIDE 0x1000U
#define USJ_CONF0 0x6000F018U
#endif

#if defined(RTEMS_BOARD_ESP32C3)
#define SLEEP_UART_FLOW 0x34U
#define SLEEP_UART_FSM 0x6CU
#define SLEEP_UART_XON BIT(2)
#define SLEEP_UART_XOFF BIT(3)
#define SLEEP_UART_SW_FLOW BIT(0)
#else
#define SLEEP_UART_FLOW 0x3CU
#define SLEEP_UART_FSM 0x70U
#define SLEEP_UART_XON BIT(19)
#define SLEEP_UART_XOFF BIT(20)
#define SLEEP_UART_SW_FLOW BIT(17)
#endif

static bool uart_enabled(unsigned port)
{
#if defined(RTEMS_BOARD_ESP32C3)
    uint32_t bit = port == 0 ? BIT(2) : BIT(5);
    return (REG(0x600C0010U) & bit) && !(REG(0x600C0018U) & bit);
#elif defined(RTEMS_BOARD_ESP32P4)
    return (REG(0x500E6018U) & BIT(18 + port))
        && (REG(0x500E601CU) & BIT(7 + port))
        && !(REG(0x500E60C4U) & BIT(13 + port));
#elif defined(RTEMS_BOARD_ESP32S31)
    return (REG(0x20587088U + port * 4U) & 7U) == 3U;
#else
    return (REG(0x60096000U + port * 12U) & 3U) == 1U;
#endif
}

static void uart_update(uintptr_t base)
{
#if defined(RTEMS_BOARD_ESP32C3)
    REG(base + 0x80U) |= BIT(31);
#else
    REG(base + 0x98U) |= BIT(0);
    while (REG(base + 0x98U) & BIT(0)) {
    }
#endif
}

static bool usb_clock_enabled(void)
{
#if defined(RTEMS_BOARD_ESP32C3)
    return (REG(0x600C0010U) & BIT(23)) && !(REG(0x600C0018U) & BIT(23));
#elif defined(RTEMS_BOARD_ESP32P4)
    return (REG(0x500E601CU) & BIT(29)) && !(REG(0x50111048U) & BIT(4));
#elif defined(RTEMS_BOARD_ESP32S31)
    return (REG(0x20587144U) & BIT(0)) && (REG(0x20359034U) & BIT(30));
#elif defined(RTEMS_BOARD_ESP32C5)
    return (REG(0x60096094U) & 3U) == 1U;
#else
    return (REG(0x6009608CU) & 3U) == 1U;
#endif
}

static void usb_clock_enable(bool enable)
{
#if defined(RTEMS_BOARD_ESP32C3)
    REG(0x600C0010U) = (REG(0x600C0010U) & ~BIT(23)) | (enable ? BIT(23) : 0);
#elif defined(RTEMS_BOARD_ESP32P4)
    REG(0x500E601CU) = (REG(0x500E601CU) & ~BIT(29)) | (enable ? BIT(29) : 0);
    REG(0x50111044U) = (REG(0x50111044U) & ~BIT(5)) | (enable ? BIT(5) : 0);
#elif defined(RTEMS_BOARD_ESP32S31)
    REG(0x20587144U) = (REG(0x20587144U) & ~BIT(0)) | (enable ? BIT(0) : 0);
    REG(0x20359034U) = (REG(0x20359034U) & ~BIT(30)) | (enable ? BIT(30) : 0);
#elif defined(RTEMS_BOARD_ESP32C5)
    REG(0x60096094U) = (REG(0x60096094U) & ~BIT(0)) | (enable ? BIT(0) : 0);
#else
    REG(0x6009608CU) = (REG(0x6009608CU) & ~BIT(0)) | (enable ? BIT(0) : 0);
#endif
}

static void usb_reset(void)
{
#if defined(RTEMS_BOARD_ESP32C3)
    const uintptr_t reset = 0x600C0018U;
    const uint32_t bit = BIT(23);
#elif defined(RTEMS_BOARD_ESP32P4)
    const uintptr_t reset = 0x50111048U;
    const uint32_t bit = BIT(4);
#elif defined(RTEMS_BOARD_ESP32S31)
    const uintptr_t reset = 0x20359034U;
    const uint32_t bit = BIT(31);
#elif defined(RTEMS_BOARD_ESP32C5)
    const uintptr_t reset = 0x60096094U;
    const uint32_t bit = BIT(1);
#else
    const uintptr_t reset = 0x6009608CU;
    const uint32_t bit = BIT(1);
#endif
    REG(reset) |= bit;
    REG(reset) &= ~bit;
}

void rtems_esp_sleep_console_prepare(bool deep, struct sleep_console_state *state)
{
    state->suspended_uarts = 0;
    for (unsigned port = 0; port < SLEEP_UART_COUNT; port++) {
        if (!uart_enabled(port)) {
            continue;
        }
        if (deep) {
            uart_tx_wait_idle(port);
        } else {
            uintptr_t base = SLEEP_UART_BASE + port * SLEEP_UART_STRIDE;
            REG(base + SLEEP_UART_FLOW) &= ~SLEEP_UART_XON;
            REG(base + SLEEP_UART_FLOW) |= SLEEP_UART_SW_FLOW | SLEEP_UART_XOFF;
            uart_update(base);
            uint32_t fsm;
            do {
                fsm = (REG(base + SLEEP_UART_FSM) >> 4) & 0xFU;
            } while (fsm != 0 && fsm != 15);
            state->suspended_uarts |= BIT(port);
        }
    }
    if (!deep) {
        state->usb_clock = usb_clock_enabled();
        if (state->usb_clock) {
            state->usb_pad = (REG(USJ_CONF0) & BIT(14)) != 0;
        } else {
            usb_clock_enable(true);
            usb_reset();
            state->usb_pad = false;
        }
        REG(USJ_CONF0) &= ~BIT(14);
        usb_clock_enable(false);
    }
}

void rtems_esp_sleep_console_resume(bool deep, const struct sleep_console_state *state)
{
    if (!deep) {
        usb_clock_enable(true);
        REG(USJ_CONF0) = (REG(USJ_CONF0) & ~BIT(14)) | (state->usb_pad ? BIT(14) : 0);
        if (!state->usb_clock) {
            usb_clock_enable(false);
        }
    }
    for (unsigned port = 0; port < SLEEP_UART_COUNT; port++) {
        if (state->suspended_uarts & BIT(port)) {
            uintptr_t base = SLEEP_UART_BASE + port * SLEEP_UART_STRIDE;
            REG(base + SLEEP_UART_FLOW) &= ~SLEEP_UART_XOFF;
            REG(base + SLEEP_UART_FLOW) |= SLEEP_UART_XON;
            REG(base + SLEEP_UART_FLOW) &= ~(SLEEP_UART_SW_FLOW | SLEEP_UART_XON);
            uart_update(base);
        }
    }
}
