/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-FileCopyrightText: 2015-2026 Espressif Systems (Shanghai) CO LTD
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include "esp_sleep_clock.h"

#if defined(RTEMS_BOARD_ESP32)

#include <bsp.h>
#include <bsp/linker-symbols.h>

#if defined(RTEMS_BOARD_ESP32P4)
#include "esp_sleep_pmu.h"
#endif

/* Ported from ESP-IDF 9a97f6c54ec638111ce55cd36581b3c192f15207:
 * esp_hw_support/port/<chip>/rtc_clk.c, esp_hal_clock/<chip>/clk_tree_ll.h,
 * esp_hal_regi2c/<chip>/regi2c_impl.c, and their SoC register definitions.
 * No IDF clock-tree refcounts exist in RTEMS. PMU chips' sleep-only mux
 * transitions do not power-cycle CPU PLLs in software; PMU owns the sleep
 * power transition. P4 MPLL has its separate IDF software disable/restore
 * sequence below. C3 instead explicitly disables BBPLL,
 * just like rtc_clk_cpu_freq_set_xtal_for_sleep(). System sleep requires
 * flash on the IDF/BSP's XTAL-safe path and radio/USB/peripherals quiesced.
 * Restore always uses the non-fast path: sleep invalidates PLL calibration
 * even if the mux/dividers happen to have retained their old values.
 */
#define CLOCK_REG(addr) (*(volatile uint32_t *) (uintptr_t) (addr))
#define CLOCK_BIT(n) (1U << (n))
#define CLOCK_SRAM BSP_FAST_TEXT_SECTION __attribute__((noinline))

/* Public ROM names are present in both the BSP maps and the flash-only
 * application maps. IDF aliases these to esp_rom_{set_cpu_ticks_per_us,delay_us}.
 */
extern void ets_update_cpu_frequency(uint32_t ticks);
extern void ets_delay_us(uint32_t us);

#if defined(RTEMS_BOARD_ESP32C3)
#define CLOCK_SOURCE_REG 0x600C0058U
#define CLOCK_SOURCE_SHIFT 10
#define CLOCK_CPU_REG CLOCK_SOURCE_REG
#define CLOCK_CPU_MASK 0x3FFU
#define CLOCK_XTAL_STORE 0x600080B8U
#define CLOCK_APB_STORE 0x600080BCU
#define CLOCK_PLL_REG 0x600C0008U
#define CLOCK_CAL_REG 0x6000E040U
#elif defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32H2)
#define CLOCK_SOURCE_SHIFT 16
#define CLOCK_CPU_MASK 0xFFU
#if defined(RTEMS_BOARD_ESP32H2)
#define CLOCK_SOURCE_REG 0x6009610CU
#define CLOCK_CPU_REG 0x60096114U
#define CLOCK_AHB_REG 0x60096118U
#define CLOCK_UPDATE_REG 0x60096148U
#define CLOCK_I2C_BASE 0x600AD800U
#define CLOCK_ANALOG_GATE 0x600AD008U
#define CLOCK_XTAL_STORE 0x600B1010U
#else
#define CLOCK_SOURCE_REG 0x60096110U
#define CLOCK_CPU_REG 0x60096118U
#define CLOCK_AHB_REG 0x6009611CU
#define CLOCK_UPDATE_REG 0x60096144U
#define CLOCK_I2C_BASE 0x600AF800U
#define CLOCK_ANALOG_GATE 0x600AF018U
#define CLOCK_XTAL_STORE 0x600B1010U
#endif
#define CLOCK_CAL_REG (CLOCK_I2C_BASE + 0x18U)
#elif defined(RTEMS_BOARD_ESP32S31)
#define CLOCK_SOURCE_REG 0x20587000U
#define CLOCK_SOURCE_SHIFT 0
#define CLOCK_CPU_REG 0x20587004U
#define CLOCK_CPU_MASK 0x3FFFU
#define CLOCK_MEM_REG 0x20587008U
#define CLOCK_AHB_REG 0x2058700CU /* SYS is parallel to MEM, not divided by MEM. */
#define CLOCK_APB_REG 0x20587010U
#define CLOCK_UPDATE_REG 0x20587014U
#define CLOCK_UPDATE_BIT CLOCK_BIT(0)
#define CLOCK_CAL_REG 0x20587174U
#define CLOCK_I2C_BASE 0x2010F800U
#define CLOCK_ANALOG_GATE 0x2010F018U
#elif defined(RTEMS_BOARD_ESP32P4)
#if !defined(BSP_ESP32P4_REVISION) || (BSP_ESP32P4_REVISION != 1 && BSP_ESP32P4_REVISION != 3)
#error "P4 sleep clocks require the BSP's explicit silicon revision (1 or 3)"
#endif
#define CLOCK_SOURCE_REG 0x50111040U
#define CLOCK_SOURCE_SHIFT 0
#define CLOCK_CPU_REG 0x500E6004U
#define CLOCK_CPU_MASK 0x1FFFFFE0U
#define CLOCK_MEM_REG 0x500E6008U
#define CLOCK_AHB_REG CLOCK_MEM_REG /* SYS is divided by MEM on P4. */
#define CLOCK_APB_REG 0x500E600CU
#define CLOCK_UPDATE_REG CLOCK_CPU_REG
#define CLOCK_UPDATE_BIT CLOCK_BIT(4)
#define CLOCK_CAL_REG 0x500E60BCU
#define CLOCK_I2C_BASE 0x50124000U
#define CLOCK_ANALOG_GATE 0x50120000U
#define CLOCK_XTAL_STORE 0x5011003CU
#else
#error "Unsupported ESP32 sleep clock target"
#endif

static inline __attribute__((always_inline)) void clock_field(uintptr_t reg, uint32_t mask, uint32_t value)
{
    CLOCK_REG(reg) = (CLOCK_REG(reg) & ~mask) | value;
}

static inline __attribute__((always_inline)) void clock_source(uint32_t source)
{
    clock_field(CLOCK_SOURCE_REG, 3U << CLOCK_SOURCE_SHIFT, source << CLOCK_SOURCE_SHIFT);
}

#if !defined(RTEMS_BOARD_ESP32C3) && !defined(RTEMS_BOARD_ESP32C6)
static inline __attribute__((always_inline)) void clock_bus_update(void)
{
#if defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32H2)
    CLOCK_REG(CLOCK_UPDATE_REG) = 1;
    while (CLOCK_REG(CLOCK_UPDATE_REG) & 1U) {
    }
#else
    CLOCK_REG(CLOCK_UPDATE_REG) |= CLOCK_UPDATE_BIT;
    while (CLOCK_REG(CLOCK_UPDATE_REG) & CLOCK_UPDATE_BIT) {
    }
#endif
}
#endif

static uint32_t clock_xtal_mhz(void)
{
#if defined(RTEMS_BOARD_ESP32C5)
    return (CLOCK_REG(CLOCK_SOURCE_REG) >> 24) & 0x7FU;
#elif defined(RTEMS_BOARD_ESP32S31)
    return CLOCK_REG(0x20701048U) & 0x7FU;
#else
    uint32_t value = CLOCK_REG(CLOCK_XTAL_STORE);
    if ((value & 0xFFFFU) == (value >> 16) && value != 0 && value != UINT32_MAX) {
        return value & 0xFFFEU; /* RTC_DISABLE_ROM_LOG is bit zero. */
    }
#if defined(RTEMS_BOARD_ESP32H2)
    return 32; /* Same invalid-store fallback as rtc_clk_xtal_freq_get(). */
#else
    return 40;
#endif
#endif
}

/* Caller holds interrupts/other CPU; preserve the analog master clock gate.
 * This is deliberately SRAM too: H2 FLASH_PLL derives from this very BBPLL.
 */
static CLOCK_SRAM uint32_t clock_analog_read_from(uint32_t block, uint32_t address)
{
#if defined(RTEMS_BOARD_ESP32C3)
    /* esp_hal_regi2c/esp32c3/rom.regi2c.ld: _regi2c_impl_read.
     * These HAL table entries have no names in the installed BSP ROM map.
     * Do not substitute rom_chip_i2c_readReg (a different lower-level ABI).
     */
    return ((uint8_t (*)(uint8_t, uint8_t, uint8_t))(uintptr_t) 0x40001954U)(block, 0, address);
#else
#if defined(RTEMS_BOARD_ESP32P4)
    clock_field(CLOCK_I2C_BASE + 0x20U, 0xFFFFFFU, block == 0x63 ? CLOCK_BIT(9) : CLOCK_BIT(11));
    clock_field(CLOCK_I2C_BASE + 0x1CU, 0xFFFFFFU, 0);
    const uintptr_t host = CLOCK_I2C_BASE;
#elif defined(RTEMS_BOARD_ESP32S31)
    /* Only MPLL uses analog I2C here; CPLL configuration is digital. */
    CLOCK_REG(CLOCK_I2C_BASE + 0x1CU) = 0xFFFFFFU & ~CLOCK_BIT(14);
    uintptr_t host = CLOCK_I2C_BASE + ((CLOCK_REG(CLOCK_I2C_BASE + 0x20U) & CLOCK_BIT(16)) ? 0 : 4);
#else
    CLOCK_REG(CLOCK_I2C_BASE + 0x1CU) = 0xFFFFFFU & ~CLOCK_BIT(7);
    uintptr_t host = CLOCK_I2C_BASE + ((CLOCK_REG(CLOCK_I2C_BASE + 0x20U) & CLOCK_BIT(9)) ? 0 : 4);
#endif
    while (CLOCK_REG(host) & CLOCK_BIT(25)) {
    }
    CLOCK_REG(host) = block | (address << 8);
    while (CLOCK_REG(host) & CLOCK_BIT(25)) {
    }
    return (CLOCK_REG(host) >> 16) & 0xFFU;
#endif
}

static CLOCK_SRAM void clock_analog_write_to(uint32_t block, uint32_t address, uint32_t mask, uint32_t value)
{
    uint32_t data = clock_analog_read_from(block, address);
    data = (data & ~mask) | value;
#if defined(RTEMS_BOARD_ESP32C3)
    /* Same IDF ROM HAL table: _regi2c_impl_write. */
    ((void (*)(uint8_t, uint8_t, uint8_t, uint8_t))(uintptr_t) 0x4000195CU)(block, 0, address, data);
#else
#if defined(RTEMS_BOARD_ESP32P4)
    const uintptr_t host = CLOCK_I2C_BASE;
#elif defined(RTEMS_BOARD_ESP32S31)
    uintptr_t host = CLOCK_I2C_BASE + ((CLOCK_REG(CLOCK_I2C_BASE + 0x20U) & CLOCK_BIT(16)) ? 0 : 4);
#else
    uintptr_t host = CLOCK_I2C_BASE + ((CLOCK_REG(CLOCK_I2C_BASE + 0x20U) & CLOCK_BIT(9)) ? 0 : 4);
#endif
    CLOCK_REG(host) = block | (address << 8) | CLOCK_BIT(24) | (data << 16);
    while (CLOCK_REG(host) & CLOCK_BIT(25)) {
    }
#endif
}

#if !defined(RTEMS_BOARD_ESP32S31)
static inline __attribute__((always_inline)) void clock_analog_write(uint32_t address, uint32_t mask, uint32_t value)
{
#if defined(RTEMS_BOARD_ESP32P4)
    clock_analog_write_to(0x67, address, mask, value);
#else
    clock_analog_write_to(0x66, address, mask, value);
#endif
}
#endif

static CLOCK_SRAM void clock_pll_restore(const struct esp_sleep_clock_state *state)
{
    (void) state; /* C6/H2 PLL frequency and crystal are fixed. */
#if defined(RTEMS_BOARD_ESP32S31)
    if (state->source == 3) {
        /* IDF's fixed BBPLL/PLL_F240M path has no software calibration. */
        CLOCK_REG(0x207040F4U) |= CLOCK_BIT(28) | CLOCK_BIT(24);
        CLOCK_REG(0x207040F4U) |= CLOCK_BIT(20);
        CLOCK_REG(0x20589000U) |= CLOCK_BIT(30);
        CLOCK_REG(0x2070401CU) |= CLOCK_BIT(28) | CLOCK_BIT(24);
        CLOCK_REG(0x2058717CU) |= CLOCK_BIT(8); /* ref_240m leaf gate */
        return;
    }
    CLOCK_REG(0x207040F4U) |= CLOCK_BIT(27) | CLOCK_BIT(23);
    CLOCK_REG(0x207040F4U) |= CLOCK_BIT(19);
    CLOCK_REG(0x20589000U) |= CLOCK_BIT(29);
    CLOCK_REG(0x2070401CU) |= CLOCK_BIT(27) | CLOCK_BIT(23);
    clock_field(0x2070104CU, 0xFFFU, 0x81U); /* XTAL40 * 8 / 1. */
    uint32_t gate = CLOCK_REG(0x2010F018U);
    uint32_t modem_apb = CLOCK_REG(0x20109C04U);
    CLOCK_REG(0x2010F018U) = gate | CLOCK_BIT(2);
    CLOCK_REG(0x20109C04U) = modem_apb | CLOCK_BIT(12);
    CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(3);
    while (!(CLOCK_REG(CLOCK_CAL_REG) & CLOCK_BIT(2))) {
    }
    ets_delay_us(10);
    CLOCK_REG(CLOCK_CAL_REG) |= CLOCK_BIT(3);
    CLOCK_REG(0x20109C04U) = modem_apb;
    CLOCK_REG(0x2010F018U) = gate;
#elif defined(RTEMS_BOARD_ESP32C3)
    CLOCK_REG(0x60008000U) &= ~(CLOCK_BIT(6) | CLOCK_BIT(8) | CLOCK_BIT(10));
    clock_field(CLOCK_PLL_REG, CLOCK_BIT(2), state->source_freq_mhz == 480 ? CLOCK_BIT(2) : 0);
    CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(2);
    CLOCK_REG(CLOCK_CAL_REG) |= CLOCK_BIT(3);
    bool pll480 = state->source_freq_mhz == 480;
    bool xtal32 = state->xtal_freq_mhz == 32;
    clock_analog_write(4, 0xFF, pll480 ? 0x6B : 0x69);
    clock_analog_write(2, 0xFF, xtal32 ? (pll480 ? 0x41 : 0x51) : 0x50);
    clock_analog_write(3, 0xFF, xtal32 ? (pll480 ? 26 : 6) : (pll480 ? 8 : 4));
    clock_analog_write(5, 7, xtal32 && pll480 ? 1 : 0);
    clock_analog_write(5, 0x70, xtal32 && pll480 ? 0x10 : 0);
    clock_analog_write(6, 0xFF, xtal32 && pll480 ? 0x90 : 0x93);
    clock_analog_write(9, 3, 2);
    clock_analog_write(6, 0x30, 0x20);
    clock_analog_write(6, 0xC0, 0x40);
    /* C3 IDF starts calibration but has no CAL_DONE/10us/stop sequence. */
#else
    uint32_t gate = CLOCK_REG(CLOCK_ANALOG_GATE);
#if defined(RTEMS_BOARD_ESP32P4)
    CLOCK_REG(0x501150CCU) |= CLOCK_BIT(27) | CLOCK_BIT(23);
    CLOCK_REG(0x501150CCU) |= CLOCK_BIT(17);
    CLOCK_REG(CLOCK_ANALOG_GATE) = gate | CLOCK_BIT(27);
    CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(3);
    clock_analog_write(2, 0xFF, 0x50);
    /* Both supported BSPs are ECO1 or later. The IDF's div+4 formula
     * applies only to ECO0 (revision <= 0.01), not the P4v1 BSP. */
    uint32_t div = state->source_freq_mhz == 400 ? 10 : 9;
    clock_analog_write(3, 0xFF, div);
    clock_analog_write(6, 0xFF, 0x73);
    while (!(CLOCK_REG(CLOCK_CAL_REG) & CLOCK_BIT(2))) {
    }
    ets_delay_us(10);
    CLOCK_REG(CLOCK_CAL_REG) |= CLOCK_BIT(3);
#else
    CLOCK_REG(0x600B00CCU) |= CLOCK_BIT(28) | CLOCK_BIT(29) | CLOCK_BIT(30);
    CLOCK_REG(0x600B00CCU) |= CLOCK_BIT(25);
    CLOCK_REG(CLOCK_ANALOG_GATE) = gate | CLOCK_BIT(2);
    CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(2);
    CLOCK_REG(CLOCK_CAL_REG) |= CLOCK_BIT(3);
#if defined(RTEMS_BOARD_ESP32H2)
    clock_analog_write(2, 0x0F, 0);
    clock_analog_write(3, 0x3F, 1);
    clock_analog_write(5, 0x30, 0x30);
    clock_analog_write(5, 0xC0, 0x40);
#elif defined(RTEMS_BOARD_ESP32C5)
    clock_analog_write(2, 0xFF, 0x51);
    clock_analog_write(3, 0xFF, state->xtal_freq_mhz == 48 ? 10 : 12);
    clock_analog_write(5, 7, state->xtal_freq_mhz == 48 ? 1 : 0);
    clock_analog_write(5, 0x70, state->xtal_freq_mhz == 48 ? 0x10 : 0);
    clock_analog_write(6, 0xC0, 0x40);
    clock_analog_write(6, 0x30, 0x30);
    clock_analog_write(9, 3, 3);
#else
    clock_analog_write(2, 0xFF, 0x50);
    clock_analog_write(3, 0xFF, 8);
    clock_analog_write(5, 7, 0);
    clock_analog_write(5, 0x70, 0);
    clock_analog_write(6, 0xFF, 0x73);
    clock_analog_write(9, 3, 2);
#endif
    while (!(CLOCK_REG(CLOCK_CAL_REG) & CLOCK_BIT(24))) {
    }
    ets_delay_us(10);
    CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(3);
    CLOCK_REG(CLOCK_CAL_REG) |= CLOCK_BIT(2);
#if defined(RTEMS_BOARD_ESP32C5)
    CLOCK_REG(0x60096128U) |= state->source == 3 ? CLOCK_BIT(0) : CLOCK_BIT(1);
#endif
#endif
    CLOCK_REG(CLOCK_ANALOG_GATE) = gate;
#endif
}

/* Actual mux/divider writes must not be inlined into flash callers. P4's
 * source mux is immediate; S31's mux is part of the bus-update transaction.
 */
static CLOCK_SRAM void clock_to_xtal(uint32_t xtal)
{
#if defined(RTEMS_BOARD_ESP32C3)
    ets_update_cpu_frequency(xtal);
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, 0);
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, 0);
    clock_source(0);
    uint32_t apb = (xtal * 1000000U) >> 12;
    CLOCK_REG(CLOCK_APB_STORE) = apb | (apb << 16);
    CLOCK_REG(0x60008000U) |= CLOCK_BIT(6) | CLOCK_BIT(8) | CLOCK_BIT(10);
#elif defined(RTEMS_BOARD_ESP32P4)
    clock_source(0); /* Must precede changing any dividers on P4. */
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, 0);
    clock_field(CLOCK_MEM_REG, 0xFFU, 0);
    clock_field(CLOCK_AHB_REG, 0xFF000000U, 0);
    clock_field(CLOCK_APB_REG, 0xFF0000U, 0);
    clock_bus_update();
    ets_update_cpu_frequency(xtal);
#elif defined(RTEMS_BOARD_ESP32S31)
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, 0);
    clock_field(CLOCK_MEM_REG, 1, 0);
    clock_field(CLOCK_AHB_REG, 0xFF, 0);
    clock_field(CLOCK_APB_REG, 0xFF, 0);
    clock_source(0);
    clock_bus_update();
    ets_update_cpu_frequency(xtal);
#else
#if defined(RTEMS_BOARD_ESP32C6)
    clock_field(CLOCK_AHB_REG, 0xFF, 0);
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, 0);
#else
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, 0);
    clock_field(CLOCK_AHB_REG, 0xFF, 0);
#endif
    clock_source(0);
#if !defined(RTEMS_BOARD_ESP32C6)
    clock_bus_update();
#endif
    ets_update_cpu_frequency(xtal);
#endif
}

bool rtems_esp_sleep_clock_prepare(struct esp_sleep_clock_state *state)
{
    state->mpll_freq_mhz = 0;
    state->source = (CLOCK_REG(CLOCK_SOURCE_REG) >> CLOCK_SOURCE_SHIFT) & 3;
    state->cpu = CLOCK_REG(CLOCK_CPU_REG) & CLOCK_CPU_MASK;
    state->xtal_freq_mhz = clock_xtal_mhz();
    uint32_t source = state->source;
    uint32_t xtal = state->xtal_freq_mhz;
    uint32_t div = (state->cpu & 0xFFU) + 1;
#if defined(RTEMS_BOARD_ESP32C3)
    if ((xtal != 32 && xtal != 40) || source > 2) {
        return false;
    }
    state->ahb = CLOCK_REG(CLOCK_APB_STORE);
    state->mem = CLOCK_REG(CLOCK_PLL_REG) & 7U;
    state->source_freq_mhz = source == 0 ? xtal : source == 1 ? ((state->mem & 4) ? 480 : 320)
                                                              : 20;
    div = state->cpu + 1;
    if (source == 1) {
        if ((state->mem & 3) > 1) {
            return false;
        }
        state->cpu_freq_mhz = (state->mem & 3) ? 160 : 80;
    } else {
        state->cpu_freq_mhz = state->source_freq_mhz / div;
    }
#elif defined(RTEMS_BOARD_ESP32S31) || defined(RTEMS_BOARD_ESP32P4)
    if (xtal != 40) {
        return false;
    }
    state->mem = CLOCK_REG(CLOCK_MEM_REG);
    state->ahb = CLOCK_REG(CLOCK_AHB_REG);
    state->apb = CLOCK_REG(CLOCK_APB_REG);
    uint32_t numerator;
    uint32_t denominator;
#if defined(RTEMS_BOARD_ESP32S31)
    uint32_t cpll = CLOCK_REG(0x2070104CU);
    uint32_t ref = cpll & 0xFU;
    state->source_freq_mhz = source == 0 ? xtal : source == 2 ? 20
        : source == 3                                         ? 240
        : ref                                                 ? xtal * ((cpll >> 4) & 0xFFU) / ref
                                                              : 0;
    if (source == 1 && state->source_freq_mhz != 320) {
        return false;
    }
    numerator = (state->cpu >> 8) & 7;
    denominator = (state->cpu >> 11) & 7;
#else
    if (source > 2) {
        return false;
    }
    state->source_freq_mhz = source == 0 ? xtal : 20;
    if (source == 1) {
        uint32_t gate = CLOCK_REG(CLOCK_ANALOG_GATE);
        CLOCK_REG(CLOCK_ANALOG_GATE) = gate | CLOCK_BIT(27);
        uint32_t fb = clock_analog_read_from(0x67, 3);
        uint32_t ref = clock_analog_read_from(0x67, 2) & 0xF;
        CLOCK_REG(CLOCK_ANALOG_GATE) = gate;
        state->source_freq_mhz = xtal * fb / (ref + 1);
        if (state->source_freq_mhz != 360 && state->source_freq_mhz != 400) {
            return false;
        }
    }
    div = ((state->cpu >> 5) & 0xFFU) + 1;
    numerator = (state->cpu >> 13) & 0xFFU;
    denominator = (state->cpu >> 21) & 0xFFU;
#endif
    if (denominator == 0) {
        denominator = 1;
        numerator = 0;
    }
    state->cpu_freq_mhz = state->source_freq_mhz * denominator / (div * denominator + numerator);
#else
    state->ahb = CLOCK_REG(CLOCK_AHB_REG) & 0xFFU;
#if defined(RTEMS_BOARD_ESP32C5)
    if (xtal != 40 && xtal != 48) {
        return false;
    }
    state->source_freq_mhz = source == 0 ? xtal : source == 1 ? 20
        : source == 2                                         ? 160
                                                              : 240;
#elif defined(RTEMS_BOARD_ESP32H2)
    if (xtal != 32) {
        return false;
    }
    state->source_freq_mhz = source == 0 ? xtal : source == 1 ? 96
        : source == 2                                         ? 8
                                                              : 64;
#else
    if (xtal != 40 || source > 2) {
        return false;
    }
    state->source_freq_mhz = source == 0 ? xtal : source == 1 ? 480
                                                              : 20;
    /* Save high-speed divider and force_120m as well as low-speed divider. */
    state->cpu = CLOCK_REG(CLOCK_CPU_REG) & 0x1FFFFU;
    if (source == 1) {
        uint32_t hs = (state->cpu >> 8) & 0xFFU;
        uint32_t root = ((CLOCK_REG(CLOCK_SOURCE_REG) >> 8) & 0xFFU) + 1;
        div = hs == 0 && (state->cpu & CLOCK_BIT(16)) ? 4 : root * (hs + 1);
    }
#endif
    state->cpu_freq_mhz = state->source_freq_mhz / div;
#endif
    if (state->cpu_freq_mhz == 0) {
        return false;
    }
#if defined(RTEMS_BOARD_ESP32S31)
    if (CLOCK_REG(0x2070401CU) & CLOCK_BIT(30)) {
        uint32_t config = CLOCK_REG(0x20701054U) & 0xFFU;
        if ((config & 7) != 1) {
            return false; /* IDF MPLL configure assumes ref_div=1. */
        }
        state->mpll_freq_mhz = xtal * ((config >> 3) + 1) / 2;
    }
#elif defined(RTEMS_BOARD_ESP32P4)
    if (CLOCK_REG(0x5011515CU) & CLOCK_BIT(24)) {
        uint32_t gate = CLOCK_REG(CLOCK_ANALOG_GATE);
        CLOCK_REG(CLOCK_ANALOG_GATE) = gate | CLOCK_BIT(27);
        uint32_t config = clock_analog_read_from(0x63, 2);
        CLOCK_REG(CLOCK_ANALOG_GATE) = gate;
        state->mpll_freq_mhz = xtal * ((config >> 3) + 1) / ((config & 7) + 1);
        /* The IDF configuration path assumes a 20 MHz reference (ref=1).
         * Reject an unrepresentable setting before changing either clock.
         */
        if ((config & 7) != 1 || !state->mpll_freq_mhz || state->mpll_freq_mhz % 20) {
            return false;
        }
    }
#endif
    clock_to_xtal(xtal);
    return true;
}

void CLOCK_SRAM rtems_esp_sleep_clock_before_entry(const struct esp_sleep_clock_state *state, bool deep)
{
#if defined(RTEMS_BOARD_ESP32P4)
    if (!deep && state->mpll_freq_mhz) {
        /* IDF pmu_sleep_start: after PMU setup and cache suspension, before
         * the sleep request. PMU prepare's rev3 bias workaround can enable
         * this gate, so disabling it earlier in clock prepare is wrong.
         */
        CLOCK_REG(0x5011515CU) &= ~CLOCK_BIT(24);
    }
#else
    (void) state;
    (void) deep;
#endif
}

static CLOCK_SRAM void clock_restore_mux(const struct esp_sleep_clock_state *state)
{
#if defined(RTEMS_BOARD_ESP32C3)
    clock_field(CLOCK_PLL_REG, 7, state->mem);
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, 0);
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, state->cpu);
    clock_source(state->source);
    CLOCK_REG(CLOCK_APB_STORE) = state->ahb;
#elif defined(RTEMS_BOARD_ESP32P4)
    /* Returning from XTAL is an upscale. Apply APB -> SYS -> MEM -> CPU
     * with separate update handshakes, then the immediate source mux, as in
     * rtc_clk_cpu_freq_to_cpll_mhz(). Restore actual BSP dividers, not a
     * presumed default CPU frequency or the other chip's topology.
     */
    clock_field(CLOCK_APB_REG, 0xFF0000U, state->apb & 0xFF0000U);
    clock_bus_update();
    clock_field(CLOCK_AHB_REG, 0xFF000000U, state->ahb & 0xFF000000U);
    clock_bus_update();
    clock_field(CLOCK_MEM_REG, 0xFFU, state->mem & 0xFFU);
    clock_bus_update();
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, state->cpu);
    clock_bus_update();
    clock_source(state->source);
#elif defined(RTEMS_BOARD_ESP32S31)
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, state->cpu);
    clock_field(CLOCK_MEM_REG, 1, state->mem & 1);
    clock_field(CLOCK_AHB_REG, 0xFF, state->ahb & 0xFF);
    clock_field(CLOCK_APB_REG, 0xFF, state->apb & 0xFF);
    clock_source(state->source);
    clock_bus_update();
#else
#if defined(RTEMS_BOARD_ESP32C6)
    clock_field(CLOCK_AHB_REG, 0xFF, state->ahb);
    clock_field(CLOCK_CPU_REG, 0x1FFFFU, state->cpu);
#else
    clock_field(CLOCK_CPU_REG, CLOCK_CPU_MASK, state->cpu);
    clock_field(CLOCK_AHB_REG, 0xFF, state->ahb);
#endif
    clock_source(state->source);
#if !defined(RTEMS_BOARD_ESP32C6)
    clock_bus_update();
#endif
#endif
    ets_update_cpu_frequency(state->cpu_freq_mhz);
}

#if defined(RTEMS_BOARD_ESP32S31)
static CLOCK_SRAM void clock_mpll_restore(const struct esp_sleep_clock_state *state)
{
    if (!state->mpll_freq_mhz) {
        return;
    }
    /* S31's eFuse barrier has already run in pmu_finish. Its MPLL dividers
     * are digital, unlike P4; the analog cap/dhref flow is shared in shape.
     */
    CLOCK_REG(0x207040F4U) |= CLOCK_BIT(22);
    CLOCK_REG(0x207040F4U) |= CLOCK_BIT(26) | CLOCK_BIT(30);
    CLOCK_REG(0x20589000U) |= CLOCK_BIT(31);
    CLOCK_REG(0x2070401CU) |= CLOCK_BIT(26) | CLOCK_BIT(30);
    uint32_t gate = CLOCK_REG(CLOCK_ANALOG_GATE);
    CLOCK_REG(CLOCK_ANALOG_GATE) = gate | CLOCK_BIT(2);
    clock_analog_write_to(0x0B, 1, 3, 3);
    clock_analog_write_to(0x0B, 1, CLOCK_BIT(2), CLOCK_BIT(2));
    clock_field(0x20701054U, 0xF8, 9U << 3);
    clock_analog_write_to(0x0B, 3, 0x30, 0x30);
    clock_analog_write_to(0x0B, 1, CLOCK_BIT(2), 0);
    CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(9);
    clock_field(0x20701054U, 0xF8, (state->mpll_freq_mhz / 20 - 1) << 3);
    while (!(CLOCK_REG(CLOCK_CAL_REG) & CLOCK_BIT(8))) {
    }
    CLOCK_REG(CLOCK_CAL_REG) |= CLOCK_BIT(9);
    CLOCK_REG(CLOCK_ANALOG_GATE) = gate;
}
#elif defined(RTEMS_BOARD_ESP32P4)
static CLOCK_SRAM void clock_mpll_restore(const struct esp_sleep_clock_state *state)
{
    uint32_t gate = CLOCK_REG(CLOCK_ANALOG_GATE);
    if (state->mpll_freq_mhz) {
        CLOCK_REG(0x5011515CU) |= CLOCK_BIT(24);
        CLOCK_REG(0x50111040U) |= CLOCK_BIT(28);
        CLOCK_REG(CLOCK_ANALOG_GATE) = gate | CLOCK_BIT(27);
#if BSP_ESP32P4_REVISION == 3
        clock_analog_write_to(0x63, 1, 3, 3);
        ets_delay_us(10);
#endif
    }
    /* PMU finish deliberately leaves the P4 eFuse barrier to this point:
     * enable/cap settling -> eFuse idle -> MPLL configure -> CPU restore.
     * The barrier is also necessary when MPLL was disabled before sleep.
     */
    rtems_esp_sleep_pmu_p4_wait_ready();
    if (state->mpll_freq_mhz) {
#if BSP_ESP32P4_REVISION == 1
        CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(9);
        uint32_t dhref = clock_analog_read_from(0x63, 3);
        clock_analog_write_to(0x63, 3, 0xFF, dhref | 0x30);
        uint32_t rstb = clock_analog_read_from(0x63, 1);
        clock_analog_write_to(0x63, 1, 0xFF, rstb & ~CLOCK_BIT(5));
        clock_analog_write_to(0x63, 1, 0xFF, rstb | CLOCK_BIT(5));
        clock_analog_write_to(0x63, 2, 0xFF, ((state->mpll_freq_mhz / 20 - 1) << 3) | 1);
#else
        clock_analog_write_to(0x63, 1, 3, 3);
        clock_analog_write_to(0x63, 1, CLOCK_BIT(2), CLOCK_BIT(2));
        clock_analog_write_to(0x63, 2, 0xF8, 9U << 3);
        clock_analog_write_to(0x63, 3, 0x30, 0x30);
        clock_analog_write_to(0x63, 1, CLOCK_BIT(2), 0);
        CLOCK_REG(CLOCK_CAL_REG) &= ~CLOCK_BIT(9);
        clock_analog_write_to(0x63, 2, 0xF8, (state->mpll_freq_mhz / 20 - 1) << 3);
#endif
        while (!(CLOCK_REG(CLOCK_CAL_REG) & CLOCK_BIT(8))) {
        }
        CLOCK_REG(CLOCK_CAL_REG) |= CLOCK_BIT(9);
        CLOCK_REG(CLOCK_ANALOG_GATE) = gate;
    }
    /* IDF applies the ECO0 CPLL/SYSPLL correction only after MPLL has
     * finished configuring, before restoring the saved CPU configuration.
     */
    rtems_esp_sleep_pmu_p4_restore_pll_defaults();
}
#endif

void CLOCK_SRAM rtems_esp_sleep_clock_finish(const struct esp_sleep_clock_state *state)
{
    uint32_t source = state->source;
#if defined(RTEMS_BOARD_ESP32C5)
    bool pll = source == 2 || source == 3;
#elif defined(RTEMS_BOARD_ESP32H2)
    (void) source;
    /* BSP flash remains on FLASH_PLL, even when the CPU used XTAL/RC_FAST.
     * Recalibrate the shared BBPLL before any flash access on every return.
     */
    bool pll = true;
#elif defined(RTEMS_BOARD_ESP32S31)
    bool pll = source == 1 || source == 3;
#else
    bool pll = source == 1;
#endif
#if defined(RTEMS_BOARD_ESP32P4) || defined(RTEMS_BOARD_ESP32S31)
    clock_mpll_restore(state);
#endif
    if (pll) {
        clock_pll_restore(state);
    }
    clock_restore_mux(state);
}

#endif
