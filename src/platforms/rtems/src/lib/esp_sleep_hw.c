/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#include "rtc.h"

#if defined(RTEMS_BOARD_ESP32)
#include "esp_sleep_regs.h"
#include <bsp/linker-symbols.h>
#include <rtems.h>

#if defined(RTEMS_BOARD_ESP32S31)
#define ESP_RTC_TIMER_BASE 0x20800000U
#define ESP_RTC_UPDATE_SHIFT 27
#elif defined(RTEMS_BOARD_ESP32P4)
#define ESP_RTC_TIMER_BASE 0x50112000U
#define ESP_RTC_UPDATE_SHIFT 28
#else
#define ESP_RTC_TIMER_BASE 0x600B0C00U
#if defined(RTEMS_BOARD_ESP32C5)
#define ESP_RTC_UPDATE_SHIFT 27
#else
#define ESP_RTC_UPDATE_SHIFT 28
#endif
#endif

RTEMS_INTERRUPT_LOCK_DEFINE(static, rtc_lock, "RTC snapshot")

uint64_t BSP_FAST_TEXT_SECTION rtems_esp_rtc_get_counter(void)
{
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&rtc_lock, &lock);
#if defined(RTEMS_BOARD_ESP32C3)
    volatile uint32_t *time_upd = (volatile uint32_t *) 0x6000800CU;
    *time_upd = (1U << 31);
    for (int i = 0; i < 100; i++) {
        if (*time_upd & (1U << 30)) {
            break;
        }
    }
    uint32_t low = *(volatile uint32_t *) 0x60008010U;
    uint32_t high = *(volatile uint32_t *) 0x60008014U;
#else
    /* ESP-IDF rtc_timer_ll_counter_snapshot(), LP_TIMER/RTC_TIMER buffer 0. */
    volatile uint32_t *update = (volatile uint32_t *) (ESP_RTC_TIMER_BASE + 0x10U);
    *update |= 1U << ESP_RTC_UPDATE_SHIFT;
    uint32_t low = *(volatile uint32_t *) (ESP_RTC_TIMER_BASE + 0x14U);
    uint32_t high = *(volatile uint32_t *) (ESP_RTC_TIMER_BASE + 0x18U);
#endif
    uint64_t rtc_ticks = ((uint64_t) (high & 0xFFFFU) << 32) | low;

    rtems_interrupt_lock_release(&rtc_lock, &lock);
    return rtc_ticks;
}

#if defined(RTEMS_BOARD_ESP32H2)
extern uint32_t Cache_Suspend_ICache(void);
extern void Cache_Resume_ICache(uint32_t autoload);
extern uint32_t esp_rom_get_cpu_ticks_per_us(void);
extern void esp_rom_set_cpu_ticks_per_us(uint32_t ticks);

/* H2 regi2c_ctrl_ll: the PMU (0x6d) and BIAS (0x6a) blocks have separate
 * master selectors and read masks. Wakeup restores BIAS before flash/cache
 * execution is safe, so this complete transaction must remain in SRAM.
 */
static BSP_FAST_TEXT_SECTION void sleep_analog_write(uint32_t block, uint32_t address, uint32_t mask, uint32_t value)
{
    const uintptr_t i2c = 0x600AF800U;
    const uintptr_t clock = 0x600AD008U;
    uint32_t saved_clock = REG(clock);
    REG(clock) = saved_clock | BIT(2);
    uint32_t select = block == 0x6DU ? 12 : 8;
    uint32_t read_bit = block == 0x6DU ? 10 : 6;
    uintptr_t host = i2c + ((REG(i2c + 0x20U) & BIT(select)) ? 0 : 4);
    uint32_t command = block | (address << 8);
    REG(i2c + 0x1CU) = 0xFFFFFFU & ~BIT(read_bit);
    while (REG(host) & BIT(25)) {
    }
    REG(host) = command;
    while (REG(host) & BIT(25)) {
    }
    uint32_t data = (REG(host) >> 16) & 0xFFU;
    REG(host) = command | BIT(24) | (((data & ~mask) | value) << 16);
    while (REG(host) & BIT(25)) {
    }
    REG(clock) = saved_clock;
}

struct usb_jtag_sleep_state
{
    uint32_t clock_enabled;
    uint32_t pad_enabled;
};

struct cpu_clock_state
{
    uint32_t sysclk;
    uint32_t cpu;
    uint32_t ahb;
    uint32_t ticks_per_us;
};

/* ESP-IDF rtc_clk_cpu_set_to_default_config(): run the CPU from the crystal
 * for the sleep transition and restore the PLL dividers on wakeup.  AHB stays
 * at 32 MHz either way: the board divides the 96 MHz PLL by three, and the
 * crystal is 32 MHz.
 */
static BSP_FAST_TEXT_SECTION void cpu_clock_to_xtal(struct cpu_clock_state *state)
{
    state->sysclk = REG(SLEEP_PCR_SYSCLK_CONF);
    state->cpu = REG(SLEEP_PCR_CPU_FREQ_CONF);
    state->ahb = REG(SLEEP_PCR_AHB_FREQ_CONF);
    state->ticks_per_us = esp_rom_get_cpu_ticks_per_us();
    REG(SLEEP_PCR_CPU_FREQ_CONF) = state->cpu & ~0xFFU;
    REG(SLEEP_PCR_AHB_FREQ_CONF) = state->ahb & ~0xFFU;
    REG(SLEEP_PCR_SYSCLK_CONF) = state->sysclk & ~(0x3U << 16);
    REG(SLEEP_PCR_BUS_CLK_UPDATE) = 1U;
    while (REG(SLEEP_PCR_BUS_CLK_UPDATE) != 0U) {
    }
    esp_rom_set_cpu_ticks_per_us(32);
}

static BSP_FAST_TEXT_SECTION void cpu_clock_restore(const struct cpu_clock_state *state)
{
    REG(SLEEP_PCR_CPU_FREQ_CONF) = state->cpu;
    REG(SLEEP_PCR_AHB_FREQ_CONF) = state->ahb;
    REG(SLEEP_PCR_SYSCLK_CONF) = state->sysclk;
    REG(SLEEP_PCR_BUS_CLK_UPDATE) = 1U;
    while (REG(SLEEP_PCR_BUS_CLK_UPDATE) != 0U) {
    }
    esp_rom_set_cpu_ticks_per_us(state->ticks_per_us);
}

/* ESP-IDF's sleep_console_usj_pad_backup_and_disable(): the USB Serial/JTAG
 * pad and its bus clock are disabled to avoid leakage during light sleep,
 * then restored on wakeup. The host may still require a physical reconnect
 * to re-enumerate the device, as documented by ESP-IDF.
 */
static BSP_FAST_TEXT_SECTION void usb_jtag_suspend(struct usb_jtag_sleep_state *state)
{
    uint32_t pcr = REG(PCR_USB_DEVICE_CONF);
    state->clock_enabled = (pcr & PCR_USB_DEVICE_CLK_EN) && !(pcr & PCR_USB_DEVICE_RST_EN);
    if (state->clock_enabled) {
        state->pad_enabled = REG(USB_JTAG_CONF0) & USB_JTAG_PAD_ENABLE;
    } else {
        /* Give the peripheral a defined state before turning it off. */
        REG(PCR_USB_DEVICE_CONF) = (pcr | PCR_USB_DEVICE_CLK_EN) & ~PCR_USB_DEVICE_RST_EN;
        REG(PCR_USB_DEVICE_CONF) = (pcr | PCR_USB_DEVICE_CLK_EN) | PCR_USB_DEVICE_RST_EN;
        REG(PCR_USB_DEVICE_CONF) = (pcr | PCR_USB_DEVICE_CLK_EN) & ~PCR_USB_DEVICE_RST_EN;
        state->pad_enabled = 0;
    }
    REG(USB_JTAG_CONF0) &= ~USB_JTAG_PAD_ENABLE;
    REG(PCR_USB_DEVICE_CONF) &= ~PCR_USB_DEVICE_CLK_EN;
}

static BSP_FAST_TEXT_SECTION void usb_jtag_resume(const struct usb_jtag_sleep_state *state)
{
    REG(PCR_USB_DEVICE_CONF) |= PCR_USB_DEVICE_CLK_EN;
    if (state->pad_enabled) {
        REG(USB_JTAG_CONF0) |= USB_JTAG_PAD_ENABLE;
    }
    if (!state->clock_enabled) {
        REG(PCR_USB_DEVICE_CONF) &= ~PCR_USB_DEVICE_CLK_EN;
    }
}
#endif

uint32_t BSP_FAST_TEXT_SECTION rtems_esp_sleep_enter(bool deep, uint32_t wakeup, uint64_t deadline)
{
#if defined(RTEMS_BOARD_ESP32C3)
    /* RTC memory, the slow clock and analog regulators remain powered. Deep
     * sleep powers down the digital wrapper and restarts through the ROM.
     */
    uint32_t power = REG(0x60008088U);
    uint32_t iso = REG(0x6000808CU);
    uint32_t options = REG(0x60008000U);
    uint32_t clock = REG(0x60008070U);
    REG(0x60008088U) = deep ? ((power & ~BIT(20)) | BIT(31)) : (power & ~(BIT(31) | BIT(29) | BIT(28)));
    if (deep) {
        REG(0x6000808CU) &= ~BIT(31); /* Release digital wrapper isolation to FSM. */
        REG(0x600080C0U) = 0; /* ROM boot, no RTC wake stub. */
    }
    REG(0x60008000U) |= BIT(13) | BIT(11); /* XTAL/PLL force on */
    REG(0x60008070U) |= BIT(28);
    REG(0x6000803CU) = (REG(0x6000803CU) & ~(0x1FFFFU << 15)) | (wakeup << 15);
    REG(0x60008068U) &= ~(0x1FFFFU << 12); /* No sleep rejection sources. */
    REG(0x6000804CU) = 3U;
    REG(0x60008018U) |= BIT(1); /* Clear prior reject cause. */
    REG(0x60008018U) |= BIT(31);
    __asm__ volatile("fence iorw, iorw" ::: "memory");
    while ((REG(0x60008044U) & 3U) == 0) {
        if (rtems_esp_rtc_get_counter() >= deadline) {
            REG(0x60008018U) &= ~BIT(31);
            REG(0x60008088U) = power;
            REG(0x6000808CU) = iso;
            REG(0x60008000U) = options;
            REG(0x60008070U) = clock;
            return 0;
        }
    }
    uint32_t status = REG(0x60008044U);
    uint32_t cause = REG(0x600080F8U);
    REG(0x6000804CU) = 3U;
    REG(0x60008088U) = power;
    REG(0x6000808CU) = iso;
    REG(0x60008000U) = options;
    REG(0x60008070U) = clock;
    return (status & BIT(1)) ? 0 : cause;
#else
    /* The HP active and sleep register groups share their layout. Copy only
     * power/clock/bias/regulator fields, excluding REGDMA backup controls.
     */
#if defined(RTEMS_BOARD_ESP32H2)
    struct usb_jtag_sleep_state usb_jtag = { 0 };
    struct cpu_clock_state cpu_clock;
    /* Complete pmu_init()'s analog regulator ownership handoff. The BSP
     * initializes awake biases; sleep also needs the sleep/force overrides
     * released so the PMU can apply its ACTIVE and SLEEP configurations.
     */
    REG(PMU_BASE + 0x14U) = (REG(PMU_BASE + 0x14U) & ~(BIT(26) | BIT(27))) | BIT(28) | BIT(29) | BIT(30);
    REG(PMU_BASE + 0x18U) = (REG(PMU_BASE + 0x18U) & ~(BIT(30) | BIT(31))) | BIT(24) | BIT(25);
    REG(PMU_BASE + 0x30U) |= BIT(31);
    /* Retain the calibrated LP regulator bias as well as the HP bias copied
     * below. Immediate PLL power-up at boot does not set these FSM fields.
     */
    REG(PMU_BASE + 0xB4U) = REG(PMU_BASE + 0x9CU);
    sleep_analog_write(0x6DU, 8, 0x0FU, 0);
    sleep_analog_write(0x6DU, 9, 0x30U, 0);
    sleep_analog_write(0x6DU, 15, BIT(2), 0);
    cpu_clock_to_xtal(&cpu_clock);
    if (!deep) {
        usb_jtag_suspend(&usb_jtag);
    }
    /* IDF pmu_init() installs separate ACTIVE and SLEEP state defaults.
     * Direct boot bypasses it: explicitly enable the active system clock
     * and release the CPU stall when the PMU returns to ACTIVE.
     */
    REG(PMU_BASE + 0x10U) &= ~BIT(29);
    REG(PMU_BASE + 0x24U) = BIT(27);
    /* PMU_SLEEP_PROTECT_HP_LP_SLEEP; preserve minimum sleep-cycle fields. */
    REG(PMU_BASE + PMU_CTRL + 12U) = (REG(PMU_BASE + PMU_CTRL + 12U) & ~(3U << 16)) | (2U << 16);
#endif
#if defined(RTEMS_BOARD_ESP32S31)
    static const uint8_t offsets[] = { 0, 4, 8, 12, 16, 20, 24, 28, 32, 48, 52, 56, 60 };
    const uint32_t sys = 0x18U, sysclk = 0x30U;
#else
    static const uint8_t offsets[] = { 0, 4, 8, 12, 16, 20, 24, 36, 40, 44, 48 };
    const uint32_t sys = 0x10U, sysclk = 0x24U;
#endif
    for (unsigned i = 0; i < sizeof(offsets); i++) {
        REG(PMU_BASE + PMU_SLEEP + offsets[i]) = REG(PMU_BASE + offsets[i]);
    }
    /* Pause watchdogs, stall CPUs, select the sleep system clock and gate it. */
#if defined(RTEMS_BOARD_ESP32H2)
    /* IDF's HP_SLEEP defaults stall the CPU; the per-sleep configuration
     * adds the pad sleep selection and watchdog pause. ACTIVE releases it.
     */
    REG(PMU_BASE + PMU_SLEEP + sys) |= BIT(27) | BIT(28) | BIT(29);
#else
    REG(PMU_BASE + PMU_SLEEP + sys) |= BIT(28) | BIT(29);
#endif
    REG(PMU_BASE + PMU_SLEEP + sysclk) = BIT(28) | BIT(29);
#if defined(RTEMS_BOARD_ESP32H2)
    /* pmu_sleep_digital_init(): retained functional clocks require the
     * global system-clock gate open. CPU stall still stops execution.
     */
    if (REG(PMU_BASE + PMU_SLEEP + 4U) != 0) {
        REG(PMU_BASE + PMU_SLEEP + sysclk) |= BIT(27);
    }
#endif
    uint32_t forces[PMU_FORCE_COUNT];
    uint32_t memory_force = REG(PMU_BASE + PMU_MEM_FORCE);
    if (deep) {
        REG(PMU_BASE + PMU_MEM_FORCE) &= ~(0xFU << 24);
        /* ROM direct boot leaves force-PU/no-reset/no-isolation asserted.
         * Hand the HP power domains to the PMU before requesting deep sleep.
         */
        for (unsigned i = 0; i < PMU_FORCE_COUNT; i++) {
            uintptr_t reg = PMU_BASE + PMU_FORCE0 + i * 4U;
            forces[i] = REG(reg);
#if defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32H2)
            if (i == 3) { /* Reserved HPPERIPH domain slot. */
                continue;
            }
#endif
            REG(reg) &= ~0x3FU;
        }
#if defined(RTEMS_BOARD_ESP32S31)
        REG(PMU_BASE + PMU_SLEEP) |= BIT(31) | BIT(30) | BIT(29) | BIT(28) | BIT(27) | BIT(21);
#else
        REG(PMU_BASE + PMU_SLEEP) |= BIT(31) | BIT(30) | BIT(29) | BIT(27) | (0xFU << 23);
#endif
        REG(PMU_BASE + PMU_SLEEP + sys) |= BIT(25) | BIT(26); /* Retain pad levels. */
    }
    REG(PMU_BASE + PMU_ICG_UPDATE) = BIT(28);
    REG(SLEEP_MODE) = (REG(SLEEP_MODE) & ~1U) | (deep ? 1U : 0U);
    REG(PMU_BASE + PMU_CTRL + 4U) = 0; /* Reject disabled. */
    REG(PMU_BASE + PMU_CTRL + 8U) = wakeup;
    REG(PMU_BASE + PMU_CLEAR) = BIT(30) | BIT(31);
    REG(PMU_BASE + PMU_CTRL + 16U) = BIT(31);
#if defined(RTEMS_BOARD_ESP32H2)
    /* Drain outstanding flash accesses before gating the system clock.
     * Everything through Cache_Resume_ICache must execute from RAM or ROM.
     */
    uint32_t cache_autoload = Cache_Suspend_ICache();
#endif
    REG(PMU_BASE + PMU_CTRL) |= BIT(31);
    __asm__ volatile("fence iorw, iorw" ::: "memory");
    bool timed_out = false;
    while ((REG(PMU_BASE + PMU_RAW) & (BIT(30) | BIT(31))) == 0) {
        if (rtems_esp_rtc_get_counter() >= deadline) {
            timed_out = true;
            break;
        }
    }
    if (deep) {
        REG(SLEEP_MODE) &= ~1U;
        REG(PMU_BASE + PMU_MEM_FORCE) = memory_force;
        for (unsigned i = 0; i < PMU_FORCE_COUNT; i++) {
            REG(PMU_BASE + PMU_FORCE0 + i * 4U) = forces[i];
        }
    }
    uint32_t status = REG(PMU_BASE + PMU_RAW);
    uint32_t cause = REG(PMU_BASE + PMU_CTRL + 32U);
    REG(PMU_BASE + PMU_CLEAR) = BIT(30) | BIT(31);
#if defined(RTEMS_BOARD_ESP32H2)
    /* pmu_sleep_finish(): BIAS DREG_0P8 is lost during sleep. */
    sleep_analog_write(0x6AU, 0, 0xF0U, 0x80U);
    cpu_clock_restore(&cpu_clock);
    Cache_Resume_ICache(cache_autoload);
    if (!deep) {
        usb_jtag_resume(&usb_jtag);
    }
#endif
    return timed_out || (status & BIT(30)) ? 0 : cause;
#endif
}

#endif
