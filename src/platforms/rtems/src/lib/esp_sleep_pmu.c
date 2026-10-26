/*
 * This file is part of AtomVM.
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-FileCopyrightText: 2020-2026 Espressif Systems (Shanghai) CO LTD
 * SPDX-License-Identifier: Apache-2.0
 */

#include "esp_sleep_pmu.h"

#if defined(RTEMS_BOARD_ESP32)
#include <bsp.h>
#include <bsp/linker-symbols.h>
#include <errno.h>
#include <stddef.h>

/* ESP-IDF 9a97f6c54ec638111ce55cd36581b3c192f15207, port/<chip>/
 * pmu_{init,param,sleep}.c, rtc_{sleep,time}.c and the corresponding LLs.
 * These are FSM configurations, NOT snapshots of the current ACTIVE state.
 * Only calibrated awake voltage fields are inherited from BSP startup.
 * Modem, PVT, CPU/TOP retention DMA and PSRAM are deliberately not supported.
 */
#define PREG(a) (*(volatile uint32_t *) (uintptr_t) (a))
#define PB(n) (UINT32_C(1) << (n))
#define SRAM_TEXT BSP_FAST_TEXT_SECTION __attribute__((noinline))
extern void ets_delay_us(uint32_t us);

static inline __attribute__((always_inline)) void pfield(uintptr_t address, uint32_t mask, uint32_t value)
{
    PREG(address) = (PREG(address) & ~mask) | (value & mask);
}

static uint32_t cycles(uint32_t us, uint32_t period)
{
    return ((uint64_t) us << 19) / period;
}

#if defined(RTEMS_BOARD_ESP32C3)
#define POWER_BASE 0x60008000U
#define TIMG0 0x6001F000U
#define FAST_GATE 0x60008070U
#define CAL_TARGET (TIMG0 + 0x68U)
extern uint8_t rom_chip_i2c_readReg(uint8_t block, uint8_t host, uint8_t address);
extern void rom_chip_i2c_writeReg(uint8_t block, uint8_t host, uint8_t address, uint8_t data);
#elif defined(RTEMS_BOARD_ESP32S31)
#define POWER_BASE 0x20704000U
#define HP_STRIDE 0x40U
#define HP_SYS 0x18U
#define HP_CLK 0x1CU
#define HP_BIAS 0x20U
#define HP_BACKUP 0x24U
#define HP_SYSCLK 0x30U
#define HP_REGULATOR 0x34U
#define HP_DRIVER 0x38U
#define HP_XTAL 0x3CU
#define LP_ACTIVE 0xC0U
#define LP_SLEEP 0xD8U
#define WAIT0 0x114U
#define WAIT1 0x118U
#define CLOCK_WAIT 0x14CU
#define WAKE_CTRL 0x150U
#define SLOW_SELECT 0x20701000U
#define TIMG0 0x20580000U
#define FAST_GATE 0x20589000U
#define FAST_GATE_BIT PB(27)
#define CAL_TARGET 0x20587118U
#elif defined(RTEMS_BOARD_ESP32P4)
#if !defined(BSP_ESP32P4_REVISION) || (BSP_ESP32P4_REVISION != 1 && BSP_ESP32P4_REVISION != 3)
#error "P4 PMU requires BSP_ESP32P4_REVISION=1 or 3"
#endif
#define POWER_BASE 0x50115000U
#define SLOW_SELECT 0x50111000U
#define TIMG0 0x500C2000U
#define FAST_GATE 0x50111028U
#define FAST_GATE_BIT PB(31)
#define CAL_TARGET 0x500E6098U
#else
#define POWER_BASE 0x600B0000U
#define SLOW_SELECT 0x600B0400U
#define FAST_GATE 0x600B0420U
#define FAST_GATE_BIT PB(31)
#if defined(RTEMS_BOARD_ESP32H2)
#define TIMG0 0x60009000U
#define CAL_TARGET 0x6009612CU
#else
#define TIMG0 0x60008000U
#define CAL_TARGET 0x60096130U
#endif
#endif

#if !defined(RTEMS_BOARD_ESP32C3)
#if !defined(RTEMS_BOARD_ESP32S31)
#define HP_STRIDE 0x34U
#define HP_SYS 0x10U
#define HP_CLK 0x14U
#define HP_BIAS 0x18U
#define HP_BACKUP 0x1CU
#define HP_SYSCLK 0x24U
#define HP_REGULATOR 0x28U
#define HP_DRIVER 0x2CU
#define HP_XTAL 0x30U
#define LP_ACTIVE 0x9CU
#define LP_SLEEP 0xB4U
#define WAIT0 0xECU
#define WAIT1 0xF0U
#if defined(RTEMS_BOARD_ESP32C5)
#define CLOCK_WAIT 0x120U
#define WAKE_CTRL 0x124U
#else
#define CLOCK_WAIT 0x11CU
#define WAKE_CTRL 0x120U
#endif
#endif
#define HP_SLEEP (2U * HP_STRIDE)
#define HP(off) (POWER_BASE + (off))
#define SL(off) (POWER_BASE + HP_SLEEP + (off))
#define LP(off) (POWER_BASE + LP_SLEEP + (off))

static inline __attribute__((always_inline)) uint32_t silicon_revision(void)
{
#if defined(RTEMS_BOARD_ESP32C5)
    uint32_t value = PREG(0x600B484CU);
    return ((value >> 4) & 3) * 100 + (value & 15);
#elif defined(RTEMS_BOARD_ESP32C6)
    uint32_t value = PREG(0x600B0850U);
    return ((value >> 22) & 3) * 100 + ((value >> 18) & 15);
#elif defined(RTEMS_BOARD_ESP32H2)
    uint32_t value = PREG(0x600B0850U);
    return ((value >> 21) & 3) * 100 + ((value >> 18) & 7);
#elif defined(RTEMS_BOARD_ESP32P4)
    uint32_t value = PREG(0x5012D04CU);
    return (((value >> 4) & 3) | (((value >> 23) & 1) << 2)) * 100 + (value & 15);
#else
    return 0;
#endif
}

#endif

/* RC_FAST is the PMU work clock even when RTC_FAST selects XTAL. Calibrate
 * this attempt, without modifying the RTC mux/store, TIMG0 counter, or IRQs.
 * A stuck/in-use calibrator is an error, never a guessed nominal frequency.
 */
static uint32_t calibrate(uint32_t xtal_mhz, uint32_t revision, bool fast, uint32_t slow)
{
    const uintptr_t cfg = TIMG0 + 0x68U;
    const uintptr_t timeout = TIMG0 + 0x80U;
    uint32_t old_cfg = PREG(cfg);
    if (old_cfg & PB(31)) {
        return 0;
    }
    uint32_t old_timeout = PREG(timeout);
    if (old_cfg & PB(12)) {
        pfield(timeout, 0xFFFFFF80U, 1U << 7);
        unsigned wait;
        for (wait = 0; wait < 10000; wait++) {
            if ((PREG(cfg) & PB(15)) || (PREG(timeout) & 1U)) {
                break;
            }
            ets_delay_us(1);
        }
        if (wait == 10000) {
            PREG(timeout) = old_timeout;
            return 0;
        }
    }
    uint32_t old_gate = PREG(FAST_GATE);
    uint32_t old_target = PREG(CAL_TARGET);
#if !defined(RTEMS_BOARD_ESP32C3)
    uint32_t old_fosc = PREG(HP(LP_ACTIVE + 0x10));
    if (fast) {
        PREG(HP(LP_ACTIVE + 0x10)) |= PB(30);
        PREG(FAST_GATE) |= FAST_GATE_BIT;
    } else if (slow == 1) {
#if defined(RTEMS_BOARD_ESP32S31)
        PREG(FAST_GATE) |= PB(19);
#else
        PREG(FAST_GATE) |= PB(28);
#endif
    } else if (slow == 2) {
        PREG(FAST_GATE) |= PB(30);
    }
#else
    (void) fast;
    (void) revision;
    PREG(FAST_GATE) |= slow == 1 ? PB(8) : (slow == 2 ? PB(9) : 0);
#endif
    ets_delay_us(50);
    uint32_t divider = 1;
    uint32_t mux = 0;
    uint32_t count = fast ? 2048 : 1024;
#if defined(RTEMS_BOARD_ESP32P4)
    (void) revision;
    divider = fast ? 50 : 1;
    count /= divider;
    uint32_t target = fast ? 7 : (slow == 0 ? 8 : (slow == 1 ? 10 : 9));
    pfield(CAL_TARGET, 0xFFFFFU, target | ((divider - 1) << 4));
    uint32_t old_cal_gate = PREG(0x500E6094U);
    PREG(0x500E6094U) |= PB(31);
#elif defined(RTEMS_BOARD_ESP32S31)
    (void) revision;
    divider = fast ? 50 : 1;
    count /= divider;
    uint32_t target = fast ? 7 : (slow == 0 ? 8 : 10);
    pfield(CAL_TARGET, 0x1FFFFFU, PB(0) | (target << 1) | ((divider - 1) << 5));
#elif defined(RTEMS_BOARD_ESP32C5)
    (void) revision;
    divider = fast ? 32 : 1;
    count /= divider;
    uint32_t target = fast ? 4 : (slow == 0 ? 3 : (slow == 1 ? 1 : 2));
    pfield(CAL_TARGET, 0xFF07U, target | (fast ? 31U << 8 : 0));
    uint32_t old_cal_gate = PREG(0x60096160U);
    PREG(0x60096160U) |= PB(2);
#elif defined(RTEMS_BOARD_ESP32C3)
    mux = slow == 0 ? 0 : (slow == 1 ? 2 : 1);
#else
#if defined(RTEMS_BOARD_ESP32H2)
    bool divided = revision >= 2;
#else
    bool divided = revision >= 1;
#endif
    if (fast && divided) {
        divider = 32;
        count /= divider;
        /* IDF uses the /32 calibration tap with REF_TICK's count at 255. */
        pfield(CAL_TARGET, 0x1FF00U, (255U << 8) | PB(16));
    }
    mux = fast ? 1 : (slow ? 2 : 0);
    uint32_t old_32k = PREG(CAL_TARGET + 4);
    if (!fast && slow) {
        pfield(CAL_TARGET + 4, 3, slow == 1 ? 1 : (slow == 2 ? 0 : 2));
    }
#endif
    /* IDF sleep_modes uses 2048 fast cycles (divided in rtc_clk_cal) and
     * rtc_time.c converts the captured count back before Q19 rounding.
     */
    pfield(cfg, PB(31) | PB(12) | (0x7FFFU << 16) | (3U << 13),
        (count << 16) | (mux << 13));
    /* Maximum 25-bit hardware timeout; bounded software wait also detects
     * an unavailable calibration clock. Slow external clocks get 100 ms.
     */
    pfield(timeout, 0xFFFFFF80U, 0xFFFFFF80U);
    PREG(cfg) |= PB(31);
    uint32_t value = 0;
    for (unsigned wait = 0; wait < 100000; wait++) {
        if (PREG(cfg) & PB(15)) {
            value = (PREG(TIMG0 + 0x6CU) >> 7) / divider;
            break;
        }
        if (PREG(timeout) & 1U) {
            break;
        }
        ets_delay_us(1);
    }
    PREG(cfg) &= ~PB(31);
    PREG(CAL_TARGET) = old_target;
    PREG(FAST_GATE) = old_gate;
#if !defined(RTEMS_BOARD_ESP32C3)
    PREG(HP(LP_ACTIVE + 0x10)) = old_fosc;
#endif
#if defined(RTEMS_BOARD_ESP32P4)
    PREG(0x500E6094U) = old_cal_gate;
#elif defined(RTEMS_BOARD_ESP32C5)
    PREG(0x60096160U) = old_cal_gate;
#elif defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32H2)
    PREG(CAL_TARGET + 4) = old_32k;
#endif
    /* A previous cycling calibration was deliberately stopped, as in IDF.
     * Do not restart it with an old captured result masquerading as ready.
     */
    PREG(cfg) = old_cfg & ~(PB(31) | PB(12));
    PREG(timeout) = old_timeout;
    uint32_t denominator = xtal_mhz * count;
    return value ? (((uint64_t) value << 19) + denominator / 2 - 1) / denominator : 0;
}

/* The ROM only exports C3's analog accessor in all supported BSP maps.
 * Other targets use the same serialized master transactions as regi2c_impl.
 * SRAM is required for H2's post-wake DREG_0P8 correction.
 */
#if defined(RTEMS_BOARD_ESP32C3) || defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32H2) || defined(RTEMS_BOARD_ESP32P4)
static SRAM_TEXT void analog_field(uint32_t block, uint32_t address, uint32_t mask, uint32_t value)
{
#if defined(RTEMS_BOARD_ESP32C3)
    uint32_t data = rom_chip_i2c_readReg(block, 0, address);
    rom_chip_i2c_writeReg(block, 0, address, (data & ~mask) | value);
#else
#if defined(RTEMS_BOARD_ESP32P4)
    const uintptr_t base = 0x50124000U;
    const uintptr_t gate = 0x50120000U;
    const uint32_t gate_bit = PB(27);
    uint32_t select = block == 0x6A ? PB(12) : (block == 0x67 ? PB(11) : (block == 0x63 ? PB(9) : PB(5)));
#else
#if defined(RTEMS_BOARD_ESP32H2)
    const uintptr_t base = 0x600AD800U;
    const uintptr_t gate = 0x600AD008U;
#else
    const uintptr_t base = 0x600AF800U;
    const uintptr_t gate = 0x600AF018U;
#endif
    const uint32_t gate_bit = PB(2);
    uint32_t select = block == 0x6A ? PB(8) : PB(12);
#endif
    uint32_t old_gate = PREG(gate);
    uint32_t old_conf1 = PREG(base + 0x1CU);
    uint32_t old_conf2 = PREG(base + 0x20U);
    PREG(gate) |= gate_bit;
#if defined(RTEMS_BOARD_ESP32P4)
    pfield(base + 0x1CU, 0xFFFFFFU, 0);
    pfield(base + 0x20U, 0xFFFFFFU, select);
    uintptr_t host = base;
#else
    PREG(base + 0x1CU) = 0xFFFFFFU & ~(block == 0x6A ? PB(6) : PB(10));
    uintptr_t host = base + ((old_conf2 & select) ? 0 : 4);
#endif
    while (PREG(host) & PB(25)) {
    }
    PREG(host) = block | (address << 8);
    while (PREG(host) & PB(25)) {
    }
    uint32_t data = ((PREG(host) >> 16) & 0xFFU & ~mask) | value;
    PREG(host) = block | (address << 8) | PB(24) | (data << 16);
    while (PREG(host) & PB(25)) {
    }
    PREG(base + 0x1CU) = old_conf1;
    PREG(base + 0x20U) = old_conf2;
    PREG(gate) = old_gate;
#endif
}
#endif

#if !defined(RTEMS_BOARD_ESP32C3)
static void sleep_bias(uint32_t revision, bool deep, uint32_t *hp, uint32_t *lp, uint32_t *atten)
{
    *hp = deep ? 0 : 1;
    *lp = deep ? 23 : 12;
    *atten = deep ? 12 : 0;
#if defined(RTEMS_BOARD_ESP32C5)
    uint32_t version = PREG(0x600B484CU);
    uint32_t block = ((version >> 11) & 3) * 100 + ((version >> 8) & 7);
    *hp = 0;
    *lp = 15;
    *atten = deep ? 9 : 1;
    if ((revision == 1 && block >= 1) || (revision >= 100 && block >= 2)) {
        uint32_t burn = PREG(0x600B4850U);
        if (deep) {
            *lp = (burn >> 28) | ((PREG(0x600B4854U) & 1U) << 4);
            *atten = (burn >> 24) & 15;
        } else {
            *hp = (burn >> 20) & 15;
            *atten = (burn >> 18) & 3;
        }
    }
#elif defined(RTEMS_BOARD_ESP32C6)
    (void) revision;
    uint32_t version = PREG(0x600B0850U);
    uint32_t block = (version >> 30) * 100 + ((version >> 27) & 7);
    if (block >= 3) {
        uint32_t burn = PREG(0x600B084CU);
        if (deep) {
            *lp = ((burn >> 19) & 15) + 23;
            *atten = ((burn >> 16) & 7) + 8;
        } else {
            *hp = (burn >> 12) & 15;
            *atten = (burn >> 10) & 3;
        }
    }
#elif defined(RTEMS_BOARD_ESP32H2)
    (void) revision;
    uint32_t version = PREG(0x600B086CU);
    uint32_t block = ((version >> 5) & 3) * 100 + ((version >> 2) & 7);
    uint32_t burn = (PREG(0x600B084CU) >> 27) & 15U;
    *lp = block >= 3 && burn ? burn : 6;
    *atten = 0;
#else
    (void) revision;
#endif
}

static void force_handoff(void)
{
    /* Low six bits are force reset/iso/pu/noreset/noiso/pd, not domain masks.
     * P4's 0x100..108 are MASK registers and must NOT be treated as domains.
     */
#if defined(RTEMS_BOARD_ESP32P4)
    pfield(HP(0xF4), 0x3F, 0);
    pfield(HP(0xF8), 0x3F, 0);
    pfield(HP(0xFC), 0x3F, 0);
    pfield(HP(0x10C), PB(2) | PB(5), 0); /* DCDC switch FSM ownership */
    pfield(HP(0x110), 0x3F, 0);
#if BSP_ESP32P4_REVISION == 3
    pfield(HP(0x218), 0x3F, 0);
#endif
#elif defined(RTEMS_BOARD_ESP32S31)
    for (unsigned off = 0x120; off <= 0x138; off += 4) {
        pfield(HP(off), 0x3F, 0);
    }
    pfield(HP(0x13C), 15U << 24, 0);
#elif defined(RTEMS_BOARD_ESP32C5)
    for (unsigned off = 0xF8; off <= 0x10C; off += 4) {
        pfield(HP(off), 0x3F, 0);
    }
    pfield(HP(0x110), 15U << 24, 0);
#else
    pfield(HP(0xF4), 0x3F, 0);
    pfield(HP(0xF8), 0x3F, 0);
    pfield(HP(0xFC), 0x3F, 0);
    pfield(HP(0x104), 0x3F, 0);
    pfield(HP(0x108), 0x3F, 0);
    pfield(HP(0x10C), 15U << 24, 0);
#endif
}

static void fsm_init(struct rtems_esp_sleep_pmu_state *s, bool xtal, bool fosc,
    uint32_t slow, uint32_t hp_bias, uint32_t lp_bias, uint32_t atten)
{
    uint32_t active_pll, isolated_pll, modem;
#if defined(RTEMS_BOARD_ESP32S31)
    active_pll = PB(22) | (3U << 23) | (3U << 27); /* CPLL and BBPLL */
    isolated_pll = PB(20) | PB(21);
    modem = 2U << 30;
#elif defined(RTEMS_BOARD_ESP32P4)
    active_pll = (15U << 23) | (15U << 27);
    isolated_pll = PB(21) | PB(22);
    modem = 2U << 30;
#else
    active_pll = PB(28) | PB(29) | PB(30);
    isolated_pll = PB(26) | PB(27);
    modem = 2U << 30;
#endif
    /* ACTIVE and unused MODEM are explicit too. No modem state is entered,
     * no backup/retention enables are installed. Preserve voltage calibration.
     */
    for (unsigned mode = 0; mode < 3; mode++) {
        uintptr_t base = HP(mode * HP_STRIDE);
        bool sleep = mode == 2;
        uint32_t digital = 0;
        if (sleep && s->deep) {
#if defined(RTEMS_BOARD_ESP32P4)
            digital = PB(21) | PB(23) | PB(30) | PB(31);
#if BSP_ESP32P4_REVISION == 3
            digital |= PB(29);
#endif
#elif defined(RTEMS_BOARD_ESP32S31)
            digital = PB(20) | PB(21) | PB(27) | PB(28) | PB(29) | PB(30) | PB(31);
#else
            digital = PB(21) | PB(27) | PB(29) | PB(30) | PB(31);
#if defined(RTEMS_BOARD_ESP32H2)
            digital &= ~PB(30); /* H2 has no HP_AON power-down field */
#endif
#endif
        }
        PREG(base) = digital;
        PREG(base + HP_CLK) = sleep ? isolated_pll : active_pll;
        bool hp_xtal = xtal;
#if defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32H2)
        /* Match this IDF revision: H2 has no XTAL_AS_RTC_FAST override;
         * C6 modifies power_default only after config->power was copied.
         */
        hp_xtal = false;
#endif
        PREG(base + HP_XTAL) = !sleep || hp_xtal ? PB(31) : 0;
        PREG(base + 4) = sleep ? 0 : UINT32_MAX;
        PREG(base + 8) = sleep ? 0 : UINT32_MAX;
#if defined(RTEMS_BOARD_ESP32S31)
        PREG(base + 0xC) = sleep ? 0 : UINT32_MAX;
        PREG(base + 0x10) = sleep ? 0 : UINT32_MAX;
        PREG(base + 0x14) = sleep ? 0 : modem;
#else
        PREG(base + 0xC) = sleep ? 0 : modem;
#endif
        PREG(base + HP_SYSCLK) = sleep ? PB(28) | PB(29) : PB(27);
        uint32_t sys = PB(24) | PB(27) | PB(28) | PB(29);
#if defined(RTEMS_BOARD_ESP32P4)
        sys &= ~PB(27); /* P4 pad selection is immediate, before entry */
#elif defined(RTEMS_BOARD_ESP32H2)
        if (s->deep) {
            sys &= ~PB(27);
        }
#elif defined(RTEMS_BOARD_ESP32S31)
        if (s->deep) {
            sys = (sys & ~PB(27)) | PB(25) | PB(26);
        } else {
            sys |= PB(23); /* C_CHANNEL */
        }
#endif
        PREG(base + HP_SYS) = sleep ? sys : 0;
        PREG(base + HP_BACKUP) = 0;
        PREG(base + HP_BACKUP + 4) = 0;
#if defined(RTEMS_BOARD_ESP32S31)
        PREG(base + HP_BACKUP + 8) = 0;
#endif
        if (!sleep) {
            /* Preserve BSP's calibrated DBIAS and regulator ownership bits.
             * Bias sleep/current-off and attenuation are not awake defaults.
             */
            pfield(base + HP_BIAS, 0xFE000000U, PB(25));
#if defined(RTEMS_BOARD_ESP32H2)
            PREG(base + HP_BIAS) |= PB(24); /* XPD_TRX */
#endif
        }
    }
    uint32_t oscillators = slow == 1 ? PB(28) : 0;
#if !defined(RTEMS_BOARD_ESP32C5) && !defined(RTEMS_BOARD_ESP32S31)
    if (slow == 2) {
        oscillators |= PB(29);
    }
#endif
    PREG(HP(LP_ACTIVE + 0xC)) = 0;
    PREG(HP(LP_ACTIVE + 0x10)) = oscillators | PB(30)
#if defined(RTEMS_BOARD_ESP32P4)
        | PB(27)
#endif
        ;
    PREG(LP(0xC)) = 0; /* RTC memory/peripheral/slow source retained */
#if defined(RTEMS_BOARD_ESP32H2)
    PREG(LP(0xC)) = PB(30); /* memory deep-sleep bias, contents retained */
#endif
    PREG(LP(0x10)) = oscillators | (fosc ? PB(30) : 0);
    PREG(LP(8)) = 0; /* PD_XTAL: RTC_FAST dependency keeps HP XTAL only */
    uint32_t hp_analog = PB(30) | PB(31);
    uint32_t lp_analog = PB(30) | PB(31) | (atten << 26);
#if defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32H2)
    if (s->deep) {
        hp_analog = 0;
    }
#endif
    if (!s->deep) {
        hp_analog |= atten << 26;
        if (fosc) {
            hp_bias = s->active_regulator >> 27;
            lp_bias = s->lp_active_regulator >> 27;
            lp_analog &= ~(15U << 26);
        }
    }
#if !defined(RTEMS_BOARD_ESP32H2)
    if (xtal) {
        hp_analog &= ~(PB(30) | PB(31) | (15U << 26));
#if defined(RTEMS_BOARD_ESP32P4)
        hp_bias = 24; /* IDF HP_CALI_ACTIVE_DBIAS_DEFAULT, not awake LDO */
#elif !defined(RTEMS_BOARD_ESP32S31)
        hp_bias = s->active_regulator >> 27;
#endif
    }
#endif
#if defined(RTEMS_BOARD_ESP32C5)
    if (!s->deep && fosc) {
        hp_analog = 0;
        lp_analog = 0;
    }
#endif
#if defined(RTEMS_BOARD_ESP32S31)
    if (fosc) {
        hp_bias = s->active_regulator >> 27;
        lp_bias = s->lp_active_regulator >> 27;
        hp_analog &= ~(15U << 26);
        lp_analog &= ~(15U << 26);
    }
    if (xtal) {
        hp_analog |= PB(31); /* XTAL_FAST clears PD_CUR, not BIAS_SLEEP */
    }
    uint32_t flash_config = PREG(0x20715048U);
    bool flash_1v8 = (flash_config & PB(13)) ? !(flash_config & PB(14)) : !(PREG(0x20583068U) & PB(4));
    if (!s->deep && flash_1v8) {
        hp_analog &= ~PB(30);
        lp_analog &= ~PB(30);
    }
#endif
    PREG(SL(HP_BIAS)) = hp_analog;
    PREG(SL(HP_REGULATOR)) = (s->deep ? 0 : PB(18)) | (hp_bias << 27);
    PREG(SL(HP_DRIVER)) = 0;
    PREG(LP(0)) = PB(22) | ((lp_bias & 31U) << 27);
    PREG(LP(4)) = 0;
    PREG(LP(0x14)) = lp_analog;
#if defined(RTEMS_BOARD_ESP32H2)
    if (!s->deep) {
        PREG(SL(HP_DRIVER)) = 0xFFFFF800U;
    } else {
        PREG(LP(4)) = 7U << 28;
    }
#elif defined(RTEMS_BOARD_ESP32P4)
    /* DCDC, not HP LDO, supplies the retained light-sleep domains. Flash
     * supply remains on (XPD_0P1A); no PSRAM or ADC monitor exceptions.
     */
    pfield(SL(HP_BIAS), (31U << 18) | (3U << 23), s->deep ? 0 : (14U << 18) | PB(23));
    pfield(HP(HP_BIAS), 3U << 23, s->deep ? 0 : PB(23));
    pfield(SL(HP_REGULATOR), PB(18) | (15U << 19),
        !s->deep && s->chip_revision >= 300 ? PB(22) : 0);
    PREG(HP(LP_ACTIVE)) = PB(22) | ((s->deep ? 26U : 29U) << 27);
    PREG(HP(LP_ACTIVE + 4)) = s->deep ? 7U << 26 : 0;
    if (!s->deep && !fosc) {
        PREG(LP(0)) = PB(22) | (12U << 27);
    }
#endif
    force_handoff();
    pfield(HP(WAKE_CTRL + 0xC), 3U << 16, 2U << 16); /* HP_LP_SLEEP */
#if defined(RTEMS_BOARD_ESP32S31)
    PREG(HP(0x104)) |= PB(31); /* update modem ICG code */
    PREG(HP(0xF8)) |= PB(28); /* update ICG switch */
#else
    PREG(HP(0xDC)) |= PB(31);
    PREG(HP(0xD0)) |= PB(28);
#endif
}

static int timing_init(struct rtems_esp_sleep_pmu_state *s, uint32_t slow_period, bool xtal)
{
    uint32_t fast = s->fast_period;
    uint32_t min = cycles(450, slow_period);
    uint32_t lp_analog = cycles(154, slow_period);
    uint32_t hp_analog = cycles(154, fast);
    uint32_t supply = cycles(2, fast);
    uint32_t hp_up = supply;
#if defined(RTEMS_BOARD_ESP32C6)
    if (s->deep) {
        lp_analog = cycles(500, slow_period);
    }
#elif defined(RTEMS_BOARD_ESP32P4)
    hp_analog = cycles(s->deep ? 260 : 945, slow_period);
    hp_up = cycles(26, fast);
#endif
    bool xtal_slow_wait = xtal;
#if defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32H2)
    xtal_slow_wait = false; /* See the chip-specific power_default note above. */
#endif
    uint32_t xtal_wait = cycles(250, xtal_slow_wait ? slow_period : fast);
    uint32_t pll_wait = cycles(50, fast);
    if (min > 255 || lp_analog > 255 || hp_analog > 65535 || supply > 127 || hp_up > 511 || xtal_wait > 65535 || pll_wait > 65535) {
        return ERANGE;
    }
    pfield(HP(WAKE_CTRL + 0xC), 0xFFFFU, min | (min << 8));
    pfield(HP(WAKE_CTRL + 0x14), 0xFF000000U, lp_analog << 24);
    pfield(HP(WAKE_CTRL + 0x1C), 0xFFFF0000U, hp_analog << 16);
    uint32_t hp_supply = supply;
#if defined(RTEMS_BOARD_ESP32C5)
    if (s->chip_revision >= 102) {
        hp_supply |= 3U; /* POWER_DOMAIN_INRUSH_CURRENT_WORKAROUND */
    }
#endif
    pfield(HP(WAIT0), (511U << 14) | (511U << 23), (hp_up << 14) | (hp_supply << 23));
#if defined(RTEMS_BOARD_ESP32P4)
    pfield(HP(WAIT1), (511U << 14) | (511U << 23), (supply << 14) | (supply << 23));
#else
    pfield(HP(WAIT1), (127U << 16) | (511U << 23), (supply << 16) | (supply << 23));
#endif
#if defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32S31)
    uint32_t control = cycles(1, fast);
    PREG(HP(WAIT1 + 4)) = control * 0x01010101U;
#endif
    PREG(HP(CLOCK_WAIT)) = xtal_wait | (pll_wait << 16);
    /* Deadline guard only, not a rollback timeout or promised wake latency.
     * Parent adds its measured software entry overhead and checks again after
     * before_entry. At least one slow-domain synchronization cycle is needed.
     */
    s->minimum_entry_us = 450 + ((slow_period + ((1U << 19) - 1)) >> 19);
    return 0;
}
#endif

int rtems_esp_sleep_pmu_prepare(struct rtems_esp_sleep_pmu_state *s,
    bool deep, uint32_t wakeup, uint32_t xtal_mhz)
{
    if (s == NULL || xtal_mhz < 16 || xtal_mhz > 48) {
        return EINVAL;
    }
    *s = (struct rtems_esp_sleep_pmu_state) { .deep = deep };
    (void) wakeup; /* Wake/reject masks and RTC GPIO are owned by caller. */
#if defined(RTEMS_BOARD_ESP32C3)
    uint32_t slow = (PREG(POWER_BASE + 0x70U) >> 30) & 3U;
    if (slow > 2) {
        return ENOTSUP;
    }
    s->slow_period = calibrate(xtal_mhz, 0, false, slow);
    if (!s->slow_period) {
        return EIO;
    }
    uint32_t slow_period = s->slow_period;
    s->c3_analog = PREG(POWER_BASE + 0x34U);
    s->c3_ulp = rom_chip_i2c_readReg(0x61, 0, 0);
    bool fosc = slow == 2;
    uint32_t dbias = deep ? (fosc ? 0 : 25) : (fosc ? 0 : 5);
    uint32_t atten = deep ? 15 : (fosc ? 0 : 5);
    uint32_t ver = PREG(0x60008858U);
    uint32_t revision = ((ver >> 24) & 3U) * 100
        + (((ver >> 23) & 1U) << 3) + ((PREG(0x60008850U) >> 18) & 7U);
    if (fosc || (deep && revision <= 2)) {
        atten = 0;
    }
    /* NO_ULTRA_LOW: RTC GPIO inputs/memory remain valid at high temperature. */
    analog_field(0x6D, 13, PB(2) | PB(3), 0);
    analog_field(0x6D, 5, 31, dbias);
    analog_field(0x6D, 7, 31, deep ? 0 : dbias);
    pfield(POWER_BASE + 0x7CU, (15U << 22) | (15U << 18) | (15U << 14),
        (atten << 18) | PB(16) | PB(14));
    if (!deep) {
        pfield(POWER_BASE + 0x7CU, 0x1FFU, PB(8) | 254U);
    }
    pfield(POWER_BASE + 0x88U,
        0xF8000000U | PB(22) | PB(20) | PB(18) | PB(16) | PB(14) | PB(12) | PB(4),
        (deep ? PB(31) | PB(29) | PB(28) : 0) | PB(30) | PB(27) | PB(16) | PB(4));
    pfield(POWER_BASE + 0x8CU, PB(31) | PB(29) | PB(27) | PB(25) | PB(23) | PB(14) | PB(12), 0);
    PREG(POWER_BASE + 0x80U) |= PB(31); /* RTC regulator forced on */
    pfield(POWER_BASE + 0x70U, PB(26) | PB(16) | PB(28), fosc ? PB(26) | PB(16) : 0);
    PREG(POWER_BASE + 0xA8U) = 0x50D83AA1U;
    PREG(POWER_BASE + 0x90U) |= PB(9); /* pause RTC WDT in sleep */
    PREG(POWER_BASE + 0xA8U) = 0;
    pfield(POWER_BASE + 0x78U, PB(22) | PB(21), deep ? PB(21) : 0);
    PREG(POWER_BASE + 0x68U) |= PB(31) | PB(30);
    PREG(POWER_BASE) &= ~(PB(13) | PB(11) | PB(9) | PB(7));
    if (deep) {
        analog_field(0x61, 0, PB(2), 0);
        PREG(POWER_BASE + 0x34U) &= ~(PB(31) | PB(30) | PB(28) | PB(27));
        PREG(POWER_BASE + 0xC0U) = 0; /* ROM reboot, no retained wake stub */
    }
    /* rtc_init's digital-domain FSM waits (no boot clock or o-code port). */
    PREG(POWER_BASE + 0x24U) = (1U << 25) | (1U << 16) | (1U << 9) | 1U;
    PREG(POWER_BASE + 0x28U) = (1U << 25) | (1U << 16) | (1U << 9) | 1U;
    pfield(POWER_BASE + 0x2CU, 255U << 8, 2U << 8);
    pfield(POWER_BASE + 0x30U, (127U << 25) | (511U << 16), (1U << 25) | (1U << 16));
    pfield(POWER_BASE + 0x1CU, (255U << 24) | (1023U << 14) | (255U << 6),
        ((deep ? 20U : 1U) << 24) | ((deep ? 100U : cycles(250, slow_period)) << 14) | ((deep ? 20U : 4U) << 6));
    s->minimum_entry_us = 450;
#else
    s->chip_revision = silicon_revision();
#if defined(RTEMS_BOARD_ESP32P4)
    if ((s->chip_revision >= 300) != (BSP_ESP32P4_REVISION == 3)) {
        return ENOTSUP; /* CPU force-register layout differs across ECO3. */
    }
#endif
    s->active_bias = PREG(HP(HP_BIAS));
    s->active_regulator = PREG(HP(HP_REGULATOR));
    s->lp_active_regulator = PREG(HP(LP_ACTIVE));
    s->lp_active_driver = PREG(HP(LP_ACTIVE + 4));
    uint32_t select = PREG(SLOW_SELECT);
#if defined(RTEMS_BOARD_ESP32S31)
    uint32_t slow = (select >> 23) & 7U;
    uint32_t fast = (select >> 26) & 3U;
    if (slow > 1 || fast > 1) {
        return ENOTSUP;
    }
#else
    uint32_t slow = select & 3U;
    uint32_t fast = (select >> 2) & 3U;
#if defined(RTEMS_BOARD_ESP32C5)
    if (slow == 2 || fast > 2) {
#elif defined(RTEMS_BOARD_ESP32P4)
    if (slow == 3 || fast > 1) {
#else
    if (fast > 1) {
#endif
        return ENOTSUP;
    }
#endif
    /* RTC_FAST is needed again after wake, not throughout sleep: only its
     * XTAL dependency is held in HP_SLEEP, as in IDF. No LP peripheral uses
     * RC_FAST in this baseline, and none uses XTAL as an independent source.
     */
    bool xtal = fast != 0;
    bool fosc = false;
    s->slow_period = calibrate(xtal_mhz, s->chip_revision, false, slow);
    s->fast_period = calibrate(xtal_mhz, s->chip_revision, true, slow);
    if (!s->slow_period || !s->fast_period) {
        return EIO;
    }
    int error = timing_init(s, s->slow_period, xtal);
    if (error) {
        return error;
    }
    uint32_t hp, lp, atten;
    sleep_bias(s->chip_revision, deep, &hp, &lp, &atten);
    fsm_init(s, xtal, fosc, slow, hp, lp, atten);
#if defined(RTEMS_BOARD_ESP32C6)
    analog_field(0x6D, 5, PB(7), PB(7));
    analog_field(0x6D, 7, PB(7), PB(7));
    analog_field(0x6D, 13, PB(2) | PB(3), 0);
#elif defined(RTEMS_BOARD_ESP32H2)
    analog_field(0x6D, 8, 15, 0);
    analog_field(0x6D, 9, 0x30, 0);
    analog_field(0x6D, 15, PB(2), 0);
    analog_field(0x6A, 0, 0xF0, 0x80);
#elif defined(RTEMS_BOARD_ESP32P4) && BSP_ESP32P4_REVISION == 3
    PREG(HP(0x15C)) |= PB(24); /* MSPI_PHY_XPD: gate belongs to MPLL */
    analog_field(0x6A, 0, 0xF0, 12U << 4);
    analog_field(0x6A, 1, 15, 12);
    analog_field(0x63, 1, 3, 3); /* MPLL IR_CAL_EXT_CAP */
    pfield(0x50110180U, 7, 4); /* HP MEM_AUX_DEEPSLEEP */
    pfield(0x50110184U, 7, 4); /* LP MEM_AUX_DEEPSLEEP */
#elif defined(RTEMS_BOARD_ESP32S31)
    /* regi2c_ctrl_ll_i2c_sar_periph_enable(), no PVT setup. */
    PREG(HP(0x184)) &= ~PB(31);
    ets_delay_us(1);
    PREG(HP(0x184)) |= PB(30) | PB(31);
#endif
#endif
    s->prepared = true;
    return 0;
}

void SRAM_TEXT rtems_esp_sleep_pmu_before_entry(struct rtems_esp_sleep_pmu_state *s)
{
    if (!s->prepared || s->entry_prepared) {
        return;
    }
#if defined(RTEMS_BOARD_ESP32P4)
    if (s->deep) {
        pfield(HP(HP_REGULATOR), PB(18) | (31U << 27), PB(18) | (30U << 27));
        pfield(HP(HP_BIAS), 31U << 18, 24U << 18);
        ets_delay_us(185);
        PREG(HP(0x204)) |= PB(1) | PB(7); /* DCDC off + done force */
        pfield(HP(HP_REGULATOR), 31U << 27, 24U << 27);
    }
    /* Caller performs register-only L1 writeback at its final sleep-enable
     * site, after deadline/alarm accesses. Returning from here can spill.
     */
    PREG(HP(0xE4)) |= PB(26); /* pads to sleep before hardware holds them */
#endif
    s->entry_prepared = true;
}

void SRAM_TEXT rtems_esp_sleep_pmu_finish(struct rtems_esp_sleep_pmu_state *s,
    bool sleep_requested, bool rejected)
{
    if (!s->prepared) {
        return;
    }
    (void) rejected;
#if defined(RTEMS_BOARD_ESP32P4)
    if (s->deep && s->entry_prepared) {
        pfield(HP(HP_BIAS), 31U << 18, 27U << 18);
        pfield(HP(0x10C), PB(2) | PB(5), 0);
        pfield(HP(0x204), PB(7), 0);
        PREG(HP(0x204)) |= PB(0);
        /* Pre-entry abort also bypassed the hardware DCDC-on sequence. */
        ets_delay_us(950);
        PREG(HP(HP_REGULATOR)) &= ~PB(18);
    }
    PREG(HP(HP_BIAS)) = s->active_bias;
    PREG(HP(HP_REGULATOR)) = s->active_regulator;
    PREG(HP(LP_ACTIVE)) = s->lp_active_regulator;
    PREG(HP(LP_ACTIVE + 4)) = s->lp_active_driver;
    /* P4 eFuse barrier is in p4_wait_ready, called by clock_finish AFTER
     * MPLL enable/cap settling and BEFORE frequency/CPU restoration. */
    (void) sleep_requested;
#elif defined(RTEMS_BOARD_ESP32C5)
    if (sleep_requested) {
        while ((PREG(0x600B49D4U) & 15U) != 1) {
        }
    }
#elif defined(RTEMS_BOARD_ESP32C6)
    if (sleep_requested) {
        while ((PREG(0x600B09D0U) & 15U) != 1) {
        }
    }
#elif defined(RTEMS_BOARD_ESP32S31)
    if (sleep_requested) {
        while ((PREG(0x207151D4U) & 15U) != 1) {
        }
    }
#elif defined(RTEMS_BOARD_ESP32H2)
    if (sleep_requested) {
        analog_field(0x6A, 0, 0xF0, 0x80);
    }
#else
    (void) sleep_requested;
    if (s->deep) {
        PREG(POWER_BASE + 0x34U) = s->c3_analog;
        analog_field(0x61, 0, PB(2), s->c3_ulp & PB(2));
    }
#endif
    s->prepared = false;
}

void SRAM_TEXT rtems_esp_sleep_pmu_p4_wait_ready(void)
{
#if defined(RTEMS_BOARD_ESP32P4)
    PREG(HP(0xE4)) |= PB(27); /* pad selection low, AFTER MPLL enable/cap */
    while ((PREG(0x5012D1D0U) & 15U) != 1) {
    }
#endif
}

void SRAM_TEXT rtems_esp_sleep_pmu_p4_restore_pll_defaults(void)
{
#if defined(RTEMS_BOARD_ESP32P4)
    if (silicon_revision() < 1) {
        analog_field(0x67, 3, 0xFF, 6); /* ECO0 CPLL -> 400 MHz */
        analog_field(0x66, 3, 0xFF, 8); /* ECO0 SYSPLL -> 480 MHz */
    }
#endif
}
#endif
