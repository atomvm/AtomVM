/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Riccardo Binetti <rbino@gmx.com>
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

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

#include <libopencm3/cm3/cortex.h>
#include <libopencm3/cm3/nvic.h>
#include <libopencm3/cm3/scb.h>
#include <libopencm3/cm3/systick.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/usart.h>

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <module.h>
#include <utils.h>

#include "lib/avm_devcfg.h"
#include "lib/avm_log.h"
#include "lib/stm_sys.h"

#define USART_CONSOLE (AVM_CONSOLE)
#define USART_TX (AVM_CONSOLE_TX)
#define USART_GPIO (AVM_CONSOLE_GPIO)
#define USART_RCC (AVM_CONSOLE_RCC)
#define AVM_ADDRESS (AVM_APP_ADDRESS)
#define AVM_FLASH_END (CFG_FLASH_END)
#define CLOCK_FREQUENCY (AVM_CLOCK_HZ)

#define TAG "AtomVM"

#define ATOMVM_BANNER                                                   \
    "\n"                                                                \
    "    ###########################################################\n" \
    "\n"                                                                \
    "       ###    ########  #######  ##     ## ##     ## ##     ## \n" \
    "      ## ##      ##    ##     ## ###   ### ##     ## ###   ### \n" \
    "     ##   ##     ##    ##     ## #### #### ##     ## #### #### \n" \
    "    ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ## \n" \
    "    #########    ##    ##     ## ##     ##  ##   ##  ##     ## \n" \
    "    ##     ##    ##    ##     ## ##     ##   ## ##   ##     ## \n" \
    "    ##     ##    ##     #######  ##     ##    ###    ##     ## \n" \
    "\n"                                                                \
    "    ###########################################################\n" \
    "\n"

int _write(int file, char *ptr, int len);
pid_t _getpid(void);
int _kill(pid_t pid, int sig);

// setup errno to work with newlib
#undef errno
extern int errno;

static void clock_setup()
{
    // Setup external clock, set divider for device clock frequency
    rcc_clock_setup_pll(AVM_CLOCK_CONFIGURATION);
}

static void usart_setup(GlobalContext *glb)
{
    // Enable clock for USART
    rcc_periph_clock_enable(USART_RCC);

    // Setup GPIO pins for USART transmit
    gpio_mode_setup(USART_GPIO, GPIO_MODE_AF, GPIO_PUPD_NONE, USART_TX);
    sys_lock_pin(glb, USART_GPIO, USART_TX);

    // Setup USART TX pin as alternate function
    gpio_set_af(USART_GPIO, GPIO_AF7, USART_TX);

    usart_set_baudrate(USART_CONSOLE, 115200);
    usart_set_databits(USART_CONSOLE, 8);
    usart_set_stopbits(USART_CONSOLE, USART_STOPBITS_1);
    usart_set_mode(USART_CONSOLE, USART_MODE_TX);
    usart_set_parity(USART_CONSOLE, USART_PARITY_NONE);
    usart_set_flow_control(USART_CONSOLE, USART_FLOWCONTROL_NONE);

    // Finally enable the USART
    usart_enable(USART_CONSOLE);
}

// Set up a timer to create 1ms ticks
// The handler is in sys.c
static void systick_setup()
{
    // ((clock rate / 1000) - 1) to get 1ms interrupt rate
    systick_set_clocksource(STK_CSR_CLKSOURCE_AHB);
    systick_set_reload((CLOCK_FREQUENCY / 1000U) - 1U);
    systick_clear();
    systick_counter_enable();
    systick_interrupt_enable();
}

// Use USART_CONSOLE as a console.
// This is a syscall for newlib
int _write(int file, char *ptr, int len)
{
    int i;

    if (file == STDOUT_FILENO || file == STDERR_FILENO) {
        for (i = 0; i < len; i++) {
            if (ptr[i] == '\n') {
                usart_send_blocking(USART_CONSOLE, '\r');
            }
            usart_send_blocking(USART_CONSOLE, ptr[i]);
        }
        return i;
    }
    errno = EIO;
    return -1;
}

// newlib stubs to support AVM_ABORT
pid_t _getpid()
{
    return 1;
}

int _kill(pid_t pid, int sig)
{
    UNUSED(pid);
    if (sig == SIGABRT) {
        fprintf(stderr, "Aborted\n");
    } else {
        fprintf(stderr, "Unknown signal %d\n", sig);
    }
    errno = EINVAL;
    return -1;
}

// Redefine weak linked while(1) loop from libopencm3/cm3/nvic.h.
void hard_fault_handler()
{
    fprintf(stderr, "\nHard Fault detected!\n");
    AVM_ABORT();
}

/* define newlib weakly defined functions to prevent compiler warnings
 * Reference: https://sourceware.org/newlib/libc.html#Stubs
 * These are minimal non-functional implementations that can be modified as needed to
 * implement functionality.
 */

int _close(int file)
{
    UNUSED(file);
    return -1;
}

int _fstat_r(int file, struct stat *st)
{
    UNUSED(file);
    st->st_mode = S_IFCHR;
    return 0;
}

int _isatty(int file)
{
    UNUSED(file);
    return 1;
}

off_t _lseek_r(int file, off_t ptr, int dir)
{
    UNUSED(file);
    UNUSED(ptr);
    UNUSED(dir);
    return 0;
}

int open(const char *name, int flags, int mode)
{
    UNUSED(name);
    UNUSED(flags);
    UNUSED(mode);
    return -1;
}

int _read_r(int file, void *ptr, size_t len)
{
    UNUSED(file);
    UNUSED(ptr);
    UNUSED(len);
    return 0;
}

int unlink(const char *name)
{
    UNUSED(name);
    errno = ENOENT;
    return -1;
}

int main()
{
    // Flash cache must be enabled before system clock is activated
    sys_enable_flash_cache();
    sys_init_icache();
    clock_setup();
    systick_setup();
    // Start core peripheral clocks now so there are no accidental resets of peripherals that share a clock later.
    sys_enable_core_periph_clocks();

    GlobalContext *glb = globalcontext_new();

    usart_setup(glb);

    fprintf(stdout, "%s", ATOMVM_BANNER);
    AVM_LOGI(TAG, "Starting AtomVM revision " ATOMVM_VERSION);
    AVM_LOGD(TAG, "Using usart mapped at register 0x%x for stdout/stderr.", USART_CONSOLE);

    const void *flashed_avm = (void *) AVM_ADDRESS;
    uint32_t size = (AVM_FLASH_END - AVM_ADDRESS);

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    AVM_LOGD(TAG, "Maximum application size: %lu KiB", (size / 1024));

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    if (!avmpack_is_valid(flashed_avm, size) || !avmpack_find_section_by_flag(flashed_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        AVM_LOGE(TAG, "Invalid AVM Pack");
        AVM_ABORT();
    }
    AVM_LOGI(TAG, "Booting file mapped at: %p, size: %lu", flashed_avm, startup_beam_size);
    AVM_LOGD(TAG, "Free application flash space: %lu KiB", ((size - startup_beam_size) / 1024));

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        AVM_LOGE(TAG, "Memory error: Cannot allocate AVMPackData.");
        AVM_ABORT();
    }
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.data = flashed_avm;
    avmpack_data->base.in_use = true;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    globalcontext_insert_module(glb, mod);
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    AVM_LOGI(TAG, "Starting: %s...\n", startup_module_name);
    fprintf(stdout, "---\n");

    context_execute_loop(ctx, mod, "start", 0);

    term ret_value = ctx->x[0];
    char *ret_atom_string = interop_atom_to_string(ctx, ret_value);
    if (ret_atom_string != NULL) {
        AVM_LOGI(TAG, "Exited with return: %s", ret_atom_string);
    } else {
        AVM_LOGI(TAG, "Exited with return value: %lx", (long) term_to_int32(ret_value));
    }
    free(ret_atom_string);

    bool reboot_on_not_ok =
#if defined(CONFIG_REBOOT_ON_NOT_OK)
        CONFIG_REBOOT_ON_NOT_OK ? true : false;
#else
        false;
#endif
    if (reboot_on_not_ok && ret_value != OK_ATOM) {
        AVM_LOGE(TAG, "AtomVM application terminated with non-ok return value.  Rebooting ...");
        scb_reset_system();
    } else {
        AVM_LOGI(TAG, "AtomVM application terminated.  Going to sleep forever ...");
        // Disable all interrupts
        cm_disable_interrupts();
        while (1) {
            ;
        }
    }
    return 0;
}
