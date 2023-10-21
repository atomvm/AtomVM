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
#include <stdio.h>
#include <unistd.h>

#include <libopencm3/cm3/nvic.h>
#include <libopencm3/cm3/systick.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/usart.h>

#include <avmpack.h>
#include <context.h>
#include <globalcontext.h>
#include <module.h>
#include <utils.h>
#include <version.h>

#include "lib/avm_log.h"
#include "lib/stm_sys.h"

#define USART_CONSOLE USART2
#define AVM_ADDRESS (0x8080000)
#define AVM_FLASH_MAX_SIZE (0x80000)
#define CLOCK_FREQUENCY (168000000)

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

static void clock_setup()
{
    // Use external clock, set divider for 168 MHz clock frequency
    rcc_clock_setup_pll(&rcc_hse_8mhz_3v3[RCC_CLOCK_3V3_168MHZ]);

    // Enable clock for USART2 GPIO
    rcc_periph_clock_enable(RCC_GPIOA);

    // Enable clock for USART2
    rcc_periph_clock_enable(RCC_USART2);
}

static void usart_setup()
{
    // Setup GPIO pins for USART2 transmit
    gpio_mode_setup(GPIOA, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO2);

    // Setup USART2 TX pin as alternate function
    gpio_set_af(GPIOA, GPIO_AF7, GPIO2);

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
    systick_set_reload((CLOCK_FREQUENCY / 1000) - 1);
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

int main()
{
    // Flash cache must be enabled before system clock is activated
    sys_enable_flash_cache();
    sys_init_icache();
    clock_setup();
    systick_setup();
    usart_setup();

    fprintf(stdout, "%s", ATOMVM_BANNER);
    AVM_LOGI(TAG, "Starting AtomVM revision " ATOMVM_VERSION);
    AVM_LOGD(TAG, "Using usart mapped at register 0x%x for stdout/stderr.", USART_CONSOLE);

    const void *flashed_avm = (void *) AVM_ADDRESS;
    uint32_t size = AVM_FLASH_MAX_SIZE;

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    AVM_LOGD(TAG, "Maximum application size: %lu", size);

    GlobalContext *glb = globalcontext_new();
    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    if (!avmpack_is_valid(flashed_avm, size) || !avmpack_find_section_by_flag(flashed_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        AVM_LOGE(TAG, "Invalid AVM Pack");
        AVM_ABORT();
    }
    AVM_LOGI(TAG, "Booting file mapped at: %p, size: %lu", flashed_avm, startup_beam_size);

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
    context_execute_loop(ctx, mod, "start", 0);
    AVM_LOGI(TAG, "Return value: %lx", (long) term_to_int32(ctx->x[0]));

    while (1)
        ;

    return 0;
}
