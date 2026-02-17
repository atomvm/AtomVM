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

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <module.h>
#include <utils.h>

#include "clock_config.h"
#include "lib/avm_devcfg.h"
#include "lib/avm_log.h"
#include "lib/stm_sys.h"

#define AVM_ADDRESS (AVM_APP_ADDRESS)
#define AVM_FLASH_END (CFG_FLASH_END)

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

static UART_HandleTypeDef huart_console;

pid_t _getpid(void);
int _kill(pid_t pid, int sig);

static void usart_setup(GlobalContext *glb)
{
#if defined(CONSOLE_1)
    __HAL_RCC_USART1_CLK_ENABLE();
#elif defined(CONSOLE_2)
    __HAL_RCC_USART2_CLK_ENABLE();
#elif defined(CONSOLE_3)
    __HAL_RCC_USART3_CLK_ENABLE();
#elif defined(CONSOLE_4)
    __HAL_RCC_UART4_CLK_ENABLE();
#elif defined(CONSOLE_5)
    __HAL_RCC_UART5_CLK_ENABLE();
#elif defined(CONSOLE_6)
    __HAL_RCC_USART6_CLK_ENABLE();
#endif

    GPIO_InitTypeDef gpio_init = { 0 };
    gpio_init.Pin = AVM_CONSOLE_TX_PIN;
    gpio_init.Mode = GPIO_MODE_AF_PP;
    gpio_init.Pull = GPIO_NOPULL;
    gpio_init.Speed = GPIO_SPEED_FREQ_HIGH;
    gpio_init.Alternate = AVM_CONSOLE_TX_AF;
    HAL_GPIO_Init(AVM_CONSOLE_TX_PORT, &gpio_init);

    sys_lock_pin(glb, (uint32_t) AVM_CONSOLE_TX_PORT, AVM_CONSOLE_TX_PIN);

    huart_console.Instance = AVM_CONSOLE_USART;
    huart_console.Init.BaudRate = 115200;
    huart_console.Init.WordLength = UART_WORDLENGTH_8B;
    huart_console.Init.StopBits = UART_STOPBITS_1;
    huart_console.Init.Parity = UART_PARITY_NONE;
    huart_console.Init.Mode = UART_MODE_TX;
    huart_console.Init.HwFlowCtl = UART_HWCONTROL_NONE;
    huart_console.Init.OverSampling = UART_OVERSAMPLING_16;
    if (HAL_UART_Init(&huart_console) != HAL_OK) {
        while (1) {
        }
    }
}

// picolibc posix-console syscall
int write(int file, const void *buf, size_t len)
{
    const char *ptr = (const char *) buf;
    size_t i;

    if (file == STDOUT_FILENO || file == STDERR_FILENO) {
        for (i = 0; i < len; i++) {
            if (ptr[i] == '\n') {
                uint8_t cr = '\r';
                HAL_UART_Transmit(&huart_console, &cr, 1, HAL_MAX_DELAY);
            }
            HAL_UART_Transmit(&huart_console, (uint8_t *) &ptr[i], 1, HAL_MAX_DELAY);
        }
        return (int) i;
    }
    errno = EIO;
    return -1;
}

// picolibc stubs to support AVM_ABORT
pid_t _getpid(void)
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

/* HardFault handler that dumps Cortex-M fault status registers.
 * The naked attribute + asm extracts the correct stack pointer (MSP or PSP)
 * and passes it to the C diagnostic function.
 * Note: Cortex-M0/M0+ does not have CFSR/HFSR/MMFAR/BFAR or IT instructions. */
#if (__CORTEX_M >= 3)
void HardFault_Diagnostic(uint32_t *stack_frame)
{
    volatile uint32_t cfsr = SCB->CFSR;
    volatile uint32_t hfsr = SCB->HFSR;
    volatile uint32_t mmfar = SCB->MMFAR;
    volatile uint32_t bfar = SCB->BFAR;

    uint32_t stacked_r0 = stack_frame[0];
    uint32_t stacked_r1 = stack_frame[1];
    uint32_t stacked_r2 = stack_frame[2];
    uint32_t stacked_r3 = stack_frame[3];
    uint32_t stacked_r12 = stack_frame[4];
    uint32_t stacked_lr = stack_frame[5];
    uint32_t stacked_pc = stack_frame[6];
    uint32_t stacked_psr = stack_frame[7];

    fprintf(stderr, "\n--- Hard Fault ---\n");
    fprintf(stderr, "CFSR:  0x%08lx\n", (unsigned long) cfsr);
    fprintf(stderr, "HFSR:  0x%08lx\n", (unsigned long) hfsr);
    fprintf(stderr, "MMFAR: 0x%08lx\n", (unsigned long) mmfar);
    fprintf(stderr, "BFAR:  0x%08lx\n", (unsigned long) bfar);
    fprintf(stderr, "PC:    0x%08lx\n", (unsigned long) stacked_pc);
    fprintf(stderr, "LR:    0x%08lx\n", (unsigned long) stacked_lr);
    fprintf(stderr, "R0:    0x%08lx\n", (unsigned long) stacked_r0);
    fprintf(stderr, "R1:    0x%08lx\n", (unsigned long) stacked_r1);
    fprintf(stderr, "R2:    0x%08lx\n", (unsigned long) stacked_r2);
    fprintf(stderr, "R3:    0x%08lx\n", (unsigned long) stacked_r3);
    fprintf(stderr, "R12:   0x%08lx\n", (unsigned long) stacked_r12);
    fprintf(stderr, "PSR:   0x%08lx\n", (unsigned long) stacked_psr);

    /* Decode CFSR bits */
    if (cfsr & 0x8000) {
        fprintf(stderr, "  BFAR valid\n");
    }
    if (cfsr & 0x2000) {
        fprintf(stderr, "  Lazy FP stacking BusFault\n");
    }
    if (cfsr & 0x1000) {
        fprintf(stderr, "  Stacking BusFault\n");
    }
    if (cfsr & 0x0800) {
        fprintf(stderr, "  Unstacking BusFault\n");
    }
    if (cfsr & 0x0400) {
        fprintf(stderr, "  Imprecise BusFault\n");
    }
    if (cfsr & 0x0200) {
        fprintf(stderr, "  Precise BusFault\n");
    }
    if (cfsr & 0x0100) {
        fprintf(stderr, "  Instruction BusFault\n");
    }
    if (cfsr & 0x80) {
        fprintf(stderr, "  MMFAR valid\n");
    }
    if (cfsr & 0x20) {
        fprintf(stderr, "  Lazy FP stacking MemManage\n");
    }
    if (cfsr & 0x10) {
        fprintf(stderr, "  Stacking MemManage\n");
    }
    if (cfsr & 0x08) {
        fprintf(stderr, "  Unstacking MemManage\n");
    }
    if (cfsr & 0x02) {
        fprintf(stderr, "  Data access MemManage\n");
    }
    if (cfsr & 0x01) {
        fprintf(stderr, "  Instruction access MemManage\n");
    }
    if (cfsr & 0x02000000) {
        fprintf(stderr, "  Divide by zero UsageFault\n");
    }
    if (cfsr & 0x01000000) {
        fprintf(stderr, "  Unaligned access UsageFault\n");
    }
    if (cfsr & 0x00080000) {
        fprintf(stderr, "  No coprocessor UsageFault\n");
    }
    if (cfsr & 0x00040000) {
        fprintf(stderr, "  Invalid PC UsageFault\n");
    }
    if (cfsr & 0x00020000) {
        fprintf(stderr, "  Invalid state UsageFault\n");
    }
    if (cfsr & 0x00010000) {
        fprintf(stderr, "  Undefined instruction UsageFault\n");
    }

    while (1) {
    }
}

__attribute__((naked)) void HardFault_Handler(void)
{
    __asm volatile(
        "tst lr, #4        \n"
        "ite eq             \n"
        "mrseq r0, msp      \n"
        "mrsne r0, psp      \n"
        "b HardFault_Diagnostic \n");
}

#else /* Cortex-M0/M0+ */

void HardFault_Handler(void)
{
    fprintf(stderr, "\n--- Hard Fault ---\n");
    while (1) {
    }
}

#endif /* __CORTEX_M >= 3 */

/* Stubs for picolibc's POSIX console and libc requirements */

int close(int file)
{
    UNUSED(file);
    return -1;
}

int fstat(int file, struct stat *st)
{
    UNUSED(file);
    st->st_mode = S_IFCHR;
    return 0;
}

int isatty(int file)
{
    UNUSED(file);
    return 1;
}

off_t lseek(int file, off_t ptr, int dir)
{
    UNUSED(file);
    UNUSED(ptr);
    UNUSED(dir);
    return 0;
}

int read(int file, void *ptr, size_t len)
{
    UNUSED(file);
    UNUSED(ptr);
    UNUSED(len);
    return 0;
}

void _exit(int status)
{
    UNUSED(status);
    while (1) {
    }
}

int main(void)
{
    HAL_Init();
    sys_enable_flash_cache();
    SystemClock_Config();
    sys_init_icache();
    sys_enable_core_periph_clocks();

    GlobalContext *glb = globalcontext_new();

    usart_setup(glb);

    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    fprintf(stdout, "%s", ATOMVM_BANNER);
    AVM_LOGI(TAG, "Starting AtomVM revision " ATOMVM_VERSION);

    const void *flashed_avm = (void *) AVM_ADDRESS;
    uint32_t size = (AVM_FLASH_END - AVM_ADDRESS);

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    AVM_LOGD(TAG, "Maximum application size: %lu KiB", (size / 1024));

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    if (!avmpack_is_valid(flashed_avm, size) || !avmpack_find_section_by_flag(flashed_avm, BEAM_START_FLAG, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
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

    AVM_LOGI(TAG, "Starting: %s...\n", startup_module_name);
    fprintf(stdout, "---\n");

    run_result_t result = globalcontext_run(glb, mod, stdout, 0, NULL);

    bool reboot_on_not_ok =
#if defined(CONFIG_REBOOT_ON_NOT_OK)
        CONFIG_REBOOT_ON_NOT_OK ? true : false;
#else
        false;
#endif
    if (reboot_on_not_ok && result != RUN_SUCCESS) {
        AVM_LOGE(TAG, "AtomVM application terminated with non-ok return value.  Rebooting ...");
        NVIC_SystemReset();
    } else {
        AVM_LOGI(TAG, "AtomVM application terminated.  Going to sleep forever ...");
        // Disable all interrupts
        __disable_irq();
        while (1) {
            ;
        }
    }
    return 0;
}
