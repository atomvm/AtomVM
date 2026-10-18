/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Riccardo Binetti <rbino@gmx.com>
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#include <rtems.h>
#if !defined(AVM_NO_SMP) && defined(RTEMS_SMP)
#include <rtems/score/percpu.h>
#endif

#ifdef RTEMS_BOARD_FDT_COMPATIBLE
#include <bsp/fdt.h>
#include <libfdt.h>
#endif

#ifdef RTEMS_HAS_STORAGE
#include "storage.h"
#endif

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <module.h>
#include <utils.h>

#include <scheduler.h>
#include <smp.h>

#include "app_loader.h"
#include "lib/network.h"
#include "lib/rtc.h"
#include "lib/rtems_sys.h"

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

extern const uint8_t embedded_avm[];
extern const size_t embedded_avm_size;

#if defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32S31)
#include <rtems/score/percpu.h>

extern void __real__RISCV_Interrupt_dispatch(uintptr_t mcause, Per_CPU_Control *cpu_self);

void __wrap__RISCV_Interrupt_dispatch(uintptr_t mcause, Per_CPU_Control *cpu_self)
{
    __real__RISCV_Interrupt_dispatch(mcause, cpu_self);

    /*
     * On RISC-V CLIC targets (ESP32-C5, ESP32-S31), hardware sets
     * mintstatus.mil to the active interrupt level on entry and only restores
     * it from mcause.mpil on mret. Because RTEMS _RISCV_Exception_handler
     * invokes _Thread_Do_dispatch before mret, context-switching to a thread
     * that yielded synchronously in thread mode would leave mintstatus.mil
     * non-zero and mask all CLIC interrupts on the core. Clear mintstatus.mil
     * via an in-place M-mode mret with MIE=0 while mstatus/mepc are already
     * saved on the interrupt frame.
     */
    uintptr_t tmp;
    uintptr_t status;
    __asm__ volatile(
        ".option push\n"
        ".option arch, +zicsr\n"
        "csrr %1, mstatus\n"
        "li %0, 0xb0000000\n"
        "csrw mcause, %0\n"
        "li %0, ~0x88\n"
        "and %0, %1, %0\n"
        "li %1, 0x1800\n"
        "or %0, %0, %1\n"
        "csrw mstatus, %0\n"
        "la %0, 1f\n"
        "csrw mepc, %0\n"
        "mret\n"
        "1:\n"
        ".option pop\n"
        : "=&r"(tmp), "=&r"(status)
        :
        : "memory");
}
#endif

#if defined(RTEMS_BSP_REALVIEW_PBX_A9_QEMU)
#include <bsp/start.h>
#if defined(RTEMS_SMP)
#include <libcpu/arm-cp15.h>
#include <rtems/score/smpimpl.h>

#define ARM_SMP_MMU_READY_MAGIC 0x52544d53U

/*
 * The BSP maps .bsp_start_data read-only when it enables the MMU.  Keep this
 * flag in writable .data, with an initial value loaded by QEMU before any
 * processor enters _start and restored by bsp_start_copy_sections().
 */
static volatile uint32_t arm_smp_mmu_ready = ~ARM_SMP_MMU_READY_MAGIC;
#endif

extern void __real_bsp_start_hook_0(void);
extern void __real_bsp_start_hook_1(void);

BSP_START_TEXT_SECTION void __wrap_bsp_start_hook_0(void)
{
#if defined(RTEMS_SMP)
    uint32_t cpu_index_self = _SMP_Get_current_processor();
    if (cpu_index_self != 0) {
        if (!_SMP_Should_start_processor(cpu_index_self)) {
            while (true) {
                _ARM_Wait_for_event();
            }
        }
        /*
         * Under QEMU -smp N, all Cortex-A9 cores enter _start simultaneously.
         * Secondary cores skip CPU 0's cache invalidation loop in start.S and
         * would otherwise enable their MMU in arm_a9mpcore_start_hook_0()
         * using bsp_translation_table_base while CPU 0 is still setting up the
         * page table or clearing .bss in bsp_start_hook_1(). Wait here (with
         * MMU and D-cache still off on secondary cores) until CPU 0 signals
         * that bsp_start_hook_1() has completed.
         */
        while (arm_smp_mmu_ready != ARM_SMP_MMU_READY_MAGIC) {
            _ARM_Wait_for_event();
        }
        _ARM_Data_memory_barrier();
    }
#endif
    __real_bsp_start_hook_0();
}

BSP_START_TEXT_SECTION void __wrap_bsp_start_hook_1(void)
{
    __real_bsp_start_hook_1();
#if defined(RTEMS_SMP)
    arm_smp_mmu_ready = ARM_SMP_MMU_READY_MAGIC;
    arm_cp15_data_cache_clean_and_invalidate_line((const void *) &arm_smp_mmu_ready);
    rtems_cache_flush_entire_data();
    _ARM_Data_synchronization_barrier();
    _ARM_Send_event();
#endif
}
#endif

#ifdef RTEMS_BOARD_FDT_COMPATIBLE
static bool board_fdt_is_valid(void)
{
    const void *fdt = bsp_fdt_get();
    return fdt != NULL && fdt_check_header(fdt) == 0
        && fdt_node_check_compatible(fdt, 0, RTEMS_BOARD_FDT_COMPATIBLE) == 0;
}
#endif

// The caller retains the pack until all modules and their borrowed pointers are destroyed.
// Return -1 for startup failure; application failures do not trigger the fallback.
static int run_app(const struct RtemsApp *app)
{
    GlobalContext *glb = globalcontext_new();
    if (IS_NULL_PTR(glb)) {
        fprintf(stderr, TAG ": Failed to create GlobalContext\n");
        return -1;
    }

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        fprintf(stderr, TAG ": Memory error: Cannot allocate AVMPackData.\n");
        nif_collection_destroy_all(glb);
        globalcontext_destroy(glb);
        return -1;
    }
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.data = app->data;
    avmpack_data->base.in_use = true;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = module_new_from_iff_binary(glb, app->startup_beam, app->startup_beam_size);
    if (IS_NULL_PTR(mod)) {
        fprintf(stderr, TAG ": Cannot load startup module: %s\n", app->startup_name);
        nif_collection_destroy_all(glb);
        globalcontext_destroy(glb);
        return -1;
    }
    if (globalcontext_insert_module(glb, mod) < 0) {
        module_destroy(mod);
        nif_collection_destroy_all(glb);
        globalcontext_destroy(glb);
        return -1;
    }

    fprintf(stdout, "Starting: %s...\n", app->startup_name);
    fprintf(stdout, "---\n");
    fflush(stdout);

    run_result_t result = globalcontext_run(glb, mod, stdout, 0, NULL);

#ifndef AVM_NO_SMP
    scheduler_stop_all(glb);
    smp_scheduler_join_all();
#endif

    nif_collection_destroy_all(glb);
    globalcontext_destroy(glb);

    return result == RUN_SUCCESS ? 0 : 1;
}

rtems_task Init(rtems_task_argument ignored)
{
    UNUSED(ignored);

    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    fprintf(stdout, "%s", ATOMVM_BANNER);
#ifndef AVM_NO_SMP
    (void) smp_is_main_thread(NULL);
    fprintf(stdout, "Starting AtomVM revision " ATOMVM_VERSION " on RTEMS (%d CPU(s) online)\n", smp_get_online_processors());
#if defined(RTEMS_SMP)
    for (uint32_t i = 1; i < (uint32_t) smp_get_online_processors(); i++) {
        const Per_CPU_Control *cpu = _Per_CPU_Get_by_index(i);
        uint32_t timeout_ticks = 100;
        while (_Per_CPU_Get_state(cpu) != PER_CPU_STATE_UP && --timeout_ticks > 0) {
            rtems_task_wake_after(1);
        }
        if (_Per_CPU_Get_state(cpu) != PER_CPU_STATE_UP) {
            fprintf(stderr, TAG ": Warning: CPU %" PRIu32 " failed to start (state %d)\n",
                i, (int) _Per_CPU_Get_state(cpu));
        }
    }
#endif
#else
    fprintf(stdout, "Starting AtomVM revision " ATOMVM_VERSION " on RTEMS\n");
#endif

    mkdir("/dev", 0755);

#if defined(RTEMS_BOARD_ESP32C3)
    esp32c3_rtc_timekeeper_restore();
#endif

#ifdef RTEMS_BOARD_FDT_COMPATIBLE
    if (!board_fdt_is_valid()) {
        fprintf(stderr, TAG ": Bootloader FDT is missing or incompatible with " RTEMS_BOARD_FDT_COMPATIBLE "\n");
        exit(1);
    }
#endif

#ifdef RTEMS_HAS_STORAGE
    bool storage_ready = storage_init();
#endif

    if (rtems_atomvm_network_init() != 0) {
        fprintf(stderr, TAG ": Network stack initialization failed\n");
        exit(1);
    }

#ifdef RTEMS_HAS_STORAGE
    struct RtemsApp file_app;
    if (storage_ready && storage_load_app(&file_app)) {
        int result = run_app(&file_app);
        free((void *) file_app.data);
        if (result >= 0) {
            exit(result);
        }
        fprintf(stderr, TAG ": Cannot start app.avm; using embedded application\n");
    }
#endif

    struct RtemsApp embedded_app;
    if (!rtems_app_validate(embedded_avm, embedded_avm_size, &embedded_app)) {
        fprintf(stderr, TAG ": Invalid or missing embedded AVM pack\n");
        exit(1);
    }
    fprintf(stdout, TAG ": Using embedded application\n");
    exit(run_app(&embedded_app) == 0 ? 0 : 1);
}
