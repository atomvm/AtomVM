
/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#include <zephyr/kernel.h>
#include <zephyr/sys/reboot.h>

#if defined(CONFIG_SOC_FAMILY_ESPRESSIF_ESP32)
#include <spi_flash_mmap.h>
#endif

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <module.h>
#include <smp.h>
#include <utils.h>

#include "lib/avm_devcfg.h"
#include "lib/avm_log.h"
#include "lib/zephyros_sys.h"

#define AVM_ADDRESS (AVM_APP_ADDRESS)

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

#if defined(AVM_ZEPHYR_TEST_AVM)
extern const uint8_t zephyr_test_modules_avm[];
extern const size_t zephyr_test_modules_avm_size;
#endif

#if !defined(AVM_ZEPHYR_BOOT_TEST)
static void append_const_avmpack(GlobalContext *glb, const void *data, uint32_t size)
{
    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        AVM_LOGE(TAG, "Memory error: Cannot allocate AVMPackData.");
        AVM_ABORT();
    }
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info, size);
    avmpack_data->base.data = data;
    avmpack_data->base.in_use = true;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);
}
#endif

int main()
{
    printk("%s", ATOMVM_BANNER);

    GlobalContext *glb = globalcontext_new();

    AVM_LOGI(TAG, "Starting AtomVM revision %s", ATOMVM_VERSION);

#if defined(AVM_ZEPHYR_BOOT_TEST)
#ifndef AVM_NO_SMP
    AVM_LOGI(TAG, "Zephyr boot test: SMP schedulers=%d online=%d", smp_get_online_processors(), glb->online_schedulers);
#else
    AVM_LOGI(TAG, "Zephyr boot test: SMP disabled");
#endif
    globalcontext_destroy(glb);
    return 0;
#endif

    uint32_t size = AVM_APP_MAX_SIZE;
    const void *flashed_avm;

#if defined(AVM_ZEPHYR_TEST_AVM)
    flashed_avm = zephyr_test_modules_avm;
    size = (uint32_t) zephyr_test_modules_avm_size;
#elif defined(CONFIG_SOC_FAMILY_ESPRESSIF_ESP32)
    spi_flash_mmap_handle_t avm_mmap_handle;
    esp_err_t mmap_result = spi_flash_mmap(AVM_ADDRESS, size, SPI_FLASH_MMAP_DATA, &flashed_avm, &avm_mmap_handle);
    if (mmap_result != ESP_OK) {
        AVM_LOGE(TAG, "Unable to map application AVM pack from flash offset 0x%lx: %d", (unsigned long) AVM_ADDRESS, mmap_result);
        AVM_ABORT();
    }
#else
    flashed_avm = (const void *) (uintptr_t) AVM_ADDRESS;
#endif

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    AVM_LOGD(TAG, "Maximum application size: %lu KiB", (unsigned long) (size / 1024));

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    if (!avmpack_is_valid(flashed_avm, size) || !avmpack_find_section_by_flag(flashed_avm, size, BEAM_START_FLAG, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        AVM_LOGE(TAG, "Invalid AVM Pack");
        AVM_ABORT();
    }
    AVM_LOGD(TAG, "Booting file mapped at: %p, size: %lu", flashed_avm, (unsigned long) startup_beam_size);
    AVM_LOGI(TAG, "Application size: %lu B, free flash space: %lu KiB", (unsigned long) startup_beam_size, (unsigned long) ((size - startup_beam_size) / 1024));

    append_const_avmpack(glb, flashed_avm, size);

#if !defined(AVM_ZEPHYR_TEST_AVM) && defined(CONFIG_AVM_LIB_ADDRESS) && (CONFIG_AVM_LIB_ADDRESS != 0)
    uint32_t lib_region_size = (uint32_t) CONFIG_AVM_LIB_SIZE;
    const void *lib_avm = NULL;
#if defined(CONFIG_SOC_FAMILY_ESPRESSIF_ESP32)
    spi_flash_mmap_handle_t lib_mmap_handle;
    esp_err_t lib_mmap_result = spi_flash_mmap(CONFIG_AVM_LIB_ADDRESS, lib_region_size, SPI_FLASH_MMAP_DATA, &lib_avm, &lib_mmap_handle);
    if (lib_mmap_result != ESP_OK) {
        AVM_LOGE(TAG, "Unable to map library AVM pack from flash offset 0x%lx: %d", (unsigned long) CONFIG_AVM_LIB_ADDRESS, lib_mmap_result);
        AVM_ABORT();
    }
#else
    lib_avm = (const void *) (uintptr_t) CONFIG_AVM_LIB_ADDRESS;
#endif
    uint32_t lib_avm_size = 0;
    if (avmpack_compute_size(lib_avm, lib_region_size, &lib_avm_size)) {
        append_const_avmpack(glb, lib_avm, lib_avm_size);
        AVM_LOGI(TAG, "Library AVM pack mapped at %p (%lu B)", lib_avm, (unsigned long) lib_avm_size);
    } else if (avmpack_is_valid(lib_avm, lib_region_size)) {
        AVM_LOGE(TAG, "Truncated or corrupt library AVM pack at %p", lib_avm);
        AVM_ABORT();
    } else {
#if defined(CONFIG_SOC_FAMILY_ESPRESSIF_ESP32)
        spi_flash_munmap(lib_mmap_handle);
#endif
        AVM_LOGW(TAG, "No valid library AVM pack at %p", lib_avm);
    }
#endif

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    globalcontext_insert_module(glb, mod);
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    printk("Starting: %s...\n", startup_module_name);
    printk("---\n");

    context_execute_loop(ctx, mod, "start", 0);

    term ret_value = ctx->x[0];
    char *ret_atom_string = interop_atom_to_string(ctx, ret_value);
    if (ret_atom_string != NULL) {
        AVM_LOGI(TAG, "Exited with return: %s", ret_atom_string);
    } else {
        AVM_LOGI(TAG, "Exited with return value: %lx", (long) term_to_int32(ret_value));
    }
    free(ret_atom_string);

#if defined(AVM_ZEPHYR_TEST_EXIT)
    if (ret_value == OK_ATOM) {
        AVM_LOGI(TAG, "AtomVM test application terminated successfully.");
        return 0;
    }
    AVM_LOGE(TAG, "AtomVM test application terminated with non-ok return value.");
    return 1;
#else
    bool reboot_on_not_ok =
#if defined(CONFIG_REBOOT_ON_NOT_OK)
        CONFIG_REBOOT_ON_NOT_OK ? true : false;
#else
        false;
#endif
    if (reboot_on_not_ok && ret_value != OK_ATOM) {
        AVM_LOGE(TAG, "AtomVM application terminated with non-ok return value.  Rebooting ...");
        sys_reboot(SYS_REBOOT_COLD);
    } else {
        AVM_LOGI(TAG, "AtomVM application terminated.  Going to sleep forever ...");
        while (1) {
            ;
        }
    }
#endif
    return 0;
}
