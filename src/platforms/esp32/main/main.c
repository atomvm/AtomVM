/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

#include <esp_event.h>
#include <esp_event_loop.h>
#include <esp_log.h>
#include <esp_partition.h>
#include <esp_system.h>
#include <freertos/FreeRTOS.h>
#include <sdkconfig.h>
#include <stdbool.h>

#include <atom.h>
#include <avmpack.h>
#include <bif.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <module.h>
#include <term.h>
#include <utils.h>
#include <version.h>

#include "component_nifs.h"
#include "component_ports.h"
#include "esp32_sys.h"

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

const void *avm_partition(const char *partition_name, int *size);

void app_main()
{
    esp32_sys_queue_init();

    fprintf(stdout, "%s", ATOMVM_BANNER);
    ESP_LOGI(TAG, "Starting AtomVM revision " ATOMVM_VERSION);

    int size;
    const void *main_avm = avm_partition("main.avm", &size);

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    GlobalContext *glb = globalcontext_new();

    component_ports_init(glb);
    port_driver_init_all(glb);
    component_nifs_init(glb);
    nif_collection_init_all(glb);

    if (!avmpack_is_valid(main_avm, size)) {
        ESP_LOGE(TAG, "Invalid main.avm packbeam.  size=%u", size);
        AVM_ABORT();
    }
    if (!avmpack_find_section_by_flag(main_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        ESP_LOGE(TAG, "Error: Failed to locate start module in main.avm packbeam.  (Did you flash a library by mistake?)");
        AVM_ABORT();
    }
    ESP_LOGI(TAG, "Found startup beam %s", startup_module_name);
    struct AVMPackData *avmpack_data = malloc(sizeof(struct AVMPackData));
    if (IS_NULL_PTR(avmpack_data)) {
        ESP_LOGE(TAG, "Memory error: Cannot allocate AVMPackData for main.avm.");
        AVM_ABORT();
    }
    avmpack_data->data = main_avm;
    list_append(&glb->avmpack_data, (struct ListHead *) avmpack_data);
    glb->avmpack_platform_data = NULL;

    const void *lib_avm = avm_partition("lib.avm", &size);
    if (!IS_NULL_PTR(lib_avm) && avmpack_is_valid(lib_avm, size)) {
        avmpack_data = malloc(sizeof(struct AVMPackData));
        if (IS_NULL_PTR(avmpack_data)) {
            ESP_LOGE(TAG, "Memory error: Cannot allocate AVMPackData for lib.avm.");
            AVM_ABORT();
        }
        avmpack_data->data = lib_avm;
        list_append(&glb->avmpack_data, (struct ListHead *) avmpack_data);
    } else {
        ESP_LOGW(TAG, "Unable to mount lib.avm partition.  Hopefully the AtomVM core libraries are included in your application.");
    }

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    if (IS_NULL_PTR(mod)) {
        ESP_LOGE(TAG, "Error!  Unable to load startup module %s", startup_module_name);
        AVM_ABORT();
    }
    globalcontext_insert_module_with_filename(glb, mod, startup_module_name);
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    ESP_LOGI(TAG, "Starting %s...", startup_module_name);
    fprintf(stdout, "---\n");

    context_execute_loop(ctx, mod, "start", 0);
    term ret_value = ctx->x[0];

    fprintf(stdout, "AtomVM finished with return value: ");
    term_display(stdout, ret_value, ctx);
    fprintf(stdout, "\n");

    bool reboot_on_not_ok =
#if defined(CONFIG_REBOOT_ON_NOT_OK)
        CONFIG_REBOOT_ON_NOT_OK ? true : false;
#else
        false;
#endif
    if (reboot_on_not_ok && ret_value != OK_ATOM) {
        ESP_LOGE(TAG, "AtomVM application terminated with non-ok return value.  Rebooting ...");
        esp_restart();
    } else {
        ESP_LOGI(TAG, "AtomVM application terminated.  Going to sleep forever ...");
        while (1) {
            // avoid task_wdt: Task watchdog got triggered. The following tasks did not reset the watchdog in time
            // ..
            vTaskDelay(5000 / portTICK_PERIOD_MS);
        }
    }
}

const void *avm_partition(const char *partition_name, int *size)
{
    const esp_partition_t *partition = esp_partition_find_first(ESP_PARTITION_TYPE_DATA, ESP_PARTITION_SUBTYPE_ANY, partition_name);
    if (!partition) {
        ESP_LOGW(TAG, "AVM partition not found for %s", partition_name);
        *size = 0;
        return NULL;

    } else {
        *size = partition->size;
    }

    const void *mapped_memory;
    spi_flash_mmap_handle_t unmap_handle;
    if (esp_partition_mmap(partition, 0, partition->size, SPI_FLASH_MMAP_DATA, &mapped_memory, &unmap_handle) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to map BEAM partition for %s", partition_name);
        AVM_ABORT();
        return NULL;
    }
    ESP_LOGI(TAG, "Loaded BEAM partition %s at address 0x%x (size=%i bytes)", partition_name, partition->address, partition->size);

    UNUSED(unmap_handle);

    return mapped_memory;
}
