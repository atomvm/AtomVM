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

#include <esp_log.h>
#include <esp_system.h>
#include <freertos/FreeRTOS.h>
#include <freertos/task.h>
#include <sdkconfig.h>
#include <stdbool.h>

#include <atom.h>
#include <avm_version.h>
#include <avmpack.h>
#include <bif.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <module.h>
#include <term.h>
#include <utils.h>

// before enabling this:
// idf.py add-dependency esp_tinyusb
// and enable USE_USB_SERIAL in menu config
#ifdef CONFIG_USE_USB_SERIAL
void init_usb_serial(void);
#include "tinyusb.h"
#include "tusb_cdc_acm.h"
#include "tusb_console.h"
#endif

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

void app_main()
{
    esp32_sys_queue_init();

#ifdef CONFIG_USE_USB_SERIAL
    init_usb_serial();
#endif

    fprintf(stdout, "%s", ATOMVM_BANNER);
    ESP_LOGI(TAG, "Starting AtomVM revision " ATOMVM_VERSION);

    spi_flash_mmap_handle_t handle;
    int size;
    const void *startup_avm = esp32_sys_mmap_partition("boot.avm", &handle, &size);
    if (IS_NULL_PTR(startup_avm)) {
        ESP_LOGI(TAG, "Trying deprecated main.avm partition.");
        startup_avm = esp32_sys_mmap_partition("main.avm", &handle, &size);
    }

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    GlobalContext *glb = globalcontext_new();

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    if (!avmpack_is_valid(startup_avm, size)) {
        ESP_LOGE(TAG, "Invalid startup avmpack. size=%u", size);
        AVM_ABORT();
    }
    if (!avmpack_find_section_by_flag(startup_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        ESP_LOGE(TAG, "Error: Failed to locate start module in startup partition. (Did you flash a library by mistake?)");
        AVM_ABORT();
    }
    ESP_LOGI(TAG, "Found startup beam %s", startup_module_name);
    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        ESP_LOGE(TAG, "Memory error: Cannot allocate AVMPackData for main.avm.");
        AVM_ABORT();
    }
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.in_use = true;
    avmpack_data->base.data = startup_avm;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    if (IS_NULL_PTR(mod)) {
        ESP_LOGE(TAG, "Error!  Unable to load startup module %s", startup_module_name);
        AVM_ABORT();
    }
    globalcontext_insert_module(glb, mod);

    ESP_LOGI(TAG, "Starting %s...", startup_module_name);
    fprintf(stdout, "---\n");

    run_result_t result = globalcontext_run(glb, mod, stdout);

    bool reboot_on_not_ok =
#if defined(CONFIG_REBOOT_ON_NOT_OK)
        CONFIG_REBOOT_ON_NOT_OK ? true : false;
#else
        false;
#endif
    if (reboot_on_not_ok && result != RUN_SUCCESS) {
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

#ifdef CONFIG_USE_USB_SERIAL
void init_usb_serial()
{
    /* Setting TinyUSB up */
    ESP_LOGI(TAG, "USB initialization");

    const tinyusb_config_t tusb_cfg = {
        .device_descriptor = NULL,
        .string_descriptor = NULL,
        .external_phy = false, // In the most cases you need to use a `false` value
#if (TUD_OPT_HIGH_SPEED)
        .fs_configuration_descriptor = NULL,
        .hs_configuration_descriptor = NULL,
        .qualifier_descriptor = NULL,
#else
        .configuration_descriptor = NULL,
#endif // TUD_OPT_HIGH_SPEED
    };

    ESP_ERROR_CHECK(tinyusb_driver_install(&tusb_cfg));

    tinyusb_config_cdcacm_t acm_cfg = { 0 }; // the configuration uses default values
    ESP_ERROR_CHECK(tusb_cdc_acm_init(&acm_cfg));

    esp_tusb_init_console(TINYUSB_CDC_ACM_0); // log to usb

    ESP_LOGI(TAG, "USB initialization: done.");
}
#endif
