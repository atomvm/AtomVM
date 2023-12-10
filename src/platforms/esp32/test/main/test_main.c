/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Davide Bettio <davide@uninstall.it>
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

#include "unity.h"
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <avmpack.h>
#include <context.h>
#include <globalcontext.h>
#include <module.h>
#include <nifs.h>
#include <platform_defaultatoms.h>

#include "esp32_sys.h"
#include "sys.h"

#include <driver/sdmmc_host.h>
#include <esp_eth.h>
#include <esp_event.h>
#include <esp_log.h>
#include <esp_netif.h>
#include <esp_vfs.h>
#include <esp_vfs_fat.h>
#include <sdmmc_cmd.h>

extern const uint8_t main_avm[] asm("_binary_esp32_test_modules_avm_start");
extern const size_t size asm("esp32_test_modules_avm_length");

#ifndef AVM_NO_SMP
#include "smp.h"
#endif

#define TAG "AtomVM"

const struct Nif *platform_nifs_get_nif(const char *nifname);

TEST_CASE("atomvm_platform_0", "[platform_nifs]")
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:platform/0");
    TEST_ASSERT(nif->nif_ptr(NULL, 0, NULL) == ESP32_ATOM);
}

TEST_CASE("atomvm_missing_0", "[platform_nifs]")
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:missing/0");
    TEST_ASSERT(nif == NULL);
}

// Derived from:
// https://github.com/espressif/esp-idf/blob/release/v4.4/examples/common_components/protocol_examples_common/connect.c

#define CONFIG_EXAMPLE_ETH_PHY_ADDR 1
#define CONFIG_EXAMPLE_ETH_PHY_RST_GPIO 5

static esp_eth_handle_t s_eth_handle = NULL;
static esp_eth_mac_t *s_mac = NULL;
static esp_eth_phy_t *s_phy = NULL;
static esp_eth_netif_glue_handle_t s_eth_glue = NULL;

static esp_netif_t *eth_start(void)
{
    char *desc;
    esp_netif_inherent_config_t esp_netif_config = ESP_NETIF_INHERENT_DEFAULT_ETH();
    // Prefix the interface description with the module TAG
    // Warning: the interface desc is used in tests to capture actual connection details (IP, gw, mask)
    asprintf(&desc, "%s: %s", TAG, esp_netif_config.if_desc);
    esp_netif_config.if_desc = desc;
    esp_netif_config.route_prio = 64;
    esp_netif_config_t netif_config = {
        .base = &esp_netif_config,
        .stack = ESP_NETIF_NETSTACK_DEFAULT_ETH
    };
    esp_netif_t *netif = esp_netif_new(&netif_config);
    assert(netif);
    free(desc);

    eth_mac_config_t mac_config = ETH_MAC_DEFAULT_CONFIG();
    eth_phy_config_t phy_config = ETH_PHY_DEFAULT_CONFIG();
    phy_config.phy_addr = CONFIG_EXAMPLE_ETH_PHY_ADDR;
    phy_config.reset_gpio_num = CONFIG_EXAMPLE_ETH_PHY_RST_GPIO;
    phy_config.autonego_timeout_ms = 100;
    s_mac = esp_eth_mac_new_openeth(&mac_config);
    s_phy = esp_eth_phy_new_dp83848(&phy_config);

    // Install Ethernet driver
    esp_eth_config_t config = ETH_DEFAULT_CONFIG(s_mac, s_phy);
    ESP_ERROR_CHECK(esp_eth_driver_install(&config, &s_eth_handle));
    // combine driver with netif
    s_eth_glue = esp_eth_new_netif_glue(s_eth_handle);
    esp_netif_attach(netif, s_eth_glue);

    esp_eth_start(s_eth_handle);
    return netif;
}

static void eth_stop(esp_netif_t *eth_netif)
{
    ESP_ERROR_CHECK(esp_eth_stop(s_eth_handle));
    ESP_ERROR_CHECK(esp_eth_del_netif_glue(s_eth_glue));
    ESP_ERROR_CHECK(esp_eth_driver_uninstall(s_eth_handle));
    ESP_ERROR_CHECK(s_phy->del(s_phy));
    ESP_ERROR_CHECK(s_mac->del(s_mac));

    esp_netif_destroy(eth_netif);
}

term avm_test_case(const char *test_module)
{
    esp32_sys_queue_init();

    GlobalContext *glb = globalcontext_new();
    TEST_ASSERT(glb != NULL);

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    TEST_ASSERT(avmpack_is_valid(main_avm, size) != 0);

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    TEST_ASSERT(avmpack_data != NULL);

    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.in_use = true;
    avmpack_data->base.data = main_avm;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = sys_load_module(glb, test_module);
    TEST_ASSERT(mod != NULL);

    globalcontext_insert_module(glb, mod);

    Context *ctx = context_new(glb);
    TEST_ASSERT(ctx != NULL);
    ctx->leader = 1;

    ESP_LOGI(TAG, "Running start/0 from %s...\n", test_module);

    context_execute_loop(ctx, mod, "start", 0);
    term ret_value = ctx->x[0];

    fprintf(stdout, "AtomVM finished with return value: ");
    term_display(stdout, ret_value, ctx);
    fprintf(stdout, "\n");

    context_destroy(ctx);

    nif_collection_destroy_all(glb);
    port_driver_destroy_all(glb);

    globalcontext_destroy(glb);

    return ret_value;
}

TEST_CASE("test_esp_partition", "[test_run]")
{
    term ret_value = avm_test_case("test_esp_partition.beam");
    TEST_ASSERT(term_to_int(ret_value) == 0);
}

TEST_CASE("test_file", "[test_run]")
{
    esp_vfs_fat_sdmmc_mount_config_t mount_config = {
        .format_if_mount_failed = true,
        .max_files = 5,
        .allocation_unit_size = 16 * 1024
    };
    sdmmc_card_t *card;
    const char mount_point[] = "/sdcard";
    ESP_LOGI(TAG, "Initializing SD card");

    ESP_LOGI(TAG, "Using SDMMC peripheral");
    // By default, SD card frequency is initialized to SDMMC_FREQ_DEFAULT (20MHz)
    // For setting a specific frequency, use host.max_freq_khz (range 400kHz - 40MHz for SDMMC)
    // Example: for fixed frequency of 10MHz, use host.max_freq_khz = 10000;
    sdmmc_host_t host = SDMMC_HOST_DEFAULT();

    // This initializes the slot without card detect (CD) and write protect (WP) signals.
    // Modify slot_config.gpio_cd and slot_config.gpio_wp if your board has these signals.
    sdmmc_slot_config_t slot_config = SDMMC_SLOT_CONFIG_DEFAULT();

    ESP_LOGI(TAG, "Mounting filesystem");
    // With qemu, this logs "sdmmc_req: sdmmc_host_wait_for_event returned 0x107" twice with v4.4.4, but it eventually succeeds.
    // See: https://github.com/espressif/qemu/issues/38
    esp_err_t ret = esp_vfs_fat_sdmmc_mount(mount_point, &host, &slot_config, &mount_config, &card);

    TEST_ASSERT(ret == ESP_OK);

    sdmmc_card_print_info(stdout, card);

    term ret_value = avm_test_case("test_file.beam");
    TEST_ASSERT(ret_value == OK_ATOM);

    esp_vfs_fat_sdcard_unmount(mount_point, card);
    ESP_LOGI(TAG, "Card unmounted");
}

TEST_CASE("test_list_to_binary", "[test_run]")
{
    term ret_value = avm_test_case("test_list_to_binary.beam");
    TEST_ASSERT(ret_value == OK_ATOM);
}

TEST_CASE("test_md5", "[test_run]")
{
    term ret_value = avm_test_case("test_md5.beam");
    TEST_ASSERT(ret_value == OK_ATOM);
}

TEST_CASE("test_crypto", "[test_run]")
{
    term ret_value = avm_test_case("test_crypto.beam");
    TEST_ASSERT(ret_value == OK_ATOM);
}

TEST_CASE("test_monotonic_time", "[test_run]")
{
    term ret_value = avm_test_case("test_monotonic_time.beam");
    TEST_ASSERT(ret_value == OK_ATOM);
}

struct pipefs_global_ctx
{
    int max_fd;
    uint8_t byte;
    bool has_byte;
    bool is_select;
    int select_nfds;
    esp_vfs_select_sem_t select_sem;
    fd_set *readfds;
    fd_set *writefds;
    fd_set readfds_orig;
    fd_set writefds_orig;
};

static struct pipefs_global_ctx pipefs_global_ctx;

static ssize_t pipefs_write(int fd, const void *data, size_t size)
{
    UNUSED(fd);

    if (size == 0) {
        return 0;
    }
    if (pipefs_global_ctx.has_byte) {
        errno = EAGAIN;
        return -1;
    }
    pipefs_global_ctx.has_byte = true;
    pipefs_global_ctx.byte = *((uint8_t *) data);
    if (pipefs_global_ctx.is_select) {
        // Notify any reader.
        bool notify = false;
        for (int i = 0; i < pipefs_global_ctx.select_nfds; i++) {
            if (FD_ISSET(i, &pipefs_global_ctx.readfds_orig)) {
                FD_SET(i, pipefs_global_ctx.readfds);
                notify = true;
            }
        }
        if (notify) {
            esp_vfs_select_triggered(pipefs_global_ctx.select_sem);
        }
    }
    return 1;
}

static ssize_t pipefs_read(int fd, void *data, size_t size)
{
    UNUSED(fd);

    if (size == 0) {
        return 0;
    }
    if (!pipefs_global_ctx.has_byte) {
        errno = EAGAIN;
        return -1;
    }
    pipefs_global_ctx.has_byte = false;
    *((uint8_t *) data) = pipefs_global_ctx.byte;
    if (pipefs_global_ctx.is_select) {
        // Notify any writer.
        bool notify = false;
        for (int i = 0; i < pipefs_global_ctx.select_nfds; i++) {
            if (FD_ISSET(i, &pipefs_global_ctx.writefds_orig)) {
                FD_SET(i, pipefs_global_ctx.writefds);
                notify = true;
            }
        }
        if (notify) {
            esp_vfs_select_triggered(pipefs_global_ctx.select_sem);
        }
    }
    return 1;
}

static int pipefs_open(const char *path, int flags, int mode)
{
    return ++pipefs_global_ctx.max_fd;
}

static int pipefs_close(int fd)
{
    UNUSED(fd);
    return 0;
}

static esp_err_t pipefs_start_select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, esp_vfs_select_sem_t sem, void **end_select_args)
{
    // Cannot select twice in parallel
    if (pipefs_global_ctx.is_select) {
        return ESP_ERR_INVALID_STATE;
    }

    pipefs_global_ctx.select_nfds = nfds;
    pipefs_global_ctx.readfds = readfds;
    pipefs_global_ctx.readfds_orig = *readfds;
    pipefs_global_ctx.writefds = writefds;
    pipefs_global_ctx.writefds_orig = *writefds;
    pipefs_global_ctx.select_sem = sem;
    pipefs_global_ctx.is_select = true;
    FD_ZERO(readfds);
    FD_ZERO(writefds);
    FD_ZERO(exceptfds);

    // Notify based on current state.
    bool notify = false;
    for (int i = 0; i < pipefs_global_ctx.select_nfds; i++) {
        if (pipefs_global_ctx.has_byte) {
            if (FD_ISSET(i, &pipefs_global_ctx.readfds_orig)) {
                FD_SET(i, pipefs_global_ctx.readfds);
                notify = true;
            }
        } else {
            if (FD_ISSET(i, &pipefs_global_ctx.writefds_orig)) {
                FD_SET(i, pipefs_global_ctx.writefds);
                notify = true;
            }
        }
    }
    if (notify) {
        esp_vfs_select_triggered(pipefs_global_ctx.select_sem);
    }

    return ESP_OK;
}

static esp_err_t pipefs_end_select(void *end_select_args)
{
    if (!pipefs_global_ctx.is_select) {
        return ESP_ERR_INVALID_STATE;
    }
    pipefs_global_ctx.is_select = false;
    return ESP_OK;
}

TEST_CASE("test_select", "[test_run]")
{
    esp_vfs_t pipefs = {
        .flags = ESP_VFS_FLAG_DEFAULT,
        .write = &pipefs_write,
        .open = &pipefs_open,
        .read = &pipefs_read,
        .close = &pipefs_close,
        .start_select = &pipefs_start_select,
        .end_select = &pipefs_end_select,
    };
    pipefs_global_ctx.has_byte = false;
    pipefs_global_ctx.is_select = false;
    pipefs_global_ctx.max_fd = 0;

    ESP_ERROR_CHECK(esp_vfs_register("/pipe", &pipefs, NULL));

    term ret_value = avm_test_case("test_select.beam");
    TEST_ASSERT(ret_value == OK_ATOM);

    esp_vfs_unregister("/pipe");
}

TEST_CASE("test_time_and_processes", "[test_run]")
{
    term ret_value = avm_test_case("test_time_and_processes.beam");
    TEST_ASSERT(term_to_int(ret_value) == 6);
}

TEST_CASE("test_tz", "[test_run]")
{
    term ret_value = avm_test_case("test_tz.beam");
    TEST_ASSERT(ret_value == OK_ATOM);
}

#ifndef AVM_NO_SMP
TEST_CASE("atomvm_smp_0", "[smp]")
{
    int cores = smp_get_online_processors();
    ESP_LOGI(TAG, "Got %i cores\n", cores);
    TEST_ASSERT(cores == 2);
}
#endif

static volatile bool network_got_ip = false;

static void got_ip_event_handler(void *arg, esp_event_base_t event_base, int32_t event_id, void *event_data)
{
    ESP_LOGI("NETWORK", "ETH GOT IP");
    network_got_ip = true;
}

TEST_CASE("test_net", "[test_run]")
{
    // esp_netif_init() was called by network_driver_init
    ESP_LOGI(TAG, "Registering handler\n");
    network_got_ip = false;
    ESP_ERROR_CHECK(esp_event_handler_register(IP_EVENT, IP_EVENT_ETH_GOT_IP, &got_ip_event_handler, NULL));
    ESP_LOGI(TAG, "Starting network\n");
    esp_netif_t *eth_netif = eth_start();

    while (!network_got_ip) {
        vTaskDelay(1);
    }

    term ret_value = avm_test_case("test_net.beam");
    TEST_ASSERT(ret_value == OK_ATOM);

    ESP_LOGI(TAG, "Stopping network\n");
    eth_stop(eth_netif);
}

TEST_CASE("test_socket", "[test_run]")
{
    // esp_netif_init() was called by network_driver_init
    ESP_LOGI(TAG, "Registering handler\n");
    network_got_ip = false;
    ESP_ERROR_CHECK(esp_event_handler_register(IP_EVENT, IP_EVENT_ETH_GOT_IP, &got_ip_event_handler, NULL));
    ESP_LOGI(TAG, "Starting network\n");
    esp_netif_t *eth_netif = eth_start();

    while (!network_got_ip) {
        vTaskDelay(1);
    }

    term ret_value = avm_test_case("test_socket.beam");
    TEST_ASSERT(term_to_int(ret_value) == 0);

    ESP_LOGI(TAG, "Stopping network\n");
    eth_stop(eth_netif);
}

TEST_CASE("test_ssl", "[test_run]")
{
    // esp_netif_init() was called by network_driver_init
    ESP_LOGI(TAG, "Registering handler\n");
    network_got_ip = false;
    ESP_ERROR_CHECK(esp_event_handler_register(IP_EVENT, IP_EVENT_ETH_GOT_IP, &got_ip_event_handler, NULL));
    ESP_LOGI(TAG, "Starting network\n");
    esp_netif_t *eth_netif = eth_start();

    while (!network_got_ip) {
        vTaskDelay(1);
    }

    term ret_value = avm_test_case("test_ssl.beam");
    TEST_ASSERT(ret_value == OK_ATOM);

    ESP_LOGI(TAG, "Stopping network\n");
    eth_stop(eth_netif);
}

TEST_CASE("test_rtc_slow", "[test_run]")
{
    term ret_value = avm_test_case("test_rtc_slow.beam");
    TEST_ASSERT(term_to_int(ret_value) == 0);
}

#if ESP_IDF_VERSION_MAJOR >= 5
TEST_CASE("test_twdt", "[test_run]")
{
    term ret_value = avm_test_case("test_twdt.beam");
    TEST_ASSERT(ret_value == OK_ATOM);
}
#endif

void app_main(void)
{
    UNITY_BEGIN();
    unity_run_all_tests();
    UNITY_END();
}
