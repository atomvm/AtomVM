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
#include <stdio.h>
#include <string.h>

#include <avmpack.h>
#include <context.h>
#include <globalcontext.h>
#include <module.h>
#include <nifs.h>
#include <platform_defaultatoms.h>

#include <esp32_sys.h>

#include <esp_eth.h>
#include <esp_event.h>
#include <esp_log.h>
#include <esp_netif.h>

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
#if !CONFIG_ESP_NETIF_TCPIP_ADAPTER_COMPATIBLE_LAYER
static esp_eth_netif_glue_handle_t s_eth_glue = NULL;
#endif

static esp_netif_t *eth_start(void)
{
#if CONFIG_ESP_NETIF_TCPIP_ADAPTER_COMPATIBLE_LAYER
    tcpip_adapter_set_default_eth_handlers();
    esp_netif_t *netif = NULL;
#else
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
#endif

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
#if !CONFIG_ESP_NETIF_TCPIP_ADAPTER_COMPATIBLE_LAYER
    // combine driver with netif
    s_eth_glue = esp_eth_new_netif_glue(s_eth_handle);
    esp_netif_attach(netif, s_eth_glue);
#endif

    esp_eth_start(s_eth_handle);
    return netif;
}

static void eth_stop(esp_netif_t *eth_netif)
{
    ESP_ERROR_CHECK(esp_eth_stop(s_eth_handle));
#if CONFIG_ESP_NETIF_TCPIP_ADAPTER_COMPATIBLE_LAYER
    tcpip_adapter_clear_default_eth_handlers();
    // Unfortunately, tcpip_adapter_clear_default_eth_handlers is a noop
    // So driver cannot be really stopped
#else
    ESP_ERROR_CHECK(esp_eth_del_netif_glue(s_eth_glue));
    ESP_ERROR_CHECK(esp_eth_driver_uninstall(s_eth_handle));
    ESP_ERROR_CHECK(s_phy->del(s_phy));
    ESP_ERROR_CHECK(s_mac->del(s_mac));

    esp_netif_destroy(eth_netif);
#endif
}

term avm_test_case(const char *test_function)
{
    esp32_sys_queue_init();

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    GlobalContext *glb = globalcontext_new();
    TEST_ASSERT(glb != NULL);

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    ESP_LOGI(TAG, "Testing avm\n");

    TEST_ASSERT(avmpack_is_valid(main_avm, size) != 0);
    TEST_ASSERT(avmpack_find_section_by_flag(main_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name) != 0);

    struct AVMPackData *avmpack_data = malloc(sizeof(struct AVMPackData));
    TEST_ASSERT(avmpack_data != NULL);

    avmpack_data->data = main_avm;
    synclist_append(&glb->avmpack_data, &avmpack_data->avmpack_head);
    glb->avmpack_platform_data = NULL;

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    TEST_ASSERT(mod != NULL);

    globalcontext_insert_module(glb, mod);

    Context *ctx = context_new(glb);
    TEST_ASSERT(ctx != NULL);
    ctx->leader = 1;

    ESP_LOGI(TAG, "Running %s...\n", test_function);

    context_execute_loop(ctx, mod, test_function, 0);
    term ret_value = ctx->x[0];

    fprintf(stdout, "AtomVM finished with return value: ");
    term_display(stdout, ret_value, ctx);
    fprintf(stdout, "\n");

    return ret_value;
}

TEST_CASE("test_time_and_processes", "[test_run]")
{
    term ret_value = avm_test_case("test_time_and_processes");
    TEST_ASSERT(term_to_int(ret_value) == 6);
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

static esp_err_t network_event_handler(void *data, system_event_t *event)
{
    if (event->event_id == SYSTEM_EVENT_ETH_CONNECTED) {
        ESP_LOGI("NETWORK", "ETH CONNECTED");
    } else if (event->event_id == SYSTEM_EVENT_ETH_GOT_IP) {
        ESP_LOGI("NETWORK", "ETH GOT IP");
        network_got_ip = true;
    } else {
        ESP_LOGI("NETWORK", "Unhandled network event: %i.", event->event_id);
    }
    return ESP_OK;
}

#ifdef ENABLE_TEST_SOCKET
TEST_CASE("test_socket", "[test_run]")
{
    ESP_LOGI(TAG, "Starting event loop\n");
    ESP_ERROR_CHECK(esp_event_loop_init(network_event_handler, NULL));
    ESP_LOGI(TAG, "Starting network\n");
    esp_netif_t *eth_netif = eth_start();

    while (!network_got_ip) {
        vTaskDelay(1);
    }

    term ret_value = avm_test_case("test_socket");
    TEST_ASSERT(term_to_int(ret_value) == 0);

    ESP_LOGI(TAG, "Stopping network\n");
    eth_stop(eth_netif);
}
#endif

void app_main(void)
{
    UNITY_BEGIN();
    unity_run_all_tests();
    UNITY_END();
}
