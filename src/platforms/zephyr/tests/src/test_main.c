/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <zephyr/ztest.h>
#include <zephyr/devicetree.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <avmpack.h>
#include <context.h>
#include <globalcontext.h>
#include <module.h>
#include <nifs.h>
#include <defaultatoms.h>

#include "../../src/lib/avm_devcfg.h"
#include "../../src/lib/avm_log.h"
#include "../../src/lib/zephyros_sys.h"

#define TAG "AtomVM"

extern const uint8_t zephyr_test_modules_avm[];
extern const size_t zephyr_test_modules_avm_size;

term avm_test_case(const char *test_module)
{
    GlobalContext *glb = globalcontext_new();
    zassert_not_null(glb, "Failed to create GlobalContext");

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    zassert_true(avmpack_is_valid(zephyr_test_modules_avm, zephyr_test_modules_avm_size), "Invalid AVM pack");

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    zassert_not_null(avmpack_data, "Failed to allocate AVMPack");

    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.in_use = true;
    avmpack_data->base.data = zephyr_test_modules_avm;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = globalcontext_load_module_from_avm(glb, test_module);
    zassert_not_null(mod, "Failed to load module %s", test_module);

    globalcontext_insert_module(glb, mod);

    Context *ctx = context_new(glb);
    zassert_not_null(ctx, "Failed to create context");
    ctx->leader = 1;

    AVM_LOGI(TAG, "Running start/0 from %s...\n", test_module);

    context_execute_loop(ctx, mod, "start", 0);
    term ret_value = ctx->x[0];

    fprintf(stdout, "AtomVM finished with return value: ");
    term_display(stdout, ret_value, ctx);
    if (ret_value == ERROR_ATOM || ret_value == EXIT_ATOM || ret_value == THROW_ATOM) {
        fprintf(stdout, " (reason: ");
        term_display(stdout, ctx->x[1], ctx);
        fprintf(stdout, ")");
    }
    fprintf(stdout, "\n");

    context_destroy(ctx);

    nif_collection_destroy_all(glb);
    port_driver_destroy_all(glb);

    globalcontext_destroy(glb);

    return ret_value;
}

ZTEST_SUITE(atomvm_tests, NULL, NULL, NULL, NULL, NULL);

#if defined(CONFIG_SOC_FAMILY_ESPRESSIF_ESP32) && defined(CONFIG_POWEROFF)
ZTEST(atomvm_tests, test_deep_sleep)
{
    term ret_value = avm_test_case("test_deep_sleep.beam");
    zassert_equal(ret_value, OK_ATOM, "test_deep_sleep did not return 'ok'");
}
#endif

#if defined(CONFIG_ADC) && DT_NODE_HAS_PROP(DT_PATH(zephyr_user), io_channels)
ZTEST(atomvm_tests, test_adc)
{
    term ret_value = avm_test_case("test_adc.beam");
    zassert_equal(ret_value, OK_ATOM, "test_adc did not return 'ok'");
}
#endif

#if defined(CONFIG_GPIO) && defined(DT_N_NODELABEL_gpio0) && DT_NODE_HAS_STATUS(DT_NODELABEL(gpio0), okay)
ZTEST(atomvm_tests, test_gpio)
{
    term ret_value = avm_test_case("test_gpio.beam");
    zassert_equal(ret_value, OK_ATOM, "test_gpio did not return 'ok'");
}
#endif

#if defined(CONFIG_RETENTION) && DT_HAS_CHOSEN(atomvm_retention)
ZTEST(atomvm_tests, test_retention)
{
    term ret_value = avm_test_case("test_retention.beam");
    zassert_equal(ret_value, OK_ATOM, "test_retention did not return 'ok'");
}
#endif

#if defined(CONFIG_SPI) && DT_HAS_CHOSEN(atomvm_spi)
ZTEST(atomvm_tests, test_spi)
{
    term ret_value = avm_test_case("test_spi.beam");
    zassert_equal(ret_value, OK_ATOM, "test_spi did not return 'ok'");
}
#endif

#if defined(CONFIG_PWM) && DT_HAS_CHOSEN(atomvm_pwm)
ZTEST(atomvm_tests, test_pwm)
{
    term ret_value = avm_test_case("test_pwm.beam");
    zassert_equal(ret_value, OK_ATOM, "test_pwm did not return 'ok'");
}
#endif

#if defined(CONFIG_DAC) && DT_HAS_CHOSEN(atomvm_dac)
ZTEST(atomvm_tests, test_dac)
{
    term ret_value = avm_test_case("test_dac.beam");
    zassert_equal(ret_value, OK_ATOM, "test_dac did not return 'ok'");
}
#endif

#if defined(CONFIG_SERIAL)
ZTEST(atomvm_tests, test_uart)
{
    term ret_value = avm_test_case("test_uart.beam");
    zassert_equal(ret_value, OK_ATOM, "test_uart did not return 'ok'");
}
#endif

ZTEST(atomvm_tests, test_list_to_atom)
{
    term ret_value = avm_test_case("test_list_to_atom.beam");
    zassert_equal(ret_value, OK_ATOM, "test_list_to_atom did not return 'ok'");
}

ZTEST(atomvm_tests, test_system_architecture)
{
    term ret_value = avm_test_case("test_system_architecture.beam");
    zassert_equal(ret_value, OK_ATOM, "test_system_architecture did not return 'ok'");
}

ZTEST(atomvm_tests, test_list_to_binary)
{
    term ret_value = avm_test_case("test_list_to_binary.beam");
    zassert_equal(ret_value, OK_ATOM, "test_list_to_binary did not return 'ok'");
}
const struct Nif *platform_nifs_get_nif(const char *nifname);

ZTEST(atomvm_tests, test_platform_nif)
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:platform/0");
    zassert_not_null(nif, "Failed to resolve atomvm:platform/0");
}

ZTEST(atomvm_tests, test_missing_nif)
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:missing/0");
    zassert_is_null(nif, "Resolved missing/0 which should be NULL");
}

ZTEST(atomvm_tests, test_tz)
{
    term ret_value = avm_test_case("test_tz.beam");
    zassert_equal(ret_value, OK_ATOM, "test_tz did not return 'ok'");
}

ZTEST(atomvm_tests, test_md5)
{
    term ret_value = avm_test_case("test_md5.beam");
    zassert_equal(ret_value, OK_ATOM, "test_md5 did not return 'ok'");
}

ZTEST(atomvm_tests, test_crypto)
{
    term ret_value = avm_test_case("test_crypto.beam");
    zassert_equal(ret_value, OK_ATOM, "test_crypto did not return 'ok'");
}

#if defined(CONFIG_WIFI)
ZTEST(atomvm_tests, test_wifi_ap)
{
    term ret_value = avm_test_case("test_wifi_ap.beam");
    zassert_equal(ret_value, OK_ATOM, "test_wifi_ap did not return 'ok'");
}

ZTEST(atomvm_tests, test_wifi_scan)
{
    term ret_value = avm_test_case("test_wifi_scan.beam");
    zassert_equal(ret_value, OK_ATOM, "test_wifi_scan did not return 'ok'");
}

ZTEST(atomvm_tests, test_wifi_managed)
{
    term ret_value = avm_test_case("test_wifi_managed.beam");
    zassert_equal(ret_value, OK_ATOM, "test_wifi_managed did not return 'ok'");
}

ZTEST(atomvm_tests, test_wifi_example)
{
    term ret_value = avm_test_case("test_wifi_example.beam");
    zassert_equal(ret_value, OK_ATOM, "test_wifi_example did not return 'ok'");
}
#endif

#if defined(CONFIG_WIFI) && defined(CONFIG_AVM_ENABLE_CRYPTO) && defined(CONFIG_NET_SOCKETS) \
    && !defined(CONFIG_SOC_SERIES_ESP32C3) && !defined(CONFIG_SOC_SERIES_ESP32S3)
ZTEST(atomvm_tests, test_ssl)
{
    term ret_value = avm_test_case("test_ssl.beam");
    zassert_equal(ret_value, OK_ATOM, "test_ssl did not return 'ok'");
}
#endif

#if defined(CONFIG_WIFI)
/* Named so ZTEST sorts after test_ssl / test_wifi_*. Leftover AP
 * sockets starve later STA cases if this runs first. */
ZTEST(atomvm_tests, test_wifi_z_devmode)
{
    term ret_value = avm_test_case("test_devmode.beam");
    zassert_equal(ret_value, OK_ATOM, "test_devmode did not return 'ok'");
}
#endif

#if defined(CONFIG_NET_SOCKETPAIR) && !defined(CONFIG_BOARD_NATIVE_SIM)
ZTEST(atomvm_tests, test_select)
{
    term ret_value = avm_test_case("test_select.beam");
    zassert_equal(ret_value, OK_ATOM, "test_select did not return 'ok'");
}
#endif

#if !defined(CONFIG_BOARD_NATIVE_SIM) && defined(CONFIG_FAT_FILESYSTEM_ELM)
ZTEST(atomvm_tests, test_mount)
{
    term ret_value = avm_test_case("test_mount.beam");
    zassert_equal(ret_value, OK_ATOM, "test_mount did not return 'ok'");
}
#endif

#if !defined(CONFIG_BOARD_NATIVE_SIM)
ZTEST(atomvm_tests, test_monotonic_time)
{
    term ret_value = avm_test_case("test_monotonic_time.beam");
    zassert_equal(ret_value, OK_ATOM, "test_monotonic_time did not return 'ok'");
}

ZTEST(atomvm_tests, test_time_and_processes)
{
    term ret_value = avm_test_case("test_time_and_processes.beam");
    zassert_equal(term_to_int(ret_value), 6, "test_time_and_processes did not return 6");
}
#endif

#if !defined(CONFIG_BOARD_NATIVE_SIM) && defined(CONFIG_FAT_FILESYSTEM_ELM)
ZTEST(atomvm_tests, test_file)
{
    term ret_value = avm_test_case("test_file.beam");
    zassert_equal(ret_value, OK_ATOM, "test_file did not return 'ok'");
}
#endif

#if defined(AVM_ZEPHYR_NATIVE_SIM_I2C_TEST)
ZTEST(atomvm_tests, test_i2c_native_sim)
{
    term ret_value = avm_test_case("test_i2c_native_sim.beam");
    zassert_equal(ret_value, OK_ATOM, "test_i2c_native_sim did not return 'ok'");
}
#endif

#if defined(CONFIG_PM)
ZTEST(atomvm_tests, test_deep_sleep_hold)
{
    term ret_value = avm_test_case("test_deep_sleep_hold.beam");
    zassert_equal(ret_value, OK_ATOM, "test_deep_sleep_hold did not return 'ok'");
}
#endif

#if defined(CONFIG_TASK_WDT)
ZTEST(atomvm_tests, test_twdt)
{
    term ret_value = avm_test_case("test_twdt.beam");
    zassert_equal(ret_value, OK_ATOM, "test_twdt did not return 'ok'");
}
#endif

ZTEST(atomvm_tests, test_platform)
{
    term ret_value = avm_test_case("test_platform.beam");
    zassert_equal(ret_value, OK_ATOM, "test_platform did not return 'ok'");
}

#if defined(CONFIG_SETTINGS)
ZTEST(atomvm_tests, test_settings)
{
    term ret_value = avm_test_case("test_settings.beam");
    zassert_equal(ret_value, OK_ATOM, "test_settings did not return 'ok'");
}
#endif

#if defined(CONFIG_FLASH_MAP)
ZTEST(atomvm_tests, test_flash_map)
{
    term ret_value = avm_test_case("test_flash_map.beam");
    zassert_equal(ret_value, OK_ATOM, "test_flash_map did not return 'ok'");
}
#endif
