/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
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

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_SPI_PORT_DRIVER

#include "include/spi_driver.h"

#include <string.h>

#include <driver/spi_master.h>

#include <esp_log.h>
#include <freertos/FreeRTOS.h>
#include <freertos/task.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "platform_defaultatoms.h"
#include "port.h"
#include "scheduler.h"
#include "term.h"
#include "utils.h"

// #define ENABLE_TRACE
#include "trace.h"

#include "esp32_sys.h"
#include "sys.h"

#define TAG "spi_driver"

#ifdef CONFIG_IDF_TARGET_ESP32S2
#define VSPI_HOST   SPI2_HOST
#define HSPI_HOST   SPI3_HOST
#elif CONFIG_IDF_TARGET_ESP32S3
#define VSPI_HOST   SPI2_HOST
#define HSPI_HOST   SPI3_HOST
#elif CONFIG_IDF_TARGET_ESP32C3
// only one user SPI bus, no VSPI
#define HSPI_HOST   SPI2_HOST
#endif

struct SPIDevice
{
    struct ListHead list_head;
    term device_name;
    spi_device_handle_t handle;
};

struct SPIData
{
    struct ListHead devices;
    spi_host_device_t host_device;
};

static NativeHandlerResult spidriver_consume_mailbox(Context *ctx);
static uint32_t spidriver_transfer_at(struct SPIDevice *device, uint64_t address, int data_len, uint32_t data, esp_err_t *err);
static term create_pair(Context *ctx, term term1, term term2);

static const char *const spi_driver_atom = "\xA" "spi_driver";
static const char *const device_not_found_atom = "\x10" "device_not_found";
static const char *const command_atom = "\x7" "command";
static const char *const address_atom = "\x7" "address";
static const char *const write_data_atom = "\xA" "write_data";
static const char *const write_bits_atom = "\xA" "write_bits";
static const char *const read_bits_atom = "\x9" "read_bits";

static term spi_driver;

static void spi_driver_init(GlobalContext *global);

enum spi_cmd
{
    SPIInvalidCmd = 0,
    SPIReadAtCmd,
    SPIWriteAtCmd,
    SPIWriteCmd,
    SPIWriteReadCmd,
    SPICloseCmd
};

static const AtomStringIntPair spi_cmd_table[] = {
    { ATOM_STR("\x7", "read_at"), SPIReadAtCmd },
    { ATOM_STR("\x8", "write_at"), SPIWriteAtCmd },
    { ATOM_STR("\x5", "write"), SPIWriteCmd },
    { ATOM_STR("\xA", "write_read"), SPIWriteReadCmd },
    { ATOM_STR("\x5", "close"), SPICloseCmd },
    SELECT_INT_DEFAULT(SPIInvalidCmd)
};

static const AtomStringIntPair spi_host_table[] = {
// aliases for different chips, deprecated for the chips after esp32s2
#ifdef SPI_HOST
    { ATOM_STR("\x3", "spi"), SPI_HOST },
#endif
#ifdef HSPI_HOST
    { ATOM_STR("\x4", "hspi"), HSPI_HOST },
#endif
#ifdef VSPI_HOST
    { ATOM_STR("\x4", "vspi"), VSPI_HOST },
#endif
#ifdef FSPI_HOST
    { ATOM_STR("\x4", "fspi"), FSPI_HOST },
#endif
    { ATOM_STR("\x4", "spi1"), SPI1_HOST },
    { ATOM_STR("\x4", "spi2"), SPI2_HOST },
#if SOC_SPI_PERIPH_NUM > 2
    { ATOM_STR("\x4", "spi3"), SPI3_HOST },
#endif
    SELECT_INT_DEFAULT(-1)
};

static spi_host_device_t get_spi_host_device(term spi_peripheral, GlobalContext *global)
{
    int peripheral = interop_atom_term_select_int(spi_host_table, spi_peripheral, global);

    if (peripheral < 0) {
        ESP_LOGW(TAG, "Unrecognized SPI peripheral.  Defaulting to spi2.");
        return SPI2_HOST;
    }

    return peripheral;
}

static struct SPIDevice *get_spi_device(struct SPIData *spi_data, term device_term)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &spi_data->devices) {
        struct SPIDevice *device = GET_LIST_ENTRY(item, struct SPIDevice, list_head);
        if (device->device_name == device_term) {
            return device;
        }
    }
    return NULL;
}

static void debug_buscfg(spi_bus_config_t *buscfg)
{
    TRACE("Bus Config\n");
    TRACE("==========\n");
    TRACE("    miso: %i\n", buscfg->miso_io_num);
    TRACE("    mosi: %i\n", buscfg->mosi_io_num);
    TRACE("    sclk: %i\n", buscfg->sclk_io_num);
    TRACE("    miso: %i\n", buscfg->miso_io_num);
    TRACE("    quadwp_io_num: %i\n", buscfg->quadwp_io_num);
    TRACE("    quadhd_io_num: %i\n", buscfg->quadhd_io_num);
}

static void debug_devcfg(spi_device_interface_config_t *devcfg)
{
    TRACE("Device Config\n");
    TRACE("==========\n");
    TRACE("    clock_speed_hz: %i\n", devcfg->clock_speed_hz);
    TRACE("    mode: %i\n", devcfg->mode);
    TRACE("    cs: %i\n", devcfg->spics_io_num);
    TRACE("    queue_size: %i\n", devcfg->queue_size);
    TRACE("    address_bits: %i\n", devcfg->address_bits);
}

void spi_driver_init(GlobalContext *global)
{
    int index = globalcontext_insert_atom(global, spi_driver_atom);
    spi_driver = term_from_atom_index(index);
}

Context *spi_driver_create_port(GlobalContext *global, term opts)
{
    TRACE("spi_driver_create_port\n");
    Context *ctx = context_new(global);

    term hspi_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "hspi"));

    term bus_config = interop_kv_get_value(opts, ATOM_STR("\xA", "bus_config"), global);
    term miso_term = interop_kv_get_value(bus_config, ATOM_STR("\x4", "miso"), global);
    term mosi_term = interop_kv_get_value(bus_config, ATOM_STR("\x4", "mosi"), global);
    term sclk_term = interop_kv_get_value(bus_config, ATOM_STR("\x4", "sclk"), global);
    term peripheral_term = interop_kv_get_value_default(bus_config, ATOM_STR("\xA", "peripheral"), hspi_atom, global);
    spi_host_device_t host_device = get_spi_host_device(peripheral_term, global);

    spi_bus_config_t buscfg = { 0 };
    buscfg.miso_io_num = term_to_int32(miso_term);
    buscfg.mosi_io_num = term_to_int32(mosi_term);
    buscfg.sclk_io_num = term_to_int32(sclk_term);
    buscfg.quadwp_io_num = -1;
    buscfg.quadhd_io_num = -1;

    debug_buscfg(&buscfg);

    // TODO: workaround until a proper semantic is investigated
    #ifdef CONFIG_IDF_TARGET_ESP32
        esp_err_t err = spi_bus_initialize(host_device, &buscfg, 1);
    #else
        esp_err_t err = spi_bus_initialize(host_device, &buscfg, SPI_DMA_CH_AUTO);
    #endif
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "SPI Bus initialization failed with error=%i", err);
        context_destroy(ctx);
        return NULL;
    } else {
        ESP_LOGI(TAG, "SPI Bus initialized.");
    }

    struct SPIData *spi_data = calloc(1, sizeof(struct SPIData));
    list_init(&spi_data->devices);
    // TODO handle out of memory errors
    spi_data->host_device = host_device;

    term device_map = interop_kv_get_value(opts, ATOM_STR("\xD", "device_config"), global);
    term device_names = term_get_map_keys(device_map);

    int n = term_get_map_size(device_map);
    for (int i = 0; i < n; ++i) {
        term device_name = term_get_tuple_element(device_names, i);
        term device_config = term_get_map_assoc(device_map, device_name, ctx->global);

        term clock_speed_hz_term = interop_kv_get_value(device_config, ATOM_STR("\xE", "clock_speed_hz"), global);
        term mode_term = interop_kv_get_value(device_config, ATOM_STR("\x4", "mode"), global);
        term cs_term = interop_kv_get_value(device_config, ATOM_STR("\x2", "cs"), global);
        term address_bits_term = interop_kv_get_value(device_config, ATOM_STR("\x10", "address_len_bits"), global);
        term command_bits_term = interop_kv_get_value(device_config, ATOM_STR("\x10", "command_len_bits"), global);

        spi_device_interface_config_t devcfg = { 0 };
        devcfg.clock_speed_hz = term_to_int32(clock_speed_hz_term);
        devcfg.mode = term_to_int32(mode_term);
        devcfg.spics_io_num = term_to_int32(cs_term);
        devcfg.queue_size = 4;
        devcfg.address_bits = term_to_int32(address_bits_term);
        devcfg.command_bits = term_to_int32(command_bits_term);

        spi_device_handle_t handle;
        err = spi_bus_add_device(host_device, &devcfg, &handle);
        if (err != ESP_OK) {
            // TODO cleanup and previously created devices
            context_destroy(ctx);
            ESP_LOGE(TAG, "Failed to add SPI device. error=%i", err);
            return NULL;
        } else {
            debug_devcfg(&devcfg);
            struct SPIDevice *spi_device = malloc(sizeof(struct SPIDevice));
            // TODO handle out of memory errors
            spi_device->device_name = device_name;
            spi_device->handle = handle;
            list_append(&spi_data->devices, &spi_device->list_head);
            char *str = interop_atom_to_string(ctx, device_name);
            ESP_LOGI(TAG, "SPI device %s added.", str);
            free(str);
        }
    }

    ctx->native_handler = spidriver_consume_mailbox;
    ctx->platform_data = spi_data;

    return ctx;
}

static term device_not_found_error(Context *ctx)
{
    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }
    return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, device_not_found_atom));
}

static term esp_err_tuple(esp_err_t err, Context *ctx)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }
    return create_pair(ctx, ERROR_ATOM, esp_err_to_term(ctx->global, err));
}

static term spidriver_close(Context *ctx)
{
    TRACE("spidriver_close\n");
    struct SPIData *spi_data = ctx->platform_data;

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &spi_data->devices) {
        struct SPIDevice *device = GET_LIST_ENTRY(item, struct SPIDevice, list_head);
        esp_err_t err = spi_bus_remove_device(device->handle);
        if (err != ESP_OK) {
            ESP_LOGW(TAG, "Error removing device.  err=%i", err);
        } else {
            ESP_LOGI(TAG, "Removed SPI device.");
        }
        list_remove(item);
        free(device);
    }

    esp_err_t err = spi_bus_free(spi_data->host_device);
    if (err != ESP_OK) {
        ESP_LOGW(TAG, "Error freeing bus.  err=%i", err);
    } else {
        ESP_LOGI(TAG, "Stopped SPI Bus.");
    }
    free(spi_data);

    return OK_ATOM;
}

static uint32_t spidriver_transfer_at(struct SPIDevice *device, uint64_t address, int data_len, uint32_t data, esp_err_t *err)
{
    TRACE("--- SPI transfer ---\n");
    TRACE("spi: address: %x, tx: %x\n", (int) address, (int) data);

    uint32_t tx_data = SPI_SWAP_DATA_TX(data, data_len);

    struct spi_transaction_t transaction = { 0 };
    transaction.flags = SPI_TRANS_USE_TXDATA | SPI_TRANS_USE_RXDATA;
    transaction.length = data_len;
    transaction.addr = address;
    transaction.tx_data[0] = tx_data;
    transaction.tx_data[1] = (tx_data >> 8) & 0xFF;
    transaction.tx_data[2] = (tx_data >> 16) & 0xFF;
    transaction.tx_data[3] = (tx_data >> 24) & 0xFF;

    //TODO: int ret = spi_device_queue_trans(device->handle, &transaction, portMAX_DELAY);
    *err = spi_device_polling_transmit(device->handle, &transaction);
    if (UNLIKELY(*err != ESP_OK)) {
        return 0;
    }

    //TODO check return code

    uint32_t rx_data = ((uint32_t) transaction.rx_data[0]) |
        ((uint32_t) transaction.rx_data[1] << 8) |
        ((uint32_t) transaction.rx_data[2] << 16) |
        ((uint32_t) transaction.rx_data[3] << 24);

    TRACE("spi: ret: %x\n", (int) ret);
    TRACE("spi: rx: %x\n", (int) rx_data);
    TRACE("--- end of transfer ---\n");

    return SPI_SWAP_DATA_RX(rx_data, data_len);
}

static inline term make_read_result_tuple(uint32_t read_value, Context *ctx)
{
    bool boxed;
    int required;
    if (read_value > MAX_NOT_BOXED_INT) {
        boxed = true;
        required = 3 + BOXED_INT_SIZE;
    } else {
        boxed = false;
        required = 3;
    }

    if (UNLIKELY(memory_ensure_free(ctx, required) != MEMORY_GC_OK)) {
        return ERROR_ATOM;
    }

    term read_value_term = boxed ? term_make_boxed_int(read_value, &ctx->heap) : term_from_int(read_value);

    term result_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, read_value_term);

    return result_tuple;
}

static term spidriver_read_at(Context *ctx, term req)
{
    TRACE("spidriver_read_at\n");
    struct SPIData *spi_data = ctx->platform_data;

    // cmd is at index 0
    term device_term = term_get_tuple_element(req, 1);
    term address_term = term_get_tuple_element(req, 2);
    term len_term = term_get_tuple_element(req, 3);

    struct SPIDevice *device = get_spi_device(spi_data, device_term);
    if (IS_NULL_PTR(device)) {
        return device_not_found_error(ctx);
    }

    avm_int64_t address = term_maybe_unbox_int64(address_term);
    avm_int_t data_len = term_to_int(len_term);

    esp_err_t err;
    uint32_t read_value = spidriver_transfer_at(device, address, data_len, 0, &err);
    if (UNLIKELY(err != ESP_OK)) {
        return esp_err_tuple(err, ctx);
    }

    return make_read_result_tuple(read_value, ctx);
}

static term spidriver_write_at(Context *ctx, term req)
{
    TRACE("spidriver_write_at\n");
    struct SPIData *spi_data = ctx->platform_data;

    // cmd is at index 0
    term device_term = term_get_tuple_element(req, 1);
    term address_term = term_get_tuple_element(req, 2);
    term len_term = term_get_tuple_element(req, 3);
    term data_term = term_get_tuple_element(req, 4);

    struct SPIDevice *device = get_spi_device(spi_data, device_term);
    if (IS_NULL_PTR(device)) {
        return device_not_found_error(ctx);
    }

    uint64_t address = term_maybe_unbox_int64(address_term);
    avm_int_t data_len = term_to_int(len_term);
    avm_int_t data = term_maybe_unbox_int(data_term);

    esp_err_t err;
    uint32_t read_value = spidriver_transfer_at(device, address, data_len, data, &err);
    if (UNLIKELY(err != ESP_OK)) {
        return esp_err_tuple(err, ctx);
    }

    return make_read_result_tuple(read_value, ctx);
}

static term populate_transaction(Context *ctx, struct spi_transaction_t *transaction, term transaction_term, bool write_only, size_t *output_size)
{
    GlobalContext *glb = ctx->global;
    term zero_term = term_from_int(0);

    term command_term = term_get_map_assoc_default(transaction_term, globalcontext_make_atom(glb, command_atom), zero_term, ctx->global);
    if (!term_is_integer(command_term)) {
        ESP_LOGE(TAG, "command transaction entry is not an integer");
        return BADARG_ATOM;
    }
    avm_int_t command_value = term_to_int(command_term);
    if (command_value < 0 || command_value > UINT16_MAX) {
        ESP_LOGE(TAG, "command transaction entry is not an integer between 0 and 2^16");
        return BADARG_ATOM;
    }
    transaction->cmd = (uint16_t) command_value;

    term address_term = term_get_map_assoc_default(transaction_term, globalcontext_make_atom(glb, address_atom), zero_term, ctx->global);
    if (!term_is_any_integer(address_term)) {
        ESP_LOGE(TAG, "address transaction entry is not an integer");
        return BADARG_ATOM;
    }
    transaction->addr = (uint64_t) term_maybe_unbox_int(address_term);

    term write_data_term = term_get_map_assoc_default(transaction_term, globalcontext_make_atom(glb, write_data_atom), UNDEFINED_ATOM, ctx->global);
    term binary_bits_term = zero_term;
    if (write_data_term != UNDEFINED_ATOM) {
        if (!term_is_binary(write_data_term)) {
            ESP_LOGE(TAG, "write_data transaction entry is not a binary");
            return BADARG_ATOM;
        }
        transaction->tx_buffer = (const uint8_t *) term_binary_data(write_data_term);

        size_t binary_bits = term_binary_size(write_data_term) * 8;
        binary_bits_term = term_from_int(binary_bits);
        term write_bits_term = term_get_map_assoc_default(transaction_term, globalcontext_make_atom(glb, write_bits_atom), binary_bits_term, ctx->global);
        if (!term_is_integer(write_bits_term)) {
            ESP_LOGE(TAG, "write_bits transaction entry is not an integer");
            return BADARG_ATOM;
        }
        size_t write_bits = (size_t) term_to_int(write_bits_term);
        if (write_bits > binary_bits) {
            ESP_LOGE(TAG, "More write bits specified (%u) than are available (%u)", write_bits, binary_bits);
            return BADARG_ATOM;
        }
        transaction->length = write_bits;
    }

    if (!write_only) {
        term read_bits_term = term_get_map_assoc_default(transaction_term, globalcontext_make_atom(glb, read_bits_atom), binary_bits_term, ctx->global);
        if (!term_is_integer(read_bits_term)) {
            ESP_LOGE(TAG, "read_bits transaction entry is not an integer");
            return BADARG_ATOM;
        }
        size_t read_bits = (size_t) term_to_int(read_bits_term);
        transaction->rxlength = read_bits;
        *output_size = (read_bits % 8) == 0 ? (read_bits / 8) : ((read_bits / 8) + 1);
    }

    return OK_ATOM;
}

//
// write operations
//

static term spidriver_write(Context *ctx, term req)
{
    TRACE("spidriver_write\n");
    struct SPIData *spi_data = ctx->platform_data;

    // cmd is at index 0
    term device_term = term_get_tuple_element(req, 1);
    term transaction_term = term_get_tuple_element(req, 2);

    struct SPIDevice *device = get_spi_device(spi_data, device_term);
    if (IS_NULL_PTR(device)) {
        return device_not_found_error(ctx);
    }

    struct spi_transaction_t transaction = { 0 };
    size_t output_size = 0;
    term err_term = populate_transaction(ctx, &transaction, transaction_term, true, &output_size);
    if (err_term != OK_ATOM) {
        ESP_LOGE(TAG, "Invalid transaction");
        // gc is ok as err_term is an atom
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_pair(ctx, ERROR_ATOM, err_term);
    }

    // TODO replace spi_device_polling_transmit with a interrupt-based mechanism
    esp_err_t err = spi_device_polling_transmit(device->handle, &transaction);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "spidriver_write failed with err=%i", err);
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_pair(ctx, ERROR_ATOM, term_from_int(err));
    }
    return OK_ATOM;
}

//
// write_read operations
//

static term spidriver_write_read(Context *ctx, term req)
{
    TRACE("spidriver_write_read\n");
    struct SPIData *spi_data = ctx->platform_data;

    // cmd is at index 0
    term device_term = term_get_tuple_element(req, 1);
    term transaction_term = term_get_tuple_element(req, 2);

    struct SPIDevice *device = get_spi_device(spi_data, device_term);
    if (IS_NULL_PTR(device)) {
        return device_not_found_error(ctx);
    }

    struct spi_transaction_t transaction = { 0 };
    size_t output_size = 0;
    term err_term = populate_transaction(ctx, &transaction, transaction_term, false, &output_size);
    if (err_term != OK_ATOM) {
        ESP_LOGE(TAG, "Invalid transaction");
        // gc is ok as err_term is an atom
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_pair(ctx, ERROR_ATOM, err_term);
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(output_size) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }
    term output_data_term = term_create_empty_binary(output_size, &ctx->heap, ctx->global);
    transaction.rx_buffer = (uint8_t *) term_binary_data(output_data_term);

    // TODO replace spi_device_polling_transmit with a interrupt-based mechanism
    esp_err_t err = spi_device_polling_transmit(device->handle, &transaction);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "spidriver_write_read failed with err=%i", err);
        return create_pair(ctx, ERROR_ATOM, term_from_int(err));
    }
    return create_pair(ctx, OK_ATOM, output_data_term);
}

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static NativeHandlerResult spidriver_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    GenMessage gen_message;
    if (UNLIKELY(port_parse_gen_message(message->message, &gen_message) != GenCallMessage)) {
        ESP_LOGW(TAG, "Received invalid message.");
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
        return NativeContinue;
    }

    term cmd_term = term_get_tuple_element(gen_message.req, 0);

    int local_process_id = term_to_local_process_id(gen_message.pid);
    Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);

    term ret;

    enum spi_cmd cmd = interop_atom_term_select_int(spi_cmd_table, cmd_term, ctx->global);
    switch (cmd) {
        case SPIReadAtCmd:
            TRACE("spi: read at.\n");
            ret = spidriver_read_at(ctx, gen_message.req);
            break;

        case SPIWriteAtCmd:
            TRACE("spi: write at.\n");
            ret = spidriver_write_at(ctx, gen_message.req);
            break;

        case SPIWriteCmd:
            TRACE("spi: write.\n");
            ret = spidriver_write(ctx, gen_message.req);
            break;

        case SPIWriteReadCmd:
            TRACE("spi: write_read.\n");
            ret = spidriver_write_read(ctx, gen_message.req);
            break;

        case SPICloseCmd:
            ret = spidriver_close(ctx);
            break;

        default:
            TRACE("spi: error: unrecognized command.\n");
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                return OUT_OF_MEMORY_ATOM;
            }
            term unkn_a = globalcontext_make_atom(ctx->global, ATOM_STR("\xF", "unknown_command"));
            return create_pair(ctx, ERROR_ATOM, esp_err_to_term(ctx->global, unkn_a));
    }

    term ret_msg;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &ret, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ret_msg = OUT_OF_MEMORY_ATOM;
    } else {
        ret_msg = create_pair(ctx, gen_message.ref, ret);
    }

    mailbox_send(target, ret_msg);
    globalcontext_get_process_unlock(ctx->global, target);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return cmd == CLOSE_ATOM ? NativeTerminate : NativeContinue;
}

bool spi_driver_get_peripheral(term spi_port, spi_host_device_t *host_dev, GlobalContext *global)
{
    if (UNLIKELY(!term_is_pid(spi_port))) {
        ESP_LOGW(TAG, "Given term is not a SPI port driver.");
        return false;
    }

    int local_process_id = term_to_local_process_id(spi_port);
    Context *ctx = globalcontext_get_process_lock(global, local_process_id);

    if (ctx->native_handler != spidriver_consume_mailbox) {
        ESP_LOGW(TAG, "Given term is not a SPI port driver.");
        globalcontext_get_process_unlock(global, ctx);
        return false;
    }

    struct SPIData *spi_data = ctx->platform_data;
    *host_dev = spi_data->host_device;
    globalcontext_get_process_unlock(global, ctx);
    return true;
}

REGISTER_PORT_DRIVER(spi, spi_driver_init, NULL, spi_driver_create_port)

#endif
