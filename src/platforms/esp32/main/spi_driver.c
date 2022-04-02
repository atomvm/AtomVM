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

#include "spi_driver.h"

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
#include "dictionary.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "platform_defaultatoms.h"
#include "term.h"
#include "utils.h"

// #define ENABLE_TRACE
#include "trace.h"

#include "esp32_sys.h"
#include "sys.h"

#define TAG "spi_driver"

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

static void spidriver_consume_mailbox(Context *ctx);
static uint32_t spidriver_transfer_at(struct SPIDevice *device, uint64_t address, int data_len, uint32_t data, bool *ok);
static term create_pair(Context *ctx, term term1, term term2);

static const char *const spi_driver_atom = "\xA" "spi_driver";
static const char *const device_not_found_atom = "\x10" "device_not_found";
static term spi_driver;

static spi_host_device_t get_spi_host_device(term spi_peripheral)
{
    switch (spi_peripheral) {
        case HSPI_ATOM:
            return HSPI_HOST;
        case VSPI_ATOM:
            return VSPI_HOST;
        default:
            ESP_LOGW(TAG, "Unrecognized SPI peripheral.  Must be either hspi or vspi.  Defaulting to hspi.");
            return HSPI_HOST;
    }
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
    TRACE("    miso_io_num: %i\n", buscfg->miso_io_num);
    TRACE("    mosi_io_num: %i\n", buscfg->mosi_io_num);
    TRACE("    sclk_io_num: %i\n", buscfg->sclk_io_num);
    TRACE("    miso_io_num: %i\n", buscfg->miso_io_num);
    TRACE("    quadwp_io_num: %i\n", buscfg->quadwp_io_num);
    TRACE("    quadhd_io_num: %i\n", buscfg->quadhd_io_num);
}

static void debug_devcfg(spi_device_interface_config_t *devcfg)
{
    TRACE("Device Config\n");
    TRACE("==========\n");
    TRACE("    clock_speed_hz: %i\n", devcfg->clock_speed_hz);
    TRACE("    mode: %i\n", devcfg->mode);
    TRACE("    spics_io_num: %i\n", devcfg->spics_io_num);
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

    term bus_config = term_get_map_assoc(ctx, opts, BUS_CONFIG_ATOM);
    term miso_io_num_term = term_get_map_assoc(ctx, bus_config, MISO_IO_NUM_ATOM);
    term mosi_io_num_term = term_get_map_assoc(ctx, bus_config, MOSI_IO_NUM_ATOM);
    term sclk_io_num_term = term_get_map_assoc(ctx, bus_config, SCLK_IO_NUM_ATOM);
    term spi_peripheral_term = term_get_map_assoc_default(ctx, bus_config, SPI_PERIPHERAL_ATOM, HSPI_ATOM);
    spi_host_device_t host_device = get_spi_host_device(spi_peripheral_term);

    spi_bus_config_t buscfg = { 0 };
    buscfg.miso_io_num = term_to_int32(miso_io_num_term);
    buscfg.mosi_io_num = term_to_int32(mosi_io_num_term);
    buscfg.sclk_io_num = term_to_int32(sclk_io_num_term);
    buscfg.quadwp_io_num = -1;
    buscfg.quadhd_io_num = -1;

    debug_buscfg(&buscfg);

    esp_err_t err = spi_bus_initialize(host_device, &buscfg, 1);
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

    term device_map = term_get_map_assoc(ctx, opts, DEVICE_CONFIG_ATOM);
    term device_names = term_get_map_keys(device_map);

    int n = term_get_map_size(device_map);
    for (int i = 0; i < n; ++i) {
        term device_name = term_get_tuple_element(device_names, i);
        term device_config = term_get_map_assoc(ctx, device_map, device_name);

        term clock_speed_hz_term = term_get_map_assoc(ctx, device_config, SPI_CLOCK_HZ_ATOM);
        term mode_term = term_get_map_assoc(ctx, device_config, SPI_MODE_ATOM);
        term spics_io_num_term = term_get_map_assoc(ctx, device_config, SPI_CS_IO_NUM_ATOM);
        term address_bits_term = term_get_map_assoc(ctx, device_config, ADDRESS_LEN_BITS_ATOM);

        spi_device_interface_config_t devcfg = { 0 };
        devcfg.clock_speed_hz = term_to_int32(clock_speed_hz_term);
        devcfg.mode = term_to_int32(mode_term);
        devcfg.spics_io_num = term_to_int32(spics_io_num_term);
        devcfg.queue_size = 4;
        devcfg.address_bits = term_to_int32(address_bits_term);

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
            list_append(&spi_data->devices, (struct ListHead *) spi_device);
            char *str = interop_atom_to_string(ctx, device_name);
            ESP_LOGI(TAG, "SPI device %s added.", str);
            free(str);
        }
    }

    ctx->native_handler = spidriver_consume_mailbox;
    ctx->platform_data = spi_data;

    return ctx;
}

static uint32_t spidriver_transfer_at(struct SPIDevice *device, uint64_t address, int data_len, uint32_t data, bool *ok)
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
    int ret = spi_device_polling_transmit(device->handle, &transaction);
    if (UNLIKELY(ret != ESP_OK)) {
        *ok = false;
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

    *ok = true;
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

    term read_value_term = boxed ? term_make_boxed_int(read_value, ctx) : term_from_int(read_value);

    term result_tuple = term_alloc_tuple(2, ctx);
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
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_pair(ctx, ERROR_ATOM, context_make_atom(ctx, device_not_found_atom));
    }

    avm_int64_t address = term_maybe_unbox_int64(address_term);
    avm_int_t data_len = term_to_int(len_term);

    bool ok;
    uint32_t read_value = spidriver_transfer_at(device, address, data_len, 0, &ok);
    if (UNLIKELY(!ok)) {
        return ERROR_ATOM;
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
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_pair(ctx, ERROR_ATOM, context_make_atom(ctx, device_not_found_atom));
    }

    uint64_t address = term_maybe_unbox_int64(address_term);
    avm_int_t data_len = term_to_int(len_term);
    avm_int_t data = term_maybe_unbox_int(data_term);

    bool ok;
    uint32_t read_value = spidriver_transfer_at(device, address, data_len, data, &ok);
    if (UNLIKELY(!ok)) {
        return ERROR_ATOM;
    }

    return make_read_result_tuple(read_value, ctx);
}

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, ctx);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static void spidriver_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term req = term_get_tuple_element(msg, 2);

    term cmd = term_get_tuple_element(req, 0);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    term ret;

    switch (cmd) {
        case READ_AT_ATOM:
            TRACE("spi: read at.\n");
            ret = spidriver_read_at(ctx, req);
            break;

        case WRITE_AT_ATOM:
            TRACE("spi: write at.\n");
            ret = spidriver_write_at(ctx, req);
            break;

        default:
            TRACE("spi: error: unrecognized command.\n");
            ret = ERROR_ATOM;
    }

    dictionary_put(&ctx->dictionary, ctx, spi_driver, ret);
    term ret_msg;
    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        ret_msg = OUT_OF_MEMORY_ATOM;
    } else {
        ret = dictionary_get(&ctx->dictionary, ctx, spi_driver);
        ref = term_get_tuple_element(msg, 1);
        ret_msg = create_pair(ctx, ref, ret);
    }
    dictionary_erase(&ctx->dictionary, ctx, spi_driver);

    mailbox_send(target, ret_msg);
    mailbox_destroy_message(message);
}
