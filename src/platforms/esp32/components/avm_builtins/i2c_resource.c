/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
 * Copyright 2023 Fred Dushin <fred@dushin.net>
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

#include <stdlib.h>
#include <string.h>

#include <driver/i2c_master.h>
#include <driver/i2c_types.h>
#include <esp_log.h>

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <module.h>
#include <nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include <esp32_sys.h>
#include <sys.h>

#include "include/i2c_bus_manager.h"

#define TAG "i2c_resource"

#define CHECK_ERROR(ctx, err, msg)                                                      \
if (UNLIKELY(err != ESP_OK)) {                                                          \
    ESP_LOGE(TAG, msg ": err: %i.", err);                                               \
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {             \
        return OUT_OF_MEMORY_ATOM;                                                      \
    }                                                                                   \
    return create_error_tuple(ctx, esp_err_to_term(ctx->global, err));                  \
}

#define DEFAULT_SEND_TIMEOUT_MS 500

#define EINPROGRESS_ATOMSTR (ATOM_STR("\xB", "einprogress"))
#define I2C_ATOMSTR (ATOM_STR("\x4", "$i2c"))

static ErlNifResourceType *i2c_resource_type;

struct I2CResource
{
    term transmitting_pid;
    i2c_master_bus_handle_t bus_handle;
    struct I2CTxBuffer tx_buf;
    int i2c_port;
    uint32_t clock_speed_hz;
    // -1 (I2C_BUS_MANAGER_TIMEOUT_INFINITE) means wait forever, otherwise
    // this is in milliseconds, as expected by the new I2C master driver.
    int32_t xfer_timeout_ms;
    uint8_t tx_address;
};

#define I2C_RESOURCE_PORT_INVALID (-1)

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static term create_error_tuple(Context *ctx, term reason)
{
    return create_pair(ctx, ERROR_ATOM, reason);
}

static bool is_i2c_resource_open(const struct I2CResource *rsrc_obj)
{
    return rsrc_obj->i2c_port != I2C_RESOURCE_PORT_INVALID;
}

static void reset_i2c_transmission_state(struct I2CResource *rsrc_obj)
{
    i2c_tx_buffer_reset(&rsrc_obj->tx_buf);
    rsrc_obj->transmitting_pid = term_invalid_term();
}

static esp_err_t close_i2c_resource(struct I2CResource *rsrc_obj)
{
    if (!is_i2c_resource_open(rsrc_obj)) {
        return ESP_OK;
    }

    esp_err_t err = i2c_bus_manager_close(rsrc_obj->bus_handle);
    // Always release any pending tx_buf allocation, even if closing the bus
    // itself failed, so that a failed close (e.g. surfaced to `i2c:close/1`)
    // doesn't leak the pending transmission buffer for the lifetime of the
    // resource.
    reset_i2c_transmission_state(rsrc_obj);
    if (err == ESP_OK) {
        rsrc_obj->i2c_port = I2C_RESOURCE_PORT_INVALID;
    }

    return err;
}

static bool is_i2c_resource(GlobalContext *global, term t)
{
    bool ret = term_is_tuple(t)
        && term_get_tuple_arity(t) == 3
        && globalcontext_is_term_equal_to_atom_string(global, term_get_tuple_element(t, 0), I2C_ATOMSTR)
        && term_is_resource_reference(term_get_tuple_element(t, 1))
        && term_is_reference(term_get_tuple_element(t, 2));

    return ret;
}

static bool to_i2c_resource(term i2c_resource, struct I2CResource **rsrc_obj, Context *ctx)
{
    if (!is_i2c_resource(ctx->global, i2c_resource)) {
        return false;
    }
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), term_get_tuple_element(i2c_resource, 1), i2c_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct I2CResource *) rsrc_obj_ptr;

    return true;
}

static bool to_open_i2c_resource(term i2c_resource, struct I2CResource **rsrc_obj, Context *ctx)
{
    if (!to_i2c_resource(i2c_resource, rsrc_obj, ctx)) {
        return false;
    }

    return is_i2c_resource_open(*rsrc_obj);
}

//
// i2c:open_nif/1
//

static term nif_i2c_open(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    GlobalContext *global = ctx->global;
    term opts = argv[0];

    //
    // Validate inputs
    //
    term scl = interop_kv_get_value(opts, ATOM_STR("\x3", "scl"), global);
    VALIDATE_VALUE(scl, term_is_integer);

    term sda = interop_kv_get_value(opts, ATOM_STR("\x3", "sda"), global);
    VALIDATE_VALUE(sda, term_is_integer);

    term clock_speed_hz = interop_kv_get_value(opts, ATOM_STR("\xE", "clock_speed_hz"), global);
    VALIDATE_VALUE(clock_speed_hz, term_is_integer);

    int i2c_port = I2C_NUM_0;
    term peripheral = interop_kv_get_value(opts, ATOM_STR("\xA", "peripheral"), global);
    if (!term_is_invalid_term(peripheral)) {
        if (!term_is_integer(peripheral)) {
            ESP_LOGE(TAG, "Invalid parameter: peripheral is not an integer");
            RAISE_ERROR(BADARG_ATOM);
        }
        i2c_port = term_to_int32(peripheral);
        if (i2c_port < 0 || i2c_port > I2C_NUM_MAX - 1) {
            ESP_LOGE(TAG, "Invalid parameter: i2c_num out of range");
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    term send_timeout_ms = interop_kv_get_value_default(opts, ATOM_STR("\xF", "send_timeout_ms"), term_from_int(DEFAULT_SEND_TIMEOUT_MS), global);
    int32_t xfer_timeout_ms_val = I2C_BUS_MANAGER_TIMEOUT_INFINITE;
    if (term_is_integer(send_timeout_ms)) {
        if (term_to_int32(send_timeout_ms) < 0) {
            ESP_LOGE(TAG, "Invalid parameter: send_timeout_ms < 0");
            RAISE_ERROR(BADARG_ATOM);
        } else {
            xfer_timeout_ms_val = term_to_int32(send_timeout_ms);
        }
    } else if (send_timeout_ms != INFINITY_ATOM) {
        ESP_LOGE(TAG, "Invalid parameter: send_timeout_ms send_timeout_ms must be a non-negative integer or `infinity`");
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // Create the I2C master bus
    //

    i2c_master_bus_handle_t bus_handle;
    esp_err_t err = i2c_bus_manager_open(
        i2c_port, term_to_int32(scl), term_to_int32(sda), &bus_handle);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "Failed to create I2C master bus.  err=%i", err);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        // TODO breaks i2c::open/1 API
        return create_error_tuple(ctx, term_from_int(err));
    }

    ESP_LOGI(TAG, "I2C driver installed using I2C port %i", i2c_port);

    //
    // allocate and initialize the Nif resource
    //

    struct I2CResource *rsrc_obj = enif_alloc_resource(i2c_resource_type, sizeof(struct I2CResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        i2c_bus_manager_close(bus_handle);
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->transmitting_pid = term_invalid_term();
    rsrc_obj->bus_handle = bus_handle;
    rsrc_obj->i2c_port = i2c_port;
    rsrc_obj->clock_speed_hz = term_to_int32(clock_speed_hz);
    rsrc_obj->xfer_timeout_ms = xfer_timeout_ms_val;
    i2c_tx_buffer_init(&rsrc_obj->tx_buf);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        close_i2c_resource(rsrc_obj);
        enif_release_resource(rsrc_obj);
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = term_from_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);

    //
    // Return result
    //

    // {'$i2c', Resource :: resource(), Ref :: reference()} :: i2c()
    size_t requested_size = TUPLE_SIZE(3) + TERM_BOXED_REFERENCE_SHORT_SIZE;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        close_i2c_resource(rsrc_obj);
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term i2c = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(i2c, 0, globalcontext_make_atom(global, I2C_ATOMSTR));
    term_put_tuple_element(i2c, 1, obj);
    uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
    term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
    term_put_tuple_element(i2c, 2, ref);

    return i2c;
}

//
// i2c:close_nif/1
//

static term nif_i2c_close(Context *ctx, int argc, term argv[])
{
    TRACE("nif_close\n");
    UNUSED(argc);
    GlobalContext *global = ctx->global;

    //
    // extract the resource
    //
    term i2c_resource = argv[0];
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!to_open_i2c_resource(i2c_resource, &rsrc_obj, ctx))) {
        ESP_LOGE(TAG, "Failed to convert i2c_resource");
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // reject close if another process has an active transmission
    //
    if (UNLIKELY(!term_is_invalid_term(rsrc_obj->transmitting_pid))) {
        ESP_LOGE(TAG, "nif_close: A process is in the process of transmitting.");

        // {error, {einprogress, Pid :: pid()}}
        if (UNLIKELY(memory_ensure_free(ctx, 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        term reason = create_pair(
            ctx,
            globalcontext_make_atom(global, EINPROGRESS_ATOMSTR),
            rsrc_obj->transmitting_pid
        );
        return create_error_tuple(ctx, reason);
    }

    esp_err_t err;

    err = close_i2c_resource(rsrc_obj);
    CHECK_ERROR(ctx, err, "nif_close; Failed to delete driver");

    return OK_ATOM;
}

//
// i2c:write_bytes_nif/2
//

static term nif_i2c_write_bytes(Context *ctx, int argc, term argv[])
{
    TRACE("nif_write_bytes\n");
    UNUSED(argc);
    GlobalContext *global = ctx->global;

    //
    // extract the resource
    //
    term i2c_resource = argv[0];
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!to_open_i2c_resource(i2c_resource, &rsrc_obj, ctx))) {
        ESP_LOGE(TAG, "Failed to convert i2c_resource");
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // ensure there is not a currently running transmission
    //
    if (UNLIKELY(!term_is_invalid_term(rsrc_obj->transmitting_pid))) {
        ESP_LOGE(TAG, "nif_write_bytes: Another process is in the process of transmitting.");

        // {error, {einprogress, Pid :: pid()}}
        if (UNLIKELY(memory_ensure_free(ctx, 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        term reason = create_pair(
            ctx,
            globalcontext_make_atom(global, EINPROGRESS_ATOMSTR),
            rsrc_obj->transmitting_pid
        );
        return create_error_tuple(ctx, reason);
    }

    //
    // extract the arguments
    //
    term req = argv[1];
    int arity = term_get_tuple_arity(req);

    term address = term_get_tuple_element(req, 0);
    uint8_t addr = term_to_int32(address);

    term data = term_get_tuple_element(req, 1);
    uint8_t datum;
    uint8_t *buf;
    size_t data_len;
    if (term_is_binary(data)) {
        buf = (uint8_t *) term_binary_data(data);
        data_len = term_binary_size(data);
    } else if (term_is_integer(data)) {
        datum = term_to_int32(data);
        buf = &datum;
        data_len = 1;
    } else {
        ESP_LOGE(TAG, "Data is nether a binary nor an integer");
        RAISE_ERROR(BADARG_ATOM);
    }

    bool has_register = false;
    uint8_t register_address = 0;
    if (arity == 3) {
        term register_ = term_get_tuple_element(req, 2);
        register_address = term_to_int32(register_);
        has_register = true;
    }

    //
    // Build the write buffer (optional register prefix + data) and send it.
    // Only the register-prefixed case needs a scratch buffer to make the
    // register address and data contiguous; otherwise `buf` can be handed
    // directly to the transmit call, avoiding an extra alloc + copy.
    //

    esp_err_t err;
    if (has_register) {
        size_t write_size = 1 + data_len;
        uint8_t *write_buf = malloc(write_size);
        if (IS_NULL_PTR(write_buf)) {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                return OUT_OF_MEMORY_ATOM;
            }
            return create_error_tuple(ctx, esp_err_to_term(ctx->global, ESP_ERR_NO_MEM));
        }
        write_buf[0] = register_address;
        memcpy(write_buf + 1, buf, data_len);

        err = i2c_bus_manager_transmit(rsrc_obj->bus_handle, addr, rsrc_obj->clock_speed_hz,
            write_buf, write_size, rsrc_obj->xfer_timeout_ms);
        free(write_buf);
    } else {
        err = i2c_bus_manager_transmit(rsrc_obj->bus_handle, addr, rsrc_obj->clock_speed_hz, buf,
            data_len, rsrc_obj->xfer_timeout_ms);
    }

    if (UNLIKELY(err != ESP_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_error_tuple(ctx, esp_err_to_term(ctx->global, err));
    }

    //
    // return result
    //

    return OK_ATOM;
}

//
// i2c:read_bytes_nif/2
//

static term nif_i2c_read_bytes(Context *ctx, int argc, term argv[])
{
    TRACE("nif_read_bytes\n");
    UNUSED(argc);
    GlobalContext *global = ctx->global;

    //
    // extract the resource
    //
    term i2c_resource = argv[0];
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!to_open_i2c_resource(i2c_resource, &rsrc_obj, ctx))) {
        ESP_LOGE(TAG, "Failed to convert i2c_resource");
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // ensure there is not a currently running transmission
    //
    if (UNLIKELY(!term_is_invalid_term(rsrc_obj->transmitting_pid))) {
        ESP_LOGE(TAG, "nif_read_bytes: Another process is already transmitting");

        // {error, {einprogress, Pid :: pid()}}
        if (UNLIKELY(memory_ensure_free(ctx, 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        term reason = create_pair(
            ctx,
            globalcontext_make_atom(global, EINPROGRESS_ATOMSTR),
            rsrc_obj->transmitting_pid
        );
        return create_error_tuple(ctx, reason);
    }

    //
    // extract the arguments
    //
    term req = argv[1];
    int arity = term_get_tuple_arity(req);

    term address = term_get_tuple_element(req, 0);
    uint8_t addr = term_to_int32(address);

    term read_bytes = term_get_tuple_element(req, 1);
    avm_int_t read_count = term_to_int32(read_bytes);

    term register_ = term_invalid_term();
    uint8_t register_address = 0;
    if (arity == 3) {
        register_ = term_get_tuple_element(req, 2);
        register_address = term_to_int32(register_);
    }

    //
    // initialize the output buffer
    //

    // {ok, Data :: binary()}
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(read_count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return OUT_OF_MEMORY_ATOM;
    }
    term data = term_create_uninitialized_binary(read_count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    //
    // Perform the transaction
    //

    esp_err_t err;
    if (!term_is_invalid_term(register_)) {
        err = i2c_bus_manager_transmit_receive(rsrc_obj->bus_handle, addr, rsrc_obj->clock_speed_hz,
            &register_address, 1, buf, read_count, rsrc_obj->xfer_timeout_ms);
    } else {
        err = i2c_bus_manager_receive(rsrc_obj->bus_handle, addr, rsrc_obj->clock_speed_hz, buf,
            read_count, rsrc_obj->xfer_timeout_ms);
    }

    if (UNLIKELY(err != ESP_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_error_tuple(ctx, esp_err_to_term(ctx->global, err));
    }

    //
    // return result
    //

    // NB. memory has already been allocated above for {ok, Data} tuple
    return create_pair(ctx, OK_ATOM, data);
}

//
// i2c:begin_transmission_nif/1
//

static term nif_i2c_begin_transmission(Context *ctx, int argc, term argv[])
{
    TRACE("nif_begin_transmission\n");
    UNUSED(argc);
    GlobalContext *global = ctx->global;

    //
    // extract the resource
    //
    term i2c_resource = argv[0];
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!to_open_i2c_resource(i2c_resource, &rsrc_obj, ctx))) {
        ESP_LOGE(TAG, "Failed to convert i2c_resource");
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // ensure there is not a currently running transmission
    //
    if (UNLIKELY(!term_is_invalid_term(rsrc_obj->transmitting_pid))) {
        ESP_LOGE(TAG, "nif_begin_transmission: Another process is in the process of transmitting.");

        // {error, {einprogress, Pid :: pid()}}
        if (UNLIKELY(memory_ensure_free(ctx, 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        term reason = create_pair(
            ctx,
            globalcontext_make_atom(global, EINPROGRESS_ATOMSTR),
            rsrc_obj->transmitting_pid
        );
        return create_error_tuple(ctx, reason);
    }

    //
    // extract the arguments
    //

    term address = argv[1];
    uint8_t addr = term_to_int32(address);

    //
    // Start accumulating the transmission; the actual bus transaction is
    // deferred until nif_i2c_end_transmission/1
    //

    i2c_tx_buffer_reset(&rsrc_obj->tx_buf);
    rsrc_obj->tx_address = addr;
    rsrc_obj->transmitting_pid = term_from_local_process_id(ctx->process_id);

    return OK_ATOM;
}

//
// i2c:enqueue_write_bytes_nif/1
//

static term nif_i2c_enqueue_write_bytes(Context *ctx, int argc, term argv[])
{
    TRACE("nif_enqueue_write_bytes\n");
    UNUSED(argc);
    GlobalContext *global = ctx->global;

    //
    // extract the resource
    //
    term i2c_resource = argv[0];
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!to_open_i2c_resource(i2c_resource, &rsrc_obj, ctx))) {
        ESP_LOGE(TAG, "Failed to convert i2c_resource");
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // ensure there the currently running transmission is the current process
    //
    term pid = term_from_local_process_id(ctx->process_id);
    if (UNLIKELY(rsrc_obj->transmitting_pid != pid)) {
        ESP_LOGE(TAG, "nif_end_transmission: Another process is in the process of transmitting.");

        // {error, {einprogress, Pid :: pid()}}
        if (UNLIKELY(memory_ensure_free(ctx, 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        term reason = create_pair(
            ctx,
            globalcontext_make_atom(global, EINPROGRESS_ATOMSTR),
            rsrc_obj->transmitting_pid
        );
        return create_error_tuple(ctx, reason);
    }

    term data = argv[1];
    const uint8_t *buf = (const uint8_t *) term_binary_data(data);
    size_t len = term_binary_size(data);

    esp_err_t err = i2c_tx_buffer_append(&rsrc_obj->tx_buf, buf, len);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "nif_enqueue_write_bytes; failed to enqueue bytes: err: %i.", err);
        reset_i2c_transmission_state(rsrc_obj);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_error_tuple(ctx, esp_err_to_term(ctx->global, err));
    }

    return OK_ATOM;
}

//
// i2c:end_transmission_nif/1
//

static term nif_i2c_end_transmission(Context *ctx, int argc, term argv[])
{
    TRACE("nif_end_transmission\n");
    UNUSED(argc);
    GlobalContext *global = ctx->global;

    //
    // extract the resource
    //
    term i2c_resource = argv[0];
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!to_open_i2c_resource(i2c_resource, &rsrc_obj, ctx))) {
        ESP_LOGE(TAG, "Failed to convert i2c_resource");
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // ensure there the currently running transmission is the current process
    //
    term pid = term_from_local_process_id(ctx->process_id);
    if (UNLIKELY(rsrc_obj->transmitting_pid != pid)) {
        ESP_LOGE(TAG, "nif_end_transmission: Another process is in the process of transmitting.");

        // {error, {einprogress, Pid :: pid()}}
        if (UNLIKELY(memory_ensure_free(ctx, 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        term reason = create_pair(
            ctx,
            globalcontext_make_atom(global, EINPROGRESS_ATOMSTR),
            rsrc_obj->transmitting_pid
        );
        return create_error_tuple(ctx, reason);
    }

    //
    // Flush the accumulated write buffer as a single bus transaction
    //

    esp_err_t err = i2c_bus_manager_transmit(rsrc_obj->bus_handle, rsrc_obj->tx_address,
        rsrc_obj->clock_speed_hz, rsrc_obj->tx_buf.data, rsrc_obj->tx_buf.len,
        rsrc_obj->xfer_timeout_ms);

    reset_i2c_transmission_state(rsrc_obj);

    if (UNLIKELY(err != ESP_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            return OUT_OF_MEMORY_ATOM;
        }
        return create_error_tuple(ctx, esp_err_to_term(ctx->global, err));
    }

    return OK_ATOM;
}

//
// entrypoints
//

static void i2c_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct I2CResource *rsrc_obj = (struct I2CResource *) obj;

    esp_err_t err = close_i2c_resource(rsrc_obj);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGW(TAG, "Failed to delete driver in resource d'tor.  err=%i", err);
    }
}

static const ErlNifResourceTypeInit I2CResourceTypeInit = {
    .members = 1,
    .dtor = i2c_resource_dtor,
};

static const struct Nif i2c_open_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_open
};
static const struct Nif i2c_close_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_close
};
static const struct Nif i2c_read_bytes_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_bytes
};
static const struct Nif i2c_write_bytes_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_bytes
};
static const struct Nif i2c_begin_transmission_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_begin_transmission
};
static const struct Nif i2c_enqueue_write_bytes_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_enqueue_write_bytes
};
static const struct Nif i2c_end_transmission_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_end_transmission
};

#ifdef CONFIG_AVM_ENABLE_I2C_RESOURCE_NIFS

void i2c_resource_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    i2c_resource_type = enif_init_resource_type(&env, "i2c_resource", &I2CResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

const struct Nif *i2c_resource_get_nif(const char *nifname)
{
    if (strcmp("i2c:open_nif/1", nifname) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_open_nif;
    }
    if (strcmp("i2c:close_nif/1", nifname) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_close_nif;
    }
    if (strcmp("i2c:read_bytes_nif/2", nifname) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_bytes_nif;
    }
    if (strcmp("i2c:write_bytes_nif/2", nifname) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_bytes_nif;
    }
    if (strcmp("i2c:begin_transmission_nif/2", nifname) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_begin_transmission_nif;
    }
    if (strcmp("i2c:enqueue_write_bytes_nif/2", nifname) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_enqueue_write_bytes_nif;
    }
    if (strcmp("i2c:end_transmission_nif/1", nifname) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_end_transmission_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(i2c_resource, i2c_resource_init, NULL, i2c_resource_get_nif)

#endif
