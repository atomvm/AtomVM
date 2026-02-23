/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "stm32_hal_platform.h"

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "avm_log.h"
#include "stm_sys.h"

#define TAG "spi_driver"
#define SPI_DEFAULT_TIMEOUT_MS 1000

static ErlNifResourceType *spi_resource_type;

struct SPIResource
{
    SPI_HandleTypeDef handle;
    int peripheral;
};

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static bool get_spi_resource(Context *ctx, term resource_term, struct SPIResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, spi_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct SPIResource *) rsrc_obj_ptr;
    return true;
}

static SPI_TypeDef *peripheral_to_instance(int peripheral)
{
    switch (peripheral) {
#ifdef SPI1
        case 1:
            return SPI1;
#endif
#ifdef SPI2
        case 2:
            return SPI2;
#endif
#ifdef SPI3
        case 3:
            return SPI3;
#endif
#ifdef SPI4
        case 4:
            return SPI4;
#endif
#ifdef SPI5
        case 5:
            return SPI5;
#endif
#ifdef SPI6
        case 6:
            return SPI6;
#endif
        default:
            return NULL;
    }
}

static void enable_spi_clock(int peripheral)
{
    switch (peripheral) {
#ifdef SPI1
        case 1:
            __HAL_RCC_SPI1_CLK_ENABLE();
            break;
#endif
#ifdef SPI2
        case 2:
            __HAL_RCC_SPI2_CLK_ENABLE();
            break;
#endif
#ifdef SPI3
        case 3:
            __HAL_RCC_SPI3_CLK_ENABLE();
            break;
#endif
#ifdef SPI4
        case 4:
            __HAL_RCC_SPI4_CLK_ENABLE();
            break;
#endif
#ifdef SPI5
        case 5:
            __HAL_RCC_SPI5_CLK_ENABLE();
            break;
#endif
#ifdef SPI6
        case 6:
            __HAL_RCC_SPI6_CLK_ENABLE();
            break;
#endif
        default:
            break;
    }
}

static uint32_t get_spi_apb_freq(int peripheral)
{
    (void) peripheral;
#if defined(STM32G0XX)
    return HAL_RCC_GetPCLK1Freq();
#else
    switch (peripheral) {
        case 2:
        case 3:
            return HAL_RCC_GetPCLK1Freq();
        default:
            return HAL_RCC_GetPCLK2Freq();
    }
#endif
}

static const uint32_t spi_prescaler_table[] = {
    SPI_BAUDRATEPRESCALER_2,
    SPI_BAUDRATEPRESCALER_4,
    SPI_BAUDRATEPRESCALER_8,
    SPI_BAUDRATEPRESCALER_16,
    SPI_BAUDRATEPRESCALER_32,
    SPI_BAUDRATEPRESCALER_64,
    SPI_BAUDRATEPRESCALER_128,
    SPI_BAUDRATEPRESCALER_256
};

static int compute_spi_prescaler_index(uint32_t apb_freq, uint32_t target_baudrate)
{
    for (int i = 0; i < 8; i++) {
        uint32_t divider = 2U << i;
        if (apb_freq / divider <= target_baudrate) {
            return i;
        }
    }
    return 7;
}

static term nif_spi_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);

    int peripheral = term_to_int(argv[0]);
    uint32_t baudrate = (uint32_t) term_to_int(argv[1]);

    SPI_TypeDef *instance = peripheral_to_instance(peripheral);
    if (IS_NULL_PTR(instance)) {
        AVM_LOGE(TAG, "Invalid SPI peripheral: %d", peripheral);
        RAISE_ERROR(BADARG_ATOM);
    }

    enable_spi_clock(peripheral);

    struct SPIResource *rsrc_obj = enif_alloc_resource(spi_resource_type, sizeof(struct SPIResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    memset(&rsrc_obj->handle, 0, sizeof(SPI_HandleTypeDef));
    rsrc_obj->handle.Instance = instance;
    rsrc_obj->peripheral = peripheral;

    uint32_t apb_freq = get_spi_apb_freq(peripheral);
    int presc_idx = compute_spi_prescaler_index(apb_freq, baudrate);
    uint32_t actual_baudrate = apb_freq / (2U << presc_idx);

    rsrc_obj->handle.Init.Mode = SPI_MODE_MASTER;
    rsrc_obj->handle.Init.Direction = SPI_DIRECTION_2LINES;
    rsrc_obj->handle.Init.DataSize = SPI_DATASIZE_8BIT;
    rsrc_obj->handle.Init.CLKPolarity = SPI_POLARITY_LOW;
    rsrc_obj->handle.Init.CLKPhase = SPI_PHASE_1EDGE;
    rsrc_obj->handle.Init.NSS = SPI_NSS_SOFT;
    rsrc_obj->handle.Init.BaudRatePrescaler = spi_prescaler_table[presc_idx];
    rsrc_obj->handle.Init.FirstBit = SPI_FIRSTBIT_MSB;
    rsrc_obj->handle.Init.TIMode = SPI_TIMODE_DISABLE;
    rsrc_obj->handle.Init.CRCCalculation = SPI_CRCCALCULATION_DISABLE;
    rsrc_obj->handle.Init.CRCPolynomial = 7;

    HAL_StatusTypeDef status = HAL_SPI_Init(&rsrc_obj->handle);
    if (status != HAL_OK) {
        enif_release_resource(rsrc_obj);
        AVM_LOGE(TAG, "HAL_SPI_Init failed: %d", (int) status);
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        HAL_SPI_DeInit(&rsrc_obj->handle);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term inner = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(inner, 0, term_from_int(actual_baudrate));
    term_put_tuple_element(inner, 1, obj);

    return create_pair(ctx, OK_ATOM, inner);
}

static term nif_spi_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    HAL_SPI_DeInit(&rsrc_obj->handle);
    rsrc_obj->handle.Instance = NULL;

    return OK_ATOM;
}

static term nif_spi_set_baudrate(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);

    uint32_t baudrate = (uint32_t) term_to_int(argv[1]);
    uint32_t apb_freq = get_spi_apb_freq(rsrc_obj->peripheral);
    int presc_idx = compute_spi_prescaler_index(apb_freq, baudrate);
    uint32_t actual = apb_freq / (2U << presc_idx);

    HAL_SPI_DeInit(&rsrc_obj->handle);
    rsrc_obj->handle.Init.BaudRatePrescaler = spi_prescaler_table[presc_idx];
    HAL_SPI_Init(&rsrc_obj->handle);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, OK_ATOM, term_from_int(actual));
}

static term nif_spi_set_format(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_integer);

    uint32_t data_bits = (uint32_t) term_to_int(argv[1]);
    uint32_t datasize;
    switch (data_bits) {
        case 8:
            datasize = SPI_DATASIZE_8BIT;
            break;
        case 16:
            datasize = SPI_DATASIZE_16BIT;
            break;
        default:
            RAISE_ERROR(BADARG_ATOM);
    }

    uint32_t cpol = (term_to_int(argv[2]) == 0) ? SPI_POLARITY_LOW : SPI_POLARITY_HIGH;
    uint32_t cpha = (term_to_int(argv[3]) == 0) ? SPI_PHASE_1EDGE : SPI_PHASE_2EDGE;

    HAL_SPI_DeInit(&rsrc_obj->handle);
    rsrc_obj->handle.Init.DataSize = datasize;
    rsrc_obj->handle.Init.CLKPolarity = cpol;
    rsrc_obj->handle.Init.CLKPhase = cpha;
    HAL_SPI_Init(&rsrc_obj->handle);

    return OK_ATOM;
}

static term nif_spi_write_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);

    HAL_SPI_Transmit(&rsrc_obj->handle, (uint8_t *) data, (uint16_t) len, SPI_DEFAULT_TIMEOUT_MS);

    return term_from_int((int) len);
}

static term nif_spi_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    uint8_t repeated_tx_data = (uint8_t) term_to_int(argv[1]);
    avm_int_t count = term_to_int(argv[2]);

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *rx_buf = (uint8_t *) term_binary_data(data);

    uint8_t *tx_buf = malloc((size_t) count);
    if (IS_NULL_PTR(tx_buf)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    memset(tx_buf, repeated_tx_data, (size_t) count);

    HAL_SPI_TransmitReceive(&rsrc_obj->handle, tx_buf, rx_buf, (uint16_t) count, SPI_DEFAULT_TIMEOUT_MS);
    free(tx_buf);

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_spi_write_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *src = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(len), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    uint8_t *dst = (uint8_t *) term_binary_data(data);

    HAL_SPI_TransmitReceive(&rsrc_obj->handle, (uint8_t *) src, dst, (uint16_t) len, SPI_DEFAULT_TIMEOUT_MS);

    return create_pair(ctx, OK_ATOM, data);
}

static void spi_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct SPIResource *rsrc_obj = (struct SPIResource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        HAL_SPI_DeInit(&rsrc_obj->handle);
        rsrc_obj->handle.Instance = NULL;
    }
}

static const ErlNifResourceTypeInit SPIResourceTypeInit = {
    .members = 1,
    .dtor = spi_resource_dtor,
};

//
// NIF structs
//
static const struct Nif spi_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_init
};
static const struct Nif spi_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_deinit
};
static const struct Nif spi_set_baudrate_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_set_baudrate
};
static const struct Nif spi_set_format_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_set_format
};
static const struct Nif spi_write_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write_blocking
};
static const struct Nif spi_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_read_blocking
};
static const struct Nif spi_write_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write_read_blocking
};

static void spi_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    spi_resource_type = enif_init_resource_type(&env, "spi_resource", &SPIResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *spi_nif_get_nif(const char *nifname)
{
    if (strncmp("spi:", nifname, 4) != 0) {
        return NULL;
    }
    const char *rest = nifname + 4;
    if (strcmp("init/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_deinit_nif;
    }
    if (strcmp("set_baudrate/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_set_baudrate_nif;
    }
    if (strcmp("set_format/4", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_set_format_nif;
    }
    if (strcmp("write_blocking/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_write_blocking_nif;
    }
    if (strcmp("read_blocking/3", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_read_blocking_nif;
    }
    if (strcmp("write_read_blocking/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_write_read_blocking_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(spi, spi_nif_init, NULL, spi_nif_get_nif)
