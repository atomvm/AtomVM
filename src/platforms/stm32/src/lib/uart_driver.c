/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
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
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "avm_log.h"
#include "stm_sys.h"

#define TAG "uart_driver"

static ErlNifResourceType *uart_resource_type;

struct UARTResource
{
    UART_HandleTypeDef handle;
    bool is_half_duplex;
};

static const AtomStringIntPair gpio_bank_table[] = {
    { ATOM_STR("\x1", "a"), (int) GPIOA },
    { ATOM_STR("\x1", "b"), (int) GPIOB },
    { ATOM_STR("\x1", "c"), (int) GPIOC },
    { ATOM_STR("\x1", "d"), (int) GPIOD },
    { ATOM_STR("\x1", "e"), (int) GPIOE },
#ifdef GPIOF
    { ATOM_STR("\x1", "f"), (int) GPIOF },
#endif
#ifdef GPIOG
    { ATOM_STR("\x1", "g"), (int) GPIOG },
#endif
#ifdef GPIOH
    { ATOM_STR("\x1", "h"), (int) GPIOH },
#endif
#ifdef GPIOI
    { ATOM_STR("\x1", "i"), (int) GPIOI },
#endif
#ifdef GPIOJ
    { ATOM_STR("\x1", "j"), (int) GPIOJ },
#endif
#ifdef GPIOK
    { ATOM_STR("\x1", "k"), (int) GPIOK },
#endif
    SELECT_INT_DEFAULT(0)
};

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static bool get_uart_resource(Context *ctx, term resource_term, struct UARTResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, uart_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct UARTResource *) rsrc_obj_ptr;
    return true;
}

static bool get_timeout_ms(term timeout_term, uint32_t *out)
{
    if (term_is_atom(timeout_term)) {
        if (timeout_term == INFINITY_ATOM) {
            *out = HAL_MAX_DELAY;
            return true;
        }
        return false;
    }
    if (!term_is_integer(timeout_term)) {
        return false;
    }
    avm_int_t val = term_to_int(timeout_term);
    if (val < 0) {
        return false;
    }
    *out = (uint32_t) val;
    return true;
}

static term hal_status_to_error(Context *ctx, HAL_StatusTypeDef status)
{
    switch (status) {
        case HAL_TIMEOUT:
            return create_pair(ctx, ERROR_ATOM, TIMEOUT_ATOM);
        case HAL_BUSY:
            return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "busy")));
        default:
            return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "eio")));
    }
}

static USART_TypeDef *peripheral_to_instance(int peripheral)
{
    switch (peripheral) {
#ifdef USART1
        case 1:
            return USART1;
#endif
#ifdef USART2
        case 2:
            return USART2;
#endif
#ifdef USART3
        case 3:
            return USART3;
#endif
#ifdef UART4
        case 4:
            return UART4;
#endif
#ifdef UART5
        case 5:
            return UART5;
#endif
#ifdef USART6
        case 6:
            return USART6;
#endif
#ifdef UART7
        case 7:
            return UART7;
#endif
#ifdef UART8
        case 8:
            return UART8;
#endif
#ifdef LPUART1
        case 9:
            return LPUART1;
#endif
        default:
            return NULL;
    }
}

static void enable_uart_clock(int peripheral)
{
    switch (peripheral) {
#ifdef USART1
        case 1:
            __HAL_RCC_USART1_CLK_ENABLE();
            break;
#endif
#ifdef USART2
        case 2:
            __HAL_RCC_USART2_CLK_ENABLE();
            break;
#endif
#ifdef USART3
        case 3:
            __HAL_RCC_USART3_CLK_ENABLE();
            break;
#endif
#ifdef UART4
        case 4:
            __HAL_RCC_UART4_CLK_ENABLE();
            break;
#endif
#ifdef UART5
        case 5:
            __HAL_RCC_UART5_CLK_ENABLE();
            break;
#endif
#ifdef USART6
        case 6:
            __HAL_RCC_USART6_CLK_ENABLE();
            break;
#endif
#ifdef UART7
        case 7:
            __HAL_RCC_UART7_CLK_ENABLE();
            break;
#endif
#ifdef UART8
        case 8:
            __HAL_RCC_UART8_CLK_ENABLE();
            break;
#endif
#ifdef LPUART1
        case 9:
            __HAL_RCC_LPUART1_CLK_ENABLE();
            break;
#endif
        default:
            break;
    }
}

static bool parse_pin(GlobalContext *glb, term pin_term, GPIO_TypeDef **port, uint16_t *pin_mask)
{
    if (!term_is_tuple(pin_term) || term_get_tuple_arity(pin_term) != 2) {
        return false;
    }
    term bank_atom = term_get_tuple_element(pin_term, 0);
    if (!term_is_atom(bank_atom)) {
        return false;
    }
    uint32_t gpio_bank = (uint32_t) interop_atom_term_select_int(gpio_bank_table, bank_atom, glb);
    if (gpio_bank == 0) {
        return false;
    }
    *port = (GPIO_TypeDef *) gpio_bank;
    term pin_num_term = term_get_tuple_element(pin_term, 1);
    if (!term_is_integer(pin_num_term)) {
        return false;
    }
    int pin_num = term_to_int(pin_num_term);
    if (pin_num < 0 || pin_num > 15) {
        return false;
    }
    *pin_mask = (uint16_t) (1U << pin_num);
    return true;
}

static term nif_uart_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term opts = argv[0];
    VALIDATE_VALUE(opts, term_is_list);

    GlobalContext *glb = ctx->global;

    static const char *const peripheral_str = ATOM_STR("\xA", "peripheral");
    static const char *const tx_str = ATOM_STR("\x2", "tx");
    static const char *const rx_str = ATOM_STR("\x2", "rx");
    static const char *const speed_str = ATOM_STR("\x5", "speed");
    static const char *const data_bits_str = ATOM_STR("\x9", "data_bits");
    static const char *const stop_bits_str = ATOM_STR("\x9", "stop_bits");
    static const char *const parity_str = ATOM_STR("\x6", "parity");
    static const char *const af_str = ATOM_STR("\x2", "af");

    term peripheral_term = interop_kv_get_value_default(opts, peripheral_str, term_from_int(1), glb);
    term tx_term = interop_kv_get_value(opts, tx_str, glb);
    term rx_term = interop_kv_get_value(opts, rx_str, glb);
    term speed_term = interop_kv_get_value_default(opts, speed_str, term_from_int(115200), glb);
    term data_bits_term = interop_kv_get_value_default(opts, data_bits_str, term_from_int(8), glb);
    term stop_bits_term = interop_kv_get_value_default(opts, stop_bits_str, term_from_int(1), glb);
    term parity_term = interop_kv_get_value_default(opts, parity_str, term_from_int(0), glb);
    term af_term = interop_kv_get_value_default(opts, af_str, term_from_int(7), glb);

    if (term_is_invalid_term(tx_term) || term_is_invalid_term(rx_term)) {
        AVM_LOGE(TAG, "tx and rx pins are required");
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(peripheral_term, term_is_integer);
    VALIDATE_VALUE(speed_term, term_is_integer);
    VALIDATE_VALUE(data_bits_term, term_is_integer);
    VALIDATE_VALUE(stop_bits_term, term_is_integer);
    VALIDATE_VALUE(parity_term, term_is_integer);
    VALIDATE_VALUE(af_term, term_is_integer);

    int peripheral = term_to_int(peripheral_term);
    uint32_t speed = (uint32_t) term_to_int(speed_term);
    int data_bits = term_to_int(data_bits_term);
    int stop_bits = term_to_int(stop_bits_term);
    int parity = term_to_int(parity_term);
    uint32_t af = (uint32_t) term_to_int(af_term);

    USART_TypeDef *instance = peripheral_to_instance(peripheral);
    if (IS_NULL_PTR(instance)) {
        AVM_LOGE(TAG, "Invalid UART peripheral: %d", peripheral);
        RAISE_ERROR(BADARG_ATOM);
    }

    GPIO_TypeDef *tx_port;
    uint16_t tx_pin;
    if (!parse_pin(glb, tx_term, &tx_port, &tx_pin)) {
        AVM_LOGE(TAG, "Invalid TX pin");
        RAISE_ERROR(BADARG_ATOM);
    }

    GPIO_TypeDef *rx_port;
    uint16_t rx_pin;
    if (!parse_pin(glb, rx_term, &rx_port, &rx_pin)) {
        AVM_LOGE(TAG, "Invalid RX pin");
        RAISE_ERROR(BADARG_ATOM);
    }

    enable_uart_clock(peripheral);

    GPIO_InitTypeDef gpio_init = {
        .Mode = GPIO_MODE_AF_PP,
        .Pull = GPIO_PULLUP,
        .Speed = GPIO_SPEED_FREQ_HIGH,
        .Alternate = af,
    };
    gpio_init.Pin = tx_pin;
    HAL_GPIO_Init(tx_port, &gpio_init);
    gpio_init.Pin = rx_pin;
    gpio_init.Mode = GPIO_MODE_AF_PP;
    HAL_GPIO_Init(rx_port, &gpio_init);

    struct UARTResource *rsrc_obj = enif_alloc_resource(uart_resource_type, sizeof(struct UARTResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    memset(&rsrc_obj->handle, 0, sizeof(UART_HandleTypeDef));
    rsrc_obj->handle.Instance = instance;
    rsrc_obj->is_half_duplex = false;

    rsrc_obj->handle.Init.BaudRate = speed;

    switch (data_bits) {
#ifdef UART_WORDLENGTH_7B
        case 7:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_7B;
            break;
#endif
        case 8:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_8B;
            break;
        case 9:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_9B;
            break;
        default:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_8B;
            break;
    }

    switch (stop_bits) {
        case 1:
            rsrc_obj->handle.Init.StopBits = UART_STOPBITS_1;
            break;
        case 2:
            rsrc_obj->handle.Init.StopBits = UART_STOPBITS_2;
            break;
        default:
            rsrc_obj->handle.Init.StopBits = UART_STOPBITS_1;
            break;
    }

    switch (parity) {
        case 0:
            rsrc_obj->handle.Init.Parity = UART_PARITY_NONE;
            break;
        case 1:
            rsrc_obj->handle.Init.Parity = UART_PARITY_ODD;
            break;
        case 2:
            rsrc_obj->handle.Init.Parity = UART_PARITY_EVEN;
            break;
        default:
            rsrc_obj->handle.Init.Parity = UART_PARITY_NONE;
            break;
    }

    rsrc_obj->handle.Init.Mode = UART_MODE_TX_RX;
    rsrc_obj->handle.Init.HwFlowCtl = UART_HWCONTROL_NONE;
    rsrc_obj->handle.Init.OverSampling = UART_OVERSAMPLING_16;

    HAL_StatusTypeDef status = HAL_UART_Init(&rsrc_obj->handle);
    if (status != HAL_OK) {
        enif_release_resource(rsrc_obj);
        AVM_LOGE(TAG, "HAL_UART_Init failed: %d", (int) status);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(glb, ATOM_STR("\x9", "uart_init")));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        HAL_UART_DeInit(&rsrc_obj->handle);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term obj = term_from_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return create_pair(ctx, OK_ATOM, obj);
}

static term nif_uart_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        return OK_ATOM;
    }
    HAL_UART_DeInit(&rsrc_obj->handle);
    rsrc_obj->handle.Instance = NULL;
    rsrc_obj->is_half_duplex = false;
    return OK_ATOM;
}

static term nif_uart_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);
    uint32_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(len > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    HAL_StatusTypeDef status = HAL_UART_Transmit(&rsrc_obj->handle, (uint8_t *) data, (uint16_t) len, timeout_ms);
    if (UNLIKELY(status != HAL_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return hal_status_to_error(ctx, status);
    }
    return term_from_int((int) len);
}

static term nif_uart_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t count = term_to_int(argv[1]);
    uint32_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(count < 0 || count > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    HAL_StatusTypeDef status = HAL_UART_Receive(&rsrc_obj->handle, buf, (uint16_t) count, timeout_ms);
    if (UNLIKELY(status != HAL_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return hal_status_to_error(ctx, status);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_uart_abort(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    HAL_StatusTypeDef status = HAL_UART_Abort(&rsrc_obj->handle);
    if (UNLIKELY(status != HAL_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return hal_status_to_error(ctx, status);
    }
    return OK_ATOM;
}

static term nif_uart_get_state(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    HAL_UART_StateTypeDef state = HAL_UART_GetState(&rsrc_obj->handle);

    static const char *const ready_str = ATOM_STR("\x5", "ready");
    static const char *const busy_str = ATOM_STR("\x4", "busy");
    static const char *const busy_tx_str = ATOM_STR("\x7", "busy_tx");
    static const char *const busy_rx_str = ATOM_STR("\x7", "busy_rx");
    static const char *const busy_tx_rx_str = ATOM_STR("\xA", "busy_tx_rx");
    static const char *const reset_str = ATOM_STR("\x5", "reset");

    const char *state_str;
    switch (state) {
        case HAL_UART_STATE_READY:
            state_str = ready_str;
            break;
        case HAL_UART_STATE_BUSY:
            state_str = busy_str;
            break;
        case HAL_UART_STATE_BUSY_TX:
            state_str = busy_tx_str;
            break;
        case HAL_UART_STATE_BUSY_RX:
            state_str = busy_rx_str;
            break;
        case HAL_UART_STATE_BUSY_TX_RX:
            state_str = busy_tx_rx_str;
            break;
        case HAL_UART_STATE_ERROR:
            return ERROR_ATOM;
        case HAL_UART_STATE_TIMEOUT:
            return TIMEOUT_ATOM;
        default:
            state_str = reset_str;
            break;
    }
    return globalcontext_make_atom(ctx->global, state_str);
}

static term nif_uart_get_error(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t err = HAL_UART_GetError(&rsrc_obj->handle);
    return term_from_int((int) err);
}

static term nif_uart_halfduplex_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term opts = argv[0];
    VALIDATE_VALUE(opts, term_is_list);

    GlobalContext *glb = ctx->global;

    static const char *const peripheral_str = ATOM_STR("\xA", "peripheral");
    static const char *const tx_str = ATOM_STR("\x2", "tx");
    static const char *const speed_str = ATOM_STR("\x5", "speed");
    static const char *const data_bits_str = ATOM_STR("\x9", "data_bits");
    static const char *const stop_bits_str = ATOM_STR("\x9", "stop_bits");
    static const char *const parity_str = ATOM_STR("\x6", "parity");
    static const char *const af_str = ATOM_STR("\x2", "af");

    term peripheral_term = interop_kv_get_value_default(opts, peripheral_str, term_from_int(1), glb);
    term tx_term = interop_kv_get_value(opts, tx_str, glb);
    term speed_term = interop_kv_get_value_default(opts, speed_str, term_from_int(115200), glb);
    term data_bits_term = interop_kv_get_value_default(opts, data_bits_str, term_from_int(8), glb);
    term stop_bits_term = interop_kv_get_value_default(opts, stop_bits_str, term_from_int(1), glb);
    term parity_term = interop_kv_get_value_default(opts, parity_str, term_from_int(0), glb);
    term af_term = interop_kv_get_value_default(opts, af_str, term_from_int(7), glb);

    if (term_is_invalid_term(tx_term)) {
        AVM_LOGE(TAG, "tx pin is required for half-duplex");
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(peripheral_term, term_is_integer);
    VALIDATE_VALUE(speed_term, term_is_integer);
    VALIDATE_VALUE(data_bits_term, term_is_integer);
    VALIDATE_VALUE(stop_bits_term, term_is_integer);
    VALIDATE_VALUE(parity_term, term_is_integer);
    VALIDATE_VALUE(af_term, term_is_integer);

    int peripheral = term_to_int(peripheral_term);
    uint32_t speed = (uint32_t) term_to_int(speed_term);
    int data_bits = term_to_int(data_bits_term);
    int stop_bits = term_to_int(stop_bits_term);
    int parity = term_to_int(parity_term);
    uint32_t af = (uint32_t) term_to_int(af_term);

    USART_TypeDef *instance = peripheral_to_instance(peripheral);
    if (IS_NULL_PTR(instance)) {
        AVM_LOGE(TAG, "Invalid UART peripheral: %d", peripheral);
        RAISE_ERROR(BADARG_ATOM);
    }

    GPIO_TypeDef *tx_port;
    uint16_t tx_pin;
    if (!parse_pin(glb, tx_term, &tx_port, &tx_pin)) {
        AVM_LOGE(TAG, "Invalid TX pin");
        RAISE_ERROR(BADARG_ATOM);
    }

    enable_uart_clock(peripheral);

    GPIO_InitTypeDef gpio_init = {
        .Mode = GPIO_MODE_AF_OD,
        .Pull = GPIO_NOPULL,
        .Speed = GPIO_SPEED_FREQ_HIGH,
        .Alternate = af,
    };
    gpio_init.Pin = tx_pin;
    HAL_GPIO_Init(tx_port, &gpio_init);

    struct UARTResource *rsrc_obj = enif_alloc_resource(uart_resource_type, sizeof(struct UARTResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    memset(&rsrc_obj->handle, 0, sizeof(UART_HandleTypeDef));
    rsrc_obj->handle.Instance = instance;
    rsrc_obj->is_half_duplex = true;

    rsrc_obj->handle.Init.BaudRate = speed;

    switch (data_bits) {
#ifdef UART_WORDLENGTH_7B
        case 7:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_7B;
            break;
#endif
        case 8:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_8B;
            break;
        case 9:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_9B;
            break;
        default:
            rsrc_obj->handle.Init.WordLength = UART_WORDLENGTH_8B;
            break;
    }

    switch (stop_bits) {
        case 1:
            rsrc_obj->handle.Init.StopBits = UART_STOPBITS_1;
            break;
        case 2:
            rsrc_obj->handle.Init.StopBits = UART_STOPBITS_2;
            break;
        default:
            rsrc_obj->handle.Init.StopBits = UART_STOPBITS_1;
            break;
    }

    switch (parity) {
        case 0:
            rsrc_obj->handle.Init.Parity = UART_PARITY_NONE;
            break;
        case 1:
            rsrc_obj->handle.Init.Parity = UART_PARITY_ODD;
            break;
        case 2:
            rsrc_obj->handle.Init.Parity = UART_PARITY_EVEN;
            break;
        default:
            rsrc_obj->handle.Init.Parity = UART_PARITY_NONE;
            break;
    }

    rsrc_obj->handle.Init.Mode = UART_MODE_TX_RX;
    rsrc_obj->handle.Init.HwFlowCtl = UART_HWCONTROL_NONE;
    rsrc_obj->handle.Init.OverSampling = UART_OVERSAMPLING_16;

    HAL_StatusTypeDef status = HAL_HalfDuplex_Init(&rsrc_obj->handle);
    if (status != HAL_OK) {
        enif_release_resource(rsrc_obj);
        AVM_LOGE(TAG, "HAL_HalfDuplex_Init failed: %d", (int) status);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(glb, ATOM_STR("\xf", "halfduplex_init")));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        HAL_UART_DeInit(&rsrc_obj->handle);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term obj = term_from_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return create_pair(ctx, OK_ATOM, obj);
}

static term nif_uart_halfduplex_enable_tx(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (!rsrc_obj->is_half_duplex) {
        return ERROR_ATOM;
    }
    HAL_StatusTypeDef status = HAL_HalfDuplex_EnableTransmitter(&rsrc_obj->handle);
    return (status == HAL_OK) ? OK_ATOM : ERROR_ATOM;
}

static term nif_uart_halfduplex_enable_rx(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (!rsrc_obj->is_half_duplex) {
        return ERROR_ATOM;
    }
    HAL_StatusTypeDef status = HAL_HalfDuplex_EnableReceiver(&rsrc_obj->handle);
    return (status == HAL_OK) ? OK_ATOM : ERROR_ATOM;
}

static void uart_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct UARTResource *rsrc_obj = (struct UARTResource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        HAL_UART_DeInit(&rsrc_obj->handle);
        rsrc_obj->handle.Instance = NULL;
    }
}

static const ErlNifResourceTypeInit UARTResourceTypeInit = {
    .members = 1,
    .dtor = uart_resource_dtor,
};

static const struct Nif uart_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_init
};

static const struct Nif uart_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_deinit
};

static const struct Nif uart_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_write
};

static const struct Nif uart_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_read
};

static const struct Nif uart_abort_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_abort
};

static const struct Nif uart_get_state_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_get_state
};

static const struct Nif uart_get_error_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_get_error
};

static const struct Nif uart_halfduplex_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_halfduplex_init
};

static const struct Nif uart_halfduplex_enable_tx_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_halfduplex_enable_tx
};

static const struct Nif uart_halfduplex_enable_rx_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_halfduplex_enable_rx
};

static void uart_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    uart_resource_type = enif_init_resource_type(&env, "uart_resource", &UARTResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *uart_nif_get_nif(const char *nifname)
{
    if (strncmp("uart:", nifname, 5) != 0) {
        return NULL;
    }
    const char *rest = nifname + 5;
    if (strcmp("init/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_deinit_nif;
    }
    if (strcmp("write/3", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_write_nif;
    }
    if (strcmp("read/3", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_read_nif;
    }
    if (strcmp("abort/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_abort_nif;
    }
    if (strcmp("get_state/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_get_state_nif;
    }
    if (strcmp("get_error/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_get_error_nif;
    }
    if (strcmp("halfduplex_init/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_halfduplex_init_nif;
    }
    if (strcmp("halfduplex_enable_tx/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_halfduplex_enable_tx_nif;
    }
    if (strcmp("halfduplex_enable_rx/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_halfduplex_enable_rx_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(uart, uart_nif_init, NULL, uart_nif_get_nif)
