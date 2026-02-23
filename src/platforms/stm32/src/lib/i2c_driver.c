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
#include <string.h>

#include "stm32_hal_platform.h"

#include <atom.h>
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

#define TAG "i2c_driver"

static ErlNifResourceType *i2c_resource_type;

struct I2CResource
{
    I2C_HandleTypeDef handle;
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

static I2C_TypeDef *peripheral_to_instance(int peripheral)
{
    switch (peripheral) {
#ifdef I2C1
        case 1:
            return I2C1;
#endif
#ifdef I2C2
        case 2:
            return I2C2;
#endif
#ifdef I2C3
        case 3:
            return I2C3;
#endif
#ifdef I2C4
        case 4:
            return I2C4;
#endif
        default:
            return NULL;
    }
}

static void enable_i2c_clock(int peripheral)
{
    switch (peripheral) {
#ifdef I2C1
        case 1:
            __HAL_RCC_I2C1_CLK_ENABLE();
            break;
#endif
#ifdef I2C2
        case 2:
            __HAL_RCC_I2C2_CLK_ENABLE();
            break;
#endif
#ifdef I2C3
        case 3:
            __HAL_RCC_I2C3_CLK_ENABLE();
            break;
#endif
#ifdef I2C4
        case 4:
            __HAL_RCC_I2C4_CLK_ENABLE();
            break;
#endif
        default:
            break;
    }
}

static bool get_i2c_resource(Context *ctx, term resource_term, struct I2CResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, i2c_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct I2CResource *) rsrc_obj_ptr;
    return true;
}

static uint32_t get_timeout_ms(term timeout_term)
{
    if (term_is_atom(timeout_term) && timeout_term == INFINITY_ATOM) {
        return HAL_MAX_DELAY;
    }
    return (uint32_t) term_to_int(timeout_term);
}

/*
 * I2C timing computation.
 * F2/F4 use the legacy I2C peripheral (ClockSpeed/DutyCycle).
 * All other supported families use the newer I2C with a Timing register.
 */
#if defined(STM32F2XX) || defined(STM32F4XX)

static void configure_i2c_timing(I2C_HandleTypeDef *handle, uint32_t clock_speed_hz)
{
    handle->Init.ClockSpeed = clock_speed_hz;
    handle->Init.DutyCycle = I2C_DUTYCYCLE_2;
}

#else

/*
 * Compute a Timing register value for a target I2C baudrate.
 * This is a simplified computation assuming analog filter on, digital filter off.
 * For production use, STM32CubeMX-generated values are recommended.
 */
static uint32_t compute_i2c_timing(uint32_t pclk_freq, uint32_t target_freq)
{
    uint32_t presc, scll, sclh;

    if (target_freq <= 100000) {
        /* Standard mode (100 kHz) */
        presc = (pclk_freq / 4000000) - 1;
        if (presc > 15) {
            presc = 15;
        }
        uint32_t t_presc = pclk_freq / (presc + 1);
        uint32_t scl_period = t_presc / target_freq;
        scll = scl_period / 2;
        sclh = scl_period - scll;
        if (scll > 0) {
            scll--;
        }
        if (sclh > 0) {
            sclh--;
        }
        if (scll > 255) {
            scll = 255;
        }
        if (sclh > 255) {
            sclh = 255;
        }
    } else {
        /* Fast mode (400 kHz) */
        presc = (pclk_freq / 8000000) - 1;
        if (presc > 15) {
            presc = 15;
        }
        uint32_t t_presc = pclk_freq / (presc + 1);
        uint32_t scl_period = t_presc / target_freq;
        scll = (scl_period * 2) / 3;
        sclh = scl_period - scll;
        if (scll > 0) {
            scll--;
        }
        if (sclh > 0) {
            sclh--;
        }
        if (scll > 255) {
            scll = 255;
        }
        if (sclh > 255) {
            sclh = 255;
        }
    }

    /* SDADEL and SCLDEL: use safe conservative values */
    uint32_t sdadel = 2;
    uint32_t scldel = 4;

    return (presc << 28) | (scldel << 20) | (sdadel << 16) | (sclh << 8) | scll;
}

static void configure_i2c_timing(I2C_HandleTypeDef *handle, uint32_t clock_speed_hz)
{
    uint32_t pclk1_freq = HAL_RCC_GetPCLK1Freq();
    handle->Init.Timing = compute_i2c_timing(pclk1_freq, clock_speed_hz);
}

#endif

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

static term nif_i2c_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term opts = argv[0];
    VALIDATE_VALUE(opts, term_is_list);

    static const char *const peripheral_str = ATOM_STR("\xA", "peripheral");
    static const char *const scl_str = ATOM_STR("\x3", "scl");
    static const char *const sda_str = ATOM_STR("\x3", "sda");
    static const char *const clock_speed_hz_str = ATOM_STR("\xE", "clock_speed_hz");
    static const char *const af_str = ATOM_STR("\x2", "af");

    GlobalContext *glb = ctx->global;

    term peripheral_term = interop_kv_get_value_default(opts, peripheral_str, term_from_int(1), glb);
    term scl_term = interop_kv_get_value(opts, scl_str, glb);
    term sda_term = interop_kv_get_value(opts, sda_str, glb);
    term clock_speed_term = interop_kv_get_value_default(opts, clock_speed_hz_str, term_from_int(100000), glb);
    term af_term = interop_kv_get_value_default(opts, af_str, term_from_int(4), glb);

    if (term_is_invalid_term(scl_term) || term_is_invalid_term(sda_term)) {
        AVM_LOGE(TAG, "scl and sda pins are required");
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(peripheral_term, term_is_integer);
    VALIDATE_VALUE(clock_speed_term, term_is_integer);
    VALIDATE_VALUE(af_term, term_is_integer);

    int peripheral = term_to_int(peripheral_term);
    uint32_t clock_speed_hz = (uint32_t) term_to_int(clock_speed_term);
    uint32_t af = (uint32_t) term_to_int(af_term);

    I2C_TypeDef *instance = peripheral_to_instance(peripheral);
    if (IS_NULL_PTR(instance)) {
        AVM_LOGE(TAG, "Invalid I2C peripheral: %d", peripheral);
        RAISE_ERROR(BADARG_ATOM);
    }

    GPIO_TypeDef *scl_port;
    uint16_t scl_pin;
    if (!parse_pin(glb, scl_term, &scl_port, &scl_pin)) {
        AVM_LOGE(TAG, "Invalid SCL pin");
        RAISE_ERROR(BADARG_ATOM);
    }

    GPIO_TypeDef *sda_port;
    uint16_t sda_pin;
    if (!parse_pin(glb, sda_term, &sda_port, &sda_pin)) {
        AVM_LOGE(TAG, "Invalid SDA pin");
        RAISE_ERROR(BADARG_ATOM);
    }

    enable_i2c_clock(peripheral);

    GPIO_InitTypeDef gpio_init = { 0 };
    gpio_init.Mode = GPIO_MODE_AF_OD;
    gpio_init.Pull = GPIO_PULLUP;
    gpio_init.Speed = GPIO_SPEED_FREQ_HIGH;
    gpio_init.Alternate = af;

    gpio_init.Pin = scl_pin;
    HAL_GPIO_Init(scl_port, &gpio_init);

    gpio_init.Pin = sda_pin;
    HAL_GPIO_Init(sda_port, &gpio_init);

    struct I2CResource *rsrc_obj = enif_alloc_resource(i2c_resource_type, sizeof(struct I2CResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    memset(&rsrc_obj->handle, 0, sizeof(I2C_HandleTypeDef));
    rsrc_obj->handle.Instance = instance;
    configure_i2c_timing(&rsrc_obj->handle, clock_speed_hz);
    rsrc_obj->handle.Init.OwnAddress1 = 0;
    rsrc_obj->handle.Init.AddressingMode = I2C_ADDRESSINGMODE_7BIT;
    rsrc_obj->handle.Init.DualAddressMode = I2C_DUALADDRESS_DISABLE;
    rsrc_obj->handle.Init.GeneralCallMode = I2C_GENERALCALL_DISABLE;
    rsrc_obj->handle.Init.NoStretchMode = I2C_NOSTRETCH_DISABLE;

    HAL_StatusTypeDef status = HAL_I2C_Init(&rsrc_obj->handle);
    if (status != HAL_OK) {
        enif_release_resource(rsrc_obj);
        AVM_LOGE(TAG, "HAL_I2C_Init failed: %d", (int) status);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(glb, ATOM_STR("\x9", "i2c_init")));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        HAL_I2C_DeInit(&rsrc_obj->handle);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return create_pair(ctx, OK_ATOM, obj);
}

static term nif_i2c_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    HAL_I2C_DeInit(&rsrc_obj->handle);
    rsrc_obj->handle.Instance = NULL;

    return OK_ATOM;
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

static term nif_i2c_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);

    uint16_t addr = (uint16_t) (term_to_int(argv[1]) << 1);
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    uint32_t timeout_ms = get_timeout_ms(argv[3]);

    HAL_StatusTypeDef status = HAL_I2C_Master_Transmit(&rsrc_obj->handle, addr, (uint8_t *) data, (uint16_t) len, timeout_ms);
    if (UNLIKELY(status != HAL_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return hal_status_to_error(ctx, status);
    }

    return term_from_int((int) len);
}

static term nif_i2c_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    uint16_t addr = (uint16_t) (term_to_int(argv[1]) << 1);
    avm_int_t count = term_to_int(argv[2]);
    uint32_t timeout_ms = get_timeout_ms(argv[3]);

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    HAL_StatusTypeDef status = HAL_I2C_Master_Receive(&rsrc_obj->handle, addr, buf, (uint16_t) count, timeout_ms);
    if (UNLIKELY(status != HAL_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return hal_status_to_error(ctx, status);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_mem_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_integer);

    uint16_t addr = (uint16_t) (term_to_int(argv[1]) << 1);
    uint16_t mem_addr = (uint16_t) term_to_int(argv[2]);
    avm_int_t count = term_to_int(argv[3]);
    uint32_t timeout_ms = get_timeout_ms(argv[4]);

    uint16_t mem_addr_size = (mem_addr > 0xFF) ? I2C_MEMADD_SIZE_16BIT : I2C_MEMADD_SIZE_8BIT;

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    HAL_StatusTypeDef status = HAL_I2C_Mem_Read(&rsrc_obj->handle, addr, mem_addr, mem_addr_size, buf, (uint16_t) count, timeout_ms);
    if (UNLIKELY(status != HAL_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return hal_status_to_error(ctx, status);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_mem_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_binary);

    uint16_t addr = (uint16_t) (term_to_int(argv[1]) << 1);
    uint16_t mem_addr = (uint16_t) term_to_int(argv[2]);
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[3]);
    size_t len = term_binary_size(argv[3]);
    uint32_t timeout_ms = get_timeout_ms(argv[4]);

    uint16_t mem_addr_size = (mem_addr > 0xFF) ? I2C_MEMADD_SIZE_16BIT : I2C_MEMADD_SIZE_8BIT;

    HAL_StatusTypeDef status = HAL_I2C_Mem_Write(&rsrc_obj->handle, addr, mem_addr, mem_addr_size, (uint8_t *) data, (uint16_t) len, timeout_ms);
    if (UNLIKELY(status != HAL_OK)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return hal_status_to_error(ctx, status);
    }

    return term_from_int((int) len);
}

static void i2c_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct I2CResource *rsrc_obj = (struct I2CResource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->handle.Instance)) {
        HAL_I2C_DeInit(&rsrc_obj->handle);
        rsrc_obj->handle.Instance = NULL;
    }
}

static const ErlNifResourceTypeInit I2CResourceTypeInit = {
    .members = 1,
    .dtor = i2c_resource_dtor,
};

static const struct Nif i2c_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_init
};
static const struct Nif i2c_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_deinit
};
static const struct Nif i2c_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write
};
static const struct Nif i2c_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read
};
static const struct Nif i2c_mem_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_mem_read
};
static const struct Nif i2c_mem_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_mem_write
};

static void i2c_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    i2c_resource_type = enif_init_resource_type(&env, "i2c_resource", &I2CResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *i2c_nif_get_nif(const char *nifname)
{
    if (strncmp("i2c:", nifname, 4) != 0) {
        return NULL;
    }
    const char *rest = nifname + 4;
    if (strcmp("init/1", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_deinit_nif;
    }
    if (strcmp("write/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_nif;
    }
    if (strcmp("read/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_nif;
    }
    if (strcmp("mem_read/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_mem_read_nif;
    }
    if (strcmp("mem_write/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_mem_write_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(i2c, i2c_nif_init, NULL, i2c_nif_get_nif)
