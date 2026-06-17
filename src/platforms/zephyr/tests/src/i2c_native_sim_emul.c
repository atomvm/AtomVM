/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <stdint.h>
#include <string.h>

#include <zephyr/device.h>
#include <zephyr/drivers/emul.h>
#include <zephyr/drivers/emul_stub_device.h>
#include <zephyr/drivers/i2c.h>
#include <zephyr/drivers/i2c_emul.h>
#include <zephyr/sys/util.h>

#define DT_DRV_COMPAT vnd_atomvm_native_sim_i2c_target

struct atomvm_native_sim_i2c_data
{
    uint8_t memory[256];
    uint8_t pointer;
};

static int atomvm_native_sim_i2c_transfer(const struct emul *target, struct i2c_msg *msgs, int num_msgs, int addr)
{
    ARG_UNUSED(addr);

    struct atomvm_native_sim_i2c_data *data = target->data;

    for (int i = 0; i < num_msgs; i++) {
        struct i2c_msg *msg = &msgs[i];

        if ((msg->flags & I2C_MSG_READ) != 0) {
            for (uint32_t pos = 0; pos < msg->len; pos++) {
                msg->buf[pos] = data->memory[data->pointer++];
            }
        } else if (msg->len > 0) {
            data->pointer = msg->buf[0];
            for (uint32_t pos = 1; pos < msg->len; pos++) {
                data->memory[data->pointer++] = msg->buf[pos];
            }
        }
    }

    return 0;
}

static struct i2c_emul_api atomvm_native_sim_i2c_bus_api = {
    .transfer = atomvm_native_sim_i2c_transfer,
};

static int atomvm_native_sim_i2c_init(const struct emul *target, const struct device *parent)
{
    ARG_UNUSED(parent);

    struct atomvm_native_sim_i2c_data *data = target->data;
    memset(data->memory, 0, sizeof(data->memory));
    data->pointer = 0;

    return 0;
}

#define ATOMVM_NATIVE_SIM_I2C_TARGET(n)                                                                                 \
    static struct atomvm_native_sim_i2c_data atomvm_native_sim_i2c_data_##n;                                            \
    EMUL_DT_INST_DEFINE(n, atomvm_native_sim_i2c_init, &atomvm_native_sim_i2c_data_##n, NULL,                           \
        &atomvm_native_sim_i2c_bus_api, NULL);

DT_INST_FOREACH_STATUS_OKAY(ATOMVM_NATIVE_SIM_I2C_TARGET)
DT_INST_FOREACH_STATUS_OKAY(EMUL_STUB_DEVICE)
