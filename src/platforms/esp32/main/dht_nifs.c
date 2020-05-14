/***************************************************************************
 *   Copyright 2020 by Fred Dushin <fred@dushin.net>                       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#define _GNU_SOURCE

#include "atom.h"
#include "defaultatoms.h"
#include "platform_defaultatoms.h"
#include "nifs.h"
#include "term.h"

#include "driver/gpio.h"
#include "esp_timer.h"
#include "rom/ets_sys.h"

//#define ENABLE_TRACE
#include "trace.h"

// References
// https://www.electronicwings.com/sensors-modules/dht11
// https://components101.com/sites/default/files/component_datasheet/DHT11-Temperature-Sensor.pdf
// https://cdn-shop.adafruit.com/datasheets/Digital+humidity+and+temperature+sensor+AM2302.pdf

//
// Handshake Send:
//
// ---+                      +---  1 (high)
//     \                    /      |
//      \                  /       |
//       \                /        |
//        +--------------+         0 (low)
//        |              |
//        |<--- 18ms --->|
//
//
//
// Handshake Receive:
//
//
//                            |<---- 80us ---->|
//                            |                |
// ---+                       +----------------+            1 (high)
//     \                     /                  \           |
//      \                   /                    \          |
//       \                 /                      \         |
//        +---------------+                        +------  0 (low)
//        |               |
//        | <--- 80us --->|
//
//
// for each bit (of 40):
//
// Data Receive 0:
//
//                            |<- 28us ->|
//                            |          |
// ---+                       +----------+            1 (high)
//     \                     /            \           |
//      \                   /              \          |
//       \                 /                \         |
//        +---------------+                  +------  0 (low)
//        |               |
//        | <--- 54us --->|
//
//
// Data Receive 1:
//
//                            |<---- 70us ---->|
//                            |                |
// ---+                       +----------------+            1 (high)
//     \                     /                  \           |
//      \                   /                    \          |
//       \                 /                      \         |
//        +---------------+                        +------  0 (low)
//        |               |
//        | <--- 54us --->|
//

#define HANDSHAKE_SEND_LOW_US (18000)
#define HANDSHAKE_RECV_LOW_US (80)
#define HANDSHAKE_RECV_HIGH_US (80)
#define DATA_RECV_LOW_US (54)
#define DATA_RECV_HIGH_ONE_US (70)
#define DATA_RECV_HIGH_ZERO_US (28)

#define MAX_WAIT_US (1000)
#define TIMEOUT (-1)
#define PROTOCOL_ERROR (-1)


static const char *const dht_bad_read         = "\x8"  "bad_read";
//                                                      123456789ABCDEF01


static inline void init_hanshake(avm_int_t pin)
{
    gpio_set_direction(pin, GPIO_MODE_OUTPUT);
    gpio_set_level(pin, 0);
    ets_delay_us(HANDSHAKE_SEND_LOW_US);
    gpio_set_level(pin, 1);
    gpio_set_direction(pin, GPIO_MODE_INPUT);
}

static inline int wait_while(avm_int_t pin, unsigned value, int max_wait)
{
    register int i = 0;
    for (;  gpio_get_level(pin) == value;  ++i) {
        if (max_wait < i) {
            return TIMEOUT;
        }
        ets_delay_us(1);
    }
    return i;
}

static inline int handshake(avm_int_t pin)
{
    init_hanshake(pin);

    if (wait_while(pin, 1, MAX_WAIT_US) == TIMEOUT) {
        TRACE("Timed out waiting to initialze handshake\n");
        return PROTOCOL_ERROR;
    }
    if (wait_while(pin, 0, HANDSHAKE_RECV_LOW_US) == TIMEOUT) {
        TRACE("Timed out waiting to recieve handshake low signal\n");
        return PROTOCOL_ERROR;
    }
    if (wait_while(pin, 1, HANDSHAKE_RECV_HIGH_US) == TIMEOUT) {
        TRACE("Timed out waiting to recieve handshake high signal\n");
        return PROTOCOL_ERROR;
    }

    return 0;
}


static int read_into(avm_int_t pin, uint8_t *buf)
{
    if (handshake(pin) == PROTOCOL_ERROR) {
        TRACE("Hanshake failed on pin %d\n", pin);
        return PROTOCOL_ERROR;
    }

    // read 40 bits into buf
    for (unsigned i = 0;  i < 40;  ++i) {
        if (wait_while(pin, 0, DATA_RECV_LOW_US) == TIMEOUT) {
            TRACE("Timed out waiting to recieve data low signal\n");
            return PROTOCOL_ERROR;
        }
        int try_one = wait_while(pin, 1, DATA_RECV_HIGH_ONE_US);
        switch (try_one) {
            case TIMEOUT:
                // pin was high for at least DATA_RECV_HIGH_ONE_US; it has to be a 1
                buf[i / 8] |= (1 << (7 - (i % 8)));
                break;
            default:
                // it could be a 0, if the pin was high for less than DATA_RECV_HIGH_ZERO_US
                // otherwise, it's a 1
                if (DATA_RECV_HIGH_ZERO_US < try_one) {
                    buf[i / 8] |= (1 << (7 - (i % 8)));
                }
                break;
        }
    }

    return 0;
}


static term nif_dht_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term pin = argv[0];
    VALIDATE_VALUE(pin, term_is_integer);

    if (UNLIKELY(memory_ensure_free(ctx, 20) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    uint8_t buf[5];
    memset(buf, 0, 5);
    int err = read_into(term_to_int(pin), buf);
    if (err == PROTOCOL_ERROR) {
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, context_make_atom(ctx, dht_bad_read));
            return error_tuple;
        }
    } else {
        term ok_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, term_from_literal_binary(buf, 5, ctx));
        return ok_tuple;
    }
}


static const struct Nif dht_read_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_dht_read
};
const struct Nif *dht_nifs_get_nif(const char *nifname)
{
    if (strcmp("dht:read/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &dht_read_nif;
    }
    return NULL;
}
