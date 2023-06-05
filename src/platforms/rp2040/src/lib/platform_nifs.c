/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#include "nifs.h"

#include "platform_defaultatoms.h"
#include "platform_nifs.h"
#include "rp2040_sys.h"

// Pico SDK
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <hardware/rtc.h>

#pragma GCC diagnostic pop

//#define ENABLE_TRACE
#include "trace.h"

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        argv[0] = ERROR_ATOM;                  \
        argv[1] = BADARG_ATOM;                 \
        return term_invalid_term();            \
    }

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    return PICO_ATOM;
}

static term nif_pico_rtc_set_datetime(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(ctx);

    term datetime_tuple = argv[0];
    VALIDATE_VALUE(datetime_tuple, term_is_tuple);
    if (UNLIKELY(term_get_tuple_arity(datetime_tuple) != 2)) {
        argv[0] = ERROR_ATOM;
        argv[1] = BADARG_ATOM;
        return term_invalid_term();
    }
    term date_tuple = term_get_tuple_element(datetime_tuple, 0);
    VALIDATE_VALUE(date_tuple, term_is_tuple);
    if (UNLIKELY(term_get_tuple_arity(date_tuple) != 3)) {
        argv[0] = ERROR_ATOM;
        argv[1] = BADARG_ATOM;
        return term_invalid_term();
    }
    term time_tuple = term_get_tuple_element(datetime_tuple, 1);
    VALIDATE_VALUE(time_tuple, term_is_tuple);
    if (UNLIKELY(term_get_tuple_arity(time_tuple) != 3)) {
        argv[0] = ERROR_ATOM;
        argv[1] = BADARG_ATOM;
        return term_invalid_term();
    }
    term year = term_get_tuple_element(date_tuple, 0);
    VALIDATE_VALUE(year, term_is_integer);
    term month = term_get_tuple_element(date_tuple, 1);
    VALIDATE_VALUE(month, term_is_integer);
    term day = term_get_tuple_element(date_tuple, 2);
    VALIDATE_VALUE(day, term_is_integer);
    term hour = term_get_tuple_element(time_tuple, 0);
    VALIDATE_VALUE(hour, term_is_integer);
    term min = term_get_tuple_element(time_tuple, 1);
    VALIDATE_VALUE(min, term_is_integer);
    term sec = term_get_tuple_element(time_tuple, 2);
    VALIDATE_VALUE(sec, term_is_integer);

    datetime_t pico_datetime;
    pico_datetime.year = term_to_int(year);
    pico_datetime.month = term_to_int(month);
    pico_datetime.day = term_to_int(day);
    pico_datetime.hour = term_to_int(hour);
    pico_datetime.min = term_to_int(min);
    pico_datetime.sec = term_to_int(sec);

    if (UNLIKELY(!rtc_set_datetime(&pico_datetime))) {
        argv[0] = ERROR_ATOM;
        argv[1] = BADARG_ATOM;
        return term_invalid_term();
    }

    return OK_ATOM;
}

static const struct Nif atomvm_platform_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};
static const struct Nif pico_rtc_set_datetime_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_pico_rtc_set_datetime
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
    if (strcmp("pico:rtc_set_datetime/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &pico_rtc_set_datetime_nif;
    }
    return NULL;
}
