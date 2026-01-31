/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

/**
 * @file nifs.h
 * @brief Private NIFs
 */

#ifndef _NIFS_H_
#define _NIFS_H_

#include "atom.h"
#include "context.h"
#include "exportedfunction.h"

#ifdef __cplusplus
extern "C" {
#endif

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        argv[0] = ERROR_ATOM;                  \
        argv[1] = BADARG_ATOM;                 \
        return term_invalid_term();            \
    }

#define RAISE_ERROR(error_type_atom)   \
    do {                               \
        ctx->x[0] = ERROR_ATOM;        \
        ctx->x[1] = (error_type_atom); \
        return term_invalid_term();    \
    } while (0);

const struct Nif *nifs_get(const char *mfa);

// spawn opt is used by distribution nifs
term nif_erlang_spawn_opt(Context *ctx, int argc, term argv[]);
#ifdef __cplusplus
}
#endif

#endif
