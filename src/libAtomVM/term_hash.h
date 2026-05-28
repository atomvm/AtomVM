/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#ifndef _TERM_HASH_H_
#define _TERM_HASH_H_

#include <stdint.h>

#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

struct GlobalContext;

/**
 * @brief Calculate a stable hash for an Erlang term.
 *
 * @details This hash is intended for AtomVM internal hash tables. It is
 * conceptually similar to the term hashing used by OTP, but it is not part of
 * the Erlang API and must not be exposed as erlang:phash2.
 *
 * @param t term to hash
 * @param global global context used to resolve atom strings
 * @return 32-bit hash value
 */
uint32_t term_hash(term t, struct GlobalContext *global);

#ifdef __cplusplus
}
#endif

#endif
