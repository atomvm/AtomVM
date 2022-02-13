/*
 * This file is part of AtomVM.
 *
 * Copyright 2021 Fred Dushin <fred@dushin.net>
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

#ifndef _REFC_BINARY_H_
#define _REFC_BINARY_H_

#include "list.h"
#include <stdbool.h>
#include <stdlib.h>

struct RefcBinary
{
    struct ListHead head;
    size_t ref_count;
    size_t size;
};

/**
 * @brief Create a reference-counted binary outside of the process heap
 *
 * @details This function will create a reference-counted binary outside of the context heap.  If the binary is non-const,
 * a blob will be allocated in the VM memory (e.g., via malloc).  The allocated data will include
 * an internal data structure that includes the data size and reference count.  If the supplied
 * data is non-NULL, the supplied data will be copied to the newly allocated region.
 * @param size the size of the data to create
 * @returns a pointer to the out-of-context data.
 */
struct RefcBinary *refc_binary_create_refc(size_t size);

/**
 * @brief get the data of the off-context binary
 *
 * @details Return the data of the off-context binary
 * @param ptr Refc binary returned from memory_create_refc_binary
 */
const char *refc_binary_get_data(const struct RefcBinary *ptr);

/**
 * @brief Increment the reference count on the refc binary
 * @param ptr the refc binary
 */
void refc_binary_increment_refcount(struct RefcBinary *ptr);

/**
 * @brief Decrement the reference count on the refc binary
 *
 * @details This function will free the the refc binary if the
 * reference count reaches 0.
 * @param ptr the refc binary
 * @return true if the refc binary was free'd; false, otherwise
 */
bool refc_binary_decrement_refcount(struct RefcBinary *ptr);

/**
 * TODO consider implementing erlang:memory/0,1 instead
 */
term refc_binary_create_binary_info(Context *ctx);

#endif // _REFC_BINARY_H_
