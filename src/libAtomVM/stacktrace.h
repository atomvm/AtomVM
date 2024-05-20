/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Fred Dushin <fred@dushin.net>
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

#ifndef _STACKTRACE_H_
#define _STACKTRACE_H_

#include "context.h"
#include "module.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

term stacktrace_create_raw(Context *ctx, Module *mod, int current_offset, term exception_class);
/**
 * @brief Build a stack trace
 * @param ctx           context
 * @param stack_info    pointer to stack info tuple
 * @param live          number of x registers to preserve, which should include stack_info
 * @return the built stack trace
 */
term stacktrace_build(Context *ctx, term *stack_info, uint32_t live);
term stacktrace_exception_class(term stack_info);

#ifdef __cplusplus
}
#endif

#endif
