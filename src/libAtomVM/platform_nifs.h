/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Fred Dushin <fred@dushin.net>
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

#ifndef _PLATFORM_NIFS_H_
#define _PLATFORM_NIFS_H_

#include "exportedfunction.h"
#include "module.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief    Returns the Nif assocatiated with a nif name.
 *
 * @details  This function is used to locate platform-specific Nifs.
 *           Each platform must include an implementation of this function,
 *           even if it just returns NULL.
 * @param    nifname a null-terminated module:function/arity name
 * @return   the Nif structure associated with the supplied nif name, or
 *           NULL, if there is no such Nif.
 */
const struct Nif *platform_nifs_get_nif(const char *nifname);

#ifdef __cplusplus
}
#endif

#endif
