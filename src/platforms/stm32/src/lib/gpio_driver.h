/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

#ifndef _GPIO_DRIVER_H_
#define _GPIO_DRIVER_H_

#include "stm_sys.h"
#include <globalcontext.h>
#include <nifs.h>
#include <platform_nifs.h>

#ifndef AVM_DISABLE_GPIO_NIFS
static const struct Nif *gpio_nif_get_nif(const char *nifname);
void gpioregister_nif_collection(void);
#endif

#ifndef AVM_DISABLE_GPIO_PORT_DRIVER
static Context *gpio_driver_create_port(GlobalContext *global, term opts);
void gpiodriver_init(GlobalContext *glb);
void gpioregister_port_driver(void);
#endif

#endif /* _GPIO_DRIVER_H_ */
