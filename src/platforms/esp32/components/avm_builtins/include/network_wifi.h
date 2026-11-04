/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Gabriel Mancini <gabriel.mancini@gmail.com>
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

#ifndef _NETWORK_WIFI_H_
#define _NETWORK_WIFI_H_

#include <esp_err.h>

/*
 * The network driver owns the Wi-Fi radio. Another driver that needs the
 * radio without a network (ESP-NOW) holds it: the network driver turns
 * Wi-Fi on as a bare STA if no network uses it, and network:stop/0 then
 * leaves it on instead of turning it off. `restarted' is called (from the
 * network driver, in the scheduler) each time the radio comes back up as a
 * bare STA after a network used it, so the holder can set its channel again.
 */
esp_err_t network_wifi_hold(void (*restarted)(void));
void network_wifi_release(void);

#endif
