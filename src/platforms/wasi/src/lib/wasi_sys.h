/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef WASI_SYS_H_
#define WASI_SYS_H_

#include <globalcontext.h>
#include <list.h>
#include <stddef.h>
#include <stdint.h>
#include <sys.h>

#ifdef __wasip2__
typedef int listener_event_t;

struct EventListener
{
    struct ListHead listeners_list_head;
    event_handler_t handler;
    listener_event_t fd;
};
#endif

/**
 * @brief Signal the event poll to wake up.
 * @details Only used internally by the WASI platform; not part of the sys.h API.
 * @param glb the global context
 */
void sys_wakeup(GlobalContext *glb);

/**
 * @brief Fill an output buffer using getentropy(3) in 256-byte chunks.
 * @details getentropy(3) (per POSIX/WASI) refuses requests larger than 256
 *          bytes; this helper hides the chunking from callers.
 * @param output destination buffer
 * @param len number of bytes to fill
 * @return 0 on success, -1 on failure (errno set by getentropy)
 */
int wasi_get_random(uint8_t *output, size_t len);

#endif /* WASI_SYS_H_ */
