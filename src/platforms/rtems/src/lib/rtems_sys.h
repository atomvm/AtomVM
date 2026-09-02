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

#ifndef _RTEMS_SYS_H_
#define _RTEMS_SYS_H_

#include <interop.h>
#include <portnifloader.h>
#include <sys.h>

#ifdef RTEMS_HAS_LIBBSD
#include <poll.h>
#endif

#define RTEMS_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "rtems"))

struct RTEMSPlatformData
{
#ifdef RTEMS_HAS_LIBBSD
    struct pollfd *fds;
    int select_events_poll_count;
#else
    int dummy;
#endif
};

#endif /* _RTEMS_SYS_H_ */
