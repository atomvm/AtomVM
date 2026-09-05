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

#include "storage.h"

#include <rtems.h>
#include <rtems/bdbuf.h>
#include <rtems/media.h>
#include <stdio.h>

bool storage_init(void)
{
    rtems_status_code sc = rtems_bdbuf_init();
    if (sc == RTEMS_SUCCESSFUL) {
        sc = rtems_media_initialize();
    }
    if (sc == RTEMS_SUCCESSFUL) {
        sc = rtems_media_server_initialize(200, 64 * 1024,
            RTEMS_DEFAULT_MODES, RTEMS_DEFAULT_ATTRIBUTES);
    }
    if (sc != RTEMS_SUCCESSFUL) {
        fprintf(stderr, "AtomVM: SD initialization failed: %s; using embedded application\n", rtems_status_text(sc));
        return false;
    }
    // The LibBSD MMC driver posts disk attachments to this media server.
    return true;
}

bool storage_load_app(struct RtemsApp *app)
{
    // LibBSD attaches cards asynchronously. Bound the wait so an absent card
    // or file still boots the embedded application. The configured path selects
    // one partition explicitly; do not accidentally run an app from another disk.
    rtems_interval delay = rtems_clock_get_ticks_per_second() / 10;
    if (delay == 0) {
        delay = 1;
    }
    for (unsigned attempt = 0; attempt <= 50; ++attempt) {
        enum RtemsAppLoadResult result = rtems_app_load_file(RTEMS_APP_PATH, app);
        if (result == RTEMS_APP_LOADED) {
            fprintf(stdout, "AtomVM: Loaded %s (%zu bytes)\n", RTEMS_APP_PATH, app->size);
            return true;
        }
        if (result != RTEMS_APP_NOT_FOUND) {
            fprintf(stderr, "AtomVM: Cannot load %s (%s); using embedded application\n",
                RTEMS_APP_PATH, result == RTEMS_APP_INVALID ? "invalid AVM pack" : result == RTEMS_APP_NO_MEMORY ? "out of memory"
                                                                                                                 : "read error");
            return false;
        }
        if (attempt < 50) {
            rtems_task_wake_after(delay);
        }
    }
    fprintf(stdout, "AtomVM: %s not found; using embedded application\n", RTEMS_APP_PATH);
    return false;
}
