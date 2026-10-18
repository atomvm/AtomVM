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

#ifndef RTEMS_APP_LOADER_H
#define RTEMS_APP_LOADER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define RTEMS_APP_MAX_SIZE (32U * 1024U * 1024U)

struct RtemsApp
{
    const void *data;
    size_t size;
    const void *startup_beam;
    uint32_t startup_beam_size;
    const char *startup_name;
};

enum RtemsAppLoadResult
{
    RTEMS_APP_LOADED,
    RTEMS_APP_NOT_FOUND,
    RTEMS_APP_INVALID,
    RTEMS_APP_IO_ERROR,
    RTEMS_APP_NO_MEMORY
};

// Bounds-check the pack and its BEAM chunks before using the VM's pack walkers.
// This checks the container, not bytecode semantics or compatibility with OTP.
bool rtems_app_validate(const void *data, size_t size, struct RtemsApp *app);

// On success app->data is malloc-owned and must remain alive while modules use it.
// On failure app is cleared and no allocation is retained.
enum RtemsAppLoadResult rtems_app_load_file(const char *path, struct RtemsApp *app);

#endif
