/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Davide Bettio <davide@uninstall.it>
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

#include "unity.h"
#include <stdio.h>
#include <string.h>

#include "avmpack.h"
#include "nifs.h"
#include "platform_defaultatoms.h"

#include "esp32_sys.h"

const struct Nif *platform_nifs_get_nif(const char *nifname);

TEST_CASE("atomvm_platform_0", "[platform_nifs]")
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:platform/0");
    TEST_ASSERT(nif->nif_ptr(NULL, 0, NULL) == ESP32_ATOM);
}

TEST_CASE("atomvm_missing_0", "[platform_nifs]")
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:missing/0");
    TEST_ASSERT(nif == NULL);
}

void app_main(void)
{
    UNITY_BEGIN();
    unity_run_all_tests();
    UNITY_END();
}
