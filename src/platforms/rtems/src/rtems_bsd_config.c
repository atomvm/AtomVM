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

#include <bsp.h>
#include <rtems/bsd/bsd.h>
#include <machine/rtems-bsd-nexus-bus.h>
#include <machine/rtems-bsd-sysinit.h>

/*
 * Do not use RTEMS_BSD_CONFIG_BSP_CONFIG: the imx7 default nexus set
 * attaches sdhci_fsl, which times out on QEMU mcimx7d-sabre. Keep the FDT
 * nexus plus ffec/ukphy for networking.
 */
RTEMS_BSD_DEFINE_NEXUS_DEVICE(ofwbus, 0, 0, NULL);
SYSINIT_DRIVER_REFERENCE(simplebus, ofwbus);
SYSINIT_DRIVER_REFERENCE(ffec, simplebus);
SYSINIT_DRIVER_REFERENCE(ukphy, miibus);

#define RTEMS_BSD_CONFIG_INIT

#include <machine/rtems-bsd-config.h>
