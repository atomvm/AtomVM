<!---
  Copyright 2026 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# STM32 Platform Maintenance Guide

This document describes how to add support for new STM32 devices and families.

## Architecture Overview

The STM32 platform uses the official STM32 HAL SDK, fetched at build time via
CMake's FetchContent. The build system parses the device name (e.g.
`stm32f411ceu6`) to determine the family, CPU core, memory layout, and clock
configuration. Most of the configuration is data-driven: adding a new device
within an already-supported family typically requires only adding entries to the
device configuration database.

### Key files

| File | Purpose |
|------|---------|
| `cmake/stm32_device.cmake` | Maps family short code to CPU/FPU/linker settings |
| `cmake/stm32_sdk.cmake` | Fetches CMSIS + HAL SDK for the family, finds startup file |
| `cmake/stm32_hal_conf.h.in` | Template for HAL configuration header |
| `cmake/stm32_linker.ld.in` | Template for linker script |
| `cmake/atomvm_dev_config.cmake` | Queries device database for clock/ROM/RAM via escript |
| `tools/device_config.hrl` | Device database: ROM size, RAM size, clock frequency |
| `tools/atomvm_stm32_config_query.erl` | Escript that queries device_config.hrl |
| `clock_configs/clock_config_*.c` | Per-family `SystemClock_Config()` implementations |
| `src/lib/stm32_hal_platform.h` | Family-conditional HAL header includes |
| `src/lib/avm_devcfg.h` | UART console pin definitions |

## Adding a New Device (Existing Family)

If the STM32 family is already supported (check `cmake/stm32_device.cmake` for
the `_FAM_xx` entry), adding a new device only requires updating the device
database:

### 1. Add device configuration to `tools/device_config.hrl`

Add a macro with ROM size, RAM size, and clock frequency:

```erlang
-define(STM32XYZW_E, #{
    rom => "ROM_512K",
    ram => "RAM_256K",
    clock => "120000000",
    flash_size_kb => "512",
    ram_size_kb => "256"
}).
```

The clock value must match a supported `AVM_CLOCK_HZ` case in the family's
`clock_configs/clock_config_*.c` file. If a new clock/HSE combination is
needed, add a new `#elif` block there (see "Clock Configuration" below).

### 2. Add device lookup to `tools/atomvm_stm32_config_query.erl`

Find the `get_XXdev_config/1` function for the family and add a case clause for
the new device line and flash code. The lookup string starts at position 7 of
the device name (after `stm32XX`), where characters 0-1 are the device line and
character 3 is the flash size code.

For example, for `stm32f411ceu6`: lookup is `"11ceu6"`, line is `"11"`, flash
is `"e"`.

### 3. Verify clock support

Check that the family's `clock_configs/clock_config_*.c` has a matching
`#if AVM_CLOCK_HZ == <clock> && HSE_VALUE == <hse>` block. If the new device
uses a different clock target or HSE frequency, add a new conditional block.

### 4. Test the build

```sh
mkdir build-newdevice && cd build-newdevice
cmake .. -DDEVICE=stm32xyzwabc -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake
cmake --build .
```

## Adding a New Family

Supporting an entirely new STM32 family requires changes to several files:

### 1. `cmake/stm32_device.cmake`

Add the CPU/FPU mapping:

```cmake
set(_FAM_xy  "cortex-mN;fpvX-Y-Z;hard")
```

If the family has non-standard RAM origin (not `0x20000000`), add a linker
override in the family-specific section at the bottom of the file.

### 2. `cmake/stm32_sdk.cmake`

Add SDK version tags:

```cmake
set(_SDK_xy  "vA.B.C;vX.Y.Z")  # CMSIS device tag; HAL driver tag
```

The build system assumes GitHub repos follow the naming convention:
- CMSIS device: `https://github.com/STMicroelectronics/cmsis_device_<family>.git`
- HAL driver: `https://github.com/STMicroelectronics/stm32<family>xx_hal_driver.git`

If the repos use a different naming convention (like U3 which uses hyphens),
add override variables:

```cmake
set(_SDK_xy_CMSIS_REPO "https://github.com/STMicroelectronics/cmsis-device-xy.git")
set(_SDK_xy_HAL_REPO   "https://github.com/STMicroelectronics/stm32xyxx-hal-driver.git")
```

Find the latest tags at:
- https://github.com/STMicroelectronics/cmsis_device_xy/tags
- https://github.com/STMicroelectronics/stm32xyxx_hal_driver/tags

### 3. `cmake/stm32_hal_conf.h.in`

Check if the new family needs a different default HSE_VALUE. Add a conditional
if needed:

```c
#elif defined(STM32XYXX)
#define HSE_VALUE 8000000U
```

Also check if the family needs any additional HAL modules enabled or has
different peripheral naming conventions.

### 4. `clock_configs/clock_config_xy.c`

Create a new clock configuration file implementing `SystemClock_Config()`.
See "Clock Configuration" below.

### 5. `src/lib/stm32_hal_platform.h`

Add the family-conditional includes:

```c
#elif defined(STM32XYXX)
#include "stm32xyxx_hal.h"
#include "stm32xyxx_ll_exti.h"
#include "stm32xyxx_ll_gpio.h"
```

### 6. `tools/device_config.hrl` and `tools/atomvm_stm32_config_query.erl`

Add device definitions and the corresponding lookup function as described in
"Adding a New Device" above. Also add a new `"stm32xy"` case in
`get_dev_config/1`.

### 7. Family-specific code

Check if any platform C code (`sys.c`, `gpio_driver.c`, `main.c`) has
family-specific conditionals that need a new case. Search for existing
`#if defined(STM32` patterns.

## Clock Configuration

### Origin of clock_config files

The `clock_configs/clock_config_*.c` files are **hand-written**, not generated
by a tool. They implement `SystemClock_Config()` using the STM32 HAL RCC API.

While STM32CubeMX can generate a `SystemClock_Config()` function, the generated
code targets a single board with a fixed HSE frequency and clock target. The
AtomVM clock configs support multiple HSE/clock combinations via preprocessor
conditionals (`#if AVM_CLOCK_HZ == ... && HSE_VALUE == ...`), so they cannot be
directly generated by CubeMX.

### Using STM32CubeMX as a reference

STM32CubeMX (part of STM32CubeIDE) can still be useful as a reference when
writing a new clock configuration:

1. Create a CubeMX project for the target device
2. Configure the clock tree to reach the desired SYSCLK frequency
3. Generate code and look at the `SystemClock_Config()` in `main.c`
4. Adapt the PLL parameters and bus dividers into the AtomVM format with
   `AVM_CLOCK_HZ` / `HSE_VALUE` conditionals

### Writing a clock config

Each clock config file must:

1. Include the family HAL header (e.g. `#include "stm32f4xx_hal.h"`)
2. Implement `void SystemClock_Config(void)`
3. Configure HSE -> PLL -> SYSCLK at the frequency defined by `AVM_CLOCK_HZ`
4. Use `HSE_VALUE` to compute PLL dividers for different crystal frequencies
5. Configure bus dividers (APBx) within the family's maximum frequencies
6. Set appropriate flash latency for the target frequency
7. Provide a `#error` fallback for unsupported combinations

PLL parameters must satisfy:
- VCO input frequency within the family's allowed range
- VCO output frequency within the family's allowed range
- PLLQ should ideally produce 48 MHz for USB (when applicable)

### Key defines

- `AVM_CLOCK_HZ`: Target SYSCLK frequency, set from `device_config.hrl`
  via cmake
- `HSE_VALUE`: External crystal frequency. Defaults are set in
  `stm32_hal_conf.h.in` and can be overridden with `-DAVM_HSE_VALUE=<hz>`

## Linker Script

The linker script template (`cmake/stm32_linker.ld.in`) is parameterized with:

- `FLASH_SIZE_KB` / `LINKER_RAM_SIZE` - from device database
- `LINKER_RAM_ORIGIN` - `0x20000000` for most families, different for F7/H7
- `LINKER_EXTRA_MEMORY` - additional memory regions (DTCM, shared RAM)
- `LINKER_EXTRA_SECTIONS` - additional sections (e.g. WB55 mailbox)
- `LINKER_EXTRA_SYMBOLS` - additional linker symbols

Overrides are in `cmake/stm32_device.cmake` under family-specific blocks.

## Build Configuration

### CMake variables

| Variable | Required | Description |
|----------|----------|-------------|
| `DEVICE` | Yes | Full device name (e.g. `stm32f411ceu6`) |
| `CMAKE_TOOLCHAIN_FILE` | Yes | Path to `cmake/arm-toolchain.cmake` |
| `BOARD` | No | Board variant (e.g. `nucleo`) for UART pin selection |
| `AVM_HSE_VALUE` | No | Override HSE crystal frequency in Hz |

### Local SDK development

To use local SDK checkouts instead of fetching from GitHub:

```sh
export STM32_CMSIS_CORE_PATH=/path/to/cmsis_core
export STM32_CMSIS_DEVICE_F4_PATH=/path/to/cmsis_device_f4
export STM32_HAL_DRIVER_F4_PATH=/path/to/stm32f4xx_hal_driver
```

Replace `F4` in the variable name with the appropriate family in uppercase.
