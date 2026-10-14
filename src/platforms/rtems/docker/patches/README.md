<!--
Copyright 2026 Peter M. <petermm@gmail.com>

SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# RTEMS Patches

Drop any `*.patch` or `*.diff` files into this directory.

When building the Docker image or running `docker-build-esp32c3.sh`, any patches
in this directory will be automatically applied to the RTEMS kernel source tree
before compiling the `riscv/esp32c3db` BSP:

```sh
# Example: apply a custom patch to RTEMS 7
cp /path/to/my-esp32c3-fix.patch src/platforms/rtems/docker/patches/
./src/platforms/rtems/tools/docker-build-esp32c3.sh --rebuild
```
