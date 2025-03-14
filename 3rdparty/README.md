<!---
  Copyright 2025 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

* [mbedlts](https://github.com/Mbed-TLS/mbedtls): v3.6.2

Partial copy of mbedtls:
- directory include/
- directory library/
- file script/config.py
- file LICENSE
- file CMakeLists.txt with some modifications as other directories are not present

Three files were added to make REUSE happy:
- CMakeLists.txt.license
- include/CMakeLists.txt.license
- library/CMakeLists.txt.license
