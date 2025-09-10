<!---
  Copyright 2017-2025 Davide Bettio <davide@uninstall.it>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

AtomVM
===========

Brings [Erlang](https://www.erlang.org/), [Elixir](https://elixir-lang.org/) and other functional
languages to really small systems.

AtomVM implements from scratch a minimal Erlang VM that supports a subset of ErlangVM features and that is able to run unmodified BEAM binaries on really small systems like MCUs.

Supported Platforms
===================

* Linux, macOS, FreeBSD, DragonFly ([generic_unix](https://doc.atomvm.org/main/getting-started-guide.html#getting-started-on-the-generic-unix-platform))
* ESP32 SoC (with IDF/FreeRTOS, see [esp32](https://doc.atomvm.org/main/getting-started-guide.html#getting-started-on-the-esp32-platform))
* STM32 MCUs (with LibOpenCM3, see [stm32](https://doc.atomvm.org/main/getting-started-guide.html#getting-started-on-the-stm32-platform))
* Raspberry Pi Pico and Pico 2 (see [rp2](https://doc.atomvm.org/main/getting-started-guide.html#getting-started-on-the-raspberry-pi-pico-platform))
* Browsers and NodeJS with WebAssembly (see [emscripten](https://doc.atomvm.org/main/getting-started-guide.html#getting-started-with-atomvm-webassembly))

AtomVM aims to be easily portable to new platforms with a minimum effort, so additional platforms
might be supported in a near future.

Getting Started
===============
There is much more information, including a more complete
["Getting Started Guide"](https://doc.atomvm.org/main/getting-started-guide.html),
[examples](https://atomvm.org/sample-code),
detailed [build instructions](https://doc.atomvm.org/main/build-instructions.html),
and [contact information](https://atomvm.org/contact) available on the
[AtomVM](https://atomvm.org) project website.

>Don't forget to check out the [examples repository](https://github.com/atomvm/atomvm_examples) to
>help get you started on your next IoT project.

**Please, use [v0.6.x](https://github.com/atomvm/AtomVM/tree/release-0.6) releases, main branch
is for development purposes and it might be unstable.**

Dependencies
------------

Required for building:
* CMake ([CMake build system](https://cmake.org/))
* gperf ([GNU Perfect Hash Function Generator](https://www.gnu.org/software/gperf/manual/gperf.html))
* erlc ([erlang compiler](https://www.erlang.org/))
* elixirc ([elixir compiler](https://elixir-lang.org))
* rebar3 ([rebar3 build tool](https://www.rebar3.org/))
* Mbed TLS ([portable TLS library, optionally required to support SSL](https://www.trustedfirmware.org/projects/mbed-tls/))
* zlib ([zlib compression and decompression library](https://zlib.net/))

Documentation and Coverage:
* gcov and lcov are optionally required to generate coverage report (`make coverage`).
* For documentation build requirements consult the [Documentation README](doc/README.md).

Step-by-Step Build Instructions (generic unix platform)
-------------------------------------------------------

```
$ mkdir build
$ cd build
$ cmake ..
$ make
$ ./src/AtomVM ./examples/erlang/hello_world.avm
```

Complete [Build Instructions](https://doc.atomvm.org/main/build-instructions.html) are
available in the documentation for
[Generic UNIX](https://doc.atomvm.org/main/build-instructions.html) (Linux, MacOS, FreeBSD, DragonFly),
[ESP32](https://doc.atomvm.org/main/build-instructions.html#building-for-esp32),
[STM32](https://doc.atomvm.org/main/build-instructions.html#building-for-stm32),
[Raspberry Pi Pico and Pico 2](https://doc.atomvm.org/main/build-instructions.html#building-for-raspberry-pi-pico)
(rp2), and
[WASM](https://doc.atomvm.org/main/build-instructions.html#building-for-nodejs-web) (NodeJS/Web).

Project Status
==============

[![Build and Test](https://github.com/atomvm/AtomVM/actions/workflows/build-and-test.yaml/badge.svg?branch=main)](https://github.com/atomvm/AtomVM/actions/workflows/build-and-test.yaml)

AtomVM is no longer just a prototype — it has reached a solid level of compatibility with the BEAM
ecosystem. It can execute unmodified, compiled BEAM modules, and most
[core standard library functions](https://doc.atomvm.org/main/api-reference-documentation.html)
are already supported.

AtomVM is tested with code compiled with any version from OTP 21 to 27 (28 is supported only in `main` branch).

Crashes may still occur in edge cases, so we recommend using either the
[latest stable release](https://github.com/atomvm/AtomVM/releases)
[(v0.6.x)](https://github.com/atomvm/AtomVM/tree/release-0.6) or the
[main branch](https://github.com/atomvm/AtomVM/tree/main) if you're experimenting with newer
features.
If you run into issues, please [open an issue](https://github.com/atomvm/AtomVM/issues/new/choose)
 — we’d love your feedback.

For a detailed list of recent changes and new features, see the
[changelog](https://github.com/atomvm/AtomVM/blob/main/CHANGELOG.md).

AtomVM is ready to power your next embedded or lightweight functional programming project.

Known Limitations
-----------------

Your existing project might not work out of the box due to a few current limitations:

- The standard library is minimal — some functions and modules are not yet implemented.
- Features that depend on a full operating system (e.g. file I/O, OS processes) may be missing or
behave differently.
- Certain BEAM features like `on_load` functions and tracing are not yet supported.
- Some functionality may simply be missing — if you find a gap, feel free to open an issue, and
we’ll take a look.

When a feature is implemented, we aim to provide behavior that is consistent with the official
[BEAM documentation](https://www.erlang.org/docs) and
[Erlang/OTP runtime](https://github.com/erlang/otp).

Some of these limitations are on the roadmap, while others are deliberate design decisions to keep
AtomVM lightweight, portable, and suitable for embedded systems.

About This Project
==================
This project has been created by [Davide Bettio](https://github.com/bettio/), and now is developed
from a growing number of [contributors](https://github.com/atomvm/AtomVM/graphs/contributors).

How to Contribute
-----------------
Any kind of [contribution](CONTRIBUTING.md) is welcome, you can either contribute to this repository
by improving the virtual machine, the core libraries or the documentation or by contributing to any
of the [organization](https://github.com/atomvm) repositories.

License
-------
This project is under the terms of the Apache 2.0 license.

❤️ Acknowledgements
-------------------

AtomVM is made possible thanks to the dedication of a vibrant community of individual contributors
who have generously shared their time and expertise. We are deeply grateful for their ongoing
support and passion over the years.

We also want to extend a special thank you to the following companies for supporting the development
of AtomVM:

[![Dashbit](doc/assets/dashbit-logo.png)](https://dashbit.co/)
[![Software Mansion](doc/assets/sw-mansion-logo.png)](https://swmansion.com/)

Thank you all for being part of this journey! 🙌
