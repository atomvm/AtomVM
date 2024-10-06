<!---
  Copyright 2017-2021 Davide Bettio <davide@uninstall.it>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

AtomVM
===========

Brings [Erlang](https://www.erlang.org/), [Elixir](https://elixir-lang.org/) and other functional
languages to really small systems.

AtomVM implements from scratch a minimal Erlang VM that supports a subset of ErlangVM features and that is able to run unmodified BEAM binaries on really small systems like MCUs.

Supported Platforms
===================

* Linux, macOS, FreeBSD, DragonFly ([generic_unix](https://www.atomvm.net/doc/main/getting-started-guide.html#getting-started-on-the-generic-unix-platform))
* ESP32 SoC (with IDF/FreeRTOS, see [esp32](https://www.atomvm.net/doc/main/getting-started-guide.html#getting-started-on-the-esp32-platform))
* STM32 MCUs (with LibOpenCM3, see [stm32](https://www.atomvm.net/doc/main/getting-started-guide.html#getting-started-on-the-stm32-platform))
* Raspberry Pi Pico and Pico 2 (see [rp2](https://www.atomvm.net/doc/main/getting-started-guide.html#getting-started-on-the-raspberry-pi-pico-platform))
* Browsers and NodeJS with WebAssembly (see [emscripten](https://www.atomvm.net/doc/main/getting-started-guide.html#getting-started-with-atomvm-webassembly))

AtomVM aims to be easily portable to new platforms with a minimum effort, so additional platforms
might be supported in a near future.

Getting Started
===============
There is much more information, including a more complete
["Getting Started Guide"](https://www.atomvm.net/doc/main/getting-started-guide.html),
[examples](https://www.atomvm.net/sample-code),
detailed [build instructions](https://www.atomvm.net/doc/main/build-instructions.html),
and [contact information](https://www.atomvm.net/contact) available on the
[AtomVM](https://atomvm.net) project website.

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

Run tests within build directory with:
```
$ ./tests/test-erlang
$ ./tests/test-enif
$ ./tests/test-mailbox
$ ./tests/test-structs
$ ./src/AtomVM ./tests/libs/estdlib/test_estdlib.avm
$ ./src/AtomVM ./tests/libs/eavmlib/test_eavmlib.avm
$ ./src/AtomVM ./tests/libs/alisp/test_alisp.avm
```

Complete [Build Instructions](https://www.atomvm.net/doc/main/build-instructions.html) are
available in the documentation for
[Generic UNIX](https://www.atomvm.net/doc/main/build-instructions.html) (Linux, MacOS, FreeBSD, DragonFly),
[ESP32](https://www.atomvm.net/doc/main/build-instructions.html#building-for-esp32),
[STM32](https://www.atomvm.net/doc/main/build-instructions.html#building-for-stm32),
[Raspberry Pi Pico and Pico 2](https://www.atomvm.net/doc/main/build-instructions.html#building-for-raspberry-pi-pico)
(rp2), and
[WASM](https://www.atomvm.net/doc/main/build-instructions.html#building-for-nodejs-web) (NodeJS/Web).

Project Status
==============

[![Build and Test](https://github.com/atomvm/AtomVM/actions/workflows/build-and-test.yaml/badge.svg?branch=main)](https://github.com/atomvm/AtomVM/actions/workflows/build-and-test.yaml)

AtomVM is still in its early stages, but it can run simple applications similar to those available
in [examples](examples/) and [tests](tests/).

AtomVM might crash with a similar message:
```
Undecoded opcode: 15
Aborted (core dumped)
```
This basically means that an instruction has not been implemented yet, or that an outdated version has been used. Please, make sure to always run AtomVM using latest version.

Known Limitations
-----------------
This project is a work in progress, so there are several known limitations, that will prevent to run unmodified software, some of them are:
* There is a minimal standard library, so several standard functions are missing.
* Several instructions are not yet implemented.

All of these limitations are going to be fixed in a reasonable amount of time.

About This Project
==================
This project has been created by [Davide Bettio](https://github.com/bettio/), and now is developed
from a growing number of contributors.

How to Contribute
-----------------
Any kind of [contribution](CONTRIBUTING.md) is welcome, you can either contribute to this repository
by improving the virtual machine, the core libraries or the documentation or by contributing to any
of the [organization](https://github.com/atomvm) repositories.

License
-------
This project is under the terms of the Apache 2.0 license.
