:アトムＶＭ
===========

Brings meme languages ＡＥＳＴＨＥＴＩＣＳ to really small systems.

AtomVM implements from scratch a minimal Erlang VM that supports a subset of ErlangVM features and that is able to run unmodified BEAM binaries on really small systems like MCUs.

Dependencies
============

* CMake ([CMake build system](https://cmake.org/)) is required to build AtomVM.
* gperf ([GNU Perfect Hash Function Generator](https://www.gnu.org/software/gperf/manual/gperf.html)) is required to build AtomVM.
* zlib ([zlib compression and decompression library](https://zlib.net/)) is optionally needed to run standard BEAM files (without uncompressed literals extension).

* gcov and lcov are optionally required to generate coverage report (make coverage).

Supported Platforms
===================

* Linux
* Darwin (MacOS)
* FreeBSD
* ESP32 SoC (with IDF and FreeRTOS software platforms, see [README.ESP32.Md](README.ESP32.Md))
* STM32 MCUs (with LibOpenCM3, see [README.STM32.Md](README.STM32.Md))

AtomVM aims to be easily portable to a new platform with a minimum effort, so more platforms might be supported in a near future.

Getting Started (on Linux)
==========================


```
$ cmake .

$ make

$ ./src/AtomVM hello_world.beam
```

Project Status
==============

[![Build Status](https://travis-ci.com/bettio/AtomVM.svg?branch=master)](https://travis-ci.com/bettio/AtomVM)

Right now only binaries compiled with OTP 20 are supported, support for newer versions will be added soon.

AtomVM is still in its early stages, but it can run simple applications similar to those available in examples and tests.
AtomVM might crash with a similar message:
```
Undecoded opcode: 15
Aborted (core dumped)
```
This basically means that an instruction has not been implemented yet, or that an outdated version has been used. Please, make sure to always run AtomVM using latest version.

Known limitations
-----------------
This project is a work in progress, so there are several known limitations, that will prevent to run unmodified software, some of them are:
* There is a minimal standard library, so several standard functions are missing.
* Several instructions are not yet implemented.
* Maps are not yet supported.
* Binaries are still poorly supported.
* Funs are not yet supported.

All of these limitations are going to be fixed in a reasonable amount of time.
