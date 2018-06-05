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

This software has been mainly tested with GCC (7.2.0 on Linux), but it also works with clang (tested with clang 4.0.1).

Supported Platforms
===================

* Linux
* ESP32 SoC (with IDF and FreeRTOS software platforms, see [README.ESP32.Md](README.ESP32.Md))

Getting Started (on Linux)
==========================


```
$ cmake .

$ make

$ ./src/AtomVM hello_world.beam
```

Project Status
==============

AtomVM is far from being complete, but it can run simple applications similar to those available in examples and tests.
Known limitations: there is no standard library right now, so every application needs to have some copy&paste boilerplate.
