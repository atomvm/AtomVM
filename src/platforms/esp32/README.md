<!---
  Copyright 2020 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

This document provides internal implementation details about the build system for AtomVM on the ESP32.  You should read this document if you are doing maintenance or adding features (such as support for peripherals or protocols) to the ESP32 port of AtomVM.

The ESP32 build currently makes use of the IDF SDK [CMake-based Build System](https://docs.espressif.com/projects/esp-idf/en/release-v4.4/esp32/api-guides/build-system.html) to build the IDF SDK libraries (i.e., components) and AtomVM virtual machine.

As such, the AtomVM build includes a set of main entrypoints for the ESP application (defined in the `main` directory), as well as system-specific code that is used by the application.

The platform-independent core AtomVM VM library is built as an IDF SDK component, and make targets can be found under the `component.mk` file in the `components/libatomvm` directory.  This make file directs the make system to build the AtomVM library using CMake, first to generate the make files, and then to run make against those generated make files.

## ESP32 integrations

The ESP32 port of AtomVM contains numerous integrations with IDF SDK peripherals and protocols, including the following ESP32 features:

* GPIO interface
* UART controller
* I2C 2-wire interface
* SPI interface
* LEDC control
* Non-volatile storage
* Wifi networking
* UDP/TCP/IP

> Note.  Additional periperhal interfaces and protocols are planned for the future.

Some of the above integrations are implemented as AtomVM Nifs (stateless functions implemented in C), while others are implemented as AtomVM ports, which are essentially stateful Erlang processes implemented in C.

Many applications do not have a need for the AtomVM image to support integration with all peripherals and protocols, and in fact, each integration does add linked code into the image, increasing its size.  For example, an application may have no need for the UART interface, or an application that only uses I2C may have no need for SPI integration, and a smaller image size can result if these integrations are excluded.  Smaller images improve flash time (reducing the time spent in the compile/flash/depbug process) and in some cases can result in decreased RAM usage at runtime.

Therefore NIFs and ports that are not needed can be disabled using `menu config`, from "AtomVM
Built-In Components" menu.
