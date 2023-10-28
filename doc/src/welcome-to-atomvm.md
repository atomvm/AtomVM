<!--
 Copyright 2021-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Welcome to AtomVM!

Welcome to AtomVM, the Erlang virtual machine for IoT devices!

## What is AtomVM?

AtomVM is a ground-up implementation of the [Bogdan Erlang Abstract Machine](https://en.wikipedia.org/wiki/BEAM_(Erlang_virtual_machine)) (a.k.a the BEAM) and is designed specifically to run on small systems, such as the [Espressif](https://www.espressif.com) [ESP32](https://www.espressif.com/en/products/socs/esp32) and [ST Microelectronics](https://www.st.com/content/st_com/en.html) [STM32](https://www.st.com/en/microcontrollers-microprocessors/stm32-32-bit-arm-cortex-mcus.html) micro-controllers.  It allows developers to implement IoT applications in the Erlang or Elixir programming languages and to deploy those applications onto tiny devices.  (Users may also target their applications for fully-fledged operating systems, such as Linux, FreeBSD, and MacOS, though in most cases deployment to traditional computers is done for development and testing purposes, only.)

AtomVM features include:

* An Erlang runtime, capable of executing bytecode instructions in compiled BEAM files;
* Support for all the major Erlang and Elixir types, including integers, strings, lists, maps, binaries, Enums, and more;
* A memory-managed environment, with efficient garbage collection and shared data, where permissible;
* Support for truly functional programming languages, making your programs easier to understand and debug;
* A concurrency-oriented platform, allowing users to spawn, monitor, and communicate with lightweight processes, making it easy for your IoT devices to perform tasks simultaneously;
* Support for symmetric multi-processing (SMP); leverage all available cores on platforms that support it (e.g., ESP32) without any code changes;
* A rich set of networking APIs, for writing robust IoT applications that communicate over IP networks;
* A rich set of APIs for interfacing with standard device protocols, such as GPIO, I2C, SPI, and UART;
* And more!

## Why Erlang/Elixir?

The environments on which AtomVM applications are deployed are significantly more constrained than typical programming environments.  For example, the typical ESP32 ships with 520K of RAM and 4MB of flash storage, roughly the specs of a mid 1980s desktop computer.  Moreover, most micro-controller environments do not support native POSIX APIs for interfacing with an operating system, and in many cases, common operating system abstractions, such as processes, threads, or files, are simply unavailable.

However, because the BEAM is provides a pre-emptive multitasking environment for your applications, many of the common operating system abstractions, particularly involving threading and concurrency, are simply not needed.  As concurrently-oriented languages, Erlang and Elixir support lightweight "processes", with message passing as the mechanism for inter-(erlang)process communication, pre-emptive multi-tasking, and per-process heap allocation and garbage collection.

In many ways, the programming model for Erlang and Elixir is closer to that of an operating system and multiple concurrent processes running on it, where operating system processes are single execution units, communicate through message passing (signals), and don't share any state with one another.  Contrast that with most popular programming languages today (C, C++, Java, Python, etc), which use threading abstractions to achieve concurrency within a single memory space, and which subsequently require close attention to cases in which multiple CPUs operate on a shared region of memory, requiring threads, locks, semaphores, and so forth.

As an implementation of the BEAM, AtomVM provides a modern, memory managed, and concurrency-oriented environment for developing applications on small devices.  This makes writing concurrent code for micro-controllers (e.g., and application that reads sensor data, services HTTP requests, and updates the system clock, all at the same time) incredibly simple and natural -- far easier writing programs that use concurrency than C, C++, or even, for example, Micropython.

In addition, because it is targeted for micro-controller environments, AtomVM provides interfaces for integrating with features commonly seen on micro-controllers, such as GPIO pins, analog-to-digital conversion, and common industry peripheral interfaces, such as I2C, SPI, and UART, making AtomVM a rich platform for developing IoT applications.

Finally, one of the exciting aspects about modern micro-controllers, such as the ESP32, is their integration with modern networking technologies, such as WiFi and Bluetooth.  AtomVM leverages Erlang and Elixir's natural affinity with telecommunications technologies to open up further possibilities for developing networked and wireless IoT devices.

We think you will agree that AtomVM provides a compelling environment not only for Erlang and Elixir development, but also as a home for interesting and fun IoT projects.

## Design Philosophy

AtomVM is designed from the start to run on small, cheap embedded devices, where system resources (memory, cpu, storage) are tightly constrained.  The smallest environment in which AtomVM runs has around 512k of addressable RAM, some of which is used by the underlying runtime (FreeRTOS), and some of which is used by the AtomVM virtual machine, itself, leaving even less RAM for your own applications.  Where there is a tradeoff between memory consumption and performance, minimizing memory consumption (and heap fragmentation) always wins.

From the developer's point of view, AtomVM is designed to make use of the existing tool chain from the Erlang and Elixir ecosystems.  This includes the Erlang and Elixir compilers, which will compile Erlang and Elixir source code to BEAM bytecode.  Where possible, AtomVM makes use of existing tool chains to reduce the amount of unnecessary features in AtomVM, thus reducing complexity, as well as the amount of system resources in use by the runtime.  AtomVM is designed to be as small and lean as possible, providing as many resources to user applications, as possible.

## Licensing

AtomVM is licensed under the terms of the [Apache2](https://www.apache.org/licenses/LICENSE-2.0) and [LGPLv2](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html) licenses.

## Source Code

The [AtomVM Github Repository](https://github.com/atomvm/AtomVM) contains the AtomVM source code, including the AtomVM virtual machine and core libraries.  The AtomVM [Build Instructions](./build-instructions.md) contains instructions for building AtomVM for Generic UNIX, ESP32, and STM32 platforms.

## Contributing

The AtomVM community welcomes contributions to the AtomVM code base and upstream and downstream projects.  Please see the [contributing guidelines](./CONTRIBUTING.md) for information about how to contribute.

AtomVM developers can be reached on the #AtomVM discord server or on Telegram at [AtomVM - Erlang and Elixir on Microcontrollers](https://t.me/atomvm).

## Where to go from here

The following guides provide more detailed information about getting started with the AtomVM virtual machine, how to develop and deploy applications, and implementation information, for anyone interested in getting more involved:

* [Getting Started Guide](./getting-started-guide.md)
* [Programmers Guide](./programmers-guide.md)
* [Example Programs](https://github.com/atomvm/atomvm_examples)
* [Build Instructions](./build-instructions.md)
