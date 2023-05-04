<!--
 Copyright 2018-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM {#mainpage}

The `AtomVM` project provides a ground-up implementation of the "Bogdan Erlang Abstract Machine" (BEAM), the underlying runtime for the Erlang and Elixer (among other) BEAM-targeted programming environments.  `AtomVM` is intended to execute Erlang or Elixer programs that have ben compiled down to the BEAM instruction set.

While `AtomVM` can be compiled for and run on destop (UNIX-like) systems, the intended target for AtomVM are tiny microcontrollers, such as the ESP32 and STM32.

Currently, AtomVM implements a strict subset of the BEAM instruction set.

The currently unsupported Erlang/OTP and BEAM features includes (but is not limited to):

* Anonymous Functions
* Maps
* Nummerous BIFs and NIFs supported by the R20 BEAM
* Numerous ports and drivers supported by the R20 BEAM
* The Erlang/OTP standard libraries (kernel, stdlib, sasl, etc)
* `epmd` and the `disterl` protocol

As such, it is highly unlikely that an existing Erlang program targeted for Erlang/OTP will run unmodified on AtomVM.  And indeed, even as AtomVM matures and additional features are added, it is more likley than not that Erlang applications will need to be ported or completely re-written to run on AtomVM.  The intended target environment (small, cheap micro-controllers) differs enough from desktop or server-class systems that special care and attention is needed to target applications for such embedded environments.

That being said, many of the features of the BEAM are supported and provide a rich and compelling development environment for embedded devices, which Erlang and Elixer developers will find natural and productive.


## Design Philosophy

AtomVM is designed to make use of the existing toolchain from the Erlang and Elixer ecosystems.  This includes the Erlang and Elixer compilers, which will compile Erlang and Eliver source code to BEAM bytecode, but as AtomVM matures, will also include using Erlang and Elixer tools for managing software that runs on devices, such as (in the future) the `disterl` protocol.  Where possible, AtomVM makes use of existing tool chains to reduce the amount of unnecessary features in AtomVM, thus reducing complexity, as well as the amount of system resources in use by the runtime.  AtiomVM is designed to be as small and lean as possible, providing as many resources to user applications, as possible.

AtomVM is designed from the start to run on small, cheap embedded devices, where system resources (memory, cpu, storage) are tightly constrained.  The smallest environment in which AtomVM runs has around 256k of addressable RAM, some of which is used by the underlying runtime (FreeRTOS), and some of which is used by the AtomVM system, itself, leaving even less RAM for your own applications.

TODO details about memory model

## Licensing

AtomVM is licensed under the terms of Apache 2.0 license.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

AtomVM project has begun under the terms of LGPL 2.1 or later license, so it
can be used under the terms of LGPL 2.1 or later (dual license) as an option.

## Building AtomVM

AtomVM can be built via the command line and can target numerous environments, including Linux, FreeBSD, MacOS, ESP32, and STM32.

In the case of embedded devices (ESP32 and STM32), toolchains from open source and third-party vendors are required in order to cross-compile AtomVM source code (most of which is written in the C programming language)

### Downloading AtomVM

At present, AtomVM is only available via a checkout of the AtomVM github repository.

> Note.  Downloading the AtomVM github repository requires the installation of the `git` program.  Consult your local OS documentation for installation of the `git` package.

	shell$ git clone https://github.com/atomvm/AtomVM
	TODO
	shell$ cd AtomVM


### Generic Unix

The following instructions apply to unix-like environments, including Linux, FreeBSD, and MacOS.

#### Pre-requisites

The following software is required in order to build AtomVM in generic UNIX systems:

* `gcc` or `llvm` tool chains
* `cmake`
* `make`
* `gperf`
* `zlib`

Consult your local OS documentation for instructions about how to install these components.

#### Build Instructions

The AtomVM build for generic UNIX systems makes use of the `cmake` tool for generating `make` files from the top level AtomVM directory, as follows:

	shell$ mkdir build
	shell$ cd build
	shell$ cmake ..
	...
	shell$ make

> Note.  You may optionally specify `-j <n>`, where `<n>` is the number of CPUs you would like to assign to run the build in parallel.

Upon completion, the `AtomVM` execcutable can be found in the `build/src` directory.

#### Special Note for MacOS users

You may build an Apple Xcode project, for developing, testing, and debugging in the Xcode IDE, by specifying the Xcode generator.  For example, from the top level AtomVM directory:

	shell$ mkdir xcode
	shell$ cmake -G Xcode ..
	...
	shell$ open AtomVM.xcodeproj

The above commands will build and open an AtomVM project in the Xcode IDE.

### ESP32




#### Pre-requisites

The following software is required in order to build AtomVM:

* Espressif Xtensa tool chains
* Espressif IDF toolkit
* `cmake`
* GNU `make`

#### Build Instructions




## Targeting AtomVM



## AtomVM Components

### Bytecode Interpreter

### Scheduler

### Memory Management

#### Garbage Collection

### Code Loading

### BIFs

### NIFs

### Ports

### PackBEAM

## AtomVM Platforms

### Generic Unix

### ESP32

### STM32

### Targeting AtomVM for a new platform

## AtomVM Libraries

## Resources
