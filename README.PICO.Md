<!--
 Copyright 2022 Paul Guyot <pguyot@kallisys.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

Building AtomVM for Raspberry Pico
==================================

* build:

```
cd src/platforms/rp2040/
mkdir build
cd build
cmake .. -G Ninja
ninja
```

> Note: requires cmake and ninja

You may want to build with option `AVM_REBOOT_ON_NOT_OK` so Pico restarts on
error.


Building AtomVM for Raspberry Pico-W
==================================

To build for Pico-W, pass the board to cmake as follows:

```
cd src/platforms/rp2040/
mkdir build
cd build
cmake .. -G Ninja -DPICO_BOARD=pico_w
ninja
```

Installing AtomVM and programs on Raspberry Pico
================================================

The approach consists in installing various uf2 files which include the
address they should be loaded to.

You typically need three uf2 files:
- `AtomVM.uf2` for the VM
- `atomvmlib.uf2` for the standard libraries
- your application's uf2.

We provide an escript-based (what else?) tool to build uf2 files called
`uf2tool` that you can use to bundle your `avm` into uf2.

If you need to upgrade AtomVM or the standard libraries, simply copy them again.

Installing AtomVM on Raspberry Pico
===================================

VM binary is file `src/platforms/rp2040/build/src/AtomVM.uf2`. Simply copy it
to the pico. The VM will crash because there is no application.

Installing atomvm library to Raspberry Pico
===========================================

AtomVM library must be installed as well.

Building it
-----------

Build of standard libraries is part of the generic unix build.

From the root of the project:

```
mkdir build
cd build
cmake .. -G Ninja
ninja
```

> Note: requires cmake, ninja, Erlang/OTP and Elixir for elixir libs

Installing it
-------------

The library to install is `build/libs/atomvmlib.uf2`

Running Hello Pico
==================

This example will print a Hello Pico message repeatedly.

It is built into `build/examples/erlang/rp2040/hello_pico.uf2`.

You can install it and then connect to the serial port with minicom.

Running your own BEAM code on Raspberry Pico
============================================

You need to create an avm file using PackBEAM binary (or rebar3 plugin).

```
./PackBEAM packed.avm module.beam
```

Then the BEAM file must be converted to UF2.
The VM currently expects the application to be loaded at address 0x10100000.

```sh
./uf2tool create -o packed.uf2 -s 0x10100000 packed.avm
```

Copy this UF2 to the Pico after you copied the VM (`AtomVM.uf2`) and the
standard libraries (`atomvmlib.uf2`).
