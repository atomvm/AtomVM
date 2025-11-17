<!---
  Copyright 2025 Mikael Karlsson <mikael.karlsson@creado.se>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# supervise_actor

This is a basic supervised actors program created with
`gleam new supervise_actor`.

AtomVM currently requires a `start/0` function which has been added to
`supervise_actor.gleam` and simply calls `main()` function.

The project can be run using Gleam's default runtime:
```sh
gleam run   # Run the project
```

It can also be run using AtomVM on generic unix with:
```sh
gleam export erlang-shipment
../../../build/src/AtomVM build/erlang-shipment/{supervise_actor,gleam_stdlib,gleam_erlang,gleam_otp}/ebin/*.beam ../../../build/libs/atomvmlib.avm
```

Alternatively, an AVM file can be generated and executed with:
```sh
gleam export erlang-shipment
../../../build/tools/packbeam/PackBEAM supervise_actor.avm build/erlang-shipment/supervise_actor/ebin/*.beam build/erlang-shipment/{gleam_stdlib,gleam_erlang,gleam_otp}/ebin/*.beam
../../../build/src/AtomVM supervise_actor.avm ../../../build/libs/atomvmlib.avm
```
