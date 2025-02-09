<!---
  Copyright 2025 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# hello_atomvm

This is a basic hello program created with `gleam new hello_atomvm`.

AtomVM currently requires a `start/0` function which has been added to
`hello_atomvm.gleam` and simply calls `main()` function.

The project can be run using Gleam's default runtime:
```sh
gleam run   # Run the project
```

It can also be run using AtomVM on generic unix with:
```sh
gleam export erlang-shipment
../../../build/src/AtomVM build/erlang-shipment/{hello_atomvm,gleam_stdlib}/ebin/*.beam ../../../build/libs/atomvmlib.avm
```

Alternatively, an AVM file can be generated and executed with:
```sh
gleam export erlang-shipment
../../../build/tools/packbeam/PackBEAM hello_atomvm.avm build/erlang-shipment/hello_atomvm/ebin/hello_atomvm*.beam build/erlang-shipment/gleam_stdlib/ebin/*.beam
../../../build/src/AtomVM hello_atomvm.avm ../../../build/libs/atomvmlib.avm
```

