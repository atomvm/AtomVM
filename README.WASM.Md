<!--
 Copyright 2023 Paul Guyot <pguyot@kallisys.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

Building AtomVM for wasm (emscripten)
=====================================

* requirements:

Install emscripten and cmake

* build:

```
cd src/platforms/emscripten/
mkdir build
cd build
emcmake cmake ..
emmake make -j
```

Running AtomVM with nodejs
==========================

By default, AtomVM is compiled for node.

It can be passed avm and beam files on the command line.

Provided that you built examples, you can run:

```
cd src/platforms/emscripten/build
node src/AtomVM.js ../../../../build/examples/erlang/hello_world.avm
```

You can also load beam files with:

```
cd src/platforms/emscripten/build
node src/AtomVM.js ../../../../build/examples/erlang/hello_world.beam ../../../../build/libs/atomvmlib.avm
```

Running AtomVM in the browser
=============================

AtomVM can also be run in the browser. For this, you need to build it with
```
AVM_EMSCRIPTEN_ENV=web
```

as follows:
```
cd src/platforms/emscripten/
mkdir build
cd build
emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web
emmake make -j
```

Please note that running emscripten code in the browser requires a server that
sends the proper headers, especially considering that AtomVM uses SharedArrayBuffer,
a feature that browsers restrict for security reasons.

Fortunately, AtomVM comes with a simple HTTP server that provides these headers.

Provided that you built AtomVM for Unix, you can run:

```
cd build
./src/AtomVM examples/emscripten/wasm_webserver.avm
```

Then you can visit http://localhost:8080/ with your browser, it will list
various examples. Make sure to check the HTML and Erlang source code of these.
