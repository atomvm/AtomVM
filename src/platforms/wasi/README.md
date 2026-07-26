<!---
  Copyright 2026 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM WASI Platform

This directory contains the AtomVM platform implementation for WASI (WebAssembly System Interface), allowing AtomVM to run as a pure WebAssembly module without JavaScript dependencies.

## Overview

WASI is a modular system interface for WebAssembly that enables portable, sandboxed execution of WebAssembly modules outside of web browsers. This platform targets:

- **WASI Preview 1** (`wasm32-wasip1`): Stable, widely supported, no networking
- **WASI Preview 1 + threads** (`wasm32-wasip1-threads`): SMP support, no networking
- **WASI Preview 2** (`wasm32-wasip2`): BSD-style TCP/UDP networking via `wasi:sockets@0.2.x`, no SMP yet

## Requirements

1. **WASI SDK** version 33 or newer recommended. Download from https://github.com/WebAssembly/wasi-sdk/releases
   ```bash
   # macOS example
   cd /opt
   wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-33/wasi-sdk-33.0-arm64-macos.tar.gz
   tar xzf wasi-sdk-33.0-arm64-macos.tar.gz
   export WASI_SDK_PATH=/opt/wasi-sdk-33.0
   ```

2. **WASI Runtime** (one of):
   - [wasmtime](https://wasmtime.dev) (recommended)
   - [WasmEdge](https://wasmedge.org)
   - [wasmer](https://wasmer.io)

## Building

```bash
cd src/platforms/wasi
mkdir build
cd build

# Configure with WASI toolchain
cmake -DCMAKE_TOOLCHAIN_FILE=../wasi-sdk.cmake ..

# Build
make -j
```

### Build Options

| Option | Description | Default |
|--------|-------------|---------|
| `AVM_DISABLE_SMP` | Disable multi-threading support | OFF |
| `AVM_USE_32BIT_FLOAT` | Use 32-bit floats | OFF |
| `AVM_VERBOSE_ABORT` | Print abort info | OFF |
| `AVM_CREATE_STACKTRACES` | Create stack traces | ON |

### Threading Support

The default target is auto-selected based on `AVM_DISABLE_SMP`:

- `AVM_DISABLE_SMP=OFF` (default) -> `wasm32-wasip1-threads` (SMP, no networking)
- `AVM_DISABLE_SMP=ON` -> `wasm32-wasip2` (networking, no SMP)

Override explicitly:
```bash
cmake -DCMAKE_TOOLCHAIN_FILE=../wasi-sdk.cmake \
      -DWASI_TARGET=wasm32-wasip2 \
      -DAVM_DISABLE_SMP=ON \
      ..
```

Note: SMP and networking are currently mutually exclusive - see [WASI Versions](#wasi-versions) below.

## Running

WASI uses capability-based security. You must explicitly grant directory access,
and (when networking is built in) network access.

### With wasmtime:
```bash
# No networking
wasmtime run --dir=. AtomVM.wasm myapp.avm

# wasip2 build with networking (DNS lookup needed for ssl/getaddrinfo)
wasmtime run --dir=. -S inherit-network -S allow-ip-name-lookup AtomVM.wasm myapp.avm

# wasip1-threads build with SMP (needs shared memory)
wasmtime run -W threads -W shared-memory -S threads --dir=. AtomVM.wasm myapp.avm
```

### With WasmEdge:
```bash
wasmedge --dir .:. AtomVM.wasm myapp.avm
```

### With wasmer:
```bash
wasmer run --dir . AtomVM.wasm -- myapp.avm
```

Note: as of May 2026, only wasmtime runs `wasm32-wasip2` builds with full
`wasi:sockets@0.2.x` networking out of the box. wasmer (tested with 7.1.0)
rejects the component module entirely ("component model feature is not
enabled"); WasmEdge's wasi-sockets plugin targets a Preview-1 socket
extension, not the Preview 2 component imports. Use wasmtime for the
networking-enabled build, or use the `wasm32-wasip1` / `wasm32-wasip1-threads`
target (no networking) on wasmer/WasmEdge.

## WASI Versions

### WASI Preview 1 (wasm32-wasip1)
- **Stable and widely supported**
- POSIX-like system calls via `wasi_snapshot_preview1` imports
- File access through preopened directories
- No networking

### WASI Preview 1 + threads (wasm32-wasip1-threads)
- SMP multi-threading via the wasi-threads proposal (shared memory)
- No networking

### WASI Preview 2 (wasm32-wasip2)
- Component-model based, runs on wasip2-aware runtimes (wasmtime >= 22, WasmEdge,
  wasmer)
- BSD-style synchronous TCP/UDP, `getaddrinfo`, `inet_*` provided by wasi-libc on
  top of `wasi:sockets@0.2.x` WIT interfaces
- No threading support yet (a `wasm32-wasip2-threads` target is being worked on
  upstream but not in any released wasi-sdk)

### Networking vs SMP

These are currently mutually exclusive: `wasm32-wasip1-threads` provides SMP but
no networking, while `wasm32-wasip2` provides networking but no SMP. A combined
target is being worked on upstream and will land here once wasi-sdk ships it.

### Detecting target at runtime

`erlang:system_info(system_architecture)` returns the actual WASI triple:

| Build                    | `system_architecture`         |
|--------------------------|-------------------------------|
| `wasm32-wasip1`          | `<<"wasm32-wasip1">>`           |
| `wasm32-wasip1-threads`  | `<<"wasm32-wasip1-threads">>`   |
| `wasm32-wasip2`          | `<<"wasm32-wasip2">>`           |
