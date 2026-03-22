<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# JIT Compilation

AtomVM includes an optional JIT (Just-In-Time) compiler that translates BEAM bytecodes into native machine code for significantly faster execution.

## Overview

The JIT compiler works at module load time: when a module is loaded, its BEAM bytecodes are compiled to native code for the target architecture. The compiled native code is stored alongside the BEAM bytecodes and used for execution.

Modules can also be ahead-of-time compiled: the native code is generated at build time and embedded into the `.beam` file as a `avmN` chunk. At load time, the precompiled native code is used directly without any compilation step.

### Supported architectures

The JIT compiler supports the following target architectures:

* `x86_64` — 64-bit x86 (Linux, macOS, FreeBSD)
* `aarch64` — 64-bit ARM (Linux, macOS)
* `armv6m` — ARM Cortex-M0+ (Raspberry Pi Pico, STM32)
* `riscv32` — 32-bit RISC-V
* `riscv64` — 64-bit RISC-V

### Requirements

* Erlang/OTP 28 or later is required to run the JIT compiler at load time (ahead-of-time compiled modules can be executed with OTP 26+).

## Building with JIT support

### Generic UNIX

To enable JIT compilation, pass `-DAVM_DISABLE_JIT=OFF` to CMake:

```shell
$ mkdir build
$ cd build
$ cmake -DAVM_DISABLE_JIT=OFF ..
$ make -j 8
```

The target architecture is auto-detected based on the host platform. To cross-compile or override, use `-DAVM_JIT_TARGET_ARCH=<arch>`.

### DWARF debug information

DWARF debug information support can be enabled to allow debugging JIT-compiled code with LLDB or GDB. It is disabled by default and can be enabled with:

```shell
$ cmake -DAVM_DISABLE_JIT=OFF -DAVM_DISABLE_JIT_DWARF=OFF ..
```

When enabled, each precompiled module includes an ELF object with DWARF debug sections containing:

* Function symbols (`module:function/arity`)
* BEAM opcode location symbols
* Label symbols
* Source file and line number mappings
* Context structure type information for inspecting VM registers

## DWARF debug support

### Debugging with LLDB

When DWARF support is enabled and `plugin.jit-loader.gdb.enable` is turned on, LLDB can set breakpoints on JIT-compiled Erlang functions by name, inspect VM registers, and show backtraces through JIT code.

#### Setting breakpoints

```
$ lldb -- tests/test-erlang add
(lldb) settings set plugin.jit-loader.gdb.enable on
(lldb) breakpoint set -n 'add:add/2'
(lldb) run
```

When the breakpoint is hit, LLDB shows the function name and the Context pointer:

```
Process stopped
* thread #1, stop reason = breakpoint 1.1
    frame #0: 0x00000001002440fa JIT(0x...)`add:add/2(ctx=0x00007f...)
```

#### Inspecting VM registers

The DWARF debug information includes location tracking for Erlang x registers. Use `frame variable` to see the VM state:

```
(lldb) frame variable
(Context *) ctx = 0x00007fc066804280
(unsigned long) x[0] = 143
```

The x register values are displayed as raw tagged terms. For small integers, the value is `(term >> 4)`, so `x[0] = 143` means the integer `8` (since `143 = 8 << 4 | 0xf`).

When the JIT compiler has cached an x register in a native CPU register, the debugger reads it directly from the CPU register instead of memory — this is tracked automatically through DWARF location lists.

#### Backtraces

```
(lldb) bt
* frame #0: add:add/2(ctx=0x...)
  frame #1: scheduler_entry_point at opcodesswitch.h
  frame #2: context_execute_loop at opcodesswitch.h
  frame #3: main at test.c
```

#### Source line mapping

If the Erlang source was compiled with debug information and the BEAM Line chunk is present, the debugger maps JIT code addresses to source file and line numbers.

```{note}
LLDB 19 (including Apple's system LLDB shipped with Xcode) has a regression in the JIT loader
that causes hangs when resolving breakpoints in JIT-loaded modules. Use LLDB 20 or later.
On macOS, install it from [MacPorts](https://www.macports.org/) (`port install lldb-20`) or
build from the [LLVM project source](https://github.com/llvm/llvm-project).
```

### Disassembling precompiled modules

Precompiled `.beam` files contain an ELF object with the native code and symbol table. You can extract and disassemble it to inspect the generated code.

When DWARF is enabled, the ELF is embedded in the `avmN` chunk of the `.beam` file. At runtime, `jit_dwarf:elf/2` produces the ELF binary that can be written to a file for offline analysis:

```erlang
{ok, _TextOffset, ElfBinary} = jit_dwarf:elf(DwarfState, NativeCode),
file:write_file("module.elf", ElfBinary).
```

The resulting ELF file can be disassembled with standard tools:

```shell
$ objdump -d module.elf
```

This will show the disassembly with function names like `module:function/arity`, making it easy to correlate the generated machine code with the original Erlang source.

### Extracting ELF from a precompiled .beam file

The ELF object is stored in the `avmN` chunk of the `.beam` file, after a small header. You can extract it from the Erlang shell using `beam_lib`:

```erlang
{ok, {_, [{_, ChunkData}]}} = beam_lib:chunks("module.beam", ["avmN"]),
<<InfoSize:32/big, _Info:InfoSize/binary, ElfData/binary>> = ChunkData,
<<16#7f, "ELF", _/binary>> = ElfData,  % verify ELF magic
file:write_file("module.elf", ElfData).
```

Then disassemble with symbols:

```shell
# x86_64
$ objdump -d module.elf

# aarch64
$ aarch64-elf-objdump -d module.elf

# armv6m
$ arm-elf-objdump -d --disassembler-options=force-thumb module.elf

# riscv32
$ riscv32-elf-objdump -d module.elf

# riscv64
$ riscv64-elf-objdump -d module.elf
```

## CMake options reference

| Option | Default | Description |
|--------|---------|-------------|
| `AVM_DISABLE_JIT` | `ON` | Disable JIT compilation |
| `AVM_DISABLE_JIT_DWARF` | `ON` | Disable DWARF debug information in JIT |
| `AVM_JIT_TARGET_ARCH` | auto-detected | Target architecture (`x86_64`, `aarch64`, `armv6m`, `riscv32`, `riscv64`) |
| `AVM_DISABLE_SMP` | `OFF` | Disable SMP support |
