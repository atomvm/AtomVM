<!--
 Copyright 2020-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Packbeam Format

AtomVM makes use of the packbeam format for aggregating beam and other file types into a single file that is used as the code base for an AtomVM application.  Typically, on an embedded device, packbeam files are uploaded (e.g., via serial connection) to a specific location on flash media.  The AtomVM runtime will locate the entrypoint into the application, and use the beam and other files flashed to the local media to run the uploaded application.

AtomVM provides a simple tool for generating packbeam files, but other tools have emerged for manipulation packbeam files using standard Erlang and Elixir tool chains, notably `Mix` and `rebar3`.

This document describes the packbeam format, so that both AtomVM and upstream/downstream tooling have a reference document on which to base implementations.

## Overview

Packbeam files are binary-encoded aggregations of BEAM and plain data files.  At a high level, a packbeam file consists of a packbeam header, followed by a sequence of files (beam or otherwise), each of which is prefixed with a header, including data about the file (name, size, flags, etc).

All binary integer values are 32-bit, in network order (big-endian).  Headers and encoded files are padded when necessary and aligned on 4-byte boundaries.

At present, the AtomVM runtime treats data in packbeam files as _read-only_ data.  There is no support for modifying the contents on an AtomVM file by the runtime.

## Packbeam Header

All AtomVM files begin with the packbeam header, a fixed 24-byte sequence of octets:

    0x23, 0x21, 0x2f, 0x75,
    0x73, 0x72, 0x2f, 0x62,
    0x69, 0x6e, 0x2f, 0x65,
    0x6e, 0x76, 0x20, 0x41,
    0x74, 0x6f, 0x6d, 0x56,
    0x4d, 0x0a, 0x00, 0x00

The ASCII encoding of this sequence is

>`#!/usr/bin/env AtomVM\n`

followed by two nil (`0x00`) bytes.

The packbeam header is followed by a sequence of 0 or more encoded files.  The number of files in a packbeam file is not indicated in the packbeam header; however, packbeam files do contain a special `end` file header, marking the end of the sequence of encoded files.

## File encodings

Each embedded file in a packbeam file contains a file header, followed by the file contents.

### File Header

The file header consists of the following 4 fields:

* `size` (32 bit, big-endian)
* `flags` (32-bit, big endian)
* `reserved` (32-bit, big-endian, currently unused)
* `module_name` (null-terminated sequence of bytes)

The `size` field indicates the size (in bytes) of the encoded file following the header.  This size includes the file content length, in addition to any padding that may have been added to the file, in order for it to align on a 4-byte boundary.

Currently, the two low-order bits of the `flags` field are used.  `0x02` indicates the file is a BEAM file, and `0x01` indicates that the file contains a `start/0` function, and is therefore suitable as an entrypoint to start code execution.

When AtomVM starts, it will scan the BEAM files in the AtomVM file, from start to finish, with which it is initialized to find the entrypoint to start code execution.  It will start execution on the first BEAM file with a `start/0` function, i.e., whose flags mask against `0x03`.  It is conventional, but not required, for the first file in an AtomVM file to be a BEAM file that has a `start/0` entrypoint.

The `reserved` field is currently unused.

The `module_name` is variable length, null terminated sequence of characters.  Because the module name is variable-length, the header may be padded with null characters (`0x00`), in order to align the start of the file contents on a 4-byte boundary.

### Example

The following BEAM header indicates a BEAM file with a length of 308 bytes (`0x00000134`), with a `start/0` entrypoint (`0x00000003`), and named `mylib.beam` (`0x6D796C69 622E6265 616D00`).  The header has a 1-byte padding of null (`0x00`) characters.

    00000134 00000003 00000000 6D796C69 622E6265 616D0000

### BEAM files

BEAM files obey [IFF](https://en.wikipedia.org/wiki/Interchange_File_Format) encoding as detailed [here](http://www.erlang.se/~bjorn/beam_file_format.html), but certain information in BEAM files is stripped out in order to minimize the amount of data stored on flash.

The following BEAM chunks are included in BEAM files:

* `AtU8`
* `Code`
* `ExpT`
* `LocT`
* `ImpT`
* `LitU`
* `FunT`
* `StrT`
* `LitT`

Any other chunks are stripped out of the BEAM files before insertion into AVM files.

In addition, data in the literals table (`LitT`) are uncompressed before insertion into AVM files, as the AtomVM runtime does not include support for `zlib` decompression.

BEAM files may be padded at the end with a sequence of 1-3 null (`0x00`) characters, in order to align on 4-byte boundaries.

```{note}
The `module_name` field in the file header will only contain the "base" name of the BEAM file, i.e., the file name
stripped of any path information.
```

### Normal Files

Normal files (e.g., text files, data files, etc.) can be stored in packbeam AVM files, as well as BEAM files.  For example, a normal file might contain static configuration information, or data that is interpreted at runtime.

Normal files contain a 32-bit big-endian size prefix, indicating the size of the file data (without padding).  Note that the `size` field in the file header includes the size of the data with padding, if applicable.

The AtomVM runtime provides access to data files via the [`atomvm:read_priv/2`](./apidocs/erlang/eavmlib/atomvm.md#read-priv2) NIF.  This function will create a path name formed by the `App` (atom) and `Path` (string) terms provided by this function, separated by `"/priv/"`.  For example, the expression

```erlang
atomvm:read_priv(mylib, "sample.txt")
```

yields a `binary` containing the contents of `mylib/priv/sample.txt`, if it exists, in the AtomVM packbeam file.

As a consequence, normal files should be included in packbeam files using module names that obey the above patterns.

```{tip}
Normal file names may encode virtual directory names, such as `mylib/priv/another/sample/text/file`.  There is no
requirement that the `Path` component of a normal file be a simple file name.
```

### `end` file

Packbeam files end with a special `end` header.  The `size` field of the `end` header is 0 bytes.

#### Example `end` header

The following sequence of bytes encodes the `end` header:

    00000000 00000000 00000000 656E6400
