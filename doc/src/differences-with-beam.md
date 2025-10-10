<!--
 Copyright 2025 Paul Guyot <pguyot@kallisys.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Differences between AtomVM and BEAM

BEAM users may be surprised by limitations or differences between AtomVM and BEAM, whether they
write Erlang, Elixir or Gleam code.

This document describes the current well known differences.

## Principles

AtomVM is designed to be compatible with the BEAM as long as it makes sense on microcontrollers or
environments such as Web Assembly.

One should distinguish features of the VM itself from features of the standard library. AtomVM
team will never reimplement the full OTP standard library, as this full library is too large for
small embedded devices. Still, most modules of the OTP standard library can be executed by AtomVM.
Typically, it is possible to run an Elixir or an
[Erlang REPL](https://github.com/pguyot/atomvm_shell) with the appropriate OTP and Elixir standard
library modules.

AtomVM tests (of the VM as well as the standard library) are typically executed with BEAM to
ensure compatibility beyond BEAM documented features. The Continuous Integration environment runs
these tests with a wide range of BEAM versions.

## Determining if code is run by AtomVM or BEAM

One can use `erlang:system_info(machine)` to find out if the code is executed by AtomVM or BEAM.

BEAM returns `"BEAM"` while AtomVM returns `"ATOM"`.

## Known limitations of the VM

AtomVM does not implement some key features of the BEAM. Some of these limitations are being
worked on and this list might be outdated. Do not hesitate to check GitHub issues or contact us
when in doubt.

### Integer precision and overflow

AtomVM supports integers up to 256-bit with an additional sign flag, while BEAM supports unlimited
precision integers. This fundamental difference has several implications:

#### Integer limits

- **Maximum value**: `16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF` (256
ones, which equals `2^256 - 1`)
- **Minimum value**: `-16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF` (which
equals `-(2^256 - 1)`)

Note that AtomVM does not use two's complement for big integers. The sign is stored as a separate
flag, which means `INTEGER_MAX = -INTEGER_MIN`.

#### Overflow errors

Unlike BEAM, AtomVM raises `overflow` errors when integer operations exceed 256-bit capacity:

```erlang
IntMax = 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
% The following will raise an overflow error on AtomVM, but succeeds on BEAM:
Result = IntMax + 1  % overflow error

% Also applies to subtraction and multiplication:
-IntMax - 1  % overflow error
IntMax * 2   % overflow error
```

Handling overflows:

```erlang
safe_calc(MaybeOvfFun) ->
    try MaybeOvfFun() of
        I when is_integer(I) -> {ok, I}
    catch
        error:overflow -> {error, overflow}
    end.

% Returns `{ok, Result}`, Result is a 255 bit integer
safe_calc(fun() -> factorial(57) end).

% Returns `{error, overflow}`, since 261 bit integers are not allowed
safe_calc(fun() -> factorial(58) end).
```

Overflow can also occur with:
- Bit shift left operations: `1 bsl 257` raises overflow (shifting beyond the 256-bit boundary).
When shifting values with multiple set bits, mask first to prevent overflow: `16#FFFF bsl 252`
would overflow, but `(16#FFFF band 0xF) bsl 252` succeeds
- Float to integer conversions: `ceil/1`, `round/1`, etc. when the result exceeds 256-bit

Note: While BEAM raises `system_limit` error for operations like
`1 bsl 2000000000000000000000000000000000`, AtomVM consistently uses `overflow` error for all
integer capacity violations.

Note: Integer literals larger than 256 bits in source code will compile successfully with
Erlang/Elixir compilers, but the resulting BEAM files will fail to load on AtomVM. This also
applies to compile-time constant expressions that evaluate to integers exceeding 256 bits, such as
`1 bsl 300`. These expressions are evaluated by the compiler and stored as constants in the BEAM
file, causing the same load-time failure. Always ensure that integer constants in your code are
within AtomVM's supported range.

Note: The `erlang:binary_to_term/1,2` function raises a `badarg` error when attempting to
deserialize binary data containing an integer larger than 256 bits. This differs from BEAM, which
can deserialize integers of any size. Applications that exchange serialized terms with BEAM nodes
should be aware of this limitation.

Note: String and binary conversion functions such as `erlang:binary_to_integer/1,2`,
`erlang:list_to_integer/1,2`, and Elixir's `String.to_integer/1,2` raise a `badarg` error when the
input represents an integer exceeding 256 bits. For example,
`erlang:binary_to_integer(<<"10000000000000000000000000000000000000000000000000000000000000000">>, 16)`
will fail with `badarg` on AtomVM, while it succeeds on BEAM. Applications parsing user input or
external data should validate that numeric values fall within AtomVM's supported range.

#### Bitwise operations edge cases

The 256-bit limitation creates specific edge cases with bitwise operations that would require 257
bits:

On BEAM (unlimited precision), returns `-IntMax - 1` (requires 257 bits):

```erlang
1> IntMax = 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF.
115792089237316195423570985008687907853269984665640564039457584007913129639935
2> integer_to_binary(-1 bxor IntMax, 16).
<<"-10000000000000000000000000000000000000000000000000000000000000000">>
3> integer_to_binary(bnot IntMax, 16).
<<"-10000000000000000000000000000000000000000000000000000000000000000">>
```

On AtomVM (256-bit limited), returns 0 (cannot represent 257th bit):

```erlang
1> IntMax = 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF.
115792089237316195423570985008687907853269984665640564039457584007913129639935
2> -1 bxor IntMax.
0
3> bnot IntMax.
0
```

This occurs because AtomVM cannot create an integer with the 257th bit set to 1 with negative sign.
Since `-0` is not allowed, the result is normalized to `0`.

### Bit syntax

AtomVM supports binaries, binary construction and binary pattern matching. Bit syntax (i.e. with
sizes not a multiple of 8) is only supported as long as they would not generate bitstrings, i.e.
binaries with a number if bits that are not multiple of 8.

The following is supported:

    <<X:3, Y:5>> = <<N>>

The following is not:

    <<X:3>>

### Code reloading

AtomVM does not support code reloading yet. Few items are prioritized before this feature but it
is part of the roadmap.

### Distribution

AtomVM support for distribution is a work in progress. Many operations are supported but some key
features such as node monitoring are not implemented yet.

It is currently possible to connect a BEAM node with an AtomVM node.

## Known limitations of the standard library

AtomVM standard library is extremely limited and while programs written for AtomVM can be run
using OTP standard library if they do not call microcontroller-specific APIs, the opposite is not
true. A program written for Erlang/OTP is very unlikely runnable on AtomVM without a lot of
changes.

Please check AtomVM's [standard library documentation](./apidocs/erlang/estdlib/README.md) to find out differences
and limitations of each module.

### OTP architecture

Support for OTP applications is currently very limited. `gen_server`, `gen_statem` and `supervisor`
are partially supported, but `proc_lib` is not.

## Memory usage and speed

AtomVM is optimized for memory usage and is much slower than BEAM, even with JIT compiler enabled.

However, AtomVM uses much less RAM. Process heap initial size and process heap growth strategy are
much more agressive on AtomVM, and garbage collector runs much more frequently. If speed matters,
it is possible to use a process heap growth strategy inspired by BEAM's with an appropriate option
passed to `spawn_opt`.

    spawn_opt(
        fun() -> ... end,
        [{atomvm_heap_growth, fibonacci}]
    )

## Other differences

### NIFs and Ports

AtomVM supports NIFs and Ports with an API similar but different from BEAM's. However NIFs
and Ports need to be linked with the VM as most embedded environments do not support dynamic
linking.

AtomVM does not implement the onload opcode and therefore the -onload attribute, which is mostly
used for NIFs support on BEAM.

AtomVM doesn't have any dirty scheduler for dirty NIFs and NIFs should return quickly to avoid
locking the VM.

Ports are also executed by the schedulers and should return quickly.

### Resources

AtomVM supports resources but for historical reasons these appear as zero-length binaries as they
used to with OTP21, and not as references as they currently do with recent versions of the BEAM.
This has some consequences on matching.

### BEAM file compatibility

AtomVM can run BEAM files generated by `erlc` compiler from OTP21 to the latest master version,
while BEAM cannot and often requires to recompile.

### Stacktraces and exceptions

Stacktraces and exception error messages on AtomVM are not as detailed as on BEAM. There is no `error_info` in AtomVM exceptions.
For example a `function_clause` error will not include the arguments that did not match any clause.

Additionally, some error tags can differ. For example, calling an unserialized function (using `binary_to_term/1`)
from a module that exists in another version may error `badfun` with BEAM and will error `undef` with AtomVM.
