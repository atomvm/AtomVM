<!--
 Copyright 2025 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Distributed Erlang

For a generic introduction to Distributed Erlang Systems, please refer to the [dedicated section](https://www.erlang.org/doc/system/distributed.html) of Erlang/OTP documentation.

AtomVM provides an implementation of Erlang distribution protocol and AtomVM nodes can take part in clusters with both AtomVM and BEAM nodes.

Distribution is currently available on all platforms with TCP/IP communication, namely:
- Generic Unix
- ESP32
- RP2 (Pico)

Two examples are provided:

- disterl in `examples/erlang/disterl.erl`: distribution on Unix systems
- epmd\_disterl in `examples/erlang/esp32/epmd_disterl.erl`: distribution on ESP32 devices

## Starting and stopping distribution

Distribution has to be started programmatically. Following Erlang/OTP, distribution relies on `kernel` which is started by `init:boot/1`.

The following lines will start distribution on Unix systems with long name `atomvm@127.0.0.1`.

```erlang
{ok, _NetKernelPid} = net_kernel:start('atomvm@127.0.0.1', #{name_domain => longnames}),
ok = net_kernel:set_cookie(<<"AtomVM">>).
```

`net_kernel:stop/0` can be used to stop distribution.

## `epmd`

AtomVM nodes can use Erlang/OTP's epmd on Unix systems. AtomVM is also bundled with a pure Erlang implementation of `epmd` which can be used on all platforms. Module is called `epmd`, to be distinguished from `erl_epmd` which is the client.

AtomVM's epmd daemon can be started with:

```erlang
{ok, _EPMDPid} = epmd:start_link([]).
```

This has to be called before invoking `net_kernel:start/2`.

## Erlang/OTP compatibility

AtomVM can connect to Erlang/OTP 24 and higher.

## Security

AtomVM supports cookie authentication. However, distribution over TLS is not supported yet.

## Alternative carrier

Following Erlang/OTP, AtomVM supports alternative carriers with distribution modules. Please refer to [Erlang/OTP's dedicated documentation](https://www.erlang.org/doc/apps/erts/alt_dist#distribution-module).

The main difference is that packets exchanged by `f_recv` and `f_send` handlers must be binaries instead of list of integers, for memory usage reasons.

AtomVM's `f_send` has the following signature:

```erlang
fun (DistCtrlr, Data :: binary()) -> ok | {error, Error}
```

AtomVM's `f_recv` has the following signature:

```erlang
fun (DistCtrlr, Length :: pos_integer(), Timeout :: timeout()) -> {ok, Packet} | {error, Reason}
```

AtomVM's distribution is based on `socket_dist` and `socket_dist_controller` modules which can also be used with BEAM by definining `BEAM_INTERFACE` to adjust for the difference.

## Distribution features

Distribution implementation is (very) partial. The most basic features are available:
- serialization of all types
- epmd protocol (client and server)
- message passing
- monitoring processes
- I/O distribution ("group leader").

RPC (remote procedure call) from Erlang/OTP to AtomVM is also supported.

Shell requires several OTP standard library modules. See [the example project](https://github.com/pguyot/atomvm_shell).

Please do not hesitate to file issues or pull requests for additional features.
