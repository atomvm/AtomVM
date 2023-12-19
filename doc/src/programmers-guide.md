<!--
 Copyright 2021-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Programmers Guide

This guide is intended for programmers who develop applications targeted for AtomVM.

As an implementation of the Erlang virtual machine, AtomVM is designed to execute unmodified byte-code instructions compiled into BEAM files, either by the Erlang or Elixir compilers.  This allow developers to write programs in their BEAM programming language of choice, and to use the common Erlang community tool-chains specific to their language platform, and to then deploy those applications onto the various devices that AtomVM supports.

This document describes the development workflow when writing AtomVM applications, as well as a high-level overview of the various APIs that are supported by AtomVM.  With an understanding of this guide, you should be able to design, implement, and deploy applications onto a device running the AtomVM virtual machine.

## AtomVM Features

Currently, AtomVM implements a strict subset of the BEAM instruction set.

A high level overview of the supported language features include:

* All the major Erlang types, including
    * integers (with size limits)
    * floats
    * tuples
    * lists
    * binaries
    * maps
* support for many Erlang BIFs and guard expressions to support the above types
* pattern matching (case statements, function clause heads, etc)
* `try ... catch ... finally` constructs
* anonymous functions
* process `spawn` and `spawn_link`
* send (`!`) and `receive` messages
* bit syntax (with some restrictions)
* reference counted binaries
* stacktraces
* symmetric multi-processing (SMP)

In addition, several features are supported specifically for integration with micro-controllers, including:

* Wifi networking (`network`)
* UDP and TCP/IP support (`inet`, `gen_tcp` and `gen_udp`)
* Peripheral and system support on micro-controllers, including
    * GPIO, including pins reads, writes, and interrupts
    * I2C interface
    * SPI interface
    * UART interface
    * LEDC (PWM)
    * non-volatile storage (NVS)
    * RTC storage
    * deep sleep

### Limitations

While the list of supported features is long and growing, the currently unsupported Erlang/OTP and BEAM features include (but are not limited to):

* Bignums.  Integer values are restricted to 64-bit values.
* Bit Syntax.  While packing and unpacking of arbitrary (but less than 64-) bit values is support, packing and unpacking of integer values at the start or end of a binary, or bordering binary packing or extraction must align on 8-bit boundaries.  Arbitrary bit length binaries are not currently supported.
* The `epmd` and the `disterl` protocols are not supported.
* There is no support for code hot swapping.
* There is no support for a Read-Eval-Print-Loop. (REPL)
* Numerous modules and functions from Erlang/OTP standard libraries (`kernel`, `stdlib`, `sasl`, etc) are not implemented.

AtomVM bit syntax is restricted to alignment on 8-bit boundaries.  Little-endian and signed insertion and extraction of integer values is restricted to 8, 16, and 32-bit values.  Only unsigned big and little endian 64-bit values can be inserted into or extracted from binaries.

It is highly unlikely that an existing Erlang program targeted for Erlang/OTP will run unmodified on AtomVM.  And indeed, even as AtomVM matures and additional features are added, it is more likely than not that Erlang applications will need to targeted specifically for the AtomVM platform.  The intended target environment (small, cheap micro-controllers) differs enough from desktop or server-class systems in both scale and APIs that special care and attention is needed to target applications for such embedded environments.

That being said, many of the features of the BEAM are supported and provide a rich and compelling development environment for embedded devices, which Erlang and Elixir developers will find natural and productive.


## AtomVM Development

This section describes the typical development environment and workflow most AtomVM developers are most likely to use.

### Development Environment

In general, for most development purposes, you should be able to get away with an Erlang/OTP development environment, and for Elixir developers, and Elixir development environment.  For specific version requirements, see the [Release Notes](./release-notes.md).

We assume most development will take place on some UNIX-like environment (e.g., Linux, FreeBSD, or MacOS).  Consult your local package manager for installation of these development environments.

Developers will want to make use of common Erlang or Elixir development tools, such as `rebar3` for Erlang developers or `mix` for Elixir developers.

Developers will need to make use of some AtomVM tooling.  Fortunately, there are several choices for developers to use:

1. AtomVM `PackBEAM` executable (described below)
1. [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin), for Erlang development using [`rebar3`](https://rebar3.readme.io).
1. [`ExAtomVM`](https://github.com/atomvm/ExAtomVM) Mix plugin, Elixir development using [`Mix`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).

Some testing can be performed on UNIX-like systems, using the `AtomVM` executable that is suitable for your development environment.  AtomVM applications that do not make use of platform-specific APIs are suitable for such tests.

Deployment and testing on micro-controllers is slightly more involved, as these platforms require additional hardware and software, described below.

#### ESP32 Deployment Requirements

In order to deploy AtomVM applications to and test on the ESP32 platform, developers will need:

* A computer running MacOS or Linux (Windows support is TBD);
* An ESP32 module with a USB/UART connector (typically part of an ESP32 development board);
* A USB cable capable of connecting the ESP32 module or board to your development machine (laptop or PC);
* The [`esptool`](https://github.com/espressif/esptool) program, for flashing the AtomVM image and AtomVM programs;
* (Optional, but recommended) A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.

#### STM32 Deployment Requirements

TODO

### Development Workflow

For the majority of users, AtomVM applications are written in the Erlang or Elixir programming language.  These applications are compiled to BEAM (`.beam`) files using standard Erlang or Elixir compiler tool chains (`erlc`, `rebar3`, `mix`, etc).  The generated BEAM files contain byte-code that can be executed by the Erlang/OTP runtime, or by the AtomVM virtual machine.

> Note.  In a small number of cases, it may be useful to write parts of an application in the C programming language, as AtomVM nifs or ports.  However, writing AtomVM nifs and ports is outside of the scope of this document.

Once Erlang and/or Elixir files are compiled to BEAM files, AtomVM provides tooling for processing and aggregating BEAM files into AtomVM Packbeam (`.avm`) files, using AtomVM tooling, distributed as part of AtomVM, or as provided through the AtomVM community.

AtomVM packbeam files are the applications and libraries that run on the AtomVM virtual machine.  For micro-controller devices, they are "flashed" or uploaded to the device; for command-line use of AtomVM (e.g., on Linux, FreeBSD, or MacOS), they are supplied as the first parameter to the AtomVM command.

The following diagram illustrates the typical development workflow, starting from Erlang or Elixir source code, and resulting in a deployed Packbeam file:


    *.erl or *.ex                  *.beam
    +-------+                   +-------+
    |       |+                  |       |+
    |       ||+                 |       ||+
    |       |||     -------->   |       |||
    |       |||  Erlang/Elixir  |       |||
    +-------+||     Compiler    +-------+||
     +-------+|                  +-------+|
      +-------+                   +-------+
         ^                           |
         |                           | packbeam
         |                           |
         |                           v
         |                       +-------+
         |                       |       |
         | test                  |       |
         | debug                 |       |
         | fix                   |       |
         |                       +-------+
         |                        app.avm
         |                           |
         |                           | flash/upload
         |                           |
         |                           v
         +-------------------- Micro-controller
                                  device


The typical compile-test-debug cycle can be summarized in the following steps:

1. Deploy the AtomVM virtual machine to your device
1. Develop an AtomVM application in Erlang or Elixir
    1. Write application
    1. Deploy application to device
    1. Test/Debug/Fix application
    1. Repeat

Deployment of the AtomVM virtual machine and an AtomVM application currently require a USB serial connection.  There is currently no support for over-the-air (OTA) updates.

For more information about deploying the AtomVM image and AtomVM applications to your device, see the [Getting Started Guide](./getting-started-guide.md)


## Applications

An AtomVM application is a collection of BEAM files, aggregated into an AtomVM "Packbeam" (`.avm`) file, and typically deployed (flashed) to some device.  These BEAM files be be compiled from Erlang, Elixir, or any other language that targets the Erlang VM.

> Note.  The return value from the `start/0` function is ignored.

Here, for example is one of the smallest AtomVM applications you can write:

    %% erlang
    -module(myapp).

    -export([start/0]).

    start() ->
        ok.

This particular application doesn't do much, of course.  The application will start and immediately terminate, with a return value of `ok`.  Typical AtomVM applications will be more complex than this one, and the AVM file that contains the application BEAM files will be considerably larger and more complex than the above program.

Most applications will spawn processes, send and receive messages between processes, and
wait for certain conditions to apply before terminating, if they terminate at all.  For applications
that spawn processes and run forever, you may need to add an empty `receive ... end` block, to
prevent the AtomVM from terminating prematurely, e.g.,

    %% erlang
    wait_forever() ->
        receive X -> X end.


### Packbeam files

AtomVM applications are packaged into Packbeam (`.avm`) files, which contain collections of files, typically BEAM (`.beam`) files that have been generated by the Erlang or Elixir compiler.

At least one BEAM module in this file must contain an exported `start/0` function.  The first module in a Packbeam file that contain this function is the entry-point of your application and will be executed when the AtomVM virtual machine starts.

Not all files in a Packbeam need to be BEAM modules -- you can embed any type of file in a Packbeam file, for consumption by your AtomVM application.

> Note.  The Packbeam format is described in more detail in the AtomVM [PackBEAM format](./packbeam-format.md).

The AtomVM community has provided several tools for simplifying your experience, as a developer.  These tools allow you to use standard Erlang and Elixir tooling (such as `rebar3` and `mix`) to build Packbeam files and deploy then to your device of choice.

### `PackBEAM` tool

The `PackBEAM` tool is a command-line application that can be used to create Packbeam files from a collection of input files:

    shell$ PackBEAM -h
    Usage: PackBEAM [-h] [-l] <avm-file> [<options>]
        -h                                                Print this help menu.
        -i                                                Include file and line information.
        -l <input-avm-file>                               List the contents of an AVM file.
        [-a] <output-avm-file> <input-beam-or-avm-file>+  Create an AVM file (archive if -a specified).

To create a packbeam file, specify the name of the AVM file to created (by convention, ending in `.avm`), followed by a list of BEAM files:

    shell$ PackBEAM foo.avm path/to/foo.beam path/to/bar.beam

You can also specify another AVM file to include.  Thus, for example, to add to BEAM file to an existing AVM file, you might enter:

    shell$ PackBEAM foo.avm foo.avm path/to/gnu.beam

To list the contents of an AVM file, use the `-l` flag:

    shell% PackBEAM -l foo.avm
    foo.beam *
    bar.beam
    gnu.beam

Any BEAM files that export a `start/0` function will contain an asterisk (`*`) in the AVM file contents.

### Running AtomVM

AtomVM is executed in different ways, depending on the platform.  On most microcontrollers (e.g., the ESP32), the VM starts when the device is powered on.  On UNIX platforms, the VM is started from the command-line using the `AtomVM` executable.

AtomVM will use the first module in the supplied AVM file that exports a `start/0` function as the entrypoint for the application.

#### `AtomVM` program syntax

On UNIX platforms, you can specify a BEAM file or AVM file as the first argument to the executable, e.g.,

    shell$ AtomVM foo.avm

> Note. If you start the `AtomVM` executable with a BEAM file, then the corresponding module may not make any calls to external function in other modules, with the exception of built-in functions and Nifs that are included in the VM.

## Core APIs

The AtomVM virtual machine provides a set of Erlang built-in functions (BIFs) and native functions (NIFs), as well as a collection of Erlang and Elixir libraries that can be used from your applications.

This section provides an overview of these APIs.  For more detailed information about specific APIs, please consult the [API reference documentation](./api-reference-documentation.md).

### Standard Libraries

AtomVM provides a limited implementations of standard library modules, including:

* `base64`
* `gen_server`
* `gen_statem`
* `io` and `io_lib`
* `lists`
* `maps`
* `proplists`
* `supervisor`
* `timer`

In addition AtomVM provides limited implementations of standard Elixir modules, including:

* `List`
* `Tuple`
* `Enum`
* `Kernel`
* `Module`
* `Process`
* `Console`

For detailed information about these functions, please consult the [API reference documentation](./api-reference-documentation.md).  These modules provide a strict subset of functionality from their Erlang/OTP counterparts.  However, they aim to be API-compatible with the Erlang/OTP interfaces, at least for the subset of provided functionality.

### Spawning Processes

AtomVM supports the actor concurrency model that is pioneered in the Erlang/OTP runtime.  As such, users can spawn processes, send messages to and receive message from processes, and can link or monitor processes to be notified if they have crashed.

To spawn a process using a defined or anonymous function, pass the function to the `spawn/1` function:

    %% erlang
    Pid = spawn(fun run_some_code/0),

The function you pass may admit closures, so for example you can pass variables defined outside of the scope of the function to the anonymous function to pass into `spawn/1`:

    %% erlang
    Args = ...
    Pid = spawn(fun() -> run_some_code_with_args(Args) end),

Alternatively, you can pass a module, function name, and list of arguments to the `spawn/3` function:

    %% erlang
    Args = ...
    Pid = spawn(?MODULE, run_some_code_with_args, [Args]),

The `spawn_opt/2,4` functions can be be used to spawn a function with additional options that control the behavior of the spawned processs, e.g.,

    %% erlang
    Pid = spawn_opt(fun run_some_code/0, [{min_heap_size, 1342}]),

The options argument is a properties list containing optionally the following entries:

| Key | Value Type | Default Value | Description |
|-----|------------|---------------|-------------|
| `min_heap_size` | `non_neg_integer()` | none | Minimum heap size of the process.  The heap will shrink no smaller than this size. |
| `max_heap_size` | `non_neg_integer()` | unbounded | Maximum heap size of the process.  The heap will grow no larger than this size. |
| `link` | `boolean()` | `false` | Whether to link the spawned process to the spawning process. |
| `monitor` | `boolean()` | `false` | Whether to link the spawning process should monitor the spawned process. |
| `atomvm_heap_growth` | `bounded_free \| minimum \| fibonacci` | `bounded_free` | [Strategy](memory-management.md#heap-growth-strategies) to grow the heap of the process. |

### Console Output

There are several mechanisms for writing data to the console.

For common debugging, many users will find `erlang:display/1` sufficient for debugging:

    %% erlang
    erlang:display({foo, [{bar, tapas}]}).

The output parameter is any Erlang term, and a newline will be appended automatically.

Users may prefer using the `io:format/1,2` functions for more controlled output:

    %% erlang
    io:format("The ~p did a ~p~n", [friddle, frop]).

Note that the `io_lib` module can be used to format string data, as well.

> Note. Formatting parameters are currently limited to `~p`, `~s`, and `~n`.

### Logging

AtomVM supports a subset of the OTP logging facility, allowing users to send log event to log handlers (by default, the console), and to install handlers that handle log events.

To log events, you are encouraged to use the logging macros from the OTP `kernel` application.  You can use these macros at compile time, and the generated code can be run in AtomVM.

For example:

    %% erlang
    -include_lib("kernel/include/logger.hrl").
    ...
    ?LOG_NOTICE("Something happened that might require your attention: ~p", [TheThing])

By default, this will result in a message displayed on the console, with a timestamp, log level, PID of the process that initiated the log message, the module, function, and function arity, together with the supplied log message:

    2023-07-04T18:34:56.387 [notice] <0.1.0> test_logger:test_default_logger/0 Something happened that might require your attention: ThatThingThatHappened

> Note that log messages need not (and generally should not) include newline separators (`~n`) in log format messages, unless necessary.

Users may provide a format string, with an optional list of arguments.  Alternatively, users can provide a map encapsulating a "report" in lieu of a format string.  Reports provide a mechanism for supplying a set of structured data directly to log handlers (see below), without necessarily incurring the cost of formatting log messages.

As with OTP, the following ordered log levels (from high to low) are supported:

* `emergency`
* `critical`
* `alert`
* `error`
* `warning`
* `notice`
* `info`
* `debug`

By default, the logging facility drops any messages below `notice` level.  To set the default log level for the logging subsystem, see the `logger_manager` section, below.

You can use the `logger` interface directly to log messages at different levels, but in general, the OTP logging macros are encouraged, as log events generated using the OTP macros include additional metadata (such as the location of the log event) you do not otherwise get using the functions in the `logger` module.

For example, the expression

    logger:notice("Something happened that might require your attention: ~p", [TheThing])

may seem similar to using the `?LOG_NOTICE` macro, but less contextual information will be included in the log event.

For more information about the OTP logging facility, see the Erlang/OTP [Logging](https://www.erlang.org/doc/apps/kernel/logger_chapter.html) chapter.

> Note.  AtomVM does not currently support programmatic configuration of the logging subsystem.  All changes to default behavior should be done via the AtomVM `logger_manager` module (see below).

#### The `logger_manager`

In order to use the `logger` interface, you will need to first start the AtomVM `logger_manager` service.

> Note.  Future versions of AtomVM may automatically start the logging subsystem as part of a kernel application, but currently, this service must be managed manually.

To start the `logger_manager`, use the `logger_manager:start_link/1` function, passing in a configuration map for the logging subsystem.

For example, the default logging framework can be started via:

    %% erlang
    {ok, _Pid} = logger_manager:start_link(#{})

> Note.  The logger_manager is a registered process, so the returned Pid may be ignored.

The configuration map supplied to the logger_manager may contain the following keys:

| Key | Type | Default| Description |
|-----|------|--------|-------------|
| `log_level` | `log_level()` | `notice` | Primary log level |
| `logger` | `logger_config()` | `{handler, default, logger_std_h, undefined}` | Log configuration |
| `module_level` | `module_level()` | `undefined` | Log level specific to a set of modules |

where `log_level()` is defined to be:

    -type log_level() :: emergency | critical | alert | error | warning | notice | info | debug.

and `logger_config()` is defined as follows:

    -type handler_id() :: default | atom().
    -type handler_config() :: #{
        id => atom(),
        module => module(),
        level => logger:level() | all | none,
        config => term()
    }.
    -type logger_config() :: [
        {handler, default, undefined} |
        {handler, HandlerId :: handler_id(), Handler :: module(), HandlerConfig :: handler_config()} |
        {module_level, logger:level(), [module()]}
    ].

You can set the log level for all log handlers by setting the `log_level` in this configuration map.  Any messages that are logged at levels "higher" than or equal to the configured log level will be logged by all log handlers.

The standard logger (`logger_std_h`) is included by default, if no default logger is specified (and if the default logger is not disabled -- see below).  The standard logger will output log events to the console.

You can specify multiple log handlers in the `logger` configuration.  If a log entry is allowed for a given log level, then each log handler will handle the log message.  For example, you might have a log handler that sends messages over the network to a syslog daemon, or you might have another handler that writes log messages to a file.

You can pass handler configuration int the `config` element of the `handler_config()` you specify when specifying a logger.  The value of the `config` element can be any term and is made available to log handlers when events are logged (see below).

If the tuple `{handler, default, undefined}` is included in the logger configuration, the default logger will be disabled.

At most one `default` logger can be specified.  If you want to replace the `default` logger (`logger_std_h`), then specify a logger with the handler id `default`.

You can specify different log levels for specific modules.  For example, if you want to set the default log level for all handlers to be `notice` or higher, you can set the log level for a given module to `info`, and all `info` and higher messages will be logged for that module or set of modules.  Conversely, you can "quiet" a module if it is particularly noisy by setting its level to something relatively high.

For more information about how to configure the logging subsystem, see the [Kernel Configuration Parameters](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#kernel-configuration-parameters) section of the OTP Logging chapter.

You can stop the `logger_manager` via the `logger_manager:stop/0` function:

    %% erlang
    ok = logger_manager:stop()

#### Writing your own log handler

Additional loggers can be enabled via handler specifications.  A handler module must implement and export the `log/2` function, which takes a log event and a term containing the configuration for the logger handler instance.

For example:

    %% erlang
    -module(my_module).

    -export([..., log/2, ...]).

    log(LogEvent, HandlerConfig) ->
        %% do something with the log event
        %% return value is ignored

You can specify this handler in the `logger_manager` configuration (see above) via a stanza such as:

    {handler, my_id, my_module, HandlerConfig}

A `LogEvent` is a map structure containing the following fields:

| Key | Type | Description |
|-----|------|-------------|
| `timestamp` | `integer()` | The time (in microseconds since the UNIX epoch) at which the log event was generated |
| `level` | `logger:level()` | The log level with which the log event was generated |
| `pid` | `pid()` | The process id of the Erlang process in which the event was generated |
| `msg`  | `string() \| {string(), list()}` | The message format and arguments passed when the event was generated |
| `meta` | `map()` | Metadata passed when the event was generated. |

If the log event was generated using a logging macro, then the `meta` map also contains a `location` field with the following fields:

| Key | Type | Description |
|-----|------|-------------|
| `file` | `string()` | The path of the file in which the event was generated |
| `line` | `non_neg_integer()` | The line number in the file in which the event was generated |
| `mfa` | `{module(), function_name(), arity()}` | The MFA of the function in which the event was generated |

The handler config is a map structure containing the id and module of the handler.

an arbitrary term and is passed into the log handler via configuration of the `logger_manager` (see above).

### Process Management

You can obtain a list of all processes in the system via `erlang:processes/0`:

    %% erlang
    Pids = erlang:processes().

And for each process, you can get detailed process information via the `erlang:process_info/2` function:

    %% erlang
    [io:format("Heap size for Pid ~p: ~p~n", [Pid, erlang:process_info(Pid, heap_size)]) || Pid <- Pids].

The return value is a tuple containing the key passed into the `erlang:process_info/2` function and its associated value.

The currently supported keys are enumerated in the following table:

| Key | Value Type | Description |
|-----|------------|-------------|
| `heap_size` | `non_neg_integer()` | Number of terms (in machine words) used in the process heap |
| `stack_size` | `non_neg_integer()` | Number of terms (in machine words) used in the process stack |
| `message_queue_len` | `non_neg_integer()` | Number of unprocessed messages in the process mailbox |
| `memory` | `non_neg_integer()` | Total number of bytes used by the process (estimate) |

See the `word_size` key in the [System APIs](#System_APIs) section for information about how to find the number of bytes used in a machine word on the current platform.

### System APIs

You can obtain system information about the AtomVM virtual machine via the `erlang:system_info/1` function, which takes an atom parameter designating the desired datum.  Allowable parameters include

* `process_count` The number of processes running in the system.
* `port_count` The number of ports running in the system.
* `atom_count` The number of atoms allocated in the system.
* `word_size` The word size (in bytes) on the current platform (typically 4 or 8).
* `atomvm_version` The version of AtomVM currently running (as a binary).

For example,

    %% erlang
    io:format("Atom Count: ~p~n", [erlang:system_info(atom_count)]).

> Note.  Additional platform-specific information is supported, depending on the platform type.  See below.

Use the `atomvm:platform/0` to obtain the system platform on which your code is running.  The return value of this function is an atom who's value will depend on the platform on which your application is running.

    %% erlang
    case atomvm:platform() of
        esp32 ->
            io:format("I am running on an ESP32!~n");
        stm32 ->
            io:format("I am running on an STM32!~n");
        generic_unix ->
            io:format("I am running on a UNIX box!~n")
    end.

Use `erlang:garbage_collect/0` or `erlang:garbage_collect/1` to force the AtomVM garbage collector to run on a give process.  Garbage collection will in general happen automatically when additional free space is needed and is rarely needed to be called explicitly.

The 0-arity version of this function will run the garbage collector on the currently executing process.

    %% erlang
    Pid = ... %% get a reference to some pid
    ok = erlang:garbage_collect(Pid).

Use the `erlang:memory/1` function to obtain information about allocated memory.

Currently, AtomVM supports the following types:

| Type | Description |
|------|-------------|
| `binary` | Return the total amount of memory (in bytes) occupied by (reference counted) binaries |

> Note.  Binary data small enough to be stored in the Erlang process heap are not counted in this measurement.

### System Time

AtomVM supports numerous function for accessing the current time on the device.

Use `erlang:timestamp/0` to get the current time since the UNIX epoch (Midnight, Jan 1, 1970, UTC), at microsecond granularity, expressed as a triple (mega-seconds, seconds, and micro-seconds):

    %% erlang
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp().

User `erlang:system_time/1` to obtain the seconds, milliseconds or microseconds since the UNIX epoch (Midnight, Jan 1, 1970, UTC):

    %% erlang
    Seconds = erlang:system_time(second).
    MilliSeconds = erlang:system_time(millisecond).
    MicroSeconds = erlang:system_time(microsecond).

User `erlang:monotonic_time/1` to obtain a (possibly not strictly) monotonically increasing time measurement.  Use the same time units to convert to seconds, milliseconds, or microseconds:

    %% erlang
    Seconds = erlang:monotonic_time(second).
    MilliSeconds = erlang:monotonic_time(millisecond).
    MicroSeconds = erlang:monotonic_time(microsecond).

> Note.  `erlang:monotonic_time/1` should not be used to calculate the wall clock time, but instead should be used by applications to compute time differences in a manner that is independent of the system time on the device, which might change, for example, due to NTP, leap seconds, or similar operations that may affect the wall time on the device.

Use `erlang:universaltime/0` to get the current time at second resolution, to obtain the year, month, day, hour, minute, and second:

    %% erlang
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime().

On some platforms, you can use the `atomvm:posix_clock_settime/2` to set the system time.  Supply a clock id (currently, the only supported clock id is the atom `realtime`) and a time value as a tuple, containing seconds and nanoseconds since the UNIX epoch (midnight, January 1, 1970).  For example,

    %% erlang
    SecondsSinceUnixEpoch = ... %% acquire the time
    atomvm:posix_clock_settime(realtime, {SecondsSinceUnixEpoch, 0})

> Note.  This operation is not supported yet on the `stm32` platform.  On most UNIX platforms, you typically need `root` permission to set the system time.

On the ESP32 platform, you can use the Wifi network to set the system time automatically.  For information about how to set system time on the ESP32 using SNTP, see the [Network Programming Guide](./network-programming-guide.md).

To convert a time (in seconds, milliseconds, or microseconds from the UNIX epoch) to a date-time, use the `calendar:system_time_to_universal_time/2` function.  For example,

    %% erlang
    Milliseconds = ... %% get milliseconds from the UNIX epoch
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:system_time_to_universal_time(Milliseconds, millisecond).

Valid time units are `second`, `millisecond`, and `microsecond`.

### Date and Time

A `datetime()` is a tuple containing a date and time, where a date is a tuple containing the year, month, and day (in the [Gregorian](https://en.wikipedia.org/wiki/Gregorian_calendar) calendar), expressed as integers, and a time is an hour, minute, and second, also expressed in integers.

The following Erlang type specification enumerates this type:

    %% erlang
    -type year() :: integer().
    -type month() :: 1..12.
    -type day() :: 1..31.
    -type date() :: {year(), month(), day()}.
    -type gregorian_days() :: integer().
    -type day_of_week() :: 1..7.
    -type hour() :: 0..23.
    -type minute() :: 0..59.
    -type second() :: 0..59.
    -type time() :: {hour(), minute(), second()}.
    -type datetime() :: {date(), time()}.

Erlang/OTP uses the Christian epoch to count time units from year 0 in the Gregorian calendar.  The, for example, the value 0 in Gregorian seconds represents the date Jan 1, year 0, and midnight (UTC), or in Erlang terms, `{{0, 1, 1}, {0, 0, 0}}`.

> Note.  AtomVM is currently limited to representing integers in at most 64 bits, with one bit representing the sign bit.  However, even with this limitation, AtomVM is able to resolve microsecond values in the Gregorian calendar for over 292,000 years, likely well past the likely lifetime of an AtomVM application (unless perhaps launched on a deep space probe).

The `calendar` module provides useful functions for converting dates to Gregorian days, and date-times to Gregorian seconds.

To convert a `date()` to the number of days since January 1, year 0, use the `calendar:date_to_gregorian_days/1` function, e.g.,

    GregorianDays = calendar:date_to_gregorian_days({2023, 7, 23})

To convert a `datetime()` to convert the number of seconds since midnight January 1, year 0, use the `calendar:datetime_to_gregorian_seconds/1` function, e.g.,

    GregorianSeconds = calendar:datetime_to_gregorian_seconds({{2023, 7, 23}, {13, 31, 7}})

> Note.  The `calendar` module does not support year values before year `0`.

### Miscellaneous

Use `atomvm:random/0` to generate a random unsigned 32-bit integer in the range `0..4294967295`:

    %% erlang
    RandomInteger = atomvm:random().

Use `crypto:strong_rand_bytes/1` to return a randomly populated binary of a specified size:

    %% erlang
    RandomBinary = crypto:strong_rand_bytes(32).

Use `base64:encode/1` and `base64:decode/1` to encode to and decode from Base64 format.  The input value to these functions may be a binary or string.  The output value from these functions is an Erlang binary.

    %% erlang
    Encoded = base64:encode(<<"foo">>).
    <<"foo">> = base64:decode(Encoded).

You can Use `base64:encode_to_string/1` and `base64:decode_to_string/1` to perform the same encoding, but to return values as Erlang list structures, instead of as binaries.

### StackTraces

You can obtain information about the current state of a process via stacktraces, which provide information about the location of function calls (possibly including file names and line numbers) in your program.

Currently in AtomVM, stack traces can be obtained in one of following ways:

* via try-catch blocks
* via catch blocks, when an error has been raised via the `error` Bif.

> Note.  AtomVM does not support `erlang:get_stacktrace/0` which was deprecated in Erlang/OTP 21 and 22, stopped working in Erlang/OTP 23 and was removed in Erlang/OTP 24.  Support for accessing the current stacktrace via `erlang:process_info/2` may be added in the future.

For example a stack trace can be bound to a variable in the catch clause in a try-catch block:

    try
        do_something()
    catch
        _Class:_Error:Stacktrace ->
            io:format("Stacktrace: ~p~n", [Stacktrace])
    end

Alternatively, a stack trace can be bound to the result of a `catch` expression, but only when the error is raised by the `error` Bif.  For example,

    {'EXIT', {foo, Stacktrace}} = (catch error(foo)),
    io:format("Stacktrace: ~p~n", [Stacktrace])

Stack traces are printed to the console in a crash report, for example, when a process dies unexpectedly.

Stacktrace data is represented as a list of tuples, each of which represents a stack “frame”.  Each tuple is of the form:

    [{Module :: module(), Function :: atom(), Arity :: non_neg_integer(), AuxData :: aux_data()}]

where `aux_data()` is a (possibly empty) properties list containing the following elements:

    [{file, File :: string(), line, Line :: pos_integer()}]

Stack frames are ordered from the frame “closest“ to the point of failure (the “top” of the stack) to the frame furthest from the point of failure (the “bottom” of the stack).

Stack frames will contain file and line information in the AuxData list if the BEAM files (typically embedded in AVM files) include <<“Line”>> chunks generated by the compiler.  Otherwise, the AuxData will be an empty list.

> Note.  Adding line information to BEAM files not only increases the size of BEAM files in storage, but calculation of file and line information can have a non-negligible impact on memory usage.  Memory-sensitive applications should consider not including line information in BEAM files.

The PackBEAM tool does not include file and line information in the AVM files it creates, but file and line information can be included via a command line option.  For information about the PackBEAM too, see the [`PackBEAM` tool](#Packbeam_tool).

### Reading data from AVM files

AVM files are generally packed BEAM files, but they can also contain non-BEAM files, such as plain text files, binary data, or even encoded Erlang terms.

Typically, these files are included from the `priv` directory in a build tree, for example, when using the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin), though the [`PackBEAM`](./packbeam-format.md) tool and the [`atomvm_packbeam`](https://github.com/atomvm/atomvm_packbeam) tool allow you to specify any location for files to include in AVM files.

By convention, these files obey the following path in an AVM file:

    <application-name>/priv/<file-path>

For example, if you wanted to embed `my_file.txt` into your application AVM file (where your application name is, for example, `my_application`), you would use:

    my_application/priv/my_file.txt

The `atomvm:read_priv/2` function can then be used to extract the contents of this file into a binary, e.g.,

    %% erlang
    MyFileBin = atomvm:read_priv(my_application, <<"my_file.txt">>)

> Note. Embedded files may contain path separators, so for example `<<"my_files/my_file.txt">>` would be used if the AVM file embeds `my_file.txt` using the path `my_application/priv/my_files/my_file.txt`

For more information about how to embed files into AVM files, see the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin).

### Code Loading

AtomVM provides a limited set of APIs for loading code and data embedded dynamically at runtime.

To load an AVM file from binary data, use the `atomvm:add_avm_pack_binary/2` function.  Supply a reference to the AVM data, together with a (possibly empty) list of options.  Specify a `name` option, whose value is an atom, if you wish to close the AVM data at a later point in the program.

For example:

    %% erlang
    AVMData = ... %% load AVM data into memory as a binary
    ok = atomvm:add_avm_pack_binary(AVMData, [{name, my_avm}])

You can also load AVM data from a file (on the `generic_unix` platform) or from a flash partition (on ESP32 platforms) using the `atomvm:add_avm_pack_file/2` function.  Specify a string (or binary) as the path to the AVM file, together with a list of options, such as `name`.

For example:

    %% erlang
    ok = atomvm:add_avm_pack_file("/path/to/file.avm", [{name, my_avm}])

On `esp32` platforms, the partition name should be prefixed with the string `/dev/partition/by-name/`.  Thus, for example, if you specify `/dev/partition/by-name/main2.avm` as the partition, the ESP32 flash should contain a data partition with the name `main2.avm`

For example:

    %% erlang
    ok = atomvm:add_avm_pack_file("/dev/partition/by-name/main2.avm", [])

To close a previous opened AVM by name, use the `atomvm:close_avm_pack/2` function.  Specify the name of the AVM pack used to add

    %% erlang
    ok = atomvm:close_avm_pack(my_avm, [])

> Note.  Currently, the options parameter is ignored, so use the empty list (`[]`) for foward compatibility.

You can load an individual BEAM file using the `code:load_binary/3` function.  Specify the Module name (as an atom), as well as the BEAM data you have loaded into memory.

For Example:

    %% erlang
    BEAMData = ... %% load BEAM data into memory as a binary
    {module, Module} = code:load_binary(Module, Filename, BEAMData)

> Note.  The `Filename` parameter is currently ignored.

You can load an individual BEAM file from the file system using the `code:load_abs/1` function.  Specify the path to the BEAM file.  This path should not include the `.beam` extension, as this extension will be added automatically.

For example:

    {module, Module} = code:load_abs("/path/to/beam/file/without/beam/extension")

> Note.  This function is currently only supported on the `generic_unix` platform.

### Math

AtomVM supports the following standard functions from the OTP `math` module:

* `cos/1`
* `acos/1`
* `acosh/1`
* `asin/1`
* `asinh/1`
* `atan/1`
* `atan2/2`
* `atanh/1`
* `ceil/1`
* `cosh/1`
* `exp/1`
* `floor/1`
* `fmod/2`
* `log/1`
* `log10/1`
* `log2/1`
* `pow/2`
* `sin/1`
* `sinh/1`
* `sqrt/1`
* `tan/1`
* `tanh/1`
* `pi/0`

The input values for these functions may be `float` or `integer` types.  The return value is always a value of `float` type.

Input values that are out of range for the specific mathematical function or which otherwise are invalid or yield an invalid result (e.g., division by 0) will result in a `badarith` error.

> Note.  If the AtomVM virtual machine is built with floating point arithmetic support disabled, these functions will result in a `badarg` error.

### Cryptographic Operations

You can hash binary date using the `crypto:hash/2` function.

    %% erlang
    crypto:hash(sha, [<<"Some binary">>, $\s, "data"])

This function takes a hash algorithm, which may be one of:

    -type md_type() :: md5 | sha | sha224 | sha256 | sha384 | sha512.

and an IO list.  The output type is a binary, who's length (in bytes) is dependent on the algorithm chosen:

| Algorithm | Hash Length (bytes) |
|-----------|-------------|
| `md5` | 16 |
| `sha` | 20 |
| `sha224` | 32 |
| `sha256` | 32 |
| `sha384` | 64 |
| `sha512` | 64 |

> Note.  The `crypto:hash/2` function is currently only supported on the ESP32 and generic UNIX platforms.

You can also use the legacy `erlang:md5/1` function to compute the MD5 hash of an input binary.  The output is a fixed-length binary (16 bytes)

    %% erlang
    Hash = erlang:md5(<<foo>>).

On ESP32, you can perform symetric encryption and decryption of any iodata data using
`crypto_one_time/4,5` function.

Following ciphers are supported:

Without IV (using `crypto_one_time/4`):

- `aes_128_ecb`
- `aes_192_ecb`
- `aes_256_ecb`

With IV (using `crypto_one_time/5`):

- `aes_128_cbc`
- `aes_192_cbc`
- `aes_256_cbc`
- `aes_128_cfb128`
- `aes_192_cfb128`
- `aes_256_cfb128`
- `aes_128_ctr`
- `aes_192_ctr`
- `aes_256_ctr`

The function is implemented using [mbedTLS](https://github.com/Mbed-TLS/mbedtls), so please to its
documentation for further details.

Please refer to
[Erlang crypto documentation](https://www.erlang.org/doc/man/crypto#crypto_one_time-4) for
additional details about these two functions.

> Note: mbedTLS doesn't support padding for ciphers other than CCB, so block size must be accounted
> otherwise output will be truncated.

## ESP32-specific APIs

Certain APIs are specific to and only supported on the ESP32 platform.  This section describes these APIs.

### System-Level APIs

As noted above, the `erlang:system_info/1` function can be used to obtain system-specific information about the platform on which your application is deployed.

You can request ESP32-specific information using using the following input atoms:

* `esp_free_heap_size` Returns the available free space in the ESP32 heap.
* `esp_largest_free_block` Returns the size of the largest free continuous block in the ESP32 heap.
* `esp_get_minimum_free_size` Returns the smallest ever free space available in the ESP32 heap since boot, this will tell you how close you have come to running out of free memory.
* `esp_chip_info` Returns map of the form `#{features := Features, cores := Cores, revision := Revision, model := Model}`, where `Features` is a list of features enabled in the chip, from among the following atoms: `[emb_flash, bgn, ble, bt]`; `Cores` is the number of CPU cores on the chip; `Revision` is the chip version; and `Model` is one of the following atoms: `esp32`, `esp32_s2`, `esp32_s3`, `esp32_c3`.
* `esp_idf_version` Return the IDF SDK version, as a string.

For example,

    %% erlang
    FreeHeapSize = erlang:system_info(esp_free_heap_size).

### Non-volatile Storage

AtomVM provides functions for setting, retrieving, and deleting key-value data in binary form in non-volatile storage (NVS) on an ESP device.  Entries in NVS survive reboots of the ESP device, and can be used a limited "persistent store" for key-value data.

> Note.  NVS storage is limited in size, and NVS keys are restricted to 15 characters.  Try to avoid writing frequently to NVS storage, as the flash storage may degrade more rapidly with repeated writes to the medium.

NVS entries are stored under a namespace and key, both of which are expressed as atoms.  AtomVM uses the namespace `atomvm` for entries under its control.  Applications may read from and write to the `atomvm` namespace, but they are strongly discouraged from doing so, except when explicitly stated otherwise.

To set a value in non-volatile storage, use the `esp:set_binary/3` function, and specify a namespace, key, and value:

    %% erlang
    Namespace = <<"my-namespace">>,
    Key = <<"my-key">>,
    esp:set_binary(Namespace, Key, <<"some-value">>).

To retrieve a value in non-volatile storage, use the `esp:get_binary/2` function, and specify a namespace and key.  You can optionally specify a default value (of any desired type), if an entry does not exist in non-volatile storage:

    %% erlang
    Value = esp:get_binary(Namespace, Key, <<"default-value">>).

To delete an entry, use the `esp:erase_key/2` function, and specify a namespace and key:

    %% erlang
    ok = esp:erase_key(Namespace, Key).

You can delete all entries in a namespace via the `esp:erase_all/1` function:

    %% erlang
    ok = esp:erase_all(Namespace).

Finally, you can delete all entries in all namespaces on the NVS partition via the `esp:reformat/0` function:

    %% erlang
    ok = esp:reformat().

Applications should use the `esp:reformat/0` function with caution, in case other applications are making using the non-volatile storage.

> Note.  NVS entries are currently stored in plaintext and are not encrypted.  Applications should exercise caution if sensitive security information, such as account passwords, are stored in NVS storage.

### Restart and Deep Sleep

You can use the `esp:restart/0` function to immediately restart the ESP32 device.  This function does not return a value.

    %% erlang
    esp:restart().

Use the `esp:reset_reason/0` function to obtain the reason for the ESP32 restart.  Possible values include:

* `esp_rst_unknown`
* `esp_rst_poweron`
* `esp_rst_ext`
* `esp_rst_sw`
* `esp_rst_panic`
* `esp_rst_int_wdt`
* `esp_rst_task_wdt`
* `esp_rst_wdt`
* `esp_rst_deepsleep`
* `esp_rst_brownout`
* `esp_rst_sdio`

Use the `esp:deep_sleep/1` function to put the ESP device into deep sleep for a specified number of milliseconds.  Be sure to safely stop any critical processes running before this function is called, as it will cause an immediate shutdown of the device.

    %% erlang
    esp:deep_sleep(60*1000).

Use the `esp:sleep_get_wakeup_cause/0` function to inspect the reason for a wakeup.  Possible return values include:
* `sleep_wakeup_ext0`
* `sleep_wakeup_ext1`
* `sleep_wakeup_timer`
* `sleep_wakeup_touchpad`
* `sleep_wakeup_ulp`
* `sleep_wakeup_gpio`
* `sleep_wakeup_uart`
* `sleep_wakeup_wifi`
* `sleep_wakeup_cocpu`
* `sleep_wakeup_cocpu_trag_trig`
* `sleep_wakeup_bt`
* `undefined` (no sleep wakeup)
* `error` (unknown other reason)

The values matches the semantics of [`esp_sleep_get_wakeup_cause`](https://docs.espressif.com/projects/esp-idf/en/release-v4.4/esp32/api-reference/system/sleep_modes.html#_CPPv426esp_sleep_get_wakeup_causev).

    %% erlang
    case esp:sleep_get_wakeup_cause() of
        sleep_wakeup_timer ->
            io:format("Woke up from a timer~n");
        sleep_wakeup_ext0 ->
            io:format("Woke up from ext0~n");
        sleep_wakeup_ext1 ->
            io:format("Woke up from ext1~n");
        _ ->
            io:format("Woke up for some other reason~n")
    end.

Use the `esp:sleep_enable_ext0_wakeup/2` and `esp:sleep_enable_ext1_wakeup` functions to configure ext0 and ext1 wakeup mechanisms. They follow the semantics of [`esp_sleep_enable_ext0_wakeup`](https://docs.espressif.com/projects/esp-idf/en/release-v4.4/esp32/api-reference/system/sleep_modes.html#_CPPv428esp_sleep_enable_ext0_wakeup10gpio_num_ti) and [`esp_sleep_enable_ext1_wakeup`](https://docs.espressif.com/projects/esp-idf/en/release-v4.4/esp32/api-reference/system/sleep_modes.html#_CPPv428esp_sleep_enable_ext1_wakeup8uint64_t28esp_sleep_ext1_wakeup_mode_t).

    %% erlang
    -spec shutdown() -> no_return().
    shutdown() ->
        % Configure wake up when GPIO 37 is set to low (M5StickC main button)
        ok = esp:sleep_enable_ext0_wakeup(37, 0),
        % Deep sleep for 1 hour
        esp:deep_sleep(60*60*1000).

#### RTC Memory

On ESP32 systems, you can use (slow) "real-time clock" memory to store data between deep sleeps.  This storage can be useful, for example, to store interim state data in your application.

> Note.  RTC memory is initialized if the device is reset.

To store data in RTC slow memory, use the `esp:rtc_slow_set_binary/1` function:

    %% erlang
    esp:rtc_slow_set_binary(<<"some binary data">>)

To retrieve data in RTC slow memory, use the `esp:rtc_slow_get_binary/0` function:

    %% erlang
    Data = esp:rtc_slow_get_binary()

By default, RTC slow memory in AtomVM is limited to 4098 (4k) bytes.  This value can be modified at build time using an IDF SDK `KConfig` setting.  For  instructions about how to build AtomVM, see the AtomVM [Build Instructions](./build-instructions.md).


### Miscellaneous

The `freq_hz` function can be used to retrieve the clock frequency of the chip.

* `esp:freq_hz/0`

The `esp:partition_list/0` function can be used to retrieve information about the paritions on an ESP32 flash.

The return type is a list of tuples, each of which contains the partition id (as a binary), partition type and sub-type (both of which are represented as integers), the start of the partition as an address along with its size, as well as a list of properties about the partition, as a properties list.

    %% erlang
    PartitionList = esp:partition_list(),
    lists:foreach(
        fun({PartitionId, PartitionType, PartitionSubtype, PartitionAddress, PartitionSize, PartitionProperties}) ->
            %% ...
        end,
        PartitionList
    )

> Note.  The partition properties are currently empty (`[]`).

For information about the encoding of partition types and sub-types, see the IDF SDK partition [type definitions](https://docs.espressif.com/projects/esp-idf/en/v4.4.5/esp32/api-reference/storage/spi_flash.html?highlight=esp_partition_get#id13).

* `esp:get_mac/1`

The `esp:get_mac/1` function can be used to retrieve the network Media Access Control ([MAC](https://en.wikipedia.org/wiki/MAC_address)) address for a given interface, `wifi_sta` or `wifi_softap`.  The return value is a 6-byte binary, in accordance with the [IEEE 802](https://en.wikipedia.org/wiki/IEEE_802) family of specifications.

    %% erlang
    MacAddress = esp:get_mac(wifi_sta)

## Peripherals

The AtomVM virtual machine and libraries support APIs for interfacing with peripheral devices connected to the ESP32 and other supported microcontrollers.  This section provides information about these APIs.  Unless otherwise stated the documentation for these peripherals is specific to the ESP32, most peripherals are not yet supported on rp2040 or stm32 devices - but work is on-going to expand support on these platforms.

### GPIO

The GPIO peripheral has nif support on all platforms. One notable difference on the STM32 platform is that `Pin`s are defined as a tuple consisting of the bank (a.k.a. port) and pin number.  For example a pin labeled PB7 on your board would be `{b,7}`.

You can read and write digital values on GPIO pins using the `gpio` module, using the `digital_read/1` and `digital_write/2` functions.  You must first set the direction of the pin using the `gpio:set_direction/2` function, using `input` or `output` as the direction parameter.

#### Digital Read

To read the value of a GPIO pin (`high` or `low`), use `gpio:digital_read/1`.

For ESP32 family:

    %% erlang
    Pin = 2,
    gpio:set_direction(Pin, input),
    case gpio:digital_read(Pin) of
        high ->
            io:format("Pin ~p is high ~n", [Pin]);
        low ->
            io:format("Pin ~p is low ~n", [Pin])
    end.

For STM32 only the line with the Pin definition needs to be a tuple:

    %% erlang
    Pin = {c, 13},
    gpio:set_direction(Pin, input),
    case gpio:digital_read(Pin) of
        high ->
            io:format("Pin ~p is high ~n", [Pin]);
        low ->
            io:format("Pin ~p is low ~n", [Pin])
    end.

The Pico has an additional initialization step `gpio:init/1` before using a pin for gpio:

    %% erlang
    Pin = 2,
    gpio:init(Pin),
    gpio:set_direction(Pin, input),
    case gpio:digital_read(Pin) of
        high ->
            io:format("Pin ~p is high ~n", [Pin]);
        low ->
            io:format("Pin ~p is low ~n", [Pin])
    end.

#### Digital Write

To set the value of a GPIO pin (`high` or `low`), use `gpio:digital_write/2`.

For ESP32 family:

    %% erlang
    Pin = 2,
    gpio:set_direction(Pin, output),
    gpio:digital_write(Pin, low).

For the STM32 use a pin tuple:

    %% erlang
    Pin = {b, 7},
    gpio:set_direction(Pin, output),
    gpio:digital_write(Pin, low).

Pico needs the extra `gpio:init/1` before `gpio:read/1` too:

    %% erlang
    Pin = 2,
    gpio:init(Pin),
    gpio:set_direction(Pin, output),
    gpio:digital_write(Pin, low).

#### Interrupt Handling

Interrupts are supported on both the ESP32 and STM32 platforms.

You can get notified of changes in the state of a GPIO pin by using the `gpio:set_int/2` function.  This function takes a reference to a GPIO Pin and a trigger.  Allowable triggers are `rising`, `falling`, `both`, `low`, `high`, and `none` (to disable an interrupt).

When a trigger event occurs, such as a pin rising in voltage, a tuple will be delivered to the process containing the atom `gpio_interrupt` and the pin.

    %% erlang
    Pin = 2,
    gpio:set_direction(Pin, input),
    GPIO = gpio:open(),
    ok = gpio:set_int(GPIO, Pin, rising),
    receive
        {gpio_interrupt, Pin} ->
            io:format("Pin ~p is rising ~n", [Pin])
    end.

Interrupts can be removed by using the `gpio:remove_int/2` function.

Use the `gpio:close/1` function to close the GPIO driver and free any resources in use by it, supplying a reference to a previously opened GPIO driver instance.  Any references to the closed GPIO instance are no longer valid after a successful call to this function, and all interrupts will be removed.

    %% erlang
    ok = gpio:close(GPIO).

Since only one instance of the GPIO driver is allowed, you may also simply use `gpio:stop/0` to remove all interrupts, free the resouces, and close the GPIO driver port.

    %% erlang
    ok = gpio:stop().


### I2C

The `i2c` module encapsulates functionality associated with the 2-wire Inter-Integrated Circuit (I2C) interface.

> Note.  Information about the ESP32 I2C interface can be found in the IDF SDK [I2C Documentation](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/i2c.html).

The AtomVM I2C implementation uses the AtomVM Port mechanism and must be initialized using the `i2c:open/1` function.  The single parameter contains a properties list, with the following elements:

| Key | Value Type | Required | Description |
|-----|------------|----------|---|
| `scl` | `integer()` | yes | I2C clock pin (SCL) |
| `sda` | `integer()` | yes | I2C data pin (SDA) |
| `clock_speed_hz` | `integer()` | yes | I2C clock frequency (in hertz) |
| `peripheral` | `string() | binary()` | no (platform dependent default) | I2C peripheral, such as `"i2c0"` |

For example,

    %% erlang
    I2C = i2c:open([
        {scl, 21}, {sda, 22}, {clock_speed_hz, 40000}
    ])

Once the port is opened, you can use the returned `I2C` instance to read and write bytes to the attached device.

Both read and write operations require the I2C bus address from which data is read or to which data is written.  A devices address is typically hard-wired for the specific device type, or in some cases may be changed by the addition or removal of a resistor.

In addition, you may optionally specify a register to read from or write to, as some devices require specification of a register value.  Consult your device's data sheet for more information and the device's I2C bus address and registers, if applicable.

There are two patterns for writing data to an I2C device:

1. Queuing `i2c:qwrite_bytes/2,3` write operations between calls to `i2c:begin_transmission/1` and `i2c:end_transmission/1`.  In this case, write operations are queued locally and dispatched to the target device when the `i2c:end_transmission/1` operation is called;
1. Writing a byte or sequence of bytes in one `i2c:write_bytes/2,3` operation.

The choice of which pattern to use will depend on the device being communicated with.  For example, some devices require a sequence of write operations to be queued and written in one atomic write, in which case the first pattern is appropriate.  E.g.,

    %% erlang
    ok = i2c:begin_transmission(I2C),
    ok = i2c:qwrite_bytes(I2C, DeviceAddress, Register1, <<"some sequence of bytes">>),
    ok = i2c:qwrite_bytes(I2C, DeviceAddress, Register2, <<"some other of bytes">>),
    ok = i2c:end_transmission(I2C),

In other cases, you may just need to write a byte or sequence of bytes in one operation to the device:

    %% erlang
    ok = i2c:write_bytes(I2C, DeviceAddress, Register1, <<"write it all in one go">>),

Reading bytes is more straightforward.  Simply use `i2c:read_bytes/3,4`, specifying the port instance, device address, optionally a register, and the number of bytes to read:

    %% erlang
    {ok, BinaryData} = i2c:read_bytes(I2C, DeviceAddress, Register, Len)

To close the I2C driver and free any resources in use by it, use the `i2c:close/1` function, supplying a reference to the I2C driver instance created via `i2c:open/1`:

    %% erlang
    ok = i2c:close(I2C)

Once the I2C driver is closed, any calls to `i2c` functions using a reference to the I2C driver instance should return with the value `{error, noproc}`.

### SPI

The `spi` module encapsulates functionality associated with the 4-wire Serial Peripheral Interface (SPI) in leader mode.

> Note.  Information about the ESP32 SPI leader mode interface can be found in the IDF SDK [SPI Documentation](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/spi_master.html).

The AtomVM SPI implementation uses the AtomVM Port mechanism and must be initialized using the `spi:open/1` function.  The single parameter to this function is a properties list containing two elements:

* `bus_config` -- a properties list containing entries for the SPI bus
* `device_config` -- a properties list containing entries for each device attached to the SPI Bus

The `bus_config` properties list contains the following entries:

| Key              | Value Type  | Required    | Description                                  |
|------------------|-------------|-------------|----------------------------------------------|
| `poci` (`miso`)  | `integer()` | yes         | SPI peripheral-out, controller-in pin (MOSI) |
| `pico` (`mosi`)  | `integer()` | yes         | SPI peripheral-in, controller-out pin (MISO) |
| `sclk`           | `integer()` | yes         | SPI clock pin (SCLK)                         |

The `device_config` entry is a properties list containing entries for each device attached to the SPI Bus.  Each entry in this list contains the user-selected name (as an atom) of the device, followed by configuration for the named device.

Each device configuration is a properties list containing the following entries:

| Key | Value Type | Required | Description |
|-----|------------|----------|---|
| `clock_speed_hz` | `integer()` | yes | SPI clock frequency (in hertz) |
| `mode` | `0..3` | yes | SPI mode, indicating clock polarity (`CPOL`) and clock phase (`CPHA`).  Consult the SPI specification and data sheet for your device, for more information about how to control the behavior of the SPI clock. |
| `cs` | `integer()` | yes | SPI chip select pin (CS) |
| `address_len_bits` | `0..64` | yes | number of bits in the address field of a read/write operation (for example, 8, if the transaction address field is a single byte) |
| `command_len_bits` | `0..16` | default: 0 | number of bits in the command field of a read/write operation (for example, 8, if the transaction command field is a single byte) |

For example,

    %% erlang
    SPIConfig = [
        {bus_config, [
            {miso, 19},
            {mosi, 27},
            {sclk, 5}
        ]},
        {device_config, [
            {my_device_1, [
                {clock_speed_hz, 1000000},
                {mode, 0},
                {cs, 18},
                {address_len_bits, 8}
            ]}
            {my_device_2, [
                {clock_speed_hz, 1000000},
                {mode, 0},
                {cs, 15},
                {address_len_bits, 8}
            ]}
        ]}
    ],
    SPI = spi:open(SPIConfig),
    ...

In the above example, there are two SPI devices, one using pin 18 chip select (named `my_device_1`), and once using pin 15 chip select (named `my_device_2`).

Once the port is opened, you can use the returned `SPI` instance, along with the selected device name, to read and write bytes to the attached device.

To read a byte at a given address on the device, use the `spi:read_at/4` function:

    %% erlang
    {ok, Byte} = spi:read_at(SPI, DeviceName, Address, 8)

To write a byte at a given address on the device, use the `spi_write_at/5` function:

    %% erlang
    write_at(SPI, DeviceName, Address, 8, Byte)

> Note.  The `spi:write_at/5` takes integer values as inputs and the `spi:read_at/4` returns integer values. You may read and write up to 32-bit integer values via these functions.

Consult your local device data sheet for information about various device addresses to read from or write to, and their semantics.

The above functions are optimized for small reads and writes to an SPI device, typically one byte at a time.

The SPI interface also supports a more generic way to read and write from an SPI device, supporting arbitrary-length reads and writes, as well as a number of different "phases" of writes, per the SPI specification.

These phases include:

* Command phase -- write of an up-to 16-bit command to the SPI device
* Address Phase -- write of an up-to 64-bit address to the SPI device
* Data Phase -- read or write of an arbitrary amount of of data to and from the device.

Any one of these phases may be included or omitted in any given SPI transaction.

In order to achieve this level of flexibility, these functions allow users to specify the SPI transaction through a map structure, which includes fields that specify the behavior of an SPI transaction.

The following table enumerates the permissible fields in this structure:

| Key | Value Type | Description |
|-----|------------|-------------|
| `command` | `integer()` (16-bit) | (Optional) SPI command.  The low-order `command_len_bits` are written to the device. |
| `address` | `integer()` (64-bit) | (Optional) Device address.  The low-order `address_len_bits` are written to the device. |
| `write_data` | `binary()` | (Optional) Data to write |
| `write_bits` | `non_neg_integer()` | Number of bits to write from `write_data'.  If not included,then all bits will be written.
| `read_bits` | `non_neg_integer()` | Number of bits to read from the SPI device.  If not included, then the same number of bits will be read as were written.|

To write a blob of data to the SPI device, for example, you would use:

    WriteData = <<"some binary data">>,
    ok = spi:write(SPI, DeviceName, #{write_data => WriteData})

To write and simultaneously read back a blob of data to the SPI device, you would use:

    {ok, ReadData} = spi:write_read(SPI, DeviceName, #{write_data => WriteData})

The size of the returned data is the same as the size of the written data, unless otherwise specified by the `read_bits` field.

Use the `spi:close/1` function to close the SPI driver and free any resources in use by it, supplying a reference to a previously opened SPI driver instance.  Any references to the closed SPI instance are no longer valid after a successful call to this function.

    %% erlang
    ok = spi:close(SPI).

### UART

The `uart` module encapsulates functionality associated with the Universal Asynchronous Receiver/Transmitter (UART) interface supported on ESP32 devices.  Some devices, such as NMEA GPS receivers, make use of this interface for communicating with an ESP32.

> Note.  Information about the ESP32 UART interface can be found in the IDF SDK [UART Documentation](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/uart.html).

The AtomVM UART implementation uses the AtomVM Port mechanism and must be initialized using the `uart:open/2` function.

The first parameter indicates the ESP32 UART hardware interface.  Legal values are:

    "UART0" | "UART1" | "UART2"

The selection of the hardware interface dictates the default RX and TX pins on the ESP32:

| Port | RX pin | TX pin |
|-------------|--------|-------|
| `UART0` | GPIO_3 | GPIO_1 |
| `UART1` | GPIO_9 | GPIO_10 |
| `UART2` | GPIO_16 | GPIO_17 |

The second parameter is a properties list, containing the following elements:

| Key | Value Type | Required | Default Value | Description |
|-----|------------|----------|---------------|-------------|
| `speed` | `integer()` | no | 115200 | UART baud rate (bits/sec) |
| `data_bits` | `5 \| 6 \| 7 \| 8` | no | 8 | UART data bits |
| `stop_bits` | `1 \| 2` | no | 1 | UART stop bits |
| `flow_control` | `hardware \| software \| none` | no | `none` | Flow control |
| `parity` | `even \| odd \| none` | no | `none` | UART parity check |

For example,

    %% erlang
    UART = uart:open("UART0", [{speed, 9600}])

Once the port is opened, you can use the returned `UART` instance to read and write bytes to the attached device.

To read data from the UART channel, use the `uart:read/1` function.  The return value from this function is a binary:

    %% erlang
    Bin = uart:read(UART)

To write data to the UART channel, use the `uart_write/2` function.  The input data is any Erlang I/O list:

    %% erlang
    uart:write(UART, [<<"any">>, $d, $a, $t, $a, "goes", <<"here">>])

Consult your local device data sheet for information about the format of data to be read from or written to the UART channel.

To close the UART driver and free any resources in use by it, use the `uart:close/1` function, supplying a reference to the UART driver instance created via `uart:open/2`:

    %% erlang
    ok = uart:close(UART)

Once the UART driver is closed, any calls to `uart` functions using a reference to the UART driver instance should return with the value `{error, noproc}`.

### LED Control

The LED Control API can be used to drive LEDs, as well as generate PWM signals on GPIO pins.

The LEDC API is encapsulated in the `ledc` module and is a direct translation of the IDF SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC API</a>, with a natural mapping into Erlang.  This API is intended for users with complex use-cases, and who require low-level access to the LEDC APIs.

The `ledc.hrl` module should be used for common modes, channels, duty cycle resolutions, and so forth.

    %% erlang
    -include("ledc.hrl").

    ...

    %% create a 5khz timer
    SpeedMode = ?LEDC_HIGH_SPEED_MODE,
    Channel = ?LEDC_CHANNEL_0,
    ledc:timer_config([
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, 5000},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {timer_num, ?LEDC_TIMER_0}
    ]).

    %% bind pin 2 to this timer in a channel
    ledc:channel_config([
        {channel, Channel},
        {duty, 0},
        {gpio_num, 2},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER_0}
    ]).

    %% set the duty cycle to 0, and fade up to 16000 over 5 seconds
    ledc:set_duty(SpeedMode, Channel, 0).
    ledc:update_duty(SpeedMode, Channel).
    TargetDuty = 16000.
    FadeMs = 5000.
    ok = ledc:set_fade_with_time(SpeedMode, Channel, TargetDuty, FadeMs).

## Protocols

AtomVM supports network programming on devices that support it, specifically the ESP32 platform, with its built-in support for WIFI networking, and of course on the UNIX platform.

This section describes the network programming APIs available on AtomVM.

### Network (ESP32 only)

The ESP32 supports WiFi connectivity as part of the built-in WiFi and Bluetooth radio (and in most modules, an integrated antenna).  The WIFI radio on an ESP32 can operate in several modes:

* STA (Station) mode, whereby it acts as a member of an existing WiFi network;
* AP (Access Point) mode, whereby the ESP32 acts as an access point for other devices; or
* AP+STA mode, whereby the ESP32 behaves both as a member of an existing WiFi network and as an access point for other devices.

AtomVM supports these modes of operation via the `network` module, which is used to initialize the network and allow applications to respond to events within the network, such as a network disconnect or reconnect, or a connection to the ESP32 from another device.

> Note. Establishment and maintenance of network connections on roaming devices is a complex and subtle art, and the AtomVM `network` module is designed to accommodate as many IoT scenarios as possible.  This section of the programmer's guide is deliberately brief and only addresses the most basic scenarios.  For a more detailed explanation of the AtomVM `network` module and its many use-cases, please refer to the [AtomVM Network Programming Guide](./network-programming-guide.md).

#### STA mode

To connect your ESP32 to an existing WiFi network, use the `network:wait_for_sta/1,2` convenience function, which abstracts away some of the more complex details of ESP32 STA mode.

This function takes a station mode configuration, as a properties list, and optionally a timeout (in milliseconds) before connecting to the network should fail.  The default timeout, if unspecified, is 15 seconds.

The station mode configuration supports the following options:

| Key | Value Type | Required | Default Value | Description |
|-----|------------|----------|---------------|-------------|
| `ssid` | `string() \| binary()` | yes | - | WiFi AP SSID |
| `psk` | `string() \| binary()` | yes, if network is encrypted | - | WiFi AP password |
| `dhcp_hostname` | `string() \| binary()` | no | `atomvm-<MAC>` where `<MAC>` is the factory-assigned MAC-address of the device | DHCP hostname for the connecting device |

> Note.  The WiFi network to which you are connecting must support DHCP and IPv4.  IPv6 addressing is not yet supported on AtomVM.

If the ESP32 device connects to the specified network successfully, the device's assigned address, netmask, and gateway address will be returned in an `{ok, ...}` tuple; otherwise, an error is returned.

For example:

    %% erlang
    Config = [
        {ssid, <<"myssid">>},
        {psk,  <<"mypsk">>},
        {dhcp_hostname, <<"mydevice">>}
    ],
    case network:wait_for_sta(Config, 15000) of
        {ok, {Address, _Netmask, _Gateway}} ->
            io:format("Acquired IP address: ~p~n", [Address]);
        {error, Reason} ->
            io:format("Network initialization failed: ~p~n", [Reason])
    end

Once connected to a WiFi network, you may begin TCP or UDP networking, as described in more detail below.

For information about how to handle disconnections and reconnections to a WiFi network, see the [AtomVM Network Programming Guide](./network-programming-guide.md).

#### AP mode

To turn your ESP32 into an access point for other devices, you can use the `network:wait_for_ap/1,2` convenience function, which abstracts away some of the more complex details of ESP32 AP mode.  When the network is started, the ESP32 device will assign itself the `192.168.4.1` address.  Any devices that connect to the ESP32 will take addresses in the `192.168.4/24` network.

This function takes an access point mode configuration, as a properties list, and optionally a timeout (in milliseconds) before starting the network should fail.  The default timeout, if unspecified, is 15 seconds.

The access point mode configuration supports the following options:

| Key | Value Type | Required | Default Value | Description |
|-----|------------|----------|---------------|-------------|
| `ssid` | `string() \| binary()` | no | `atomvm-<MAC>` where `<MAC>` is the factory-assigned MAC-address of the device | WiFi AP SSID |
| `ssid_hidden` | `boolean()` | no | `false` | Whether the AP SSID should be hidden (i.e., not broadcast) |
| `psk` | `string() \| binary()` | yes, if network is encrypted | - | WiFi AP password.  Warning: If this option is not specified, the network will be an open network, to which anyone who knows the SSID can connect and which is not encrypted. |
| `ap_max_connections` | `non_neg_integer()` | no | `4` | Maximum number of devices that can be connected to this AP |

If the ESP32 device starts the AP network successfully, the `ok` atom is returned; otherwise, an error is returned.

For example:

    %% erlang
    Config = [
        {psk,  <<"mypsk">>}
    ],
    case network:wait_for_ap(Config, 15000) of
        ok ->
            io:format("AP network started at 192.168.4.1~n");
        {error, Reason} ->
            io:format("Network initialization failed: ~p~n", [Reason])
    end

Once the WiFi network is started, you may begin TCP or UDP networking, as described in more detail below.

For information about how to handle connections and disconnections from attached devices, see the [AtomVM Network Programming Guide](./network-programming-guide.md).

#### STA+AP mode

For information about how to run the AtomVM network in STA and AP mode simultaneously, see the [AtomVM Network Programming Guide](./network-programming-guide.md).

#### SNTP

For information about how to use SNTP to synchronize the clock on your device, see the [AtomVM Network Programming Guide](./network-programming-guide.md).

### UDP

AtomVM supports network programming using the User Datagram Protocol (UDP) via the `gen_udp` module.  This modules obeys the syntax and semantics of the Erlang/OTP [`gen_udp`](https://erlang.org/doc/man/gen_udp.html) interface.

> Note.  Not all of the Erlang/OTP `gen_udp` functionality is implemented in AtomVM.  For details, consults the AtomVM API documentation.

To open a UDP port, use the `gen_udp:open/1,2` function.  Supply a port number, and if your application plans to receive UDP messages, specify that the port is active via the `{active, true}` property in the optional properties list.

For example:

    %% erlang
    Port = 44404,
    case gen_udp:open(Port, [{active, true}, binary]) of
        {ok, Socket} ->
            {ok, SockName} = inet:sockname(Socket)
            io:format("Opened UDP socket on ~p.~n", [SockName])
        Error ->
            io:format("An error occurred opening UDP socket: ~p~n", [Error])
    end

If the port is active, you can receive UDP messages in your application.  They will be delivered as a 5-tuple, starting with the `udp` atom, and containing the socket, address and port from which the message was sent, as well as the datagram packet, itself, as a list (by default) or a binary. To choose the format, pass `list` or `binary` in options, as with Erlang/OTP.

    %% erlang
    receive
        {udp, _Socket, Address, Port, Packet} ->
            io:format("Received UDP packet ~p from address ~p port ~p~n", [Packet, Address, Port)])
    end,

With a reference to a UDP `Socket`, you can send messages to a target UDP endpoint using the `gen_udp:send/4` function.  Specify the UDP socket returned from `gen_udp:open/1,2`, the address (as a 4-tuple of octets), port number, and the datagram packet to send:

    Packet = <<":アトムＶＭ">>,
    Address = {192, 168, 1, 101},
    Port = 44404,
    case gen_udp:send(Socket, Address, Port, Packet) of
        ok ->
            io:format("Sent ~p~n", [Packet]);
        Error ->
            io:format("An error occurred sending a packet: ~p~n", [Error])
    end

> Note.  IPv6 networking is not currently supported in AtomVM.

### TCP

AtomVM supports network programming using the Transport Connection Protocol (TCP) via the `gen_tcp` module.  This modules obeys the syntax and semantics of the Erlang/OTP [`gen_tcp`](https://erlang.org/doc/man/gen_tcp.html) interface.

> Note.  Not all of the Erlang/OTP `gen_tcp` functionality is implemented in AtomVM.  For details, consults the AtomVM API documentation.


#### Server-side TCP

Server side TCP requires opening a listening socket, and then waiting to accept connections from remote clients.  Once a connection is established, the application may then use a combination of sending and receiving packets over the established connection to or from the remote client.

> Note.  Programming TCP on the server-side using the `gen_tcp` interface is a subtle art, and this portion of the documentation will not go into all of the design choices available when designing a TCP application.

Start by opening a listening socket using the `gen_tcp:listen/2` function.  Specify the port number on which the TCP server should be listening:

    %% erlang
    case gen_tcp:listen(44405, []) of
        {ok, ListenSocket} ->
            {ok, SockName} = inet:sockname(Socket),
            io:format("Listening for connections at address ~p.~n", [SockName]),
            spawn(fun() -> accept(ListenSocket) end);
        Error ->
            io:format("An error occurred listening: ~p~n", [Error])
    end.

In this particular example, the server will spawn a new process to wait to accept a connection from a remote client, by calling the `gen_tcp:accept/1` function, passing in a reference to the listening socket.  This function will block until a client has established a connection with the server.

When a client connects, the function will return a tuple `{ok, Socket}`, where `Socket` is a reference to the connection between the client and server:

    %% erlang
    accept(ListenSocket) ->
        io:format("Waiting to accept connection...~n"),
        case gen_tcp:accept(ListenSocket) of
            {ok, Socket} ->
                {ok, SockName} = inet:sockname(Socket),
                {ok, Peername} = inet:peername(Socket),
                io:format("Accepted connection.  local: ~p peer: ~p~n", [SockName, Peername]),
                spawn(fun() -> accept(ListenSocket) end),
                echo();
            Error ->
                io:format("An error occurred accepting connection: ~p~n", [Error])
        end.

> Note that immediately after accepting a connection, this example code will spawn a new process to accept any new connections from other clients.

The socket returned from `gen_tcp:accept/1` can then be used to send and receive messages to the connected client:

    %% erlang
    echo() ->
        io:format("Waiting to receive data...~n"),
        receive
            {tcp_closed, _Socket} ->
                io:format("Connection closed.~n"),
                ok;
            {tcp, Socket, Packet} ->
                {ok, Peername} = inet:peername(Socket),
                io:format("Received packet ~p from ~p.  Echoing back...~n", [Packet, Peername]),
                gen_tcp:send(Socket, Packet),
                echo()
        end.

In this case, the server program will continuosuly echo the received input back to the client, until the client closes the connection.

For more information about the `gen_tcp` server interface, consult the AtomVM [API Reference Documentation](./api-reference-documentation.md).

#### Client-side TCP

Client side TCP requires establishing a connection with an endpoint, and then using a combination of sending and receiving packets over the established connection.

Start by opening a connection to another TCP endpoint using the `gen_tcp:connect/3` function.  Supply the address and port of the TCP endpoint.

For example:

    %% erlang
    Address = {192, 168, 1, 101},
    Port = 44405,
    case gen_tcp:connect(Address, Port, []) of
        {ok, Socket} ->
            {ok, SockName} = inet:sockname(Socket),
            {ok, Peername} = inet:peername(Socket),
            io:format("Connected to ~p from ~p~n", [Peername, SockName]);
        Error ->
            io:format("An error occurred connecting: ~p~n", [Error])
    end

Once a connection is established, you can use a combination of

    %% erlang
    SendPacket = <<":アトムＶＭ">>,
    case gen_tcp:send(Socket, SendPacket) of
        ok ->
            receive
                {tcp_closed, _Socket} ->
                    io:format("Connection closed.~n"),
                    ok;
                {tcp, _Socket, ReceivedPacket} ->
                    {ok, Peername} = inet:peername(Socket),
                    io:format("Received ~p from ~p~n", [ReceivedPacket, Peername])
            end;
        Error ->
            io:format("An error occurred sending a packet: ~p~n", [Error])
    end.

For more information about the `gen_tcp` client interface, consults the AtomVM API documentation.

## Socket Programming

AtomVM supports a subset of the OTP [`socket`](https://www.erlang.org/doc/man/socket.html) interface, giving users more fine-grained control in socket programming.

The OTP socket APIs are relatively new (they were introduced in OTP 22 and have seen revisions in OTP 24).  These APIs broadly mirror the [BSD Sockets](https://en.wikipedia.org/wiki/Berkeley_sockets) API, and should be familiar to most programmers who have had to work with low-level operating system networking interfaces.  AtomVM supports a strict subset of the OTP APIs.  Future versions of AtomVM may add additional coverage of these APIs.

The following types are relevant to this interface and are referenced in the remainder of this section:

    -type domain() :: inet.
    -type type() :: stream | dgram.
    -type protocol() :: tcp | udp.
    -type socket() :: any().
    -type sockaddr() :: sockaddr_in().
    -type sockaddr_in() :: #{
        family := inet,
        port := port_number(),
        addr := any | loopback | in_addr()
    }.
    -type in_addr() :: {0..255, 0..255, 0..255, 0..255}.
    -type port_number() :: 0..65535.
    -type socket_option() :: {socket, reuseaddr} | {socket, linger}.

Create a socket using the `socket:open/3` function, providing a domain, type, and protocol.  Currently, AtomVM supports the `inet` domain, `stream` and `dgram` types, and `tcp` and `udp` protocols.

For example:

    %% erlang
    {ok, Socket} = socket:open(inet, stream, tcp),

### Server-side TCP Socket Programming

To program using sockets on the server side, you can bind an opened socket to an address and port number using the `socket:bind/2` function, supplying a map that specifies the address and port number.

This map may contain the following entries:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `family` | `inet` | | The address family.  (Currently, only `inet` is supported) |
|  `addr` | `in_addr()` \| `any` \| `loopback` | The address to which to bind.  The `any` value will bind the socket to all interfaces on the device.  The `loopback` value will bind the socket to the loopback interface on the device. |
|  `port` | `port_number()` |  | The port to which to bind the socket.  If no port is specified, the operating system will choose a port for the user. |

For example:

    %% erlang
    PortNumber = 8080,
    ok = socket:bind(Socket, #{family => inet, addr => any, port => PortNumber}),

To listen for connections, use the `socket:listen/1` function:

    %% erlang
    ok = socket:listen(Socket),

Once your socket is listening on an interface and port, you can wait to accept a connection from an incoming client using the `socket:accept/1` function.

This function will block the current execution context (i.e., Erlang process) until a client establishes a TCP connection with the server:

    %% erlang
    {ok, ConnectedSocket} = socket:accept(Socket),

> Note.  Many applications will spawn processes to listen for socket connections, so that the main execution context of your application is not blocked.

### Client-side TCP Socket Programming

To program using sockets on the client side, you can connect an opened socket to an address and port number using the `socket:connect/2` function, supplying a map that specifies the address and port number.

This map may contain the following entries:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `family` | `inet` | | The address family.  (Currently, only `inet` is supported) |
|  `addr` | `in_addr()` \| `loopback` | | The address to which to connect. The `loopback` value will connect the socket to the loopback interface on the device. |
|  `port` | `port_num()` |  | The port to which to connect the socket. |

    %% erlang
    ok = socket:connect(Socket, #{family => inet, addr => loopback, port => 44404})

### Sending and Receiving Data

Once you have a connected socket (either via `socket:connect/2` or `socket:accept/1`), you can send and receive data on that socket using the `socket:send/2` and `socket:recv/1` functions.  Like the  `socket:accept/1` function, these functions will block until data is sent to a connected peer (or until the data is written to operating system buffers) or received from a connected peer.

The `socket:send/2` function can take a binary blob of data or an io-list, containing binary data.

For example, a process that receives data and echos it back to the connected peer might be implemented as follows:

    %% erlang
    case socket:recv(ConnectedSocket) of
        {ok, Data} ->
            case socket:send(ConnectedSocket, Data) of
                ok ->
                    io:format("All data was sent~n");
                {ok, Rest} ->
                    io:format("Some data was sent.  Remaining: ~p~n", [Rest]);
                {error, Reason} ->
                    io:format("An error occurred sending data: ~p~n", [Reason])
            end;
        {error, closed} ->
            io:format("Connection closed.~n");
        {error, Reason} ->
            io:format("An error occurred waiting on a connected socket: ~p~n", [Reason])
    end.

The `socket:recv/1` function will block the current process until a packet has arrived or until the local or remote socket has been closed, or some other error occurs.

Note that the `socket:send/2` function may return `ok` if all of the data has been sent, or `{ok, Rest}`, where `Rest` is the remaining part of the data that was not sent to the operating system.  If the supplied input to `socket:send/2` is an io-list, then the `Rest` will be a binary containing the rest of the data in the io-list.

### Getting Information about Connected Sockets

You can obtain information about connected sockets using the `socket:sockname/1` and `socket:peername/1` functions.  Supply the connected socket as a parameter.  The address and port are returned in a map structure

For example:

    %% erlang
    {ok, #{addr := LocalAddress, port := LocalPort}} = socket:sockname(ConnectedSocket),
    {ok, #{addr := PeerAddress, port := PeerPort}} = socket:peername(ConnectedSocket),

### Closing and Shutting down Sockets

Use the `socket:close/1` function to close a connected socket:

    %% erlang
    ok = socket:close(ConnectedSocket)

> Note.  Data that has been buffered by the operating system may not be delivered, when a socket is closed via the `close/1` operation.

For a more controlled way to close full-duplex connected sockets, use the `socket:shutdown/2` function.  Provide the atom `read` if you only want to shut down the reads on the socket, `write` if you want to shut down writes on the socket, or `read_write` to shut down both reads and writes on a socket.  Subsequent reads or writes on the socket will result in an `einval` error on the calls, depending on how the socket has been shut down.

For example:

    %% erlang
    ok = socket:shutdown(Socket, read_write)

### Setting Socket Options

You can set options on a socket using the `socket:setopt/3` function.  This function takes an opened socket, a key, and a value, and returns `ok` if setting the option succeeded.

Currently, the following options are supported:

| Option Key | Option Value | Description |
|------------|--------------|-------------|
| `{socket, reuseaddr}` | `boolean()` | Sets `SO_REUSEADDR` on the socket. |
| `{socket, linger}` | `#{onoff => boolean(), linger => non_neg_integer()}` | Sets `SO_LINGER` on the socekt. |
| `{otp, rcvbuf}` | `non_neg_integer()` | Sets the default buffer size (in bytes) on receive calls.  This value is only used if the `Length` parameter of the `socket:recv` family of functions has the value `0`; otherwise, the specified non-zero length in the `socket:recv` takes precendence.  Note that the OTP option value `default` is not currently supported.|

For example:

    %% erlang
    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:setopt(Socket, {socket, linger}, #{onoff => true, linger => 0}),
    ok = socket:setopt(Socket, {otp, rcvbuf}, 1024),

### UDP Socket Programming

You can use the `socket` interface to send and receive messages over the User Datagram Protocol (UDP), in addition to TCP.

To use UDP sockets, open a socket using the `dgram` type and `udp` protocol.

For example:

    %% erlang
    {ok, Socket} = socket:open(inet, dgram, udp)

To listen for UDP connections, use the `socket:bind/2` function, as described above.

For example:

    %% erlang
    PortNumber = 512,
    ok = socket:bind(Socket, #{family => inet, addr => any, port => PortNumber}),

Use the `socket:recvfrom/1` function to receive UDP packets from clients on your network.  When a packet arrives, this function will return the received packet, as well as the address of the client that sent the packet.

For example:

    %% erlang
    case socket:recvfrom(dSocket) of
        {ok, {From, Packet}} ->
            io:format("Received packet ~p from ~p~n", [Packet, From]);
        {error, Reason} ->
            io:format("Error on recvfrom: ~p~n", [Reason])
    end;

> Note.  The `socket:recvfrom/1` function will block the current process until a packet has arrived or until the local or remote socket has been closed, or some other error occurs.

Use the `socket:sendto/3` function to send UDP packets to a specific destination.  Specify the socket, data, and destination address you would like the packet to be delivered to.

For example:

    %% erlang
    Dest = #{family => inet, addr => loopback, port => 512},
    case socket:sendto(Socket, Data, Dest) of
        ok ->
            io:format("Send packet ~p to ~p.~n", [Data, Dest]);
        {ok, Rest} ->
            io:format("Send packet ~p to ~p.  Remaining: ~p~n", [Data, Dest, Rest]);
        {error, Reason} ->
            io:format("An error occurred sending a packet: ~p~n", [Reason])
    end

Close a UDP socket just as you would a TCP socket, as described above.

### Miscellaneous Networking APIs

You can retrieve information about hostnames and services using the `net:getaddrinfo/1` and  `net:getaddrinfo/2` functions.  The return value is a list of maps each of which contains address information about the host, including its family (`inet`), protocol (`tcp` or `udp`), type (`stream` or `dgram`), and the address, currently an IPv4 tuple.

> Note.  Currently, the `net:getaddrinfo/1,2` functions only supports reporting of IPv4 addresses.

For example:

    %% erlang
    {ok, AddrInfos} = net:getaddrinfo("www.atomvm.net"),

    lists:foreach(
        fun(AddrInfo) ->
            #{
                family := Family,
                protocol := Protocol,
                type := Type,
                address := Address
            } = AddrInfo,

            io:format("family: ~p prototcol: ~p type: ~p address: ~p", [Family, Protocol, Type, Address])

        end,
        AddrInfos
    ),

The `host` parameter can be a domain name (typically) or a dotted pair IPv4 address.

The returned map contains the network family (currently, only `inet` is supported), the protocol, type, and address of the host.

The address is itself a map, containing the family, port and IPv4 address of the requested host, e.g.,

    #{family => inet, port => 0, addr => {192, 168, 212, 153}}

> Note.  The [OTP documentation](https://www.erlang.org/doc/man/net#type-address_info) states that the address is returned under the `address` key in the address info map.  However, OTP appears to use `addr` as the key.  For compatibility with OTP 22 ff., AtomVM supports both the `address` and `addr` keys in this map (they reference the same inner map).

If you want to narrow the information you get back to a specific service type, you can specify a service name or port number (as a string value) as the second parameter:

    %% erlang
    {ok, AddrInfos} = net:getaddrinfo("www.atomvm.net", "https"),
    ...

Service names are well-known identifiers on the internet, but they may vary from operating system to operating system.  See the `services(3)` man pages for more information.

> Note.  Narrowing results via the service parameter is not supported on all platforms.  In the case where it is not supported, AtomVM will resort to retrying the request without the service parameter.
