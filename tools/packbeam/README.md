<!--
 Copyright 2018 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `PackBEAM`

The `PackBEAM` tool is used to pack a collection of BEAM files into a single AtomVM (`.avm`) file, or to list the contents of a previously created AVM file.

Packing multiple BEAM files into a single AVM file allows you to load multiple BEAM modules into AtomVM, instead of just running a single module.

Two types of AVM file may be created:

* Runnable AVM files, suitable for flashing or supplying to the AtomVM command;
* Archive AVM files, which are used to aggregate collections of BEAM files into libraries, for subsequent use when creating runnable AVM files.

The only difference between runnable and archive AVM files is that the first module in a runnable AVM file contains the exported function `start/0`, which is the initial entry point for the AtomVM program.  Archive AVM files do not require a `start/0` entrypoint.

Archive AVM files are typically used to create AtomVM "libraries", which can then be used as inputs when creating a runnable AVM.

When creating an AVM file, you must specify:

* Name name of the output AVM file, first in the list, followed by
* a sequence of compiled BEAM files (as compiled by the `erlc` compiler), or previously created AVM files.
    * If you are creating a runnable AVM, the first file in this sequence must either be a BEAM file that contain an exported `start/0` function or an AVM file whose first module contain an exported `start/0` function.

When listing modules in an AVM file, just specify the AVM file to list its included modules.

## Usage

    Usage: PackBEAM <options>
    Options:
        -h                                                Print this help menu.
        -l <input-avm-file>                               List the contents of an AVM file.
        [-a] <output-avm-file> <input-beam-or-avm-file>+  Create an AVM file (archive if -a specified).

## Examples

Consider the three modules `mail.erl`, `foo.erl`, and `bar.erl`, defined as follows:

`main.erl`:

    -module(main).
    -export([start/0]).
    
    start() ->
        hello:say_hello(world).

`hello.erl`:

    -module(hello).
    -export([say_hello/1]).
    
    say_hello(Term) ->
        greet:say(hello, Term).

`greet.erl`:

    -module(greet).
    -export([say/2]).
    
    say(Greeting, Entity) ->
        erlang:display({Greeting, Entity}).

You can compile these modules using `erlc`:

    shell$ erlc main.erl hello.erl greet.erl

And then pack the second two beam files into an archive using the `-a flag` as follows:

    shell$ PackBEAM -a lib.avm hello.beam greet.beam

This will create the archive AVM `lib.avm`.

You can list the contents of this AVM via the `-l` flag:

    shell$ PackBEAM -l lib.avm 
    greet.beam
    hello.beam

Note that this AVM file is not runnable:

    shell$ AtomVM lib.avm
    lib.avm cannot be started.

To create a runnable AVM file, specify the output AVM file, the beam file that contains the `start/0` function, and a sequence of 1 or more BEAM files or AVM files.  For example,

    shell$ PackBEAM -a main.avm main.beam lib.avm

This will create the output file `main.avm`, which you can run through AtomVM, e.g.,

    shell$ AtomVM main.avm
    {hello,world}
    Return value: 3b

You can list the modules in an AVM file via the `-l` flag:

    shell$ PackBEAM -l main.avm
    main.beam *
    hello.beam 
    greet.beam 

The module containing the `start/0` entrypoint will contain an asterisk (`*`).

## Additional Notes

* `PackBeam` does not require that BEAM or AVM files have any specific file suffix.  You may use any suffix you like, though `.beam` and `.avm` are conventional.
* `PackBeam` makes no effort to find beam files with the `start/0` function, and order them first.  In order to create a runnable AVM file, the first input file must be either a BEAM file with an exported `start/0` function, or another AVM file whose first module has an exported `start/0` function.
* `PackBeam` makes no effort the remove duplicates modules that are packed.  AtomVM will only use the first module by name in an AVM files, so adding duplicate modules has no effect on the runtime behavior of the output AVM file.
* Because `PackBeam` uses positional arguments when creating AVM files, an attempt to specify a BEAM file (or other non-AVM file) as output, if it already exists, will result in a failure.  This is to prevent accidental omission of an output AVM file as the first argument to `PackBeam` when creating AVM files.
