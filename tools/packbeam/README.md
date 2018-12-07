
# `PackBEAM`

The `PackBEAM` tool is used to pack a collection of BEAM files into a single AtomVM (`.avm`) file, or to list the contents of a previously created AVM file.

Packing multiple BEAM files into a single AVM file allows you to load multiple BEAM modules into AtomVM, instead of just running a single module.

> Note.  Future versions of this tool will allow you to pack multiple AVM files, as "libraries" to include into a single output AVM file.

When creating an AVM file, you must specify:

* Name name of the output AVM file, first in the list, followed by:
* a sequence of compiled BEAM files (as compiled by the `erlc` compiler).  The first BEAM file in this list is required and must contain an exported function called `start` with arity 0.

When listing modules in an AVM file, just specify the AVM file to list its included modules.

## Usage

    Usage: PackBEAM [-h] [-l] <avm-file> [<options>]
        -h    Print this help menu.
        -l    List modules in an AVM file.
              Without the -l flag, <options> must be: <beam-file> [<beam-file>]*, where
              <beam-file> is a compiled ERTS beam file, and
              the first beam file in the list contains an exported start/0 function.

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

And then pack them as follows:

    shell$ PackBEAM main.avm main.beam hello.beam greet.beam

This will create the output file `main.avm`, which you can run through AtomVM, e.g.,

    shell$ AtomVM main.avm
    {hello,world}
    Return value: 3b

You can list the contents of an AVM file via the `-l` flag:

    shell$ PackBEAM -l main.avm
    main.beam *
    hello.beam 
    greet.beam 

The file containing the `start/0` entrypoint will contain an asterisk (`*`).
