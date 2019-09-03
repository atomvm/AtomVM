BIFs
====

AtomVM implements a number of BIFs, which are required to accomplish basic operations.
BIFs are builtins, so no additional modules are required and they can be safely used by freestanding modules.
Following BIFs are currently supported:

* erlang:self/0
* erlang:length/1
* erlang:byte_size/1
* erlang:is_atom/1
* erlang:is_binary/1
* erlang:is_integer/1
* erlang:is_list/1
* erlang:is_number/1
* erlang:is_pid/1
* erlang:is_reference/1
* erlang:is_tuple/1
* erlang:not/1
* erlang:and/2
* erlang:or/2
* erlang:xor/2
* erlang:==/2
* erlang:/=/2
* erlang:=:=/2
* erlang:=/=/2
* erlang:>/2
* erlang:</2
* erlang:=</2
* erlang:>=/2
* erlang:+/2
* erlang:-/2
* erlang:*/2
* erlang:div/2
* erlang:rem/2
* erlang:-/1
* erlang:abs/1
* erlang:bor/2
* erlang:band/2
* erlang:bxor/2
* erlang:bsl/2
* erlang:bsr/2
* erlang:bnot/1
* erlang:hd/1
* erlang:tl/1
* erlang:element/2
* erlang:tuple_size/1

* erlang:+/2
* erlang:-/2
* erlang:*/2
* erlang:div/2
* erlang:rem/2
* erlang:-/1
* erlang:abs/1

Notes
-----
* `erlang:+/2`: may raise `overflow` errors
* `erlang:-/2`: may raise `overflow` errors
* `erlang:*/2`: may raise `overflow` errors
* `erlang:div/2`: may raise `overflow` errors
* `erlang:rem/2`: may raise `overflow` errors
* `erlang:-/1`: may raise `overflow` errors
* `erlang:abs/1`: may raise `overflow` errors
