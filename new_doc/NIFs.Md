NIFs
====

AtomVM implements a number of functions as builtins written in C and compiled into the VM.
NIFs do not require any additional module so they can be safely used by freestanding modules.
Following functions are currently implemented as NIFs:

* binary:at/2
* binary:first/1
* binary:last/1
* binary:part/3
* binary:split/2
* erlang:++/2
* erlang:atom_to_binary/2
* erlang:atom_to_list/1
* erlang:binary_to_atom/2
* erlang:binary_to_existing_atom/2
* erlang:binary_to_integer/1
* erlang:binary_to_list/1
* erlang:delete_element/2
* erlang:display/1
* erlang:insert_element/3
* erlang:integer_to_binary/1
* erlang:integer_to_list/1
* erlang:is_process_alive/1
* erlang:list_to_atom/1
* erlang:list_to_binary/1
* erlang:list_to_existing_atom/1
* erlang:list_to_integer/1
* erlang:make_ref/0
* erlang:make_tuple/2
* erlang:open_port/2
* erlang:processes/0
* erlang:process_flag/3
* erlang:process_info/2
* erlang:register/2
* erlang:send/2
* erlang:setelement/3
* erlang:spawn/1
* erlang:spawn/3
* erlang:spawn_opt/2
* erlang:spawn_opt/4
* erlang:system_info/1
* erlang:system_time/1
* erlang:timestamp/0
* erlang:tuple_to_list/1
* erlang:universaltime/0
* erlang:whereis/1
* erts_debug:flat_size/1

Notes
-----
* `erlang:binary_to_integer/1` may raise overflow errors
* `erlang:binary_to_atom/2` supports only latin1
* `erlang:list_to_integer/1` may raise overflow errors
* `erlang:system_time/1` supports only second time unit
