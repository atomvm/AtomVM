##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(examples_erlang)

include(BuildErlang)

add_subdirectory(esp32)

pack_runnable(hello_world hello_world eavmlib)
pack_runnable(udp_server udp_server estdlib eavmlib)
pack_runnable(udp_client udp_client estdlib eavmlib)
pack_runnable(server server estdlib eavmlib)
