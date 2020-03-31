##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(examples_erlang)

include(BuildErlang)

add_subdirectory(esp32)

pack_runnable(hello_world hello_world eavmlib)
pack_runnable(udp_server udp_server estdlib eavmlib)
pack_runnable(udp_client udp_client estdlib eavmlib)
pack_runnable(tcp_client tcp_client estdlib eavmlib)
pack_runnable(tcp_server tcp_server estdlib eavmlib)
pack_runnable(hello_world_server hello_world_server estdlib eavmlib)
pack_runnable(system_info_server system_info_server estdlib eavmlib)
pack_runnable(code_lock code_lock estdlib eavmlib)
pack_runnable(mqtt_client mqtt_client estdlib eavmlib)
