##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(examples_erlang_esp32)

include(BuildErlang)

pack_runnable(blink blink eavmlib estdlib)
pack_runnable(setup_network setup_network eavmlib estdlib)
