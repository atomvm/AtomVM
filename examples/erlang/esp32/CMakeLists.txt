##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(examples_erlang_esp32)

include(BuildErlang)

pack_runnable(blink blink eavmlib estdlib)
pack_runnable(sta_network sta_network eavmlib estdlib)
