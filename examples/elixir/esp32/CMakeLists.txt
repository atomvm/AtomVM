##
## Copyright (c) 2019 Riccardo Binetti <rbino@gmx.com>
##

project(examples_elixir_esp32)

include(BuildElixir)

pack_runnable(Blink Blink estdlib eavmlib exavmlib)
