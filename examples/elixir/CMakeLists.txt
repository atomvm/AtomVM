##
## Copyright (c) 2019 Riccardo Binetti <rbino@gmx.com>
##

project(examples_elixir)

include(BuildElixir)

add_subdirectory(esp32)
add_subdirectory(stm32)

pack_runnable(HelloWorld HelloWorld estdlib eavmlib exavmlib)
