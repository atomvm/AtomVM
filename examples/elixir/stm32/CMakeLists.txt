##
## Copyright (c) 2019 Riccardo Binetti <rbino@gmx.com>
##

project(examples_elixir_stm32)

include(BuildElixir)

pack_runnable(MultiBlink MultiBlink estdlib eavmlib exavmlib)
