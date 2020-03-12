##
## Copyright (c) 2019 Riccardo Binetti <rbino@gmx.com>
##

project(exavmlib)

include(BuildElixir)

set(ELIXIR_MODULES
    Console
    GPIO
    Access
    Enum
    List
    Keyword
    Kernel
    Process
    Tuple
)

pack_archive(exavmlib ${ELIXIR_MODULES})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/exavmlib.avm DESTINATION lib/AtomVM/ebin)
