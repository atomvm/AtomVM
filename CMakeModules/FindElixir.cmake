##
## Copyright (c) 2019 Riccardo Binetti <rbino@gmx.com>
##

find_program(ELIXIRC_PATH elixirc)

if (ELIXIRC_PATH)
    set(Elixir_FOUND TRUE)
elseif(Elixir_FIND_REQUIRED)
    message(FATAL_ERROR "Elixir compiler not found")
endif()
