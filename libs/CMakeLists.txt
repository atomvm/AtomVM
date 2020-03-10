##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##
project(libs)

add_subdirectory(estdlib/src)
add_subdirectory(eavmlib/src)
add_subdirectory(etest/src)
if (Elixir_FOUND)
    add_subdirectory(exavmlib/src)
else()
    message("Unable to find elixirc -- skipping Elixir libs")
endif()
