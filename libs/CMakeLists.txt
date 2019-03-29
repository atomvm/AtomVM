##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##
project(libs)

add_subdirectory(estdlib)
add_subdirectory(eavmlib)
add_subdirectory(etest)
if (Elixir_FOUND)
    add_subdirectory(exavmlib)
else()
    message("Unable to find elixirc -- skipping Elixir libs")
endif()
