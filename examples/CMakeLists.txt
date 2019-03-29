##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##
project(libs)

add_subdirectory(erlang)
if (Elixir_FOUND)
    add_subdirectory(elixir)
else()
    message("Unable to find elixirc -- skipping Elixir examples")
endif()
