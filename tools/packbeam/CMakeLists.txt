cmake_minimum_required (VERSION 2.6)
project (PackBEAM)

set(PACKBEAM_SOURCES
    packbeam.c
)

find_package(ZLIB)
if (ZLIB_FOUND)
    add_definitions(-DWITH_ZLIB)
else()
    set(ZLIB_LIBRARIES "")
endif (ZLIB_FOUND)

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

add_executable(PackBEAM packbeam.c)
target_link_libraries(PackBEAM ${ZLIB_LIBRARIES})
set_property(TARGET libAtomVM PROPERTY C_STANDARD 99)
