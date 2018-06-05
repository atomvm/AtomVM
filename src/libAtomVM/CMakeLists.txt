cmake_minimum_required (VERSION 2.6)
project (libAtomVM)

set(LIBATOMVM_SOURCES
    atom.c
    atomshashtable.c
    avmpack.c
    bif.c
    context.c
    debug.c
    externalterm.c
    globalcontext.c
    iff.c
    mailbox.c
    memory.c
    module.c
    nifs.c
    scheduler.c
    valueshashtable.c
)

find_package(ZLIB)
if (ZLIB_FOUND)
    add_definitions(-DWITH_ZLIB)
else()
    set(ZLIB_LIBRARIES "")
endif (ZLIB_FOUND)

function(gperf_generate input output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND gperf -t ${input} > ${output}
        DEPENDS ${input}
        COMMENT "Hashing ${input}"
    )
endfunction()

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

gperf_generate(${CMAKE_CURRENT_SOURCE_DIR}/bifs.gperf bifs_hash.h)

add_custom_target(generated DEPENDS bifs_hash.h)

add_library(libAtomVM ${LIBATOMVM_SOURCES})
add_dependencies(libAtomVM generated)
target_link_libraries(libAtomVM ${ZLIB_LIBRARIES})
set_property(TARGET libAtomVM PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set_target_properties(libAtomVM PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
