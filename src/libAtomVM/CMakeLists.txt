cmake_minimum_required (VERSION 2.6)
project (libAtomVM)

find_package(ZLIB REQUIRED)
if (ZLIB_FOUND)
    add_definitions(-DWITH_ZLIB)
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

add_library(libAtomVM atom.c bif.c debug.c externalterm.c globalcontext.c iff.c mailbox.c mapped_file.c scheduler.c sys.c context.c module.c)
add_dependencies(libAtomVM generated)
target_link_libraries(libAtomVM ${ZLIB_LIBRARIES})
set_property(TARGET libAtomVM PROPERTY C_STANDARD 99)
