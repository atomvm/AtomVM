cmake_minimum_required (VERSION 2.6)
project (libAtomVM)

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

add_library(libAtomVM atom.c bif.c debug.c globalcontext.c iff.c mapped_file.c scheduler.c sys.c Context.c Module.c)
add_dependencies(libAtomVM generated)
set_property(TARGET libAtomVM PROPERTY C_STANDARD 99)
