cmake_minimum_required (VERSION 2.6)
project (AtomVM)

function(gperf_generate input output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND gperf -t ${input} > ${output}
        DEPENDS ${input}
        COMMENT "Hashing ${input}"
    )
endfunction()

include_directories(${CMAKE_CURRENT_BINARY_DIR} src/)
gperf_generate(${CMAKE_CURRENT_SOURCE_DIR}/src/bifs.gperf bifs_hash.h)

add_custom_target(generated DEPENDS bifs_hash.h)

add_executable(AtomVM src/main.c src/atom.c src/bif.c src/debug.c src/iff.c src/Module.c)
add_dependencies(AtomVM generated)
set_property(TARGET AtomVM PROPERTY C_STANDARD 99)
