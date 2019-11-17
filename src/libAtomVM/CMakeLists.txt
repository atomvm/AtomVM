cmake_minimum_required (VERSION 2.6)
project (libAtomVM)

include_directories(${CMAKE_CURRENT_BINARY_DIR})

if(${CMAKE_GENERATOR} STREQUAL "Xcode")
    set(HEADER_FILES
        atom.h
        atomshashtable.h
        avmpack.h
        bif.h
        context.h
        ccontext.h
        debug.h
        defaultatoms.h
        exportedfunction.h
        externalterm.h
        globalcontext.h
        iff.h
        interop.h
        list.h
        linkedlist.h
        mailbox.h
        memory.h
        module.h
        opcodesswitch.h
        overflow_helpers.h
        network.h
        network_driver.h
        nifs.h
        platform_nifs.h
        port.h
        scheduler.h
        socket.h
        socket_driver.h
        sys.h
        term_typedef.h
        term.h
        timer_wheel.h
        trace.h
        utils.h
        valueshashtable.h
        version.h
    )
endif()
set(SOURCE_FILES
    atom.c
    atomshashtable.c
    avmpack.c
    bif.c
    context.c
    debug.c
    defaultatoms.c
    externalterm.c
    globalcontext.c
    iff.c
    interop.c
    mailbox.c
    memory.c
    module.c
    network.c
    nifs.c
    port.c
    scheduler.c
    socket.c
    term.c
    timer_wheel.c
    valueshashtable.c
)
if (${CMAKE_SYSTEM_NAME} STREQUAL "Darwin" OR ${CMAKE_SYSTEM_NAME} STREQUAL "Linux" OR ${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
    find_package(ZLIB)
    if (ZLIB_FOUND)
        add_definitions(-DWITH_ZLIB)
    else(ZLIB_FOUND)
        set(ZLIB_LIBRARIES "")
    endif(ZLIB_FOUND)
else()
    set(ZLIB_LIBRARIES "")
endif()

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

if (NOT AVM_DISABLE_FP)
    set(MATH_LIB "m")
endif()

gperf_generate(${CMAKE_CURRENT_SOURCE_DIR}/bifs.gperf bifs_hash.h)
gperf_generate(${CMAKE_CURRENT_SOURCE_DIR}/nifs.gperf nifs_hash.h)

add_custom_target(generated DEPENDS bifs_hash.h)
add_custom_target(generated-nifs-hash DEPENDS nifs_hash.h)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/version.h.in version.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_library(libAtomVM ${SOURCE_FILES} ${HEADER_FILES})
add_dependencies(libAtomVM generated generated-nifs-hash)
target_link_libraries(libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX} ${ZLIB_LIBRARIES} ${MATH_LIB})
set_property(TARGET libAtomVM PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set_target_properties(libAtomVM PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
