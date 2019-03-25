##
## Copyright (c) 2019 Riccardo Binetti <rbino@gmx.com>
##

macro(pack_archive avm_name)

    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams && elixirc -o ${CMAKE_CURRENT_BINARY_DIR}/beams ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            COMMENT "Compiling ${module_name}.ex"
            VERBATIM
        )
        set(BEAMS ${BEAMS} ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}_beams ALL
        DEPENDS ${BEAMS}
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}_beams PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${avm_name}.avm ${BEAMS}
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_beams PackBEAM)

endmacro()

macro(pack_runnable avm_name main)

    add_custom_command(
        OUTPUT Elixir.${main}.beam
        COMMAND elixirc ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        COMMENT "Compiling ${main}.ex"
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_main
        DEPENDS Elixir.${main}.beam
    )

    foreach(archive_name ${ARGN})
        set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_target(
        ${avm_name} ALL
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${avm_name}.avm Elixir.${main}.beam ${ARCHIVES}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_main ${ARCHIVE_TARGETS} PackBEAM)

endmacro()
