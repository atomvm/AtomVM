##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##


macro(pack_archive avm_name)

    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT ${module_name}.beam
            COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            COMMENT "Compiling ${module_name}.erl"
            VERBATIM
        )
        set(BEAMS ${BEAMS} ${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}
        DEPENDS ${BEAMS}
    )

    add_custom_command(
        OUTPUT  ${avm_name}.avm
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${avm_name}.avm ${BEAMS}
        DEPENDS ${avm_name} PackBEAM
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_avm ALL
        DEPENDS ${avm_name}.avm
    )

endmacro()


macro(pack_runnable avm_name main)

    add_custom_command(
        OUTPUT ${main}.beam
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl
        COMMENT "Compiling ${main}.erl"
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_main
        DEPENDS ${main}.beam
    )

    foreach(archive_name ${ARGN})
        set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name}_avm)
    endforeach()

    add_custom_command(
        OUTPUT  ${avm_name}.avm
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${avm_name}.avm ${main}.beam ${ARCHIVES}
        DEPENDS ${avm_name}_main ${ARCHIVE_TARGETS} PackBEAM
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_avm ALL
        DEPENDS ${avm_name}.avm
    )

endmacro()
