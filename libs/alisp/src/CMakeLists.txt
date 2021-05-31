##
## Copyright (c) 2021 Davide Bettio <davide@uninstall.it>
##

project(alisp)

include(BuildErlang)

set(ERLANG_MODULES
    alisp
    alisp_stdlib
    arepl
    sexp_lexer
    sexp_parser
    sexp_serializer
)

pack_archive(alisp ${ERLANG_MODULES})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/alisp.avm DESTINATION lib/AtomVM/ebin)
