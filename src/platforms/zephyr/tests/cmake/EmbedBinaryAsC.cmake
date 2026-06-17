#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

if(NOT DEFINED DATA_FILE)
    message(FATAL_ERROR "DATA_FILE is required")
endif()
if(NOT DEFINED SOURCE_FILE)
    message(FATAL_ERROR "SOURCE_FILE is required")
endif()
if(NOT DEFINED SYMBOL)
    message(FATAL_ERROR "SYMBOL is required")
endif()

file(READ "${DATA_FILE}" DATA_HEX HEX)
string(REGEX REPLACE "([0-9a-f][0-9a-f])" "0x\\1," DATA_BYTES "${DATA_HEX}")
string(REGEX REPLACE "((0x[0-9a-f][0-9a-f],){16})" "\\1\n" DATA_BYTES "${DATA_BYTES}")

file(WRITE "${SOURCE_FILE}" "/* Generated from ${DATA_FILE}. */\n")
file(APPEND "${SOURCE_FILE}" "#include <stddef.h>\n")
file(APPEND "${SOURCE_FILE}" "#include <stdint.h>\n\n")
file(APPEND "${SOURCE_FILE}" "const uint8_t ${SYMBOL}[] __attribute__((aligned(4))) = {\n")
file(APPEND "${SOURCE_FILE}" "${DATA_BYTES}\n")
file(APPEND "${SOURCE_FILE}" "};\n")
file(APPEND "${SOURCE_FILE}" "const size_t ${SYMBOL}_size = sizeof(${SYMBOL});\n")
