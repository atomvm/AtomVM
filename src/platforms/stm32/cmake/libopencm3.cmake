#
# This file is part of AtomVM.
#
# Copyright 2018 Riccardo Binetti <rbino@gmx.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Utility

function(JOIN VALUES GLUE OUTPUT)
    string(REGEX REPLACE "([^\\]|^);" "\\1${GLUE}" _TMP_STR "${VALUES}")
    string(REGEX REPLACE "[\\](.)" "\\1" _TMP_STR "${_TMP_STR}") #fixes escaping
    set(${OUTPUT} "${_TMP_STR}" PARENT_SCOPE)
endfunction()

# -----------

# LibOpenCM3 Stuff
if (NOT LIBOPENCM3_DIR)
    set(LIBOPENCM3_DIR ${CMAKE_CURRENT_SOURCE_DIR}/libopencm3/)
endif ()

if (NOT EXISTS ${LIBOPENCM3_DIR}/Makefile)
    message(FATAL_ERROR "libopencm3 does not exist or LIBOPENCM3_DIR not set to checkout " ${LIBOPENCM3_DIR})
endif ()

add_custom_target(
    libopencm3 make
    WORKING_DIRECTORY ${LIBOPENCM3_DIR}
)
set(OPENCM3_LIB ${LIBOPENCM3_DIR}/lib)
set(OPENCM3_INCLUDE ${LIBOPENCM3_DIR}/include)

# Generate linker information for device, based on libopencm3/mk/genlink-config.mk
if (NOT DEVICE)
    message(FATAL_ERROR "No DEVICE specified for linker script generator")
endif ()

find_program(PYTHON python)
if (NOT PYTHON)
    message(FATAL_ERROR "python is required to generate the linker script, please install it.")
endif ()
mark_as_advanced(PYTHON)

set(GENLINK_SCRIPT "${LIBOPENCM3_DIR}/scripts/genlink.py")
set(DEVICES_DATA "${LIBOPENCM3_DIR}/ld/devices.data")
execute_process(
    COMMAND "${PYTHON}" "${GENLINK_SCRIPT}" "${DEVICES_DATA}" "${DEVICE}" "FAMILY"
    OUTPUT_VARIABLE GENLINK_FAMILY
)
execute_process(
    COMMAND "${PYTHON}" "${GENLINK_SCRIPT}" "${DEVICES_DATA}" "${DEVICE}" "SUBFAMILY"
    OUTPUT_VARIABLE GENLINK_SUBFAMILY
)
execute_process(
    COMMAND "${PYTHON}" "${GENLINK_SCRIPT}" "${DEVICES_DATA}" "${DEVICE}" "CPU"
    OUTPUT_VARIABLE GENLINK_CPU
)
execute_process(
    COMMAND "${PYTHON}" "${GENLINK_SCRIPT}" "${DEVICES_DATA}" "${DEVICE}" "FPU"
    OUTPUT_VARIABLE GENLINK_FPU
)
execute_process(
    COMMAND "${PYTHON}" "${GENLINK_SCRIPT}" "${DEVICES_DATA}" "${DEVICE}" "CPPFLAGS"
    OUTPUT_VARIABLE GENLINK_CPPFLAGS
)
execute_process(
    COMMAND "${PYTHON}" "${GENLINK_SCRIPT}" "${DEVICES_DATA}" "${DEVICE}" "DEFS"
    OUTPUT_VARIABLE GENLINK_DEFS
)

message("-----------Device Linker Info-----------")
message(STATUS "Family      : ${GENLINK_FAMILY}")
message(STATUS "Sub-family  : ${GENLINK_SUBFAMILY}")
message(STATUS "CPU         : ${GENLINK_CPU}")
message(STATUS "FPU         : ${GENLINK_FPU}")
message(STATUS "CPP Flags   : ${GENLINK_CPPFLAGS}")
message(STATUS "Linker Defs : ${GENLINK_DEFS}")

# Generate flags
set(ARCH_FLAGS "-mcpu=${GENLINK_CPU}")

# Check CPU
set(CORTEX_CPU cortex-m0 cortex-m0plus cortex-m3 cortex-m4 cortex-m7)
list(FILTER CORTEX_CPU INCLUDE REGEX ${GENLINK_CPU})
if (GENLINK_CPU STREQUAL CORTEX_CPU)
    list(APPEND ARCH_FLAGS "-mthumb -mthumb-interwork")
endif ()

# Check FPU
if (GENLINK_FPU STREQUAL "soft")
    list(APPEND ARCH_FLAGS "-msoft-float")
elseif (GENLINK_FPU STREQUAL "hard-fpv4-sp-d16")
    list(APPEND ARCH_FLAGS "-mfloat-abi=hard -mfpu=fpv4-sp-d16")
elseif (GENLINK_FPU STREQUAL "hard-fpv5-sp-d16")
    list(APPEND ARCH_FLAGS "-mfloat-abi=hard -mfpu=fpv5-sp-d16")
else ()
    message(WARNING "No match for the FPU flags")
endif ()

# Check family
if (NOT GENLINK_FAMILY)
    message(WARNING "${DEVICE} not found in ${DEVICES_DATA}")
endif ()

# Linker stuff
set(LINKER_SCRIPT "generated.${DEVICE}.ld")

if (EXISTS "${LIBOPENCM3_DIR}/lib/libopencm3_${GENLINK_FAMILY}.a")
    set(OPENCM3_RUNTIME "opencm3_${GENLINK_FAMILY}")
else ()
    if (EXISTS "${LIBOPENCM3_DIR}/lib/libopencm3_${GENLINK_SUBFAMILY}.a")
        set(OPENCM3_RUNTIME "opencm3_${GENLINK_SUBFAMILY}")
    else ()
        message(WARNING "${LIBOPENCM3_DIR}/lib/libopencm3_${GENLINK_FAMILY}. A library variant for the selected device does not exist.")
    endif ()
endif ()

# ARCH_FLAGS and GENLINK_DEFS has to be passed as a list here
string(REPLACE " " ";" GENLINK_DEFS ${GENLINK_DEFS})
# Get rid of any spaces and turn the thing into a list
JOIN("${ARCH_FLAGS}" " " ARCH_FLAGS)
string(REPLACE " " ";" ARCH_FLAGS ${ARCH_FLAGS})
# ------------------
execute_process(
    COMMAND ${ARM_CXX} ${ARCH_FLAGS} ${GENLINK_DEFS} "-P" "-E" "${LIBOPENCM3_DIR}/ld/linker.ld.S"
    OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${LINKER_SCRIPT}"
)
message("-----------Target Specific Info---------")
message(STATUS "Generated Linker File   : ${CMAKE_CURRENT_BINARY_DIR}/${LINKER_SCRIPT}")

# ARCH_FLAGS has to be passed as a string here
JOIN("${ARCH_FLAGS}" " " ARCH_FLAGS)
# Set linker flags
set(LINKER_FLAGS "${LINKER_FLAGS} -specs=nosys.specs -nostartfiles -Wl,--undefined,_printf_float -Wl,--undefined,_scanf_float -T${CMAKE_CURRENT_BINARY_DIR}/${LINKER_SCRIPT} ${ARCH_FLAGS}")
message(STATUS "Linker Flags            : ${LINKER_FLAGS}")

# Compiler flags
set(TARGET_SPECIFIC_FLAGS "${GENLINK_CPPFLAGS} ${ARCH_FLAGS}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${TARGET_SPECIFIC_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${TARGET_SPECIFIC_FLAGS}")

# Use linker flags to detect symbols
set(CMAKE_TRY_COMPILE_TARGET_TYPE EXECUTABLE)
set(CMAKE_REQUIRED_FLAGS ${LINKER_FLAGS})

message(STATUS "Target Specific Flags   : ${TARGET_SPECIFIC_FLAGS}")

# Replace `add_executable` with custom macro with same name that adds libopencm3 as a linking target
macro(add_executable _name)
    # invoke built-in add_executable
    if ("${ARGV1}" STREQUAL "")
        message(FATAL_ERROR "No source files added to executable")
    endif ()
    _add_executable(${ARGV})

    if (TARGET ${_name} AND NOT ${_name} MATCHES ".*Doxygen.*")
        # Set target properties
        set_target_properties(${_name} PROPERTIES LINK_FLAGS ${LINKER_FLAGS})
        set_target_properties(${_name} PROPERTIES OUTPUT_NAME "${PROJECT_EXECUTABLE}")

        # Set output file locations
        string(REGEX MATCH "^(.*)\\.[^.]*$" dummy "${_name}")
        set(bin ${CMAKE_MATCH_1}.bin)
        set(elf ${_name})
        set(bin_out ${CMAKE_MATCH_1}.bin)
        get_target_property(elf_in ${PROJECT_EXECUTABLE} OUTPUT_NAME)

        # Add target
        add_custom_target(
            ${bin} ALL
            COMMAND ${ARM_OBJCOPY} -Obinary ${elf_in} ${bin_out}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
            DEPENDS ${elf}
        )

        message("------------Output Locations------------")
        message(STATUS "BIN: ${CMAKE_BINARY_DIR}/${bin_out}")
        message(STATUS "ELF: ${CMAKE_BINARY_DIR}/${elf_in}")
    endif ()
endmacro()
