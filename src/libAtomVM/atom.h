/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/**
 * @file atom.h
 * @brief Atoms manipulation functions
 *
 * @details This header provides atom manipulation functions, such as atom_string_len, atom_string_data, etc...
 */

#ifndef _ATOM_H_
#define _ATOM_H_

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @details no-op macro: just syntax sugar for avoiding mistakes or clang-format dividing atoms in multiple
 * lines. Usage: ATOM_STR("\\x5", "hello").
 *
 * @param LENSTR must be less than 255 (\\xFF), to fit within a uint8.
 * @param STR the string to be used as an atom.
 */
#define ATOM_STR(LENSTR, STR) (LENSTR STR)

typedef const void *AtomString;
typedef uint32_t atom_index_t;

/**
 * @brief Gets a C string from an AtomString
 *
 * @details Copies the atom string data to a buffer and makes it null terminated.
 * @param atom_string that will be converted to a C string.
 * @param buf the buffer where the C string will be written.
 * @param bufsize available buf size.
 */
void atom_string_to_c(AtomString atom_string, char *buf, size_t bufsize);

/**
 * @brief Check for equality two AtomStrings
 *
 * @details Returns 1 if two atom strings are equal, 0 otherwise. This function doesn't behave like strcmp.
 * @param a first atom string to be compared.
 * @param b second atom string to be compared.
 * @returns 1 if they are equals, 0 otherwise.
 */
int atom_are_equals(AtomString a, AtomString b);

/**
 * @brief Returns the length of an atom string in bytes.
 *
 * @details Returns the length stored in the AtomString (which means that length is not computed).
 * @param atom_str the atom string.
 * @returns the specified atom string length.
 */
static inline size_t atom_string_len(AtomString atom_str)
{
    return *((const uint8_t *) atom_str);
}

/**
 * @brief Gets actual atom string data.
 *
 * @details Returns actual atom string data, which is not a \0 terminated C string.
 * @param atom_str an AtomString pointer.
 * @returns const pointer to string data (chars array).
 */
static inline const void *atom_string_data(AtomString atom_str)
{
    return ((const uint8_t *) atom_str) + 1;
}

/**
 * @brief Write module:function/arity to the supplied buffer.
 *
 * @details Write module:function/arity to the supplied buffer.  This function will abort
 *          if the written module, function, and arity are longer than the supplied
 *          buffer size.
 * @param   buf the buffer to write into
 * @param   buf_size the amount of room in the buffer
 * @param   module_name_len length of the module name
 * @param   module_data the module name
 * @param   function_name_len length of the function name
 * @param   function_data the function name
 * @param   arity the function arity
 */
void atom_write_mfa(char *buf, size_t buf_size, size_t module_name_len, const void *module_data, size_t function_name_len, const void *function_data, unsigned int arity);

#ifdef __cplusplus
}
#endif

#endif
