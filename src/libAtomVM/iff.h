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
 * @file iff.h
 * @brief IFF/BEAM file parsing and constants.
 *
 * @details BEAM module parser function and related defines.
 */

#ifndef _IFF_H_
#define _IFF_H_

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/** UTF-8 Atoms table section */
#define AT8U 0
/** Code chunk section */
#define CODE 1

/** Exported functions table section */
#define EXPT 2
/** Local functions table section */
#define LOCT 3
/** Imported functions table section */
#define IMPT 4
/** Literals table section with all the compressed zlib literals data */
#define LITT 5
/** Uncompressed literals table section */
#define LITU 6
/** Funs table section */
#define FUNT 7
/** Str table section */
#define STRT 8
/** Str table section */
#define LINT 9
/** Native code section */
#define AVMN 10
/** Type table section */
#define TYPE 11

/** Required size for offsets array */
#define MAX_OFFS 12
/** Required size for sizes array */
#define MAX_SIZES 12

/** sizeof IFF section header in bytes */
#define IFF_SECTION_HEADER_SIZE 8

/**
 * @brief parse a BEAM/IFF file and build a sections offsets table
 *
 * @details Read a buffer containing a BEAM module file and set all found IFF sections into offsets array.
 * @param iff_binary is BEAM module data.
 * @param file_size is the BEAM module size in bytes.
 * @param offsets all the relative offsets, each entry will be set to the offset of a different IFF section.
 * @param sizes the computed sections sizes.
 */
void scan_iff(const void *iff_binary, int file_size, unsigned long *offsets, unsigned long *sizes);

/**
 * @brief Returns \c true if pointed binary is valid BEAM IFF.
 *
 * @details Checks if the pointed binary has a valid BEAM IFF header.
 * @param beam_data a pointer to the beam_data binary
 * @returns \c true if beam_data points to a valid binary, otherwise \c false is returned.
 */
bool iff_is_valid_beam(const void *beam_data);

#ifdef __cplusplus
}
#endif

#endif
