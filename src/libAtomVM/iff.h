/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

/**
 * @file iff.h
 * @brief IFF/BEAM file parsing and constants.
 *
 * @details BEAM module parser function and related defines.
 */

#ifndef _IFF_H_
#define _IFF_H_

#include <stdint.h>

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


/** Required size for offsets array */
#define MAX_OFFS 9
/** Required size for sizes array */
#define MAX_SIZES 9

/** sizeof IFF section header in bytes */
#define IFF_SECTION_HEADER_SIZE 8

/**
 * @brief parse a BEAM/IFF file and build a sections offsets table
 *
 * @details Read a buffer contaning a BEAM module file and set all found IFF sections into offsets array.
 * @param data is BEAM module data.
 * @param file_size is the BEAM module size in bytes.
 * @param offsets all the relative offsets, each entry will be set to the offset of a different IFF section.
 * @param sizes the computed sections sizes.
 */
void scan_iff(const void *iff_binary, int file_size, unsigned long *offsets, unsigned long *sizes);

/**
 * @brief Returns 1 if pointed binary is valid BEAM IFF.
 *
 * @details Checks if the pointed binary has a valid BEAM IFF header.
 * @param beam_data a pointer to the beam_data binary
 * @returns 1 if beam_data points to a valid binary, otherwise 0 is returned.
 */
int iff_is_valid_beam(const void *beam_data);

#endif
