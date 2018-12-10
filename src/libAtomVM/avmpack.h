/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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
 * @file avmpack.h
 * @brief AVM Pack file format parsing function.
 */

#ifndef _AVMPACK_H_
#define _AVMPACK_H_

#include <stdint.h>

#define END_OF_FILE 0
#define BEAM_START_FLAG 1
#define BEAM_CODE_FLAG 2

/**
 * @brief callback function for AVMPack section traversal.
 */
typedef void *(*avmpack_fold_fun)(void *accum, const void *ptr, uint32_t size, uint32_t flags, const char *name);

/**
 * @brief Finds an AVM Pack section that has certain flags set.
 *
 * @details Finds an AVM Pack section that has certain flags set and returns a pointer to it, its size and its name.
 * @param avmpack_binary a pointer to valid AVM Pack file data.
 * @param flags_mask that will be matched against file sections.
 * @param ptr will point to the found file section.
 * @param size will be set to the file section size that has been found, if the section has not been found it will not be updated.
 * @returns 1 if the file section has been found, 0 otherwise.
 */
int avmpack_find_section_by_flag(const void *avmpack_binary, uint32_t flags_mask, const void **ptr, uint32_t *size, const char **name);

/**
 * @brief Finds an AVM Pack section that has certain name.
 *
 * @details Finds an AVM Pack section with a certain name and returns a pointer to it and its size.
 * @param avmpack_binary a pointer to valid AVM Pack file data.
 * @param name the file section name that will be searched.
 * @param ptr will point to the found file section, if the section has not been found it will not be updated.
 * @param size will be set to the file section size that has been found, if the section has not been found it will not be updated.
 * @returns 1 if the file section has been found, 0 otherwise.
 */

int avmpack_find_section_by_name(const void *avmpack_binary, const char *name, const void **ptr, uint32_t *size);

/**
 * @brief Returns 1 if the pointed binary is a valid AVM Pack.
 *
 * @details Returns if the pointed binary is a valid AVM Pack binary or not.
 * @param avmpack_binary a pointer to an AVM Pack binary.
 * @param size the size of AVM Pack binary.
 * @returns 1 if it is a valid AVM Pack binary, 0 otherwise.
 */
int avmpack_is_valid(const void *avmpack_binary, uint32_t size);

/**
 * @brief Traverse all the sections in an AVM Pack.
 *
 * @details This function will call the callback on each section of the AVM Pack.
 * @param avmpack_binary a pointer to an AVM Pack binary.
 * @param callback callback funtion will be called for each section.
 */
void *avmpack_fold(void *accum, const void *avmpack_binary, avmpack_fold_fun fold_fun);

#endif
