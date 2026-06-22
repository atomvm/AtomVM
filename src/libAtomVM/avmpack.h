/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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
 * @file avmpack.h
 * @brief AVM Pack file format parsing function.
 */

#ifndef _AVMPACK_H_
#define _AVMPACK_H_

#include "globalcontext.h"
#include "list.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define END_OF_FILE 0
#define BEAM_START_FLAG 1
#define BEAM_CODE_FLAG 2
#define END_OF_FILE_MASK 255

struct AVMPackData;

struct AVMPackInfo
{
    void (*destructor)(struct AVMPackData *obj, GlobalContext *global);
};

struct AVMPackData
{
    const struct AVMPackInfo *obj_info;
    struct ListHead avmpack_head;
    bool in_use;
    int name_atom_id;
    const void *data;
    uint32_t size;
};

static inline void avmpack_data_init(
    struct AVMPackData *avm_pack_data, const struct AVMPackInfo *info, uint32_t size)
{
    avm_pack_data->obj_info = info;
    avm_pack_data->in_use = false;
    avm_pack_data->name_atom_id = 0;
    avm_pack_data->data = NULL;
    avm_pack_data->size = size;
}

static inline void avmpack_data_destroy(struct AVMPackData *avm_pack_data, GlobalContext *global)
{
    avm_pack_data->obj_info->destructor(avm_pack_data, global);
}

struct InMemoryAVMPack
{
    struct AVMPackData base;
};

extern const struct AVMPackInfo in_memory_avm_pack_info;

struct ConstAVMPack
{
    struct AVMPackData base;
};

extern const struct AVMPackInfo const_avm_pack_info;

/**
 * @brief callback function for AVMPack section fold.
 * @details Instances of this function are supplied to the avmpack_fold function, in order to
 * provide a callback mechanism for folding over the contents of the AVM binary.
 * @param accum The accumulator supplied by the application.
 * @param section_ptr a pointer to the start of the AVM section (including the module header).
 * @param section_size the size of the entire section (including the module header).
 * @param beam_ptr the start of the beam module portion of the section.  This pointers starts immediately after
 * the (aligned) header.
 * @param flags the section flags, as defined in the module header.
 * @param section_name the section name, as defined in the module header.
 * @return an accumulator, which will be supplied to the next call to this function, and eventually returned from the avmpack_fold function.
 */
typedef void *(*avmpack_fold_fun)(void *accum, const void *section_ptr, uint32_t section_size, const void *beam_ptr, uint32_t flags, const char *section_name);

/**
 * @brief Finds an AVM Pack section that has certain flags set.
 *
 * @details Finds an AVM Pack section that has certain flags set and returns a pointer to it, its
 * size and its name. The scan is bounded by \p avmpack_size and validates every section header, so
 * a miss (or a truncated/corrupt pack) returns 0 instead of reading past the end of the pack.
 * @param avmpack_binary a pointer to valid AVM Pack file data.
 * @param avmpack_size the size in bytes of the AVM Pack (used to bound the scan).
 * @param flags_mask that will be matched against file sections.
 * @param flags_value that will be matched against file sections.
 * @param ptr will point to the found file section.
 * @param size will be set to the number of bytes readable at \p ptr (the section payload, which
 * excludes the section header and the padded name), if the section has not been found it will not
 * be updated.
 * @param name the section name, as defined in the module header.
 * @returns 1 if the file section has been found, 0 otherwise.
 */
int avmpack_find_section_by_flag(const void *avmpack_binary, uint32_t avmpack_size,
    uint32_t flags_mask, uint32_t flags_value, const void **ptr, uint32_t *size, const char **name);

/**
 * @brief Finds an AVM Pack section that has certain name.
 *
 * @details Finds an AVM Pack section with a certain name and returns a pointer to it and its size.
 * The scan is bounded by \p avmpack_size and validates every section header, so a miss (or a
 * truncated/corrupt pack) returns 0 instead of reading past the end of the pack.
 * @param avmpack_binary a pointer to valid AVM Pack file data.
 * @param avmpack_size the size in bytes of the AVM Pack (used to bound the scan).
 * @param name the file section name that will be searched.
 * @param ptr will point to the found file section, if the section has not been found it will not be
 * updated.
 * @param size will be set to the number of bytes readable at \p ptr (the section payload, which
 * excludes the section header and the padded name), if the section has not been found it will not
 * be updated.
 * @returns 1 if the file section has been found, 0 otherwise.
 */

int avmpack_find_section_by_name(const void *avmpack_binary, uint32_t avmpack_size,
    const char *name, const void **ptr, uint32_t *size);

/**
 * @brief Returns \c true if the pointed binary starts with a valid AVM Pack header.
 *
 * @details Performs a cheap check of the 24-byte magic header only. It does not validate the
 * section chain or detect truncation.
 * @param avmpack_binary a pointer to an AVM Pack binary.
 * @param size the size of AVM Pack binary.
 * @returns \c true if it is a valid AVM Pack binary, \c false otherwise.
 */
bool avmpack_is_valid(const void *avmpack_binary, uint32_t size);

/**
 * @brief Fold over all the sections in an AVM Pack.
 *
 * @details This function will call the callback on each section of the AVM Pack, passing in
 * the current section of each module in the AVM binary to the supplied fold function. The scan
 * is bounded by \p avmpack_size and stops on the terminator or on the first invalid section header.
 * @param accum The accumulator supplied by the application.
 * @param avmpack_binary a pointer to an AVM Pack binary.
 * @param avmpack_size the size in bytes of the AVM Pack (used to bound the scan).
 * @param fold_fun function that will be called for each AVM section.
 */
void *avmpack_fold(
    void *accum, const void *avmpack_binary, uint32_t avmpack_size, avmpack_fold_fun fold_fun);

#ifdef __cplusplus
}
#endif

#endif
