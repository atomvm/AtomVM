/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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
 * @file posix_nifs.h
 * @brief Declaration of NIFs based on POSIX functions
 */

#ifndef _POSIX_NIFS_H_
#define _POSIX_NIFS_H_

#include "exportedfunction.h"
#include "globalcontext.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

#if HAVE_OPEN && HAVE_CLOSE
extern const ErlNifResourceTypeInit posix_fd_resource_type_init;
extern const struct Nif atomvm_posix_open_nif;
extern const struct Nif atomvm_posix_close_nif;
extern const struct Nif atomvm_posix_read_nif;
extern const struct Nif atomvm_posix_write_nif;
extern const struct Nif atomvm_posix_select_read_nif;
extern const struct Nif atomvm_posix_select_write_nif;
extern const struct Nif atomvm_posix_select_stop_nif;
#endif
#if HAVE_MKFIFO
extern const struct Nif atomvm_posix_mkfifo_nif;
#endif
#if HAVE_UNLINK
extern const struct Nif atomvm_posix_unlink_nif;
#endif
#if defined(HAVE_CLOCK_SETTIME) || defined(HAVE_SETTIMEOFDAY)
extern const struct Nif atomvm_posix_clock_settime_nif;
#endif
#if HAVE_OPENDIR && HAVE_READDIR && HAVE_CLOSEDIR
extern const ErlNifResourceTypeInit posix_dir_resource_type_init;
extern const struct Nif atomvm_posix_opendir_nif;
extern const struct Nif atomvm_posix_readdir_nif;
extern const struct Nif atomvm_posix_closedir_nif;
#endif

/**
 * @brief Convenient function to return posix errors as atom.
 *
 * @details POSIX does not define the values of errno errors, so this function
 * makes sure Erlang code can interpret error codes whatever the platform.
 *
 * @param err the error code, typically obtained from `errno(3)`
 * @param glb the global context
 * @return an atom representing the error or an integer if the error
 * number is not known.
 */
term posix_errno_to_term(int err, GlobalContext *glb);

#ifdef __cplusplus
}
#endif

#endif
