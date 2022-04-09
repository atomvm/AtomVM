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

#ifndef _INTEROP_H_
#define _INTEROP_H_

#include "context.h"
#include "term.h"

char *interop_term_to_string(term t, int *ok);
char *interop_binary_to_string(term binary);
char *interop_list_to_string(term list, int *ok);
char *interop_atom_to_string(Context *ctx, term atom);
term interop_proplist_get_value(term list, term key);
term interop_proplist_get_value_default(term list, term key, term default_value);
term interop_map_get_value(Context *ctx, term map, term key);
term interop_map_get_value_default(Context *ctx, term map, term key, term default_value);

int interop_iolist_size(term t, int *ok);
int interop_write_iolist(term t, char *p);

#endif
