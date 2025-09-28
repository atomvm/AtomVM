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
 * @file exportedfunction.h
 * @brief Module exported functions structs and macros.
 *
 * @details Structs required to handle both exported/imported NIFs and functions.
 */

#ifndef _EXPORTEDFUNCTION_H_
#define _EXPORTEDFUNCTION_H_

#include "term.h"

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

#ifndef TYPEDEF_MODULENATIVEINTERFACE
#define TYPEDEF_MODULENATIVEINTERFACE
typedef struct ModuleNativeInterface ModuleNativeInterface;
#endif

#ifndef TYPEDEF_JITSTATE
#define TYPEDEF_JITSTATE
typedef struct JITState JITState;
#endif

typedef term (*BifImpl0)(Context *ctx);
typedef term (*BifImpl1)(Context *ctx, uint32_t fail_label, term arg1);
typedef term (*BifImpl2)(Context *ctx, uint32_t fail_label, term arg1, term arg2);

typedef term (*GCBifImpl1)(Context *ctx, uint32_t fail_label, int live, term arg1);
typedef term (*GCBifImpl2)(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
typedef term (*GCBifImpl3)(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2, term arg3);

typedef term (*NifImpl)(Context *ctx, int argc, term argv[]);

typedef Context *(*ModuleNativeEntryPoint)(Context *ctx, JITState *jit_state, const ModuleNativeInterface *p);

enum FunctionType
{
    InvalidFunctionType = 0,
    NIFFunctionType = 2,
    UnresolvedFunctionCall = 3,
    ModuleFunction = 4,
    BIFFunctionType = 5,
    GCBIFFunctionType = 6,
#ifndef AVM_NO_JIT
    ModuleNativeFunction = 7
#endif
};

struct ExportedFunction
{
    enum FunctionType type;
};

struct Bif
{
    struct ExportedFunction base;
    union
    {
        BifImpl0 bif0_ptr;
        BifImpl1 bif1_ptr;
        BifImpl2 bif2_ptr;
    };
};

struct GCBif
{
    struct ExportedFunction base;
    union
    {
        GCBifImpl1 gcbif1_ptr;
        GCBifImpl2 gcbif2_ptr;
        GCBifImpl3 gcbif3_ptr;
    };
};

struct Nif
{
    struct ExportedFunction base;
    NifImpl nif_ptr;
};

struct UnresolvedFunctionCall
{
    struct ExportedFunction base;
    atom_index_t module_atom_index;
    atom_index_t function_atom_index;
    int arity;
};

struct ModuleFunction
{
    struct ExportedFunction base;
    Module *target;
    union
    {
        ModuleNativeEntryPoint entry_point;
        int label;
    };
};

#define EXPORTED_FUNCTION_TO_BIF(func) \
    ((const struct Bif *) (((char *) (func)) - ((unsigned long) &((const struct Bif *) 0)->base)))

#define EXPORTED_FUNCTION_TO_GCBIF(func) \
    ((const struct GCBif *) (((char *) (func)) - ((unsigned long) &((const struct GCBif *) 0)->base)))

#define EXPORTED_FUNCTION_TO_NIF(func) \
    ((const struct Nif *) (((char *) (func)) - ((unsigned long) &((const struct Nif *) 0)->base)))

#define EXPORTED_FUNCTION_TO_UNRESOLVED_FUNCTION_CALL(func) \
    ((struct UnresolvedFunctionCall *) (((char *) (func)) - ((unsigned long) &((struct UnresolvedFunctionCall *) 0)->base)))

#define EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func) \
    ((const struct ModuleFunction *) (((char *) (func)) - ((unsigned long) &((const struct ModuleFunction *) 0)->base)))

#endif
