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
 * @file exportedfunction.h
 * @brief Module exported functions structs and macros.
 *
 * @details Structs required to handle both exported/imported NIFs and functions.
 */

#ifndef _EXPORTEDFUNCTION_H_
#define _EXPORTEDFUNCTION_H_

#include "term.h"

typedef term (*BifImpl)();
typedef term (*BifImpl0)(Context *ctx);
typedef term (*BifImpl1)(Context *ctx, uint32_t failure_label, term arg1);
typedef term (*BifImpl2)(Context *ctx, uint32_t failure_label, term arg1, term arg2);

typedef term (*GCBifImpl1)(Context *ctx, uint32_t failure_label, int live, term arg1);
typedef term (*GCBifImpl2)(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);

typedef term (*NifImpl)(Context *ctx, int argc, term argv[]);

enum FunctionType
{
    InvalidFunctionType = 0,
    NIFFunctionType = 2,
    UnresolvedFunctionCall = 3,
    ModuleFunction = 4
};

struct ExportedFunction
{
    enum FunctionType type;
};

struct Nif
{
    struct ExportedFunction base;
    NifImpl nif_ptr;
};

struct UnresolvedFunctionCall
{
    struct ExportedFunction base;
    int module_atom_index;
    int function_atom_index;
    int arity;
};

struct ModuleFunction
{
    struct ExportedFunction base;
    Module *target;
    int label;
};

#define EXPORTED_FUNCTION_TO_NIF(func) \
    ((const struct Nif *) (((char *) (func)) - ((unsigned long) &((const struct Nif *) 0)->base)))

#define EXPORTED_FUNCTION_TO_UNRESOLVED_FUNCTION_CALL(func) \
    ((struct UnresolvedFunctionCall *) (((char *) (func)) - ((unsigned long) &((struct UnresolvedFunctionCall *) 0)->base)))

#define EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func) \
    ((const struct ModuleFunction *) (((char *) (func)) - ((unsigned long) &((const struct ModuleFunction *) 0)->base)))

union imported_func
{
    const struct ExportedFunction *func;
    BifImpl bif;
};

#endif
