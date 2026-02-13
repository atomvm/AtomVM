/**
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * @name Free of pointer type never used in malloc
 * @kind problem
 * @problem.severity error
 * @id atomvm/mismatched-free-type
 * @tags correctness
 * @precision high
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

import cpp

import semmle.code.cpp.dataflow.new.DataFlow

string getMallocedTypeNameDirect(FunctionCall malloc) {
    malloc.getTarget().hasGlobalName("malloc") and
    exists(SizeofTypeOperator sizeofExpr |
        sizeofExpr = malloc.getArgument(0).getAChild*() and
        result = sizeofExpr.getTypeOperand().stripType().getName()
    )
}

string getMallocedTypeNameIndirect(FunctionCall malloc) {
    malloc.getTarget().hasGlobalName("malloc") and
    exists(SizeofTypeOperator sizeofExpr, DataFlow::Node source, DataFlow::Node sink |
        source.asExpr().getAChild*() = sizeofExpr and
        sink.asExpr() = malloc.getArgument(0) and
        DataFlow::localFlow(source, sink) and
        result = sizeofExpr.getTypeOperand().stripType().getName()
    )
}

string getCallocedTypeName(FunctionCall calloc) {
    calloc.getTarget().hasGlobalName("calloc") and
    exists(SizeofTypeOperator sizeofExpr |
        sizeofExpr = calloc.getArgument(1).getAChild*() and
        result = sizeofExpr.getTypeOperand().stripType().getName()
    )
}

predicate isAllocatedType(string typeName) {
    exists(FunctionCall malloc | typeName = getMallocedTypeNameDirect(malloc))
    or
    exists(FunctionCall malloc | typeName = getMallocedTypeNameIndirect(malloc))
    or
    exists(FunctionCall calloc | typeName = getCallocedTypeName(calloc))
}

predicate isDoublePointer(Type t) {
    exists(PointerType pt |
        pt = t.getUnspecifiedType() and
        pt.getBaseType().getUnspecifiedType() instanceof PointerType
    )
}

string getPointedStructName(Type t) {
    not isDoublePointer(t) and
    exists(PointerType pt, Struct s |
        pt = t.getUnspecifiedType() and
        s = pt.getBaseType().getUnspecifiedType() and
        result = s.getName()
    )
}

predicate isCastToVoidPtr(Expr e) {
    exists(CStyleCast cast |
        cast = e.getConversion*() and
        cast.getType().getUnspecifiedType().(PointerType).getBaseType() instanceof VoidType
    )
}

from FunctionCall freeCall, Expr arg, string freedTypeName
where
    freeCall.getTarget().hasGlobalName("free") and
    arg = freeCall.getArgument(0) and
    freedTypeName = getPointedStructName(arg.getType()) and
    not isAllocatedType(freedTypeName) and
    // Exclude void* which is generic
    freedTypeName != "" and
    // Allow suppression via explicit cast to (void *)
    not isCastToVoidPtr(arg)
select freeCall, "Freeing pointer of type 'struct " + freedTypeName + "' but no malloc(sizeof(struct " + freedTypeName + ")) or calloc exists in codebase. Cast to (void *) to suppress."
