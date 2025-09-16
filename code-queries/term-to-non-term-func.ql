/**
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
 *
 * @name Passing a term to a function expecting a non-term
 * @kind problem
 * @problem.severity error
 * @id atomvm/term-to-non-term-func
 * @tags correctness
 * @precision high
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */
import cpp


predicate isTermType(Type t) {
    t.getName() = "term" or
    (
        t instanceof TypedefType
        and isTermType(t.(TypedefType).getBaseType())
    ) or
    (
        t instanceof SpecifiedType
        and isTermType(t.(SpecifiedType).getBaseType())
    )
}

from FunctionCall functioncall, Type expected_type, Expr expr, int i
where
  functioncall.getExpectedParameterType(i) = expected_type and
  expected_type.getName() != "unknown" and // This includes variadic arguments
  not isTermType(expected_type) and
  isTermType(expr.getExplicitlyConverted().getType()) and
  functioncall.getArgument(i) = expr
select functioncall, "Passing a term to a function expecting a non-term, without an explicit cast"
