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

from FunctionCall functioncall, Type expected_type, Expr expr, int i
where
  functioncall.getExpectedParameterType(i) = expected_type and
  expected_type.getName() != "term" and
  expected_type.getName() != "unknown" and // This includes variadic arguments
  expr.getExplicitlyConverted().getType().getName() = "term" and
  functioncall.getArgument(i) = expr
select functioncall, "Passing a term to a function expecting a non-term, without an explicit cast"
