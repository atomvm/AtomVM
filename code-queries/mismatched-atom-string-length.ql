/**
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * @name Mismatched AtomString length
 * @kind problem
 * @problem.severity error
 * @id atomvm/mismatched-atom-string-length
 * @tags correctness
 * @precision high
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

import cpp

int hexCharToInt(string c) {
  c = "0" and result = 0
  or
  c = "1" and result = 1
  or
  c = "2" and result = 2
  or
  c = "3" and result = 3
  or
  c = "4" and result = 4
  or
  c = "5" and result = 5
  or
  c = "6" and result = 6
  or
  c = "7" and result = 7
  or
  c = "8" and result = 8
  or
  c = "9" and result = 9
  or
  c = "a" and result = 10
  or
  c = "A" and result = 10
  or
  c = "b" and result = 11
  or
  c = "B" and result = 11
  or
  c = "c" and result = 12
  or
  c = "C" and result = 12
  or
  c = "d" and result = 13
  or
  c = "D" and result = 13
  or
  c = "e" and result = 14
  or
  c = "E" and result = 14
  or
  c = "f" and result = 15
  or
  c = "F" and result = 15
}

bindingset[lenStr]
int parseLenStr(string lenStr) {
  exists(string inner |
    inner = lenStr.substring(1, lenStr.length() - 1) and
    (
      // \xH - single hex digit
      inner.length() = 3 and
      inner.prefix(2) = "\\x" and
      result = hexCharToInt(inner.charAt(2))
      or
      // \xHH - two hex digits
      inner.length() = 4 and
      inner.prefix(2) = "\\x" and
      result = hexCharToInt(inner.charAt(2)) * 16 + hexCharToInt(inner.charAt(3))
    )
  )
}

bindingset[str]
int parseStrLen(string str) {
  result = str.length() - 2
}

/**
 * Check 1: ATOM_STR and X macro invocations where the length byte
 * doesn't match the string length.
 */
predicate macroMismatch(MacroInvocation mi, string msg) {
  exists(string lenArg, string strArg, int declaredLen, int actualLen |
    (
      mi.getMacroName() = "ATOM_STR" and
      lenArg = mi.getUnexpandedArgument(0) and
      strArg = mi.getUnexpandedArgument(1)
      or
      mi.getMacroName() = "X" and
      mi.getFile().getBaseName().matches("%.def") and
      lenArg = mi.getUnexpandedArgument(1) and
      strArg = mi.getUnexpandedArgument(2)
    ) and
    declaredLen = parseLenStr(lenArg) and
    actualLen = parseStrLen(strArg) and
    declaredLen != actualLen and
    msg =
      "AtomString length mismatch: declared length " + declaredLen.toString() + " (from " + lenArg +
        ") but actual string " + strArg + " has length " + actualLen.toString()
  )
}

/**
 * Holds if `f` has a parameter at position `i` whose type resolves to AtomString
 * (i.e. const void *).
 */
predicate isAtomStringParam(Function f, int i) {
  exists(Parameter p |
    p = f.getParameter(i) and
    p.getType().(TypedefType).getName() = "AtomString"
  )
}

/**
 * Holds if `f` has a parameter at position `i` whose struct field type is AtomString.
 * This catches AtomStringIntPair initializers.
 */
predicate isAtomStringField(Struct s, int i) {
  exists(Field field |
    field = s.getCanonicalMember(i) and
    field.getType().(TypedefType).getName() = "AtomString"
  )
}

/**
 * Check 2: String literals used where AtomString is expected (function arguments
 * or variable initializers), where the first byte (length prefix) doesn't match
 * the remaining string length.
 *
 * After ATOM_STR expansion, the string literal is e.g. "\x05hello" (6 bytes).
 * getValue() returns the decoded string, so charAt(0).toUnicode() == 5 and
 * getValue().length() == 6. The check is: first byte != length - 1.
 */
predicate stringLiteralMismatch(StringLiteral sl, string msg) {
  not sl.isInMacroExpansion() and
  exists(int declaredLen, int actualLen |
    declaredLen = sl.getValue().codePointAt(0) and
    actualLen = sl.getValue().length() - 1 and
    declaredLen != actualLen and
    declaredLen > 0 and
    (
      // Passed as argument to a function with AtomString parameter
      exists(FunctionCall fc, int i |
        isAtomStringParam(fc.getTarget(), i) and
        fc.getArgument(i) = sl
      )
      or
      // Used in initializer of a const char * variable with _atom suffix (typical pattern)
      exists(Variable v |
        v.getInitializer().getExpr() = sl and
        v.getType().stripTopLevelSpecifiers().(PointerType).getBaseType().stripTopLevelSpecifiers()
          instanceof CharType and
        v.getName().matches("%_atom")
      )
      or
      // Used in an aggregate literal initializing an AtomStringIntPair-like struct
      exists(ClassAggregateLiteral agg, int i |
        isAtomStringField(agg.getType().stripType(), i) and
        agg.getAFieldExpr(agg.getType().stripType().(Struct).getCanonicalMember(i)) = sl
      )
    ) and
    msg =
      "AtomString length mismatch in string literal: first byte is " + declaredLen.toString() +
        " but string data length is " + actualLen.toString()
  )
}

from Element e, string msg
where
  macroMismatch(e, msg)
  or
  stringLiteralMismatch(e, msg)
select e, msg
