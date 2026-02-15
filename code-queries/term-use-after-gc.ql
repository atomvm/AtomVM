/**
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * @name Use of term variable after garbage collection
 * @kind problem
 * @problem.severity error
 * @id atomvm/term-use-after-gc
 * @tags correctness
 * @precision high
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

import cpp

import semmle.code.cpp.controlflow.SSA
import semmle.code.cpp.controlflow.Dominance

predicate isTermType(Type t) {
    t.getName() = "term"
    or
    (
        t instanceof TypedefType
        and isTermType(t.(TypedefType).getBaseType())
    )
    or
    (
        t instanceof SpecifiedType
        and isTermType(t.(SpecifiedType).getBaseType())
    )
}

class GCCall extends FunctionCall {
    GCCall() {
        this.getTarget().hasName("memory_ensure_free_with_roots")
        or this.getTarget().hasName("memory_ensure_free_opt")
        or this.getTarget().hasName("memory_ensure_free")
    }
}

predicate isImmediateCreatingCall(FunctionCall fc) {
    fc.getTarget().hasName([
        "term_from_int", "term_from_int4", "term_from_int11", "term_from_int28",
        "term_from_int32", "term_from_int64", "term_nil", "term_from_atom_index",
        "term_from_local_process_id", "term_port_from_local_process_id",
        "term_from_catch_label", "term_invalid_term",
        "module_get_atom_term_by_id", "globalcontext_make_atom",
        "term_pid_or_port_from_context",
        "posix_errno_to_term"
    ])
}

predicate isImmediateExpr(Expr e) {
    isImmediateCreatingCall(e)
    or
    (
        e.isInMacroExpansion() and
        e.isConstant() and
        exists(MacroInvocation mi |
            e = mi.getAGeneratedElement() and
            (mi.toString().matches("%_ATOM") or mi.toString().matches("TERM_%"))
        )
    )
    or
    (
        e instanceof EnumConstantAccess and
        e.(EnumConstantAccess).getTarget().toString().matches("%_ATOM")
    )
}

/**
 * Holds if CFG node `a` is strictly before `b` in the dominance ordering:
 * either `a`'s basic block strictly dominates `b`'s basic block,
 * or they are in the same basic block and `a` comes first.
 */
predicate strictlyBeforeInDomTree(ControlFlowNode a, ControlFlowNode b) {
    bbStrictlyDominates(a.getBasicBlock(), b.getBasicBlock())
    or
    exists(BasicBlock bb, int ia, int ib |
        bb = a.getBasicBlock() and
        bb = b.getBasicBlock() and
        bb.getNode(ia) = a and
        bb.getNode(ib) = b and
        ia < ib
    )
}

/**
 * Holds if the variable has its address taken anywhere (e.g. `&v` passed
 * as root to `memory_ensure_free_with_roots`). Such variables are updated
 * in-place by the GC and remain valid.
 */
predicate isAddressTaken(StackVariable v) {
    exists(AddressOfExpr aoe |
        aoe.getOperand().(VariableAccess).getTarget() = v
    )
}

/**
 * List of functions that check whether a term is an immediate type.
 */
predicate isImmediateTypeCheckFunction(string name) {
    name = [
        "term_is_atom", "term_is_int", "term_is_integer",
        "term_is_nil", "term_is_local_pid", "term_is_local_port",
        "term_is_local_pid_or_port"
    ]
}

/**
 * Leaf functions that consume a term that must be an immediate type at
 * a specific parameter index.
 */
predicate isImmediateConsumingParam(string funcName, int paramIndex) {
    (funcName = "term_to_local_process_id" or funcName = "term_to_atom_index") and
    paramIndex = 0
}

/**
 * Holds if function `f` requires its parameter at `paramIndex` to be
 * an immediate, because it (transitively) passes it to a leaf
 * immediate-consuming function such as term_to_local_process_id.
 */
predicate isFunctionRequiringImmediateParam(Function f, int paramIndex) {
    exists(FunctionCall innerCall, int innerParamIndex |
        innerCall.getEnclosingFunction() = f and
        isImmediateConsumingParam(innerCall.getTarget().getName(), innerParamIndex) and
        innerCall.getArgument(innerParamIndex).(VariableAccess).getTarget() =
            f.getParameter(paramIndex)
    )
    or
    exists(FunctionCall innerCall, int innerParamIndex |
        innerCall.getEnclosingFunction() = f and
        isFunctionRequiringImmediateParam(innerCall.getTarget(), innerParamIndex) and
        innerCall.getArgument(innerParamIndex).(VariableAccess).getTarget() =
            f.getParameter(paramIndex)
    )
}

/**
 * Holds if there is a call to a function that checks whether a term is
 * an immediate type (atom, integer, nil, pid, port) on variable `v`
 * before the GC call. The pattern is typically:
 *   VALIDATE_VALUE(v, term_is_atom)  -- expands to: if (!term_is_atom(v)) RAISE_ERROR(...)
 * After such a check, v is known to be immediate and safe across GC.
 *
 * Also matches calls to functions that consume an immediate term (e.g.
 * term_to_local_process_id) -- if the call is dominated by the definition
 * and dominates the GC call, the term must be immediate.
 */
predicate hasImmediateTypeGuard(StackVariable v, GCCall gcCall) {
    exists(FunctionCall typeCheck |
        isImmediateTypeCheckFunction(typeCheck.getTarget().getName()) and
        typeCheck.getArgument(0).(VariableAccess).getTarget() = v and
        strictlyBeforeInDomTree(typeCheck, gcCall)
    )
    or
    exists(FunctionCall consumeCall, int paramIndex |
        isImmediateConsumingParam(consumeCall.getTarget().getName(), paramIndex) and
        consumeCall.getArgument(paramIndex).(VariableAccess).getTarget() = v and
        strictlyBeforeInDomTree(consumeCall, gcCall)
    )
    or
    exists(FunctionCall consumeCall, int paramIndex |
        isFunctionRequiringImmediateParam(consumeCall.getTarget(), paramIndex) and
        consumeCall.getArgument(paramIndex).(VariableAccess).getTarget() = v and
        strictlyBeforeInDomTree(consumeCall, gcCall)
    )
}

/**
 * Holds if the value assigned to the SSA definition is from a variable
 * that has an immediate type guard before the GC call. This handles patterns
 * like: if (!term_is_atom(x)) RAISE_ERROR(...); y = x; GC; use(y);
 */
predicate hasIndirectImmediateTypeGuard(SsaDefinition ssaDef, StackVariable v, GCCall gcCall) {
    // Direct: defining value is a variable access to a guarded variable
    exists(Expr defValue |
        defValue = ssaDef.getAnUltimateDefiningValue(v) and
        exists(StackVariable sourceVar |
            defValue.(VariableAccess).getTarget() = sourceVar and
            hasImmediateTypeGuard(sourceVar, gcCall)
        )
    )
    or
    // Array access: defining value and guard argument are same array element
    // Handles: VALIDATE_VALUE(argv[0], term_is_atom); module = argv[0];
    exists(Expr defValue, FunctionCall typeCheck |
        defValue = ssaDef.getAnUltimateDefiningValue(v) and
        isImmediateTypeCheckFunction(typeCheck.getTarget().getName()) and
        strictlyBeforeInDomTree(typeCheck, gcCall) and
        exists(ArrayExpr defArr, ArrayExpr checkArr |
            defArr = defValue and
            checkArr = typeCheck.getArgument(0) and
            defArr.getArrayBase().(VariableAccess).getTarget() =
                checkArr.getArrayBase().(VariableAccess).getTarget() and
            defArr.getArrayOffset().getValue() = checkArr.getArrayOffset().getValue()
        )
    )
}

/**
 * Helper: holds if `neq` is of the form `v != IMM` where IMM is immediate.
 */
predicate isNEAgainstImmediate(NEExpr neq, StackVariable v) {
    (neq.getLeftOperand().(VariableAccess).getTarget() = v and isImmediateExpr(neq.getRightOperand()))
    or
    (neq.getRightOperand().(VariableAccess).getTarget() = v and isImmediateExpr(neq.getLeftOperand()))
}

/**
 * Holds if the variable is validated against immediate values before the GC
 * call via a negative validation pattern like:
 *   if (v != IMM) { RAISE_ERROR/return; }   (no else branch)
 * or:
 *   if (v != IMM1 && v != IMM2) { RAISE_ERROR/return; }   (no else branch)
 * After such a check, code only continues if v is one of the immediate values.
 * Only matches if-statements WITHOUT an else branch, ensuring the GC is on
 * the fall-through path where the condition is false (i.e., v IS the immediate).
 */
predicate hasComparisonGuard(StackVariable v, GCCall gcCall) {
    exists(IfStmt ifStmt |
        strictlyBeforeInDomTree(ifStmt, gcCall) and
        // No else branch: the then-branch exits, code continues only if false
        not exists(ifStmt.getElse()) and
        // Condition involves v != IMM
        exists(NEExpr neq |
            (
                neq = ifStmt.getCondition()
                or
                neq = ifStmt.getCondition().(LogicalAndExpr).getAnOperand()
                or
                neq = ifStmt.getCondition().(LogicalAndExpr).getAnOperand().(LogicalAndExpr).getAnOperand()
            ) and
            isNEAgainstImmediate(neq, v)
        )
    )
}

/**
 * Holds if all return statements of a static function return immediate
 * values. Calls to such functions are known to produce immediates.
 * Handles both direct returns (e.g. return ATOM;) and returns through
 * local variables whose SSA defining values are all immediate.
 */
predicate isStaticFunctionReturningOnlyImmediates(Function f) {
    f.isStatic() and
    exists(ReturnStmt ret | ret.getEnclosingFunction() = f) and
    forall(ReturnStmt ret | ret.getEnclosingFunction() = f |
        isImmediateExpr(ret.getExpr())
        or
        exists(SsaDefinition retDef, StackVariable retVar |
            ret.getExpr().(VariableAccess).getTarget() = retVar and
            retDef.getAUse(retVar) = ret.getExpr() and
            exists(retDef.getAnUltimateDefiningValue(retVar)) and
            forall(Expr defVal | defVal = retDef.getAnUltimateDefiningValue(retVar) |
                isImmediateExpr(defVal)
            )
        )
    )
}

/**
 * Holds if expression `e` is known to produce an immediate value,
 * including through ternary expressions where both branches are immediate,
 * or calls to static functions that only return immediates.
 */
predicate isKnownImmediateExpr(Expr e) {
    isImmediateExpr(e)
    or
    (
        e instanceof ConditionalExpr and
        isKnownImmediateExpr(e.(ConditionalExpr).getThen()) and
        isKnownImmediateExpr(e.(ConditionalExpr).getElse())
    )
    or
    isStaticFunctionReturningOnlyImmediates(e.(FunctionCall).getTarget())
}

/**
 * Holds if expression `e` is known to be an immediate at a call site,
 * either directly or through SSA tracing of a local variable.
 */
predicate isKnownImmediateAtCallSite(Expr e) {
    isKnownImmediateExpr(e)
    or
    exists(SsaDefinition argDef, StackVariable argVar |
        e.(VariableAccess).getTarget() = argVar and
        argDef.getAUse(argVar) = e and
        forall(Expr defVal | defVal = argDef.getAnUltimateDefiningValue(argVar) |
            isKnownImmediateExpr(defVal)
        )
    )
}

/**
 * Holds if the variable is a parameter of a static function and all call
 * sites pass an immediate expression for that argument. Since the function
 * is static, all callers are visible within the translation unit.
 */
predicate isStaticParamAlwaysImmediate(StackVariable v) {
    exists(Parameter p |
        p = v and
        p.getFunction().isStatic() and
        forall(FunctionCall fc | fc.getTarget() = p.getFunction() |
            isKnownImmediateAtCallSite(fc.getArgument(p.getIndex()))
        )
    )
}

/**
 * Holds if the GC call has a NOLINT(term-use-after-gc) suppression comment
 * on the same line or the line before
 */
predicate hasSuppressionComment(GCCall gcCall) {
    exists(Comment c |
        c.getLocation().getFile() = gcCall.getLocation().getFile() and
        (
            c.getLocation().getStartLine() = gcCall.getLocation().getStartLine() or
            c.getLocation().getStartLine() = gcCall.getLocation().getStartLine() - 1
        ) and
        c.getContents().matches("%NOLINT(term-use-after-gc)%")
    )
}

/**
 * Holds if the variable is used as the source argument (second argument) of
 * memory_copy_term_tree after the GC call. This means the GC was done to
 * make room for copying the term into the context's heap, and the term
 * itself lives on a different heap (e.g. ETS table, mailbox) that is
 * unaffected by the GC. All uses of the same variable after this GC call
 * are safe.
 */
predicate isCopiedFromExternalHeap(SsaDefinition ssaDef, StackVariable v, GCCall gcCall) {
    exists(FunctionCall copyCall, VariableAccess copyArg |
        copyCall.getTarget().hasName("memory_copy_term_tree") and
        copyArg = copyCall.getArgument(1) and
        copyArg.(VariableAccess).getTarget() = v and
        ssaDef.getAUse(v) = copyArg and
        strictlyBeforeInDomTree(gcCall, copyCall)
    )
}

/**
 * Holds if the GC call operates on a different Context than the primary
 * "ctx" parameter. This handles cases like do_spawn() where GC runs on
 * new_ctx but variables come from ctx.
 */
predicate gcOnDifferentContext(GCCall gcCall) {
    exists(Variable gcCtxVar |
        gcCall.getArgument(0).(VariableAccess).getTarget() = gcCtxVar and
        gcCtxVar.getName() != "ctx" and
        exists(Parameter p |
            p.getFunction() = gcCall.getEnclosingFunction() and
            p.getName() = "ctx"
        )
    )
}

/**
 * Holds if the variable (same SSA definition) is used in a context that
 * requires it to be an immediate value. This proves the variable holds
 * an immediate regardless of whether the usage is before or after the GC.
 *
 * For example, if `pid` is passed to port_send_reply (which transitively
 * calls term_to_local_process_id), pid must be a local PID (immediate).
 */
predicate isImpliedImmediateByUsage(SsaDefinition ssaDef, StackVariable v) {
    exists(FunctionCall fc, VariableAccess va, int paramIndex |
        va.getTarget() = v and
        ssaDef.getAUse(v) = va and
        va = fc.getArgument(paramIndex) and
        (
            isImmediateConsumingParam(fc.getTarget().getName(), paramIndex)
            or
            isFunctionRequiringImmediateParam(fc.getTarget(), paramIndex)
        )
    )
}

/**
 * Holds if the expression directly produces a term from a message heap.
 * Each mailbox message has its own heap, and `message->message` (where
 * message is `struct Message`) gives a term on that heap.
 * Sub-terms extracted via term_get_tuple_element also live on the message
 * heap and are unaffected by GC on the context's heap.
 */
predicate isExternalHeapTerm(Expr e) {
    // message->message field access on struct Message
    e.(PointerFieldAccess).getTarget().hasName("message") and
    e.(PointerFieldAccess).getTarget().getDeclaringType().hasName("Message")
    or
    // term_get_tuple_element(external_term, N)
    e.(FunctionCall).getTarget().hasName("term_get_tuple_element") and
    isKnownExternalHeapValue(e.(FunctionCall).getArgument(0))
}

/**
 * Holds if the expression is known to be a term from an external heap,
 * either directly or through SSA tracing of a local variable.
 */
predicate isKnownExternalHeapValue(Expr e) {
    isExternalHeapTerm(e)
    or
    exists(SsaDefinition def, StackVariable var |
        e.(VariableAccess).getTarget() = var and
        def.getAUse(var) = e and
        isExternalHeapTerm(def.getAnUltimateDefiningValue(var))
    )
}

/**
 * Holds if the expression is known to be safe across GC: either an
 * immediate value or a term from an external heap.
 */
predicate isSafeAcrossGCExpr(Expr e) {
    isKnownImmediateExpr(e)
    or
    isExternalHeapTerm(e)
}

/**
 * Holds if the expression at a call site is known to be safe across GC,
 * either directly or through SSA tracing where all defining values are safe.
 */
predicate isSafeAcrossGCAtCallSite(Expr e) {
    isSafeAcrossGCExpr(e)
    or
    exists(SsaDefinition argDef, StackVariable argVar |
        e.(VariableAccess).getTarget() = argVar and
        argDef.getAUse(argVar) = e and
        exists(argDef.getAnUltimateDefiningValue(argVar)) and
        forall(Expr defVal | defVal = argDef.getAnUltimateDefiningValue(argVar) |
            isSafeAcrossGCExpr(defVal)
        )
    )
}

/**
 * Holds if the variable is a parameter of a static function and all call
 * sites pass a value that is safe across GC (either immediate or from an
 * external heap such as a message heap).
 */
predicate isStaticParamAlwaysSafeAcrossGC(StackVariable v) {
    exists(Parameter p |
        p = v and
        p.getFunction().isStatic() and
        forall(FunctionCall fc | fc.getTarget() = p.getFunction() |
            isSafeAcrossGCAtCallSite(fc.getArgument(p.getIndex()))
        )
    )
}

from SsaDefinition ssaDef, StackVariable v, GCCall gcCall, VariableAccess use
where
    isTermType(v.getType()) and
    not isAddressTaken(v) and
    ssaDef.getAVariable() = v and
    ssaDef.getAUse(v) = use and
    strictlyBeforeInDomTree(ssaDef.getDefinition(), gcCall) and
    strictlyBeforeInDomTree(gcCall, use) and
    not isSafeAcrossGCExpr(ssaDef.getAnUltimateDefiningValue(v)) and
    not hasImmediateTypeGuard(v, gcCall) and
    not hasIndirectImmediateTypeGuard(ssaDef, v, gcCall) and
    not hasComparisonGuard(v, gcCall) and
    not isCopiedFromExternalHeap(ssaDef, v, gcCall) and
    not isStaticParamAlwaysImmediate(v) and
    not isStaticParamAlwaysSafeAcrossGC(v) and
    not isImpliedImmediateByUsage(ssaDef, v) and
    not gcOnDifferentContext(gcCall) and
    not hasSuppressionComment(gcCall)
select use,
    "Term variable '" + v.getName() +
        "' may hold a stale value after GC call $@. Defined $@.",
    gcCall, "here", ssaDef.getDefinition(), "here"
