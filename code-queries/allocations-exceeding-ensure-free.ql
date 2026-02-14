/**
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * @name Allocations exceeding ensure_free
 * @kind problem
 * @problem.severity error
 * @id atomvm/allocations-exceeding-ensure-free
 * @tags correctness
 * @precision high
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

import cpp
import semmle.code.cpp.controlflow.Dominance

/**
 * Gets the constant integer value of expression `e`, either directly
 * from a compile-time constant expression, or by tracing through a
 * local variable that was initialized with a constant expression and
 * never reassigned (no assignments or increment/decrement operations).
 */
pragma[noinline]
int constExprValue(Expr e) {
    result = e.getValue().toInt()
    or
    exists(LocalVariable v |
        e.(VariableAccess).getTarget() = v and
        result = v.getInitializer().getExpr().getValue().toInt() and
        // Only trace if the variable is never modified after initialization
        not exists(Assignment a | a.getLValue().(VariableAccess).getTarget() = v) and
        not exists(CrementOperation co | co.getOperand().(VariableAccess).getTarget() = v)
    )
}

/**
 * Holds if the function `f` directly calls `memory_heap_alloc`.
 */
predicate directlyCallsHeapAlloc(Function f) {
    exists(FunctionCall fc |
        fc.getEnclosingFunction() = f and
        fc.getTarget().hasName("memory_heap_alloc")
    )
}

/**
 * Holds if `f` directly calls any memory_ensure_free variant.
 */
pragma[noinline]
predicate directlyCallsEnsureFree(Function f) {
    exists(FunctionCall fc |
        fc.getEnclosingFunction() = f and
        (
            fc.getTarget().hasName("memory_ensure_free") or
            fc.getTarget().hasName("memory_ensure_free_opt") or
            fc.getTarget().hasName("memory_ensure_free_with_roots") or
            fc.getTarget().hasName("memory_erl_nif_env_ensure_free")
        )
    )
}

/**
 * Holds if `caller` directly calls `callee` (cached edge relation for
 * call-graph traversal, avoiding repeated FunctionCall joins).
 */
pragma[noinline]
predicate callEdge(Function caller, Function callee) {
    exists(FunctionCall fc |
        fc.getEnclosingFunction() = caller and
        callee = fc.getTarget()
    )
}

/**
 * Holds if `f` directly or transitively calls any memory_ensure_free variant.
 * Functions that do this manage their own heap allocation and should not be
 * checked against the caller's ensure_free.
 */
pragma[nomagic]
predicate callsEnsureFree(Function f) {
    directlyCallsEnsureFree(f)
    or
    exists(Function callee |
        callEdge(f, callee) and
        callsEnsureFree(callee)
    )
}

/**
 * Holds if `f` transitively calls `memory_heap_alloc` (directly or through callees)
 * AND does not call any ensure_free variant (meaning it relies on the caller to
 * have ensured enough heap space).
 */
pragma[nomagic]
predicate transitivelyCallsHeapAllocWithoutEnsureFree(Function f) {
    directlyCallsHeapAlloc(f) and not callsEnsureFree(f)
    or
    not callsEnsureFree(f) and
    exists(Function callee |
        callEdge(f, callee) and
        transitivelyCallsHeapAllocWithoutEnsureFree(callee)
    )
}

/**
 * Gets the constant allocation size for a direct `memory_heap_alloc(heap, size)` call
 * within function `f`, when the size argument is fully constant.
 */
int directFullyConstAllocSize(Function f) {
    exists(FunctionCall fc |
        fc.getEnclosingFunction() = f and
        fc.getTarget().hasName("memory_heap_alloc") and
        result = constExprValue(fc.getArgument(1))
    )
}

/**
 * Gets the index of the parameter that flows into the `memory_heap_alloc` size
 * argument as part of an addition with a constant, within function `f`.
 * Also binds `constPart` to the constant part of that addition.
 */
predicate directParamPlusConstAlloc(Function f, int paramIndex, int constPart) {
    exists(FunctionCall fc, AddExpr add, Expr constOperand, Expr paramOperand |
        fc.getEnclosingFunction() = f and
        fc.getTarget().hasName("memory_heap_alloc") and
        add = fc.getArgument(1) and
        (
            constOperand = add.getLeftOperand() and paramOperand = add.getRightOperand()
            or
            constOperand = add.getRightOperand() and paramOperand = add.getLeftOperand()
        ) and
        constPart = constExprValue(constOperand) and
        paramOperand.(VariableAccess).getTarget() = f.getParameter(paramIndex)
    )
}

/**
 * Computes the constant allocation size for a call to a function that
 * transitively calls `memory_heap_alloc` without its own ensure_free.
 *
 * Case 1: The callee has a direct `memory_heap_alloc` with a fully constant size.
 * Case 2: The callee has `memory_heap_alloc(heap, constant + param)` and the
 *         caller passes a constant for that parameter.
 * Case 3: Wrapper function -- the callee doesn't directly call `memory_heap_alloc`
 *         but calls another function that does, and we can compute its size
 *         via a function-level summary (avoids recursing over calls).
 */
pragma[noinline]
int getConstAllocSize(FunctionCall call) {
    exists(Function callee | callee = call.getTarget() |
        // Case 1: Direct memory_heap_alloc with fully constant size
        directlyCallsHeapAlloc(callee) and
        not callsEnsureFree(callee) and
        result = directFullyConstAllocSize(callee) and
        // Ensure there's no parameter-dependent alloc (avoid double-counting)
        not directParamPlusConstAlloc(callee, _, _)
        or
        // Case 2: Direct memory_heap_alloc with constant + param, caller passes constant
        exists(int paramIndex, int constPart, int paramVal |
            directlyCallsHeapAlloc(callee) and
            not callsEnsureFree(callee) and
            directParamPlusConstAlloc(callee, paramIndex, constPart) and
            paramVal = constExprValue(call.getArgument(paramIndex)) and
            result = constPart + paramVal
        )
        or
        // Case 3: Wrapper function -- use function-level summary
        not directlyCallsHeapAlloc(callee) and
        not callsEnsureFree(callee) and
        transitivelyCallsHeapAllocWithoutEnsureFree(callee) and
        result = funcConstAllocSize(callee)
    )
}

/**
 * Gets the constant allocation size reachable within function `f`
 * (through inner calls that allocate without their own ensure_free).
 * This is a function-level summary that avoids per-call recursion.
 * May return multiple values; callers use maxConstAllocSize to take the max.
 */
pragma[nomagic]
int funcConstAllocSize(Function f) {
    exists(FunctionCall innerCall |
        innerCall.getEnclosingFunction() = f and
        transitivelyCallsHeapAllocWithoutEnsureFree(innerCall.getTarget()) and
        not isEnsureFreeCall(innerCall) and
        not innerCall.getTarget().hasName("memory_heap_alloc") and
        result = getConstAllocSize(innerCall)
    )
}

/**
 * Holds if the function call `fc` is a call to one of the memory_ensure_free variants.
 */
predicate isEnsureFreeCall(FunctionCall fc) {
    fc.getTarget().hasName("memory_ensure_free")
    or
    fc.getTarget().hasName("memory_ensure_free_opt")
    or
    fc.getTarget().hasName("memory_ensure_free_with_roots")
    or
    fc.getTarget().hasName("memory_erl_nif_env_ensure_free")
}

/**
 * Holds if `fc` is an ensure_free call that actually reserves heap space
 * (i.e., size > 0). Calls with size 0 are GC/shrink operations and don't
 * establish an allocation budget.
 */
predicate isReservingEnsureFreeCall(FunctionCall fc) {
    isEnsureFreeCall(fc) and
    not constExprValue(fc.getArgument(1)) = 0
}

/**
 * Holds if `allocCall` is a function call that transitively calls
 * `memory_heap_alloc` without its own ensure_free (i.e., it relies on
 * the caller to have ensured enough heap space).
 */
predicate isAllocatingCall(FunctionCall allocCall) {
    transitivelyCallsHeapAllocWithoutEnsureFree(allocCall.getTarget()) and
    // Exclude the ensure_free functions themselves
    not isEnsureFreeCall(allocCall) and
    // Exclude memory_heap_alloc itself (we care about wrapper calls)
    not allocCall.getTarget().hasName("memory_heap_alloc")
}

/**
 * Gets the position of a control-flow node within its basic block.
 * Used for precise intra-BB ordering instead of line numbers.
 */
pragma[noinline]
int nodeIndexInBB(ControlFlowNode node, BasicBlock bb) {
    bb.getNode(result) = node
}

/**
 * Holds if `before` precedes `after` in the CFG. For the same basic block,
 * uses node position. For different basic blocks, uses dominance (which
 * guarantees execution order).
 */
pragma[inline]
predicate cfgPrecedes(ControlFlowNode before, BasicBlock beforeBB, ControlFlowNode after, BasicBlock afterBB) {
    beforeBB = afterBB and
    nodeIndexInBB(before, beforeBB) < nodeIndexInBB(after, afterBB)
    or
    beforeBB != afterBB and
    bbStrictlyDominates(beforeBB, afterBB)
}

/**
 * Gets the nearest preceding reserving ensure_free call that dominates
 * the allocation in the CFG. Uses dominance to correctly scope across
 * switch cases (an ensure_free in one case does not dominate another case).
 * Uses CFG node ordering instead of line numbers for precise ordering.
 */
pragma[nomagic]
FunctionCall nearestPrecedingEnsureFree(FunctionCall allocCall) {
    exists(Function enclosing, BasicBlock allocBB, BasicBlock resultBB |
        enclosing = allocCall.getEnclosingFunction() and
        allocBB = allocCall.getBasicBlock() and
        result.getEnclosingFunction() = enclosing and
        isReservingEnsureFreeCall(result) and
        resultBB = result.getBasicBlock() and
        // Ensure_free must precede alloc in CFG
        cfgPrecedes(result, resultBB, allocCall, allocBB) and
        // No other dominating reserving ensure_free between them
        not exists(FunctionCall other, BasicBlock otherBB |
            isReservingEnsureFreeCall(other) and
            other.getEnclosingFunction() = enclosing and
            otherBB = other.getBasicBlock() and
            cfgPrecedes(result, resultBB, other, otherBB) and
            cfgPrecedes(other, otherBB, allocCall, allocBB)
        )
    )
}

/**
 * Gets the worst-case (maximum) constant allocation size for a single call.
 * When getConstAllocSize returns multiple values (e.g., from multiple
 * allocation paths within the callee), takes the maximum.
 */
pragma[noinline]
int maxConstAllocSize(FunctionCall call) {
    result = max(int size | size = getConstAllocSize(call))
}

/**
 * Cached mapping from allocating call to its nearest preceding ensure_free.
 * Materializing this relation avoids repeated evaluation of
 * nearestPrecedingEnsureFree inside the sum aggregation.
 */
pragma[nomagic]
predicate allocToBudget(FunctionCall allocCall, FunctionCall ensureFreeCall) {
    isAllocatingCall(allocCall) and
    ensureFreeCall = nearestPrecedingEnsureFree(allocCall)
}

/**
 * Computes the cumulative allocation size at a given allocation call,
 * summing the worst-case allocation of all preceding calls that share
 * the same ensure_free budget, using CFG ordering (dominance for cross-BB,
 * node position for same-BB).
 */
pragma[noinline]
int cumulativeAllocSize(FunctionCall allocCall, FunctionCall ensureFreeCall) {
    exists(BasicBlock allocBB |
        allocBB = allocCall.getBasicBlock() and
        result =
            sum(FunctionCall other, int otherSize |
                other.getEnclosingFunction() = allocCall.getEnclosingFunction() and
                allocToBudget(other, ensureFreeCall) and
                otherSize = maxConstAllocSize(other) and
                (
                    other = allocCall
                    or
                    exists(BasicBlock otherBB |
                        otherBB = other.getBasicBlock() and
                        cfgPrecedes(other, otherBB, allocCall, allocBB)
                    )
                )
            |
                otherSize
            )
    )
}

// ============================================================
// Symbolic analysis: ensure_free(ctx, var + C) with allocations
// that pass `var` as a parameter to functions like term_alloc_tuple.
// ============================================================

/**
 * Holds if expression `e` decomposes into `v + c` where `v` is a variable
 * and `c` is a compile-time constant. Handles:
 *   - Direct AddExpr: `v + c` or `c + v` (including macro-expanded forms)
 *   - Variable tracing: a non-reassigned local variable whose initializer
 *     is itself `v + c`
 */
pragma[noinline]
predicate exprIsVarPlusConst(Expr e, Variable v, int c) {
    exists(AddExpr add |
        add = e and
        (
            add.getLeftOperand().(VariableAccess).getTarget() = v and
            c = add.getRightOperand().getValue().toInt()
            or
            add.getRightOperand().(VariableAccess).getTarget() = v and
            c = add.getLeftOperand().getValue().toInt()
        )
    )
    or
    // Trace through a non-reassigned local variable to its initializer
    exists(LocalVariable lv |
        e.(VariableAccess).getTarget() = lv and
        not exists(Assignment a | a.getLValue().(VariableAccess).getTarget() = lv) and
        not exists(CrementOperation co | co.getOperand().(VariableAccess).getTarget() = lv) and
        exprIsVarPlusConst(lv.getInitializer().getExpr(), v, c)
    )
}

/**
 * Gets the constant part of a symbolic ensure_free size expression.
 * Holds when the ensure_free size argument is `sharedVar + constPart`.
 * Only matches when the size is NOT a fully-constant expression
 * (those are handled by the constant analysis path).
 */
pragma[noinline]
predicate ensureFreeSymbolicConstPart(
    FunctionCall efCall, Variable sharedVar, int constPart
) {
    isReservingEnsureFreeCall(efCall) and
    exprIsVarPlusConst(efCall.getArgument(1), sharedVar, constPart) and
    // Only for truly symbolic cases (not already handled by constant path)
    not exists(constExprValue(efCall.getArgument(1)))
}

/**
 * Gets the constant part of an allocation call's size when the caller
 * passes `sharedVar` directly as the parameter to a function with
 * `memory_heap_alloc(heap, constPart + param)`.
 */
pragma[noinline]
int symbolicAllocConstPart(FunctionCall allocCall, Variable sharedVar) {
    exists(Function callee, int paramIndex |
        callee = allocCall.getTarget() and
        not callsEnsureFree(callee) and
        directParamPlusConstAlloc(callee, paramIndex, result) and
        allocCall.getArgument(paramIndex).(VariableAccess).getTarget() = sharedVar
    )
}

/**
 * Gets the effective constant cost of an allocation relative to a shared
 * variable. Only counts allocations that provably share the variable,
 * returning just the constant offset (the variable part cancels with the
 * ensure_free). Allocations that don't share the variable are skipped
 * to avoid false positives from functions that consume the variable
 * budget through a different parameter interface.
 */
pragma[noinline]
int effectiveConstCost(FunctionCall allocCall, Variable sharedVar) {
    result = symbolicAllocConstPart(allocCall, sharedVar)
}

/**
 * Computes the cumulative effective constant cost at a given allocation call
 * under a symbolic ensure_free. Sums the effective constant costs of all
 * preceding allocations that share the same ensure_free and variable,
 * using CFG ordering (dominance for cross-BB, node position for same-BB).
 */
pragma[noinline]
int cumulativeEffectiveConstCost(
    FunctionCall allocCall, FunctionCall ensureFreeCall, Variable sharedVar
) {
    exists(BasicBlock allocBB |
        allocBB = allocCall.getBasicBlock() and
        result =
            sum(FunctionCall other, int otherCost |
                other.getEnclosingFunction() = allocCall.getEnclosingFunction() and
                allocToBudget(other, ensureFreeCall) and
                otherCost = effectiveConstCost(other, sharedVar) and
                (
                    other = allocCall
                    or
                    exists(BasicBlock otherBB |
                        otherBB = other.getBasicBlock() and
                        cfgPrecedes(other, otherBB, allocCall, allocBB)
                    )
                )
            |
                otherCost
            )
    )
}

// ============================================================
// Redundant ensure_free detection: a reserving ensure_free
// whose budget is never used because a subsequent call resets
// the heap budget (by calling ensure_free internally).
// ============================================================

/**
 * Holds if `efCall` is a redundant reserving ensure_free: no allocating call
 * uses its budget, and `supersedingCall` is a subsequent call that resets
 * the heap budget (either a direct ensure_free or a function like
 * enif_make_resource that internally calls ensure_free).
 */
pragma[nomagic]
predicate isRedundantEnsureFree(FunctionCall efCall, FunctionCall supersedingCall) {
    isReservingEnsureFreeCall(efCall) and
    // No allocating call uses this ensure_free's budget
    not exists(FunctionCall a | allocToBudget(a, efCall)) and
    // Find a subsequent call that resets the heap budget
    exists(BasicBlock efBB, BasicBlock superBB |
        efBB = efCall.getBasicBlock() and
        supersedingCall.getEnclosingFunction() = efCall.getEnclosingFunction() and
        superBB = supersedingCall.getBasicBlock() and
        (
            // Another direct reserving ensure_free call
            isReservingEnsureFreeCall(supersedingCall) and
            supersedingCall != efCall
            or
            // A function that internally calls ensure_free (e.g., enif_make_resource)
            not isEnsureFreeCall(supersedingCall) and
            callsEnsureFree(supersedingCall.getTarget())
        ) and
        cfgPrecedes(efCall, efBB, supersedingCall, superBB) and
        // Exclude superseding calls on the error-handling path of the
        // ensure_free's own failure check. Pattern:
        //   if (UNLIKELY(ensure_free(...) != MEMORY_GC_OK)) {
        //       RAISE_ERROR(...);  // contains stacktrace_create_raw
        //   }
        // When the if-condition is true (ensure_free FAILED), we enter
        // the error handler. Superseding calls there are irrelevant
        // because the ensure_free budget was never established.
        not exists(BasicBlock trueBB |
            trueBB = efBB.getATrueSuccessor() and
            bbDominates(trueBB, superBB)
        )
    )
}

from FunctionCall problemCall, string msg, FunctionCall relatedCall, string relatedLabel
where
    (
        // Allocation exceeding budget
        exists(int cumCost, int budget |
            allocToBudget(problemCall, relatedCall) and
            (
                // Fully constant comparison
                exists(maxConstAllocSize(problemCall)) and
                cumCost = cumulativeAllocSize(problemCall, relatedCall) and
                budget = constExprValue(relatedCall.getArgument(1)) and
                cumCost > budget
                or
                // Symbolic comparison (ensure_free is var + const,
                //         allocations share the same variable)
                exists(Variable sharedVar |
                    ensureFreeSymbolicConstPart(relatedCall, sharedVar, budget) and
                    exists(effectiveConstCost(problemCall, sharedVar)) and
                    cumCost = cumulativeEffectiveConstCost(problemCall, relatedCall, sharedVar)
                ) and
                cumCost > budget
            ) and
            msg =
                "Cumulative constant-part allocation of " + cumCost.toString() +
                    " terms exceeds ensure_free budget of " + budget.toString() + " terms at $@."
        ) and
        relatedLabel = "this ensure_free call"
    )
    or
    (
        // Redundant ensure_free
        isRedundantEnsureFree(problemCall, relatedCall) and
        msg =
            "Redundant ensure_free: no allocations occur before $@ which resets the heap budget." and
        relatedLabel = relatedCall.getTarget().getName()
    )
select problemCall, msg, relatedCall, relatedLabel
