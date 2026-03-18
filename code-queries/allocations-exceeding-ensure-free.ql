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
 * Holds if `leafCall` is a call reachable from function `f` (through wrapper
 * functions that don't call ensure_free) to a function that directly calls
 * `memory_heap_alloc`. This is a monotonic reachability predicate that avoids
 * non-monotonic recursion through aggregation.
 */
pragma[nomagic]
predicate reachableLeafAllocCall(Function f, FunctionCall leafCall) {
    // Base: f directly contains a call to a wrapper that directly allocates
    leafCall.getEnclosingFunction() = f and
    not isEnsureFreeCall(leafCall) and
    not leafCall.getTarget().hasName("memory_heap_alloc") and
    directlyCallsHeapAlloc(leafCall.getTarget()) and
    not callsEnsureFree(leafCall.getTarget())
    or
    // Recursive: f calls wrapper g which has reachable leaf alloc calls
    exists(FunctionCall callToG, Function g |
        callToG.getEnclosingFunction() = f and
        g = callToG.getTarget() and
        not callsEnsureFree(g) and
        not isEnsureFreeCall(callToG) and
        not g.hasName("memory_heap_alloc") and
        reachableLeafAllocCall(g, leafCall)
    )
}

/**
 * Gets the constant allocation size for a call to a function that directly
 * calls `memory_heap_alloc` (a leaf wrapper). Non-recursive: only considers
 * the direct `memory_heap_alloc` calls within the callee.
 */
pragma[noinline]
int leafAllocSize(FunctionCall call) {
    exists(Function callee | callee = call.getTarget() |
        directlyCallsHeapAlloc(callee) and
        not callsEnsureFree(callee) and
        result =
            sum(int s | s = directFullyConstAllocSize(callee) | s)
            +
            sum(int paramIndex, int constPart |
                directParamPlusConstAlloc(callee, paramIndex, constPart) and
                exists(constExprValue(call.getArgument(paramIndex)))
            | constPart + constExprValue(call.getArgument(paramIndex)))
    )
}

// ============================================================
// Branch resolution: when the caller passes a constant, determine
// which branch of a conditional in the callee is actually taken,
// so that allocations on the not-taken branch are excluded.
// ============================================================

/**
 * Holds if `predFunc` is a simple threshold predicate function: it has
 * a single return statement of the form `param < THRESHOLD`, returning
 * true when the parameter is below the threshold.
 */
pragma[noinline]
predicate isLTThresholdPredicate(Function predFunc, int paramIndex, int threshold) {
    exists(ReturnStmt ret, LTExpr lt |
        strictcount(ReturnStmt r | r.getEnclosingFunction() = predFunc) = 1 and
        ret.getEnclosingFunction() = predFunc and
        (
            lt = ret.getExpr()
            or
            lt = ret.getExpr().(Conversion).getExpr()
        ) and
        lt.getLeftOperand().(VariableAccess).getTarget() = predFunc.getParameter(paramIndex) and
        threshold = constExprValue(lt.getRightOperand())
    )
}

/**
 * Holds if function `wrapper` directly passes its parameter `wrapperParamIdx`
 * as argument `calleeParamIdx` in a call to `callee`.
 */
pragma[noinline]
predicate paramPassthrough(Function wrapper, int wrapperParamIdx, Function callee, int calleeParamIdx) {
    exists(FunctionCall directCall |
        directCall.getEnclosingFunction() = wrapper and
        directCall.getTarget() = callee and
        directCall.getArgument(calleeParamIdx).(VariableAccess).getTarget() =
            wrapper.getParameter(wrapperParamIdx)
    )
}

/**
 * Gets the constant value that `targetFunc.getParameter(targetParamIdx)`
 * receives when `outerCall` is invoked, tracing constants through up to
 * two levels of wrapper functions that pass the parameter through.
 */
pragma[noinline]
int resolvedParamValue(FunctionCall outerCall, Function targetFunc, int targetParamIdx) {
    // Direct: targetFunc is the immediate callee
    targetFunc = outerCall.getTarget() and
    result = constExprValue(outerCall.getArgument(targetParamIdx))
    or
    // One level deep: outerCallee passes through to targetFunc
    exists(Function outerCallee, int outerParamIdx |
        outerCallee = outerCall.getTarget() and
        paramPassthrough(outerCallee, outerParamIdx, targetFunc, targetParamIdx) and
        result = constExprValue(outerCall.getArgument(outerParamIdx))
    )
    or
    // Two levels deep: outerCallee -> intermediate -> targetFunc
    exists(
        Function outerCallee, Function intermediate, int outerParamIdx, int intermediateParamIdx
    |
        outerCallee = outerCall.getTarget() and
        paramPassthrough(outerCallee, outerParamIdx, intermediate, intermediateParamIdx) and
        paramPassthrough(intermediate, intermediateParamIdx, targetFunc, targetParamIdx) and
        result = constExprValue(outerCall.getArgument(outerParamIdx))
    )
}

/**
 * Holds if `leafCall` in function `leafFunc` is on a branch that is provably
 * not taken when the outer call `outerCall` passes constant arguments.
 *
 * Detects the pattern: if (predicateFunc(param)) { ... } else { ... }
 * where predicateFunc is a simple `param < THRESHOLD` function, and the
 * constant value of param resolves the condition.
 */
pragma[noinline]
predicate leafOnResolvedNotTakenBranch(FunctionCall outerCall, FunctionCall leafCall) {
    exists(
        Function leafFunc, IfStmt ifStmt, FunctionCall condCall, Function condFunc,
        int condFuncParamIdx, int threshold, int leafFuncParamIdx, int paramValue,
        BasicBlock condBB, BasicBlock notTakenBB
    |
        leafCall.getEnclosingFunction() = leafFunc and
        ifStmt.getEnclosingFunction() = leafFunc and
        // The condition is a call to a simple threshold predicate
        (
            condCall = ifStmt.getCondition()
            or
            condCall = ifStmt.getCondition().(Conversion).getExpr()
        ) and
        condFunc = condCall.getTarget() and
        isLTThresholdPredicate(condFunc, condFuncParamIdx, threshold) and
        // The predicate receives a parameter of leafFunc
        condCall.getArgument(condFuncParamIdx).(VariableAccess).getTarget() =
            leafFunc.getParameter(leafFuncParamIdx) and
        // Resolve the constant value from the outer call
        paramValue = resolvedParamValue(outerCall, leafFunc, leafFuncParamIdx) and
        condBB = condCall.getBasicBlock() and
        // Determine which branch is NOT taken
        (
            // param < threshold is TRUE → false branch not taken
            paramValue < threshold and
            notTakenBB = condBB.getAFalseSuccessor()
            or
            // param < threshold is FALSE → true branch not taken
            paramValue >= threshold and
            notTakenBB = condBB.getATrueSuccessor()
        ) and
        // The leaf call is on the not-taken branch
        bbDominates(notTakenBB, leafCall.getBasicBlock())
    )
}

/**
 * Holds if two function calls within the same function are on mutually
 * exclusive control-flow branches (different successors of the same
 * branch point dominate each call, so they cannot both execute).
 */
pragma[inline]
predicate mutuallyExclusiveInFunction(FunctionCall call1, FunctionCall call2) {
    call1.getEnclosingFunction() = call2.getEnclosingFunction() and
    call1 != call2 and
    exists(BasicBlock branchBB, BasicBlock succ1, BasicBlock succ2 |
        succ1 = branchBB.getASuccessor() and
        succ2 = branchBB.getASuccessor() and
        succ1 != succ2 and
        bbDominates(succ1, call1.getBasicBlock()) and
        bbDominates(succ2, call2.getBasicBlock())
    )
}

/**
 * Holds if all reachable leaf allocating calls from `callee` are direct
 * (within the callee itself) and pairwise mutually exclusive. In this case,
 * the worst-case allocation is the max across branches, not the sum.
 *
 * This handles functions like `term_make_maybe_boxed_int64` which call
 * different allocating functions on exclusive branches (e.g.,
 * `term_make_boxed_int64` OR `term_make_boxed_int`, never both).
 */
pragma[nomagic]
predicate allLeafCallsDirectAndExclusive(Function callee) {
    // All reachable leaf calls must be directly within the callee
    forex(FunctionCall leaf |
        reachableLeafAllocCall(callee, leaf)
    |
        leaf.getEnclosingFunction() = callee
    ) and
    // All pairs of distinct leaf calls must be mutually exclusive
    forall(FunctionCall leaf1, FunctionCall leaf2 |
        reachableLeafAllocCall(callee, leaf1) and
        reachableLeafAllocCall(callee, leaf2) and
        leaf1 != leaf2
    |
        mutuallyExclusiveInFunction(leaf1, leaf2)
    )
}

/**
 * Gets the leaf allocation contribution for a call, considering mutual
 * exclusivity. When all reachable leaf calls are direct children on
 * mutually exclusive branches, takes the max (worst-case branch) instead
 * of the sum. Falls back to sum (conservative) otherwise.
 */
pragma[noinline]
int leafAllocContribution(FunctionCall call, Function callee) {
    // When all leaf calls are direct and mutually exclusive, take the max
    allLeafCallsDirectAndExclusive(callee) and
    (
        result =
            max(FunctionCall leafCall, int leafSize |
                reachableLeafAllocCall(callee, leafCall) and
                leafSize = leafAllocSize(leafCall) and
                not leafOnResolvedNotTakenBranch(call, leafCall)
            | leafSize)
        or
        // All leaf calls are on not-taken branches
        not exists(FunctionCall leafCall |
            reachableLeafAllocCall(callee, leafCall) and
            exists(leafAllocSize(leafCall)) and
            not leafOnResolvedNotTakenBranch(call, leafCall)
        ) and
        result = 0
    )
    or
    // Default: sum all leaf contributions (conservative)
    not allLeafCallsDirectAndExclusive(callee) and
    result =
        sum(FunctionCall leafCall, int leafSize |
            reachableLeafAllocCall(callee, leafCall) and
            leafSize = leafAllocSize(leafCall) and
            not leafOnResolvedNotTakenBranch(call, leafCall)
        | leafSize)
}

/**
 * Computes the total constant allocation size for a call to a function that
 * transitively calls `memory_heap_alloc` without its own ensure_free.
 *
 * Aggregates all applicable constant contributions from the callee:
 * - All fully-constant direct `memory_heap_alloc` sizes
 * - All `memory_heap_alloc(heap, constant + param)` where the caller passes
 *   a constant for that parameter
 * - All transitive wrapper function contributions (via reachableLeafAllocCall),
 *   using max for mutually exclusive branches, sum otherwise
 */
pragma[noinline]
int getConstAllocSize(FunctionCall call) {
    exists(Function callee | callee = call.getTarget() |
        not callsEnsureFree(callee) and
        transitivelyCallsHeapAllocWithoutEnsureFree(callee) and
        // At least one constant contribution must exist
        (
            exists(directFullyConstAllocSize(callee))
            or
            exists(int pi, int cp |
                directParamPlusConstAlloc(callee, pi, cp) and
                exists(constExprValue(call.getArgument(pi)))
            )
            or
            exists(FunctionCall leafCall |
                reachableLeafAllocCall(callee, leafCall) and
                exists(leafAllocSize(leafCall)) and
                not leafOnResolvedNotTakenBranch(call, leafCall)
            )
        ) and
        result =
            // Sum all fully-constant direct allocations
            sum(int s | s = directFullyConstAllocSize(callee) | s)
            +
            // Sum all param+const allocations where caller passes a constant
            sum(int paramIndex, int constPart |
                directParamPlusConstAlloc(callee, paramIndex, constPart) and
                exists(constExprValue(call.getArgument(paramIndex)))
            | constPart + constExprValue(call.getArgument(paramIndex)))
            +
            // Transitive wrapper contributions via reachable leaf calls,
            // using max for mutually exclusive branches, sum otherwise
            leafAllocContribution(call, callee)
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
