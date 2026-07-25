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
 * Holds if `f` sets up its own local stack heap via `BEGIN_WITH_STACK_HEAP`, so
 * it allocates into that heap rather than the caller's context heap and must not
 * be charged to a caller's ensure_free.
 *
 * Matches the `BEGIN_WITH_STACK_HEAP` expansion specifically -- a
 * `memory_init_heap_root_fragment` call whose root fragment is a stack-local
 * struct. This excludes `memory_init_heap`, which passes a malloc'd fragment and
 * would misclassify ordinary heap-growing functions as own-heap managers.
 */
pragma[noinline]
predicate directlyInitializesOwnHeap(Function f) {
    exists(FunctionCall fc, AddressOfExpr fragmentAddr |
        fc.getEnclosingFunction() = f and
        fc.getTarget().hasName("memory_init_heap_root_fragment") and
        fragmentAddr = fc.getArgument(1).getAChild*() and
        fragmentAddr.getOperand().(VariableAccess).getTarget() instanceof StackVariable
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
 * Holds if `f` directly or transitively manages its own heap budget -- either
 * by calling a memory_ensure_free variant or by setting up its own local stack
 * heap (`BEGIN_WITH_STACK_HEAP`). Such functions must not be charged to the
 * caller's ensure_free.
 *
 * For detecting a call that resets the *caller's* context budget (the
 * redundant-ensure_free rule), use `transitivelyCallsEnsureFreeOnly` instead,
 * which excludes own-heap setup.
 */
pragma[nomagic]
predicate callsEnsureFree(Function f) {
    directlyCallsEnsureFree(f)
    or
    directlyInitializesOwnHeap(f)
    or
    exists(Function callee |
        callEdge(f, callee) and
        callsEnsureFree(callee)
    )
}

/**
 * Holds if `f` directly or transitively calls a memory_ensure_free variant --
 * i.e. resets the *caller's* context heap budget. Unlike `callsEnsureFree`, this
 * excludes functions that merely set up their own stack heap, which leave the
 * caller's budget untouched. Used by the redundant-ensure_free reset check.
 */
pragma[nomagic]
predicate transitivelyCallsEnsureFreeOnly(Function f) {
    directlyCallsEnsureFree(f)
    or
    exists(Function callee |
        callEdge(f, callee) and
        transitivelyCallsEnsureFreeOnly(callee)
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
 * Holds if all reachable leaf allocating calls from `callee` live in a single
 * function (`callee` itself, or one helper it calls) and are pairwise mutually
 * exclusive there. In this case the worst-case allocation is the max across
 * branches, not the sum.
 *
 * Handles e.g. `term_make_maybe_boxed_int64` (which calls `term_make_boxed_int64`
 * OR `term_make_boxed_int`, never both), including when reached through a thin
 * wrapper -- the exclusive branches sit one level down. Requiring a single
 * common host keeps it sound: leaves in distinct helpers are never
 * `mutuallyExclusiveInFunction`, so `leafAllocContribution` falls back to the sum.
 */
pragma[nomagic]
predicate allLeafCallsDirectAndExclusive(Function callee) {
    // All reachable leaf calls live in one and the same host function.
    exists(Function leafHost |
        forex(FunctionCall leaf |
            reachableLeafAllocCall(callee, leaf)
        |
            leaf.getEnclosingFunction() = leafHost
        )
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

// ============================================================
// Multi-level constant propagation: a constant supplied at a wrapper call
// (e.g. term_alloc_map(3)) flows down through parameter-passthrough wrappers to
// leaf memory_heap_alloc(heap, const + param) calls. leafAllocSize only resolves
// the constant at the leaf wrapper's own call site, so multi-level helpers like
//   term_alloc_map(n) -> term_alloc_map_maybe_shared(n, ..) -> memory_heap_alloc(2 + n)
//                                                           -> term_alloc_tuple(n) -> memory_heap_alloc(1 + n)
// were costed as 0. This recovers their real size (TERM_MAP_SIZE(n) = 3 + 2n).
// ============================================================

/**
 * Holds if `e` is statically a `term_invalid_term()` call -- the marker
 * `term_alloc_map_maybe_shared` tests to decide whether to allocate a fresh keys
 * tuple. Distinguishes a shared-keys map call from `term_alloc_map`.
 */
predicate isInvalidTermExpr(Expr e) {
    e.(FunctionCall).getTarget().hasName("term_invalid_term")
    or
    isInvalidTermExpr(e.(Conversion).getExpr())
}

/**
 * Holds if `leafCall` only executes when parameter `paramIdx` of its enclosing
 * function is an invalid term -- it sits on the true branch of a
 * `term_is_invalid_term(param)` ternary, e.g. the keys-tuple allocation inside
 * `term_alloc_map_maybe_shared`:
 *   keys = term_is_invalid_term(keys) ? term_alloc_tuple(size, heap) : keys;
 */
pragma[noinline]
predicate leafGuardedByInvalidTermParam(FunctionCall leafCall, int paramIdx) {
    exists(Function f, FunctionCall guard, ConditionalExpr ce |
        f = leafCall.getEnclosingFunction() and
        ce.getEnclosingFunction() = f and
        (ce.getCondition() = guard or ce.getCondition().(Conversion).getExpr() = guard) and
        guard.getTarget().hasName("term_is_invalid_term") and
        guard.getArgument(0).(VariableAccess).getTarget() = f.getParameter(paramIdx) and
        ce.getThen() = leafCall.getParent*()
    )
}

/**
 * Gets the size of a single leaf allocator call `leafCall` (whose target
 * directly calls `memory_heap_alloc(heap, constPart + param)`) when its size
 * argument is a forwarded parameter whose value is resolved from the constant
 * passed at the ancestor wrapper call `wc`.
 *
 * Only matches leaves whose size argument is not itself constant (those are
 * handled by `leafAllocSize`), so this never double-counts. Excludes a leaf
 * guarded by `term_is_invalid_term(param)` when `wc` passes a real value for that
 * parameter -- the guarded allocation does not happen.
 */
pragma[noinline]
int propagatedLeafAllocSize(FunctionCall wc, FunctionCall leafCall) {
    exists(
        Function leafWrapper, int leafParamIdx, int constPart, Function encl, int enclParamIdx
    |
        leafWrapper = leafCall.getTarget() and
        directParamPlusConstAlloc(leafWrapper, leafParamIdx, constPart) and
        not exists(constExprValue(leafCall.getArgument(leafParamIdx))) and
        encl = leafCall.getEnclosingFunction() and
        leafCall.getArgument(leafParamIdx).(VariableAccess).getTarget() =
            encl.getParameter(enclParamIdx) and
        result = constPart + resolvedParamValue(wc, encl, enclParamIdx) and
        not exists(int gIdx |
            leafGuardedByInvalidTermParam(leafCall, gIdx) and
            wc.getTarget() = encl and
            not isInvalidTermExpr(wc.getArgument(gIdx))
        )
    )
}

/**
 * Gets the total additional allocation triggered by a wrapper call `wc` whose
 * constant argument resolves otherwise-unresolved leaf allocations beneath it.
 * Only holds when that total is strictly positive.
 */
pragma[noinline]
int propagatedCallAllocSize(FunctionCall wc) {
    result =
        sum(FunctionCall leafCall |
            reachableLeafAllocCall(wc.getTarget(), leafCall)
        |
            propagatedLeafAllocSize(wc, leafCall)
        ) and
    result > 0
}

/**
 * Holds if function `f` is reachable from `root` through a chain of callees
 * that do not manage their own heap (none call ensure_free), so all of their
 * allocations are charged to `root`'s caller's ensure_free budget.
 */
pragma[nomagic]
predicate reachableNoEnsureFreeFunction(Function root, Function f) {
    f = root
    or
    exists(Function mid |
        reachableNoEnsureFreeFunction(root, mid) and
        callEdge(mid, f) and
        not callsEnsureFree(f)
    )
}

/**
 * Gets the constant allocation recovered by multi-level constant propagation
 * for an allocating call `call`: the propagated size of `call` itself, plus the
 * propagated size of every wrapper call located in the no-ensure_free subgraph
 * reachable from `call`'s target (e.g. a `term_alloc_map(3)` call nested inside
 * a helper reached through `call`).
 */
pragma[noinline]
int subgraphPropagatedAllocSize(FunctionCall call) {
    result =
        sum(FunctionCall wc |
            wc = call
            or
            exists(Function f |
                reachableNoEnsureFreeFunction(call.getTarget(), f) and
                wc.getEnclosingFunction() = f
            )
        |
            propagatedCallAllocSize(wc)
        )
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
 * - Multi-level propagated contributions (subgraphPropagatedAllocSize), which
 *   recover constants supplied at nested wrapper calls such as term_alloc_map(n)
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
            or
            subgraphPropagatedAllocSize(call) > 0
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
            +
            // Multi-level constants forwarded through parameter-passthrough
            // wrappers (e.g. term_alloc_map(n) -> ... -> memory_heap_alloc(c + n))
            subgraphPropagatedAllocSize(call)
    )
}

/**
 * Holds if the function call `fc` is a call to one of the memory_ensure_free variants.
 */
pragma[noinline]
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
pragma[noinline]
predicate isReservingEnsureFreeCall(FunctionCall fc) {
    isEnsureFreeCall(fc) and
    not constExprValue(fc.getArgument(1)) = 0
}

/**
 * Holds if function `f` contains at least one reserving ensure_free call.
 *
 * Gates the whole budget analysis: a report can only fire inside such a function
 * (the ensure_free and allocation must share an enclosing function). Restricting
 * `isAllocatingCall` up front prunes the expensive per-call const-size machinery
 * to the functions that establish a budget, without changing any result.
 */
pragma[noinline]
predicate functionHasReservingEnsureFree(Function f) {
    exists(FunctionCall ef |
        ef.getEnclosingFunction() = f and
        isReservingEnsureFreeCall(ef)
    )
}

/**
 * Holds if `allocCall` is a function call that transitively calls
 * `memory_heap_alloc` without its own ensure_free (i.e., it relies on
 * the caller to have ensured enough heap space).
 */
pragma[noinline]
predicate isAllocatingCall(FunctionCall allocCall) {
    // Only allocations in functions that establish a budget can ever be reported
    // (same-function requirement).
    functionHasReservingEnsureFree(allocCall.getEnclosingFunction()) and
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
 * Holds if reserving ensure_free `ef` sits in the then-branch of an `if` and the
 * allocation `allocCall` is a later sibling statement of that `if` in the same
 * block -- the `if (cond) { ...; ensure_free(B); ... } ...; alloc(...)` pattern.
 * Pure AST (no CFG), so it is cheap and excludes `switch` cases for free.
 */
predicate conditionalGuardThenSibling(FunctionCall ef, FunctionCall allocCall) {
    exists(IfStmt guardIf, BlockStmt block, int efIdx, int allocIdx, Stmt allocSibling |
        ef.getEnclosingStmt().getParentStmt*() = guardIf.getThen() and
        guardIf = block.getStmt(efIdx) and
        allocSibling = block.getStmt(allocIdx) and
        efIdx < allocIdx and
        allocSibling = allocCall.getEnclosingStmt().getParentStmt*()
    )
}

/**
 * Gets a conditionally-executed reserving ensure_free that governs `allocCall`,
 * for the pattern `if (cond) { ensure_free(B); } ... alloc(...)` where the
 * ensure_free does not run on the `cond == false` path.
 *
 * Only applies when `allocCall` has no dominating reserving ensure_free at all.
 * Sound for under-allocation detection: on every execution either this
 * ensure_free ran (the allocation faces its budget) or it was skipped (the
 * allocation faces no reservation), so an oversized allocation overflows either way.
 */
pragma[nomagic]
FunctionCall conditionalPrecedingEnsureFree(FunctionCall allocCall) {
    not exists(nearestPrecedingEnsureFree(allocCall)) and
    isReservingEnsureFreeCall(result) and
    result.getEnclosingFunction() = allocCall.getEnclosingFunction() and
    conditionalGuardThenSibling(result, allocCall)
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
 * Cached mapping from allocating call to its preceding ensure_free budget --
 * a dominating one (nearestPrecedingEnsureFree) or, when none dominates, a
 * conditionally-executed one (conditionalPrecedingEnsureFree). Mutually
 * exclusive, so no allocation is charged twice.
 */
pragma[nomagic]
predicate allocToBudget(FunctionCall allocCall, FunctionCall ensureFreeCall) {
    isAllocatingCall(allocCall) and
    (
        ensureFreeCall = nearestPrecedingEnsureFree(allocCall)
        or
        ensureFreeCall = conditionalPrecedingEnsureFree(allocCall)
    )
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
 * Gets the context (or environment) variable that an ensure_free call reserves
 * heap on -- the first argument, e.g. `ctx` in `memory_ensure_free_opt(ctx,..)`.
 * Used to ensure a "superseding" reset acts on the *same* context.
 */
pragma[noinline]
Variable ensureFreeContextVar(FunctionCall efCall) {
    isEnsureFreeCall(efCall) and
    result = efCall.getArgument(0).(VariableAccess).getTarget()
}

/**
 * Holds if `consumer` consumes the heap budget reserved on `efCall`'s context
 * without going through `memory_heap_alloc`. `memory_copy_term_tree(&ctx->heap,
 * t)` bumps the destination heap pointer directly, so it is invisible to
 * `allocToBudget`, yet it is often the reason such an ensure_free exists (e.g.
 * spawn reserves then copies the args in). Recognising it stops the
 * redundant-ensure_free rule from flagging those reservations. `consumer` must
 * mention the context variable.
 */
pragma[noinline]
predicate consumesContextBudget(FunctionCall efCall, FunctionCall consumer) {
    consumer.getEnclosingFunction() = efCall.getEnclosingFunction() and
    (
        consumer.getTarget().hasName("memory_copy_term_tree") or
        consumer.getTarget().hasName("memory_copy_term_tree_to_storage")
    ) and
    consumer.getAnArgument().getAChild*().(VariableAccess).getTarget() =
        ensureFreeContextVar(efCall)
}

/**
 * Holds if `f` tears down a context (e.g. `context_destroy`): it frees the
 * context rather than re-establishing a usable heap budget on it.
 */
predicate isContextTeardown(Function f) {
    f.hasName("context_destroy")
}

/**
 * Holds if `efCall` is a redundant reserving ensure_free: no allocating call
 * uses its budget, and `supersedingCall` is a subsequent call that resets
 * the heap budget (either a direct ensure_free or a function like
 * enif_make_resource that internally calls ensure_free).
 */
pragma[nomagic]
predicate isRedundantEnsureFree(FunctionCall efCall, FunctionCall supersedingCall) {
    isReservingEnsureFreeCall(efCall) and
    // Test code calls ensure_free for its GC side effects
    not efCall.getFile().getRelativePath().matches("tests/%") and
    // No allocating call uses this ensure_free's budget
    not exists(FunctionCall a | allocToBudget(a, efCall)) and
    // ...and no pointer-bumping consumer (memory_copy_term_tree) uses it either
    not exists(FunctionCall c, BasicBlock efBB0, BasicBlock cBB |
        consumesContextBudget(efCall, c) and
        efBB0 = efCall.getBasicBlock() and
        cBB = c.getBasicBlock() and
        cfgPrecedes(efCall, efBB0, c, cBB)
    ) and
    // Find a subsequent call that resets the heap budget
    exists(BasicBlock efBB, BasicBlock superBB, Variable ctxVar |
        efBB = efCall.getBasicBlock() and
        supersedingCall.getEnclosingFunction() = efCall.getEnclosingFunction() and
        superBB = supersedingCall.getBasicBlock() and
        // The reset must act on the SAME context this ensure_free reserves on:
        // a reset of a different context (e.g. the spawned `new_ctx` vs the
        // caller's `ctx`) does not make this reservation redundant.
        ctxVar = ensureFreeContextVar(efCall) and
        (
            // Another direct reserving ensure_free call on the same context
            isReservingEnsureFreeCall(supersedingCall) and
            supersedingCall != efCall and
            ensureFreeContextVar(supersedingCall) = ctxVar
            or
            // A function that internally calls ensure_free on the caller's
            // context (e.g., enif_make_resource), passed that same context as an
            // argument. Uses the ensure_free-only notion: own-heap setup does not
            // reset this context's budget. Context teardown (context_destroy) is
            // excluded: it frees the context rather than re-establishing a budget.
            not isEnsureFreeCall(supersedingCall) and
            not isContextTeardown(supersedingCall.getTarget()) and
            transitivelyCallsEnsureFreeOnly(supersedingCall.getTarget()) and
            supersedingCall.getAnArgument().(VariableAccess).getTarget() = ctxVar
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

// ============================================================
// Full affine (symbolic) accounting.
//
// Every size (an ensure_free budget or an allocation) is treated as an affine
// form: a constant plus a sum of symbolic "atoms" keyed by the variable carrying
// the byte size. A constant shortfall is reported only when the symbolic atoms
// cancel exactly (same atom, same coefficient on both sides).
//
// Soundness: if ANY summand cannot be modelled, the whole comparison for that
// ensure_free is suppressed -- so unmodelled code yields missed bugs, never
// false positives. The only modelled atom is binary byte data; everything else
// must reduce to a compile-time constant.
// ============================================================

/**
 * Gets an additive summand of `root`: a non-`+` expression reached through the
 * operands of a chain of `+` expressions, tracing through non-reassigned local
 * variables whose initializer is itself a size expression (e.g. the local
 * `ensure_packet_avail` / `requested_size` accumulators).
 */
predicate additiveSummand(Expr root, Expr s) {
    not root instanceof AddExpr and
    not isTraceableSizeLocal(root) and
    s = root
    or
    exists(AddExpr a | a = root |
        additiveSummand(a.getLeftOperand(), s)
        or
        additiveSummand(a.getRightOperand(), s)
    )
    or
    exists(LocalVariable lv |
        isTraceableSizeLocal(root) and
        root.(VariableAccess).getTarget() = lv and
        additiveSummand(lv.getInitializer().getExpr(), s)
    )
}

/**
 * Holds if `e` is an access to a non-reassigned local variable that has an
 * initializer -- a size accumulator that should be traced into rather than
 * treated as an opaque symbolic summand.
 */
predicate isTraceableSizeLocal(Expr e) {
    exists(LocalVariable lv |
        e.(VariableAccess).getTarget() = lv and
        exists(lv.getInitializer().getExpr()) and
        not exists(Assignment a | a.getLValue().(VariableAccess).getTarget() = lv) and
        not exists(CrementOperation co | co.getOperand().(VariableAccess).getTarget() = lv)
    )
}

/**
 * Binary-creating allocators and the index of the argument carrying the binary
 * byte size. The heap-binary worst case allocates
 * `term_binary_data_size_in_terms(size) + 1` words on the process heap.
 */
predicate binaryCreator(string name, int sizeArgIdx) {
    name = "term_create_uninitialized_binary" and sizeArgIdx = 0
    or
    name = "term_create_empty_binary" and sizeArgIdx = 0
    or
    name = "term_from_literal_binary" and sizeArgIdx = 1
    or
    // term_reuse_binary either reuses an existing refc binary in place (no heap
    // growth) or, when the source is not a reusable refcount-1 refc binary,
    // falls back to term_create_empty_binary(size) -- a size-dependent heap
    // binary. The fallback cannot be ruled out statically, so a correct caller
    // must reserve for it; model it as a binary creator.
    // term_from_const_binary is deliberately NOT listed: it always allocates a
    // fixed TERM_BOXED_REFC_BINARY_SIZE boxed term (const data is never copied
    // onto the process heap), so its byte-size argument is not a heap-data atom.
    name = "term_reuse_binary" and sizeArgIdx = 1
}

/**
 * Holds if `ac` allocates a binary whose byte size is the variable `v` (a
 * symbolic binary atom). A constant size is not an atom -- it folds into the
 * constant part and is handled by the existing constant paths.
 */
predicate binaryAllocAtom(FunctionCall ac, Variable v) {
    exists(string name, int idx |
        binaryCreator(name, idx) and
        ac.getTarget().hasName(name) and
        ac.getArgument(idx).(VariableAccess).getTarget() = v
    )
}

/**
 * Holds if `summand` is a binary-data budget atom over variable `v`, with the
 * constant words the summand contributes on top of the symbolic byte data:
 *  - term_binary_data_size_in_terms(v)  reserves the data + size field only (0),
 *  - term_binary_heap_size(v)           reserves additionally BINARY_HEADER_SIZE (2).
 * The heap binary itself allocates term_binary_data_size_in_terms(v) + 1, so a
 * bare term_binary_data_size_in_terms budget is short by one word.
 */
predicate budgetBinSummand(FunctionCall ef, Expr summand, Variable v, int constContrib) {
    isReservingEnsureFreeCall(ef) and
    additiveSummand(ef.getArgument(1), summand) and
    summand.(FunctionCall).getArgument(0).(VariableAccess).getTarget() = v and
    (
        summand.(FunctionCall).getTarget().hasName("term_binary_data_size_in_terms") and
        constContrib = 0
        or
        // BINARY_HEADER_SIZE
        summand.(FunctionCall).getTarget().hasName("term_binary_heap_size") and
        constContrib = 2
    )
}

/** Holds if `summand` is a binary-data budget atom over variable `v`. */
predicate budgetBinAtom(FunctionCall ef, Expr summand, Variable v) {
    budgetBinSummand(ef, summand, v, _)
}

/**
 * Gets the worst-case constant value of a budget summand: its compile-time
 * value, or, for a `cond ? A : B` conditional with both branches constant, the
 * larger branch (worst-case budget keeps the comparison sound).
 */
int budgetConstSummandValue(Expr s) {
    result = s.getValue().toInt()
    or
    not exists(s.getValue()) and
    exists(ConditionalExpr ce | ce = s |
        result = ce.getThen().getValue().toInt().maximum(ce.getElse().getValue().toInt())
    )
}

/**
 * Gets the constant part of an ensure_free budget: the sum of constant summands
 * plus the constant words contributed by binary-atom summands (e.g. the
 * BINARY_HEADER_SIZE inside term_binary_heap_size).
 */
int budgetConstPart(FunctionCall ef) {
    isReservingEnsureFreeCall(ef) and
    result =
        sum(Expr s | additiveSummand(ef.getArgument(1), s) | budgetConstSummandValue(s)) +
        sum(Expr s, int c | budgetBinSummand(ef, s, _, c) | c)
}

/**
 * Gets the coefficient of binary atom `v` in the budget (count of summands).
 *
 * Uses `strictcount` so the predicate is undefined (not `0`) when `v` is not a
 * budget atom; a plain `count` would materialize the full `FunctionCall x
 * Variable` cross product, the dominant cost of this query.
 */
pragma[noinline]
int budgetBinCoeff(FunctionCall ef, Variable v) {
    result = strictcount(Expr s | budgetBinAtom(ef, s, v))
}

/**
 * Holds if the budget has a summand that is neither a compile-time constant nor
 * a modelled binary atom -- an unrecognised symbolic term that forces the
 * comparison to be suppressed.
 */
predicate budgetHasUnmodeledSummand(FunctionCall ef) {
    isReservingEnsureFreeCall(ef) and
    exists(Expr s |
        additiveSummand(ef.getArgument(1), s) and
        not exists(budgetConstSummandValue(s)) and
        not budgetBinAtom(ef, s, _)
    )
}

/**
 * Gets the affine constant contribution of an allocation: a binary allocation
 * contributes its constant header (1 word, the symbolic byte data being an
 * atom), any other allocation contributes its constant size.
 */
int affineAllocConst(FunctionCall a) {
    binaryAllocAtom(a, _) and result = 1
    or
    not binaryAllocAtom(a, _) and result = maxConstAllocSize(a)
}

/**
 * Holds if `a` is an allocation charged to `ef` that cannot be modelled (not a
 * binary atom and with no constant size) -- forces suppression.
 */
predicate affineUnmodeledAlloc(FunctionCall a, FunctionCall ef) {
    allocToBudget(a, ef) and
    not binaryAllocAtom(a, _) and
    not exists(maxConstAllocSize(a))
}

/**
 * Gets the cumulative affine constant cost at `allocCall`: the sum of the
 * affine constant contributions of every allocation sharing `ef`'s budget that
 * precedes (or is) `allocCall` in the CFG.
 */
int affineCumulativeConst(FunctionCall allocCall, FunctionCall ef) {
    exists(BasicBlock allocBB |
        allocBB = allocCall.getBasicBlock() and
        result =
            sum(FunctionCall other, int sz |
                allocToBudget(other, ef) and
                sz = affineAllocConst(other) and
                (
                    other = allocCall
                    or
                    exists(BasicBlock otherBB |
                        otherBB = other.getBasicBlock() and
                        cfgPrecedes(other, otherBB, allocCall, allocBB)
                    )
                )
            |
                sz
            )
    )
}

/**
 * Gets the binary-atom coefficient for `v` among allocations sharing `ef` up to
 * `allocCall`. Uses `strictcount` so it is undefined (not `0`) when `v` is not
 * an allocation atom, avoiding the `(allocCall, ef, v)` cross product.
 */
pragma[noinline]
int allocBinCoeff(FunctionCall allocCall, FunctionCall ef, Variable v) {
    exists(BasicBlock allocBB |
        allocBB = allocCall.getBasicBlock() and
        result =
            strictcount(FunctionCall other |
                allocToBudget(other, ef) and
                binaryAllocAtom(other, v) and
                (
                    other = allocCall
                    or
                    exists(BasicBlock otherBB |
                        otherBB = other.getBasicBlock() and
                        cfgPrecedes(other, otherBB, allocCall, allocBB)
                    )
                )
            )
    )
}

/**
 * Holds if the symbolic atoms of the budget exactly match those of the
 * allocations charged to it up to `allocCall` (same atom, same coefficient).
 */
predicate affineAtomsMatch(FunctionCall allocCall, FunctionCall ef) {
    // Every budget atom must have a matching allocation coefficient and
    // vice-versa. budgetBinCoeff/allocBinCoeff are undefined (not 0) for
    // non-atoms, so each forall ranges only over the atoms present on that side;
    // a missing counterpart fails the equality and suppresses the comparison.
    forall(Variable v | exists(budgetBinCoeff(ef, v)) |
        budgetBinCoeff(ef, v) = allocBinCoeff(allocCall, ef, v)
    ) and
    forall(Variable v | exists(allocBinCoeff(allocCall, ef, v)) |
        allocBinCoeff(allocCall, ef, v) = budgetBinCoeff(ef, v)
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
                or
                // Affine comparison: the budget mixes a constant part with
                // modelled symbolic atoms (binary byte data). Compare constant
                // parts only once every symbolic atom cancels exactly and no
                // summand or charged allocation is unmodelled.
                exists(maxConstAllocSize(problemCall)) and
                budgetBinCoeff(relatedCall, _) > 0 and
                not budgetHasUnmodeledSummand(relatedCall) and
                not exists(FunctionCall u |
                    affineUnmodeledAlloc(u, relatedCall) and
                    (
                        u = problemCall
                        or
                        exists(BasicBlock ub, BasicBlock pb |
                            ub = u.getBasicBlock() and
                            pb = problemCall.getBasicBlock() and
                            cfgPrecedes(u, ub, problemCall, pb)
                        )
                    )
                ) and
                affineAtomsMatch(problemCall, relatedCall) and
                cumCost = affineCumulativeConst(problemCall, relatedCall) and
                budget = budgetConstPart(relatedCall) and
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
