/**
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * @name Allocations without ensure_free in NIFs and port handlers
 * @kind problem
 * @problem.severity error
 * @id atomvm/allocations-without-ensure-free
 * @tags correctness
 * @precision high
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

import cpp
import semmle.code.cpp.controlflow.BasicBlocks

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
            fc.getTarget().hasName("memory_erl_nif_env_ensure_free") or
            fc.getTarget().hasName("memory_init_heap") or
            fc.getTarget().hasName("memory_init_heap_root_fragment")
        )
    )
}

/**
 * Holds if `caller` directly calls `callee`.
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
 * Holds if `fc` is a call to one of the memory_ensure_free variants.
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
 * Holds if `allocCall` is a function call that transitively calls
 * `memory_heap_alloc` without its own ensure_free.
 */
predicate isAllocatingCall(FunctionCall allocCall) {
    transitivelyCallsHeapAllocWithoutEnsureFree(allocCall.getTarget()) and
    not isEnsureFreeCall(allocCall) and
    not allocCall.getTarget().hasName("memory_heap_alloc")
}

/**
 * Holds if `f` is a NIF: returns `term` and has parameters (Context *, int, term []).
 */
predicate isNif(Function f) {
    f.getNumberOfParameters() = 3 and
    f.getType().toString() = "term" and
    f.getParameter(0).getType().stripType().(Struct).hasName("Context") and
    f.getParameter(2).getType().toString() = "term[]"
}

/**
 * Holds if `f` is a port handler: returns `NativeHandlerResult` and has
 * a single parameter (Context *).
 */
predicate isPortHandler(Function f) {
    f.getNumberOfParameters() = 1 and
    f.getType().toString() = "NativeHandlerResult" and
    f.getParameter(0).getType().stripType().(Struct).hasName("Context")
}

/**
 * Holds if `f` is a NIF or port handler entry point.
 */
predicate isEntryPoint(Function f) {
    isNif(f) or isPortHandler(f)
}

/**
 * Holds if `bb` contains at least one call to an ensure_free variant
 * or to a function that transitively calls ensure_free.
 */
predicate isBarrierBlock(BasicBlock bb) {
    exists(FunctionCall fc |
        fc.getBasicBlock() = bb and
        (
            isEnsureFreeCall(fc)
            or
            callsEnsureFree(fc.getTarget())
        )
    )
}

/**
 * Holds if `bb` is reachable from the entry point of `f` via a path
 * that does not pass through any barrier block (a block containing
 * an ensure_free call or a call that transitively calls ensure_free).
 */
pragma[nomagic]
predicate reachableFromEntryAvoidingBarriers(BasicBlock bb, Function f) {
    bb = f.getEntryPoint().getBasicBlock() and
    not isBarrierBlock(bb)
    or
    exists(BasicBlock pred |
        reachableFromEntryAvoidingBarriers(pred, f) and
        pred.getASuccessor() = bb and
        not isBarrierBlock(bb)
    )
}

/**
 * Holds if there is an ensure_free call (or a call to a function that
 * transitively calls ensure_free) before `allocCall` within the same
 * basic block.
 */
predicate hasEnsureFreeBeforeInBlock(FunctionCall allocCall) {
    exists(FunctionCall ef, BasicBlock bb, int efIdx, int allocIdx |
        (isEnsureFreeCall(ef) or callsEnsureFree(ef.getTarget())) and
        bb = allocCall.getBasicBlock() and
        ef.getBasicBlock() = bb and
        bb.getNode(efIdx) = ef and
        bb.getNode(allocIdx) = allocCall and
        efIdx < allocIdx
    )
}

/**
 * Holds if the alloc call has a NOLINT(allocations-without-ensure-free)
 * suppression comment on the same line or the line before.
 */
predicate hasSuppressionComment(FunctionCall allocCall) {
    exists(Comment c |
        c.getLocation().getFile() = allocCall.getLocation().getFile() and
        (
            c.getLocation().getStartLine() = allocCall.getLocation().getStartLine() or
            c.getLocation().getStartLine() = allocCall.getLocation().getStartLine() - 1
        ) and
        c.getContents().matches("%NOLINT(allocations-without-ensure-free)%")
    )
}

/**
 * Holds if `allocCall` is an allocating call (or direct memory_heap_alloc call)
 * inside an entry point that can be reached from the function entry without
 * passing through any ensure_free call on every path.
 */
predicate allocWithoutPrecedingEnsureFree(FunctionCall allocCall) {
    isEntryPoint(allocCall.getEnclosingFunction()) and
    (isAllocatingCall(allocCall) or allocCall.getTarget().hasName("memory_heap_alloc")) and
    not hasEnsureFreeBeforeInBlock(allocCall) and
    exists(Function f, BasicBlock allocBB |
        f = allocCall.getEnclosingFunction() and
        allocBB = allocCall.getBasicBlock() and
        (
            // allocBB is reachable from entry through non-barrier blocks
            reachableFromEntryAvoidingBarriers(allocBB, f)
            or
            // allocBB is itself a barrier (ensure_free is in the block but after the alloc)
            // and is reachable from entry through non-barrier blocks
            isBarrierBlock(allocBB) and
            (
                allocBB = f.getEntryPoint().getBasicBlock()
                or
                exists(BasicBlock pred |
                    reachableFromEntryAvoidingBarriers(pred, f) and
                    pred.getASuccessor() = allocBB
                )
            )
        )
    )
}

from FunctionCall allocCall, Function entryPoint, string entryKind
where
    entryPoint = allocCall.getEnclosingFunction() and
    (
        isNif(entryPoint) and entryKind = "NIF"
        or
        isPortHandler(entryPoint) and entryKind = "port handler"
    ) and
    allocWithoutPrecedingEnsureFree(allocCall) and
    not hasSuppressionComment(allocCall)
select allocCall,
    "Call to " + allocCall.getTarget().getName() +
        " allocates without any preceding ensure_free in " + entryKind + " $@.",
    entryPoint, entryPoint.getName()
