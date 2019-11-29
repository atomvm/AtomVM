#include "defaultatoms.h"
#include <stdio.h>

static const char *const false_atom = "\x05" "false";
static const char *const true_atom = "\x04" "true";

static const char *const ok_atom = "\x2" "ok";
static const char *const error_atom = "\x5" "error";

static const char *const undefined_atom = "\x9" "undefined";

static const char *const badarg_atom = "\x6" "badarg";
static const char *const badarith_atom = "\x08" "badarith";
static const char *const badarity_atom = "\x08" "badarity";
static const char *const badfun_atom = "\x06" "badfun";
static const char *const system_limit_atom = "\xC" "system_limit";
static const char *const function_clause_atom = "\x0F" "function_clause";
static const char *const try_clause_atom = "\xA" "try_clause";
static const char *const out_of_memory_atom = "\xD" "out_of_memory";
static const char *const overflow_atom = "\x8" "overflow";

static const char *const flush_atom = "\x5" "flush";
static const char *const heap_size_atom  = "\x9" "heap_size";
static const char *const latin1_atom = "\x6" "latin1";
static const char *const max_heap_size_atom ="\xD" "max_heap_size";
static const char *const memory_atom = "\x6" "memory";
static const char *const message_queue_len_atom = "\x11" "message_queue_len";
static const char *const puts_atom = "\x4" "puts";
static const char *const stack_size_atom = "\xA" "stack_size";
static const char *const min_heap_size_atom ="\xD" "min_heap_size";
static const char *const process_count_atom = "\xD" "process_count";
static const char *const port_count_atom = "\xA" "port_count";
static const char *const atom_count_atom = "\xA" "atom_count";
static const char *const system_architecture_atom = "\x13" "system_architecture";
static const char *const wordsize_atom = "\x8" "wordsize";

static const char *const decimals_atom = "\x8" "decimals";
static const char *const scientific_atom = "\xA" "scientific";
static const char *const compact_atom = "\x7" "compact";

static const char *const badmatch_atom = "\x8" "badmatch";
static const char *const case_clause_atom = "\xB" "case_clause";
static const char *const if_clause_atom = "\x9" "if_clause";
static const char *const throw_atom = "\x5" "throw";

void defaultatoms_init(GlobalContext *glb)
{
    int ok = 1;

    ok &= globalcontext_insert_atom(glb, false_atom) == FALSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, true_atom) == TRUE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, ok_atom) == OK_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, error_atom) == ERROR_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, undefined_atom) == UNDEFINED_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, badarg_atom) == BADARG_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badarith_atom) == BADARITH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badarity_atom) == BADARITY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badfun_atom) == BADFUN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, function_clause_atom) == FUNCTION_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, try_clause_atom) == TRY_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, out_of_memory_atom) == OUT_OF_MEMORY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, overflow_atom) == OVERFLOW_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, system_limit_atom) == SYSTEM_LIMIT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, flush_atom) == FLUSH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, heap_size_atom) == HEAP_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, latin1_atom) == LATIN1_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, max_heap_size_atom) == MAX_HEAP_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, memory_atom) == MEMORY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, message_queue_len_atom) == MESSAGE_QUEUE_LEN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, puts_atom) == PUTS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, stack_size_atom) == STACK_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, min_heap_size_atom) == MIN_HEAP_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, process_count_atom) == PROCESS_COUNT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, port_count_atom) == PORT_COUNT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, atom_count_atom) == ATOM_COUNT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, system_architecture_atom) == SYSTEM_ARCHITECTURE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, wordsize_atom) == WORDSIZE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, decimals_atom) == DECIMALS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, scientific_atom) == SCIENTIFIC_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, compact_atom) == COMPACT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, badmatch_atom) == BADMATCH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, case_clause_atom) == CASE_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, if_clause_atom) == IF_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, throw_atom) == THROW_ATOM_INDEX;

    if (!ok) {
        abort();
    }

    platform_defaultatoms_init(glb);
}
