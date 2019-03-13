# AtomVM Memory Management

Like most managed execution environments, AtomVM provides automated memory management for compiled Erlang/Elixir applications that run on the platform, allowing developers to focus on the logic of application programs, instead of the minutiae of managing the allocation and disposal of memory in the process heap of the program.

Because Erlang/Elixir, and the BEAM, specifically, is a shared-nothing, concurrency-based language, AtomVM can manage memory independently, for each unit of concurrency, viz., the Erlang process.  While there is some global state, internally, that AtomVM managed (e.g., to manage all running processes in the system), memory management for each individual process can be performed independently of any other process.

AtomVM internally uses a "Context" abstraction, to manage aspects of a process (including memory management), and we use "execution context" and "Erlang process" interchangeably in this document.  As usual, an Erlang process should be distinguished from the Operating System (OS) process in which Erlang processes run.

For any given execution context, there are three regions of memory that are relevant: i) the stack, ii) the heap, and iii) registers.  The stack and heap actually occupy one region of memory allocated in the OS process heap (via malloc or equiv), and grow in opposite directions towards each other.  Registers in AtomVM are a fixed size array of 16 elements.

The fundamental unit of memory that occupies space in the stack, heap, and registers is the `term`, which is typedef'd internally to be an integral type that fits in a single word of machine memory (i.e., a C `int`).  Various tricks are used, described below, to manage and reference multi-word terms, but in general, a term (or in some cases, a term pointer) is intended to fit into a single word or memory.

This document describes the memory layout for each execution context (i.e., Erlang/Elixir process), how memory is allocated and used, how terms are represented internally, and how AtomVM makes room for more terms, as memory usage increases and as terms go out of scope and are no longer used by the application, and can hence be garbage collected.

## Heap and Stack

The heap and stack for each AtomVM process are stored in a single allocated block of memory in the heap space of the AtomVM program.  The heap starts at the bottom of the block of memory, and grows incrementally towards the top of the allocated block, as memory is allocated in the program.  Each word in the heap and stack (or in some cases, a sequence of words) represent a term that has been allocated.

The heap contains all of the allocated terms in an execution context.  In some cases, the terms occupy more than one word of memory (e.g., a tuple), but in general, the heap contains a record of memory in use by the program.

The heap grows incrementally, as memory is allocated, and terms are allocated sequentially, in increasing memory addresses.  There is, therefore, no memory fragmentation, properly speaking, at least insofar as a portion of memory might be in use and then freed.  However, it is possible that previously allocated blocks of memory in the context heap are no longer referenced by the program.  In this case, the allocated blocks are "garbage", and are reclaimed at the next garbage collection.

> Note. It is possible for the AtomVM heap, as provided by the underlying operating system, to become fragmented, as the execution context stack and heap are allocted via `malloc` or equiv.

The stack grows from the top of the allocated block toward the heap in decreasing addresses.  Terms in the stack, as opposed to the heap, are either single-word terms, i.e., simple terms like small integers, process ids, etc, or _pointers_ to terms in the heap.  In either case, they only occupy one word of memory.

The region between the stack and heap is the free space available to the Erlang/Elixir process.

The initial size of the allocated block for the stack and heap in AtomVM is 8 words.

The following diagram illustrates an allocated block of memory that stores terms (or term pointers) in the heap and stack:

    +================================+ <- heap_start -------------        | increasing
    |             word[0]            |                ^         ^         | memory
    +--------------------------------+                |         |         | address
    |             word[1]            |                |         |         v
    +--------------------------------+                |         |
    |             word[2]            |                | heap    |
    +--------------------------------+                |         |
    |               ...              |                |         |
    +--------------------------------+                |         |
    |                                |                v         | allocated
    +--------------------------------+ <- heap_ptr -----        | memory
    |                                |                ^         |
    |                                |                |         |
    |                                |                |         |
    |                                |                | free    |
    |                                |                |         |
    |                                |                |         |
    |                                |                v         |
    +--------------------------------+ <- e ------------        |
    |                                |                ^         |
    +--------------------------------+                |         |
    |                                |                |         |
    +--------------------------------+                | stack   |
    |                                |                |         |
    +--------------------------------+                |         |
    |           word[n-1]            |                v         v
    +================================+ <- stack_base -------------

As heap and stack allocations grow, eventually, the amount of free space will decrease to the point where a garbage collection is required.  In this case, a new but larger (typically by 2x) block of memory is allocated by the AtomVM OS process, and terms are copied from the old stack and heap to the new stack and heap.  Garbage collection is described in more detail below.

## Registers

Registered are allocated in an array of 16 terms (words) and are referenced by the `x` field in the Context data structure:

    +---------+---------+---------+--------+
    |   x[0]  |   x[1]  |   ...   |  x[15] |
    +---------+---------+---------+--------+

Like terms in the stack, terms in registers are either single-word terms, i.e., simple terms like small integers, process ids, etc, or _pointers_ to terms in the heap.  In either case, they only occupy one word of memory.

Registers are used as part of the BEAM instruction set to store and retrieve values that are passed between BEAM instruction opcodes.

## Memory Graph

Memory is allocated in the execution context heap, and structured types, such as tuples and lists, generally include references to the blocks of memory that have beenn previously allocated.

For example, if we look at the memory allocated for the term

    {foo, [{bar, self()}]}

we would generally see something like the following in the execution context heap:

                                            |           ...             |
                                            |                           |
                                    ------- +---------------------------+
                                     ^      |          tuple            |<--+
                                     |      +---------------------------+   |
                    {bar, self()}    |      |          bar              |   |
                                     |      +---------------------------+   |
                                     v      |          <0.1.0>          |   |
                                    ------- +---------------------------+   |
                                     ^      |          []               |<- | --+
                 [{bar, self()}]     |      +---------------------------+   |   |
                                     v      |          tuple ptr        |---+   |
                                    ------- +---------------------------+       |
                                     ^      |          tuple            |       |
                                     |      +---------------------------+       |
           {foo, [{bar, self()}]}    |      |          foo              |       |
                                     |      +---------------------------+       |
                                     v      |          list ptr         |-------+
                                    ------- +---------------------------+
                                            |                           |
                                            |           ...             |

The tuple `{bar, self()}` is allocated in a block, and the list `[{bar, self()}]` (or, technically, `[{bar, self()} | []]`) contains elements that _point_ to it elements (in this case, `[]` and `{bar, self()}` -- note that in general, in AtomVM, the address of the tail of a list occupies the first byte in the list -- more details on that below).  Finally, the tuple `{foo, [{bar, self()}]}` contains the atom `foo` and a _pointer_ to the list it contains.

In this way, the set of allocated blocks in the execution context heap forms a directed graph of objects, whose nodes are structured terms (lists, tuples, etc) and whose leaves are simple terms, like atoms, pids, and so forth.  Note that because Erlang/Elixer are functional programming languages, these directed graphs have no cycles.

The stack and heap contain pointers to terms in the heap.  We call these terms "root" nodes, and any term in the path of a root node a referenced term.  Some terms in the heap are not the descendent of a root node.  We call these terms "garbage".

# Terms

The fundamental unit of memory in AtomVM is the `term` object, which is designed to fit either into a single machine work (single-word terms), or into multiple words (so called "boxed terms" and lists).

This section enumerates the AtomVM term types, and how they are represented in memory.

> Note.  The term type is overloaded in some cases to store raw pointers to memory addresses, but this is rare and well controlled.

## Single-word terms ("immediates")

The following term types take up a single word, referred to as "immediates" in the BEAM documentation[1].  The low-order bits of the word are used to represent the type of the term, and the high order bits represent the term contents, in a manner described in the following sections.

### Atoms

An atom is representated as a single word, with the low-order 6 bits having the value `0xB` (`001011b`).  The high order word-size-6 bits are used to represent the index of the atom in the global atom table:

                              |< 6  >|
    +=========================+======+
    |       atom index        |001011| <- 0xB
    +=========================+======+
    |                                |
    |<---------- word-size --------->|

There may therefore only be `2^{word-size-6}` atoms in an AtomVM program (e.g., on a 32-bit platform, `67,108,864`).  Plenty to work with!

> Note. The global atom table is a table of all allocated atoms, and is generally (at least in the limit, as modules are loaded) a fixed size table.  Management of the global atom table is outside of the scope of this document.

### Integers

An integer is representated as a single word, with the low-order 4 bits having the value `0xF` (`1111b`).  The high order word-size-6 bits are used to represent the integer value:

                                |< 4>|
    +===========================+====+
    |       integer value       |1111| <- 0xF
    +===========================+====+
    |                                |
    |<---------- word-size --------->|

The magnitude of an integer is therefore limited to `2^{word-size - 4}` in an AtomVM program (e.g., on a 32-bit platform, `+- 134,217,728`).

> Note.  Multi-byte integers (bignums) are not currently supported in AtomVM.

### nil

The special value `nil` (typically the tail of the tail ... of the tail of a list, or `[]`) is the special value 0x3B:

    +================================+
    |000         ...       0000111011| <- 0x3B
    +================================+
    |                                |
    |<---------- word-size --------->|


### Pids

A Pid is represented as a single word, with the low order 4 bits indicating the Pid term type (`0x03`), and (for now), the high order `word-size - 4` bits store the local process id:

                                |< 4>|
    +===========================+====+
    |     local process id      |0011| <- 0x3
    +===========================+====+
    |                                |
    |<---------- word-size --------->|

There may therefore only be `2^{word-size - 4}` Pids in an AtomVM program (e.g., on a 32-bit platform, `268,435,456`).

> Note.  Global process IDs are not currently supported, but they may be in the future, which may result in segmentation of the high order `word-size - 4` bits.

## Boxed terms

Some term types cannot fit in a single word, and must therfore used a sequence of contiguois words to represent the term contents.  Boxed terms use the low-order 6 bits of the first byte (`boxed[0]`) to represent the term type, and the high order `word-size - 6` bits to represent the remaining size (in words) of the boxed term, not including the first word.

### Boxed term pointers

Because boxed terms can be elements of other terms (e.g., tuples or lists), we need a way to reference boxed terms in a way that fits in a single word.  We accomplish this via boxed term pointers.

A boxed term pointer is a single-word term that contains the address of the referenced term in the high-order `word-size - 2` bits, and `0x2` (`10b`) in the low-order 2 bits.

                                  |2 |
    +=============================+==+
    |       term address          |10| <- term pointer type (2 bits)
    +=============================+==+
    |                                |
    |<---------- word-size --------->|

Because words are always aligned on boundaries that are divisible by the word size, the low-order 2 bits of a term address are always 0.  Consequently, the term address may always safely be shifted 2 bits to the left, at the expense of being able to address at most `word-size - 2` terms in any given AtomVM process (e.g., `1,073,741,824` terms, on a 32-bit platform), which should be plenty!

### References

A reference stores a 64-bit incrementing counter value (a "ref tick").  On 64 bit machines, a Reference takes up two words -- the boxed header and the 64-bit value, which of course can fit in a single word.  On 32-bit platforms, the high-order 28 bits are stored in `boxed[1]`, and the low-order 32 bits are stored in `boxed[2]`:

                              |< 6  >|
    +=========================+======+
    |        boxed-size       |010000| boxed[0]
    +-------------------------+------+
    |     high-order ref-ticks       | boxed[1]
    +================================+
    |      low-order ref-ticks       | boxed[2] (32-bit only)
    += = = = = = = = = = = = = = = ==+
    |                                |
    |<---------- word-size --------->|


### Tuples

Tuples are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x00` (`000000b`), followed by a sequence of `n`-many words, which may either (copies of) single-word terms, or boxed term pointers, where `n` is the arity of the tuple:

                              |< 6  >|
    +=========================+======+
    |    boxed-size (n)       |000000| boxed[0]
    +-------------------------+------+
    |             element-1          | boxed[1]
    +--------------------------------+
    |             element-2          | boxed[2]
    +--------------------------------+
    |               ...              | boxed[i]
    +--------------------------------+
    |             element-n          | boxed[n]
    +================================+
    |                                |
    |<---------- word-size --------->|

### Binaries

Binaries are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x024` (`100100b`), followed by the size in bytes of the binary, and then a sequence of `n`-many words, which contains the sequence of `size`-many bytes (`<= word-size * n`):

                              |< 6  >|
    +=========================+======+
    |    boxed-size (n)       |100100| boxed[0]
    +-------------------------+------+
    |         size (in bytes)        | boxed[1]
    +--------------------------------+
    | byte-1, bytes-2, bytes-3, ...  | boxed[2]
    +--------------------------------+
    |               ...              | boxed[i]
    +-------------------+------------+
    | ..., byte-{size-1}| -unused-   | boxed[n+1]
    +===================+============+
    |                                |
    |<---------- word-size --------->|

> Note.  If the number of bytes in a binary is not evenly divisible by the machine word size, then the remaining sequence of bytes in the last word are unused.

## Lists

A list is, very simply, a cons cell, i.e., a sequence of two words, whose first word is a term (single word or term pointer) representing the tail of the list, and the second of which represents the head of the list.

    +================================+
    |                tail            | list_elem[0]
    +--------------------------------+
    |                head            | list_elem[1]
    +================================+
    |                                |
    |<---------- word-size --------->|

> Note.  Lists are typically terminated with the empty list (`[]`), represented by the nil term, described above.  However, nothing in Erlang requires that a sequence of cons cells is `nil`-terminated.

Unlike boxed terms, the low-order two bits of list pointers are `0x1` (`01b`):

    +=============================+==+
    |       term address          |01| <- list pointer type (2 bits)
    +=============================+==+
    |                                |
    |<---------- word-size --------->|

### Strings

Strings are just lists of integers, but they are efficiently allocated at creation time so that a contiguous block of cons cells are created in the heap.  They otherwise have the same properties of a list described above.

    +================================+
    |    address-of-next-cons     |01| elem[1]
    +--------------------------------+
    |             int-value          |
    +--------------------------------+
    |    address-of-next-cons     |01| elem[2]
    +--------------------------------+
    |             int-value          |
    +--------------------------------+
    |                ...          |01| elem[i]
    +--------------------------------+
    |                ...             |
    +--------------------------------+
    |                nil             | elem[n]
    +--------------------------------+
    |             int-value          |
    +================================+
    |                                |
    |<---------- word-size --------->|


> Note.  It is not clear whether string elements remain contiguous after a garbage collection event.

### Functions

Functions are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x14` (`010100b`), followed by the raw memory address of the Module data structure in which the function is defined, and the function index (so that the function can be located).

In addition, if there are any terms that are used outside of the scope of the function (i.e., closures), these terms are copied from registers into the function objects

                              |< 6  >|
    +=========================+======+
    |    boxed-size (n)       |010100| boxed[0]
    +-------------------------+------+
    |         module address         | boxed[1]
    +--------------------------------+
    |         function index         | boxed[2]
    +--------------------------------+ ---------------
    |            closure_1           | boxed[3]     ^
    +- - - - - - - - - - - - - - - - +              |
    |               ...              |              | closure
    +- - - - - - - - - - - - - - - - +              |
    |            closure_k           | boxed[n-1]   v
    += = = = = = = = = = = = = = = = + ---------------
    |                                |
    |<---------- word-size --------->|

## Special Stack Types

Some terms are only used in the stack.

### Continuation Pointer

A continuation pointer is a raw address.  Because words are aligned on word boundaries, the low order two bits of a continuation pointer are always `0x0` (`00b`):

    +================================+
    |            raw address      |00|
    +================================+

### Catch Labels

A catch label is used to indicate a position in code to which to jump in a try-catch expression.  The term occupies a single term, with the low order 6 bits having the value `0x1B`, the high order 8 bits holding the module index (`m_i`), and the middle 18 bits holding the catch label index (`l_i`):

    |<   8  >|<     18       >|< 6  >|
    +========+================+======+
    |   m_i  |     l_i        |011011|
    +========+================+======+
    |                                |
    |<---------- word-size --------->|

# Garbage Collection

The heap and stack occupy a single region of malloc'd memory.  Because this region is fixed, every allocation in the heap or stack results in less free space for the Erlang process.  When free space reaches a limit, AtomVM will run a garbage collection, which will allocate a new block of memory to hold the new heap and stack (typically, 2 times the size of the currently allocated block), and then migrate terms from the old heap and stack to the new heap and stack.  Any terms that no longer have references from term pointers in the stack or registers are not copied to the new stack, and are therfore "collected" as garbage.  Garbage collection is a _synchronous_ operation in each Context (Erlang process), but conceptually no other execution contexts are impacted (i.e., no global locks, other than those required for memory allocation in the OS process heap).

                                            +-------------------+
                                            |        new        |
                                            |        heap       |
                                            +-------------------+
    +-------------------+                   |                   |
    |                   |                   |                   |
    |        old        |                   |                   |
    |        heap       |                   |                   |
    |                   |        ===>       |                   |
    +===================+         gc        |        free       |
    |        old        |                   |                   |
    |        stack      |                   |                   |
    +-------------------+                   |                   |
                                            +-------------------+
                                            |        new        |
                                            |        stack      |
                                            +-------------------+
    
    
                +---+---+---+-------------------+---+
                | 0 | 1 | 2 |                   | 15|
                +---+---+---+-------------------+---+
                registers

The process of migration from the old memory regions to the new works as follows:

* AtomVM iterates through the terms in the list of registers.  Terms stored in registers are either single word terms or references to terms in the heap:
    * If the term is a single-word term, it is copied directly to the new heap;
    * If the term is a (pointer to a) boxed term or a (pointer to a) list term, then that means the term in the register is a reference to a multi-word term in the (old) heap.  In that case, the sequence of terms that make up the boxed term from the old heap (e.g., a the terms in a tuple) is copied to the new heap, but the term that was copied in the old heap is marked as moved, so that when the heap is garbage collected, it is not copied again;
    * Note that this marking process over-writes the first two bytes in the old heap -- one byte for the marker (`0x2B`), and one for a pointer to newly copied multi-word term in the new heap;
    * The reference in the register is updated to reference the term in the new heap.

After the registers are traversed, each term in a register is either a simple (on-word) term, or a pointer to a boxed term or list in the _new_ heap, and the new `heap_ptr` points to the end of the (currently allocated) new heap, which now includes terms that have been moved from the old heap to the new heap.

This process of copying copying terms from the old heap to the new heap is called a "shallow" copy, as it is not traversing the memory graph to find the entire memory graph of the term, but is instead only copying the term structure, whether it is a single-word term, a boxed term, or a list.  Any pointers in these terms are copied verbatim.

Example:

Suppose `register[i]` is a term pointer to a tuple of arity 2 in the heap:

    |                           |                       |                           |           |
    |           ...             |                       |           ...             |           | used
    |                           |                       |                           |           v
    +---------------------------+                       +===========================+ <----------- new heap_ptr
    |           tuple           |<----------+           |                           |           ^
    +---------------------------+           |           |                           |           |
    |           elt-1           |           |           |                           |           |
    +---------------------------+           |           |                           |           |
    |           elt-2           |           |           |                           |           | free
    +---------------------------+           |           |                           |           |
    |                           |           |           |                           |           |
    |           ...             |           |           |           ...             |           |
    |                           |           |           |                           |           |
                                            |
            old heap                        |                     new heap
                                            |
                                ---+----------------+---
                               ... |     old-ptr    | ...
                                ---+----------------+---
                                    register[i]

The boxed term is then copied to the new heap:

    |                           |                       |                           |           |
    |           ...             |                       |           ...             |           |
    |                           |                       |                           |           |
    +---------------------------+ >>>>>>>>>>>>>>>>>>>>> +---------------------------+           |
    |           tuple           |                       |           tuple           |           | used
    +---------------------------+                       +---------------------------+           |
    |           elt-1           |        COPY           |           elt-1           |           |
    +---------------------------+                       +---------------------------+           |
    |           elt-2           |                       |           elt-2           |           v
    +---------------------------+ >>>>>>>>>>>>>>>>>>>>> +===========================+ <----------- new heap_ptr
    |                           |                       |                           |           ^
    |           ...             |                       |            ...            |           | free
    |                           |                       |                           |           |
                                                                              
            old heap                                              new heap

And the register is updated to point to the new heap.  In addition, the first byte of the boxed term in the old heap is marked with a special value (`0x2B`) that is otherwise unused, and the second word in the term is updated with the same pointer.  This marker and pointer will be used when the heap is scanned.  (See below)

> Question: What happens if the term is a tuple of length 0?

    |                           |                       |                           |           |
    |           ...             |                       |           ...             |           |
    |                           |                       |                           |           |
    +---------------------------+                       +---------------------------+           |
    |             0x2B          |           +---------->|                           |           | used
    +---------------------------+           |           +---------------------------+           |
    |             ptr           |-----------+           |                           |           |
    +---------------------------+           |           +---------------------------+           |
    |                           |           |           |                           |           v
    +---------------------------+           |           +===========================+ <----------- new heap_ptr
    |                           |           |           |                           |           ^
    |           ...             |           |           |           ...             |           | free
    |                           |           |           |                           |           |
                                            |
            old heap                        |                     new heap
                                            |
                                ---+----------------+---
                               ... |     new ptr    | ...
                                ---+----------------+---
                                    register[i]

> Note.  Due to the fact that this was a _shallow_ copy, it is possible (and likely) that some of the boxed terms or list cons cells that were copied still point to terms in the old heap.

* AtomVM applies the same algorithm described above for registers, but applied to terms (or term pointers) in the stack.

After the stack is traversed, each term in the new stack is either a simple (on-word) term, or a pointer to a boxed term or list in the _new_ heap, and the new `heap_ptr` points to the end of the (currently allocated) new heap, which now includes terms that have been moved from the old heap to the new heap.

In addtion, because all of the "roots" of the memory graph have been copied into the new heap, the only references to terms in the old heap are now in the new heap.

* Finally, the heap is garage collected, as follows:
    * Starting from the bottom of the new heap to the new `heap_ptr`, which is the block of memory that new terms have been copied into the new heap, perform the following, steps, for each term in the block:
        * If the term is a single-word term, then there is nothing to do, and skip over the term;
        * If the term the start of a boxed term (i.e., it has a boxed header, which indicates the size, meaning it is not a reference to such a term), then extract the size of the term, and do the following, according to the term type:
            * If the term is a tuple, then do a shallow copy of each element in the tuple, as described above.  Note that any terms that are copied from the old heap to the new heap will be written _after_ the block of memory currently being scanned;
            * If the term is a function, then do a shallow copy of any function closures
            * If the term is a reference or binary, then do nothing and just skip to the next term
        * If the term is a pointer to a boxed term or list, then do a shallow copy of the referenced boxed term or list
    * After the above steps, we have scanned the first block and removed any references to terms in the old heap.  Bute we may have created a new block of terms in the new heap after the block we just scanned which may have references to terms in the old heap.  So we move on to the next block in teh new heap, and perform the same steps as descibed above, removing references to terms in the old heap and (possibly) copying data from the old heap to the new heap.
    * We keep scanning newly created blocks in the new heap until we get a block that contains no references to terms in the old heap.

## Example

Let's see what during garbage collection with the following root term:

    [{foo, [{bar, self()}]}]

Before GC:



    DEBUG: heap start: 0x109a25f60 end: 0x109a25fa0 size: 8
    DEBUG: heap 0x109a25f60   0: (00000000000000000000000010000000)b 0x000000080: tuple(2)
    DEBUG: heap 0x109a25f68   1: (00000000000000000000000100001011)b 0x00000010b: bar
    DEBUG: heap 0x109a25f70   2: (00000000000000000000000000010011)b 0x000000013: <0.1.0>
    DEBUG: heap 0x109a25f78   3: (00000000000000000000000000111011)b 0x00000003b: []
    DEBUG: heap 0x109a25f80   4: (00001001101000100101111101100010)b 0x109a25f62: boxed(0x109a25f60)
    DEBUG: heap 0x109a25f88   5: (00000000000000000000000010000000)b 0x000000080: tuple(2)
    DEBUG: heap 0x109a25f90   6: (00000000000000000000000101001011)b 0x00000014b: foo
    DEBUG: heap 0x109a25f98   7: (00001001101000100101111101111001)b 0x109a25f79: list(0x109a25f78)
    DEBUG:
    DEBUG:
    DEBUG: stack start: 0x109a25ff0 end: 0x109a26000 size: 2
    DEBUG: stack 0x109a25ff0   0: (00000000000000000000000000111011)b 0x00000003b: []
    DEBUG: stack 0x109a25ff8   1: (00000000000000000000000110001000)b 0x000000188: unknown
    DEBUG:
    DEBUG:
    DEBUG: register start: 0x109a1fef8 end: 0x109a1ff78 size: 16
    DEBUG: register 0x109a1fef8   0: (00001001101000100101111101111001)b 0x109a25f79: list(0x109a25f78)
    DEBUG: register 0x109a1ff00   1: (00001001101000100101111110001010)b 0x109a25f8a: boxed(0x109a25f88)
    DEBUG: register 0x109a1ff08   2: (00000000000000000000000000111011)b 0x00000003b: []
    ...
    DEBUG: register 0x109a1ff70  15: (00000000000000000000000000111011)b 0x00000003b: []
    DEBUG:


After GC:

    DEBUG:
    DEBUG: heap start: 0x109a2df60 end: 0x109a2dfa0 size: 8
    DEBUG: heap 0x109a2df60   0: (00000000000000000000000000111011)b 0x00000003b: []
    DEBUG: heap 0x109a2df68   1: (00001001101000101101111110001010)b 0x109a2df8a: boxed(0x109a2df88)
    DEBUG: heap 0x109a2df70   2: (00000000000000000000000010000000)b 0x000000080: tuple(2)
    DEBUG: heap 0x109a2df78   3: (00000000000000000000000101001011)b 0x00000014b: foo
    DEBUG: heap 0x109a2df80   4: (00001001101000101101111101100001)b 0x109a2df61: list(0x109a2df60)
    DEBUG: heap 0x109a2df88   5: (00000000000000000000000010000000)b 0x000000080: tuple(2)
    DEBUG: heap 0x109a2df90   6: (00000000000000000000000100001011)b 0x00000010b: bar
    DEBUG: heap 0x109a2df98   7: (00000000000000000000000000010011)b 0x000000013: <0.1.0>
    DEBUG:
    DEBUG:
    DEBUG: stack start: 0x109a2dff0 end: 0x109a2e000 size: 2
    DEBUG: stack 0x109a2dff0   0: (00000000000000000000000000111011)b 0x00000003b: []
    DEBUG: stack 0x109a2dff8   1: (00000000000000000000000110001000)b 0x000000188: unknown
    DEBUG:
    DEBUG:
    DEBUG: register start: 0x109a1fef8 end: 0x109a1ff78 size: 16
    DEBUG: register 0x109a1fef8   0: (00001001101000101101111101100001)b 0x109a2df61: list(0x109a2df60)
    DEBUG: register 0x109a1ff00   1: (00001001101000101101111101110010)b 0x109a2df72: boxed(0x109a2df70)
    DEBUG: register 0x109a1ff08   2: (00000000000000000000000000111011)b 0x00000003b: []
    ...
    DEBUG: register 0x109a1ff70  15: (00000000000000000000000000111011)b 0x00000003b: []
    DEBUG:
