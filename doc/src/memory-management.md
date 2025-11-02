<!--
 Copyright 2019-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Memory Management

Like most managed execution environments, AtomVM provides automated memory management for compiled Erlang/Elixir applications that run on the platform, allowing developers to focus on the logic of application programs, instead of the minutiae of managing the allocation and disposal of memory in the process heap of the program.

Because Erlang/Elixir, and the BEAM, specifically, is a shared-nothing, concurrency-based language, AtomVM can manage memory independently, for each unit of concurrency, viz., the Erlang process.  While there is some global state, internally, that AtomVM manages (e.g., to manage all running processes in the system), memory management for each individual process can be performed independently of any other process.

AtomVM internally uses a "Context" structure, to manage aspects of a process (including memory management), and we use "execution context" and "Erlang process" interchangeably in this document.  As usual, an Erlang process should be distinguished from the Operating System (OS) process in which Erlang processes run.

For any given execution context, there are three regions of memory that are relevant: i) the stack, ii) the heap, and iii) registers.  The stack and heap actually occupy one region of memory allocated in the OS process heap (via malloc or equiv), and grow in opposite directions towards each other.  Registers in AtomVM are a fixed size array of 16 elements.

The fundamental unit of memory that occupies space in the stack, heap, and registers is the `term`, which is typedef'd internally to be an integral type that fits in a single word of machine memory (i.e., a C `int`).  Various tricks are used, described below, to manage and reference multi-word terms, but in general, a term (or in some cases, a term pointer) is intended to fit into a single word or memory.

This document describes the memory layout for each execution context (i.e., Erlang/Elixir process), how memory is allocated and used, how terms are represented internally, and how AtomVM makes room for more terms, as memory usage increases and as terms go out of scope and are no longer used by the application, and can hence be garbage collected.

## The Context structure

### The Heap and Stack

The heap and stack for each AtomVM process are stored in a single allocated block of memory (e.g., via the `malloc` C function) in the heap space of the AtomVM program, and the AtomVM runtime manages the allocation of portions of this memory during the execution of a program.  The heap starts at the bottom of the block of memory, and grows incrementally towards the top of the allocated block, as memory is allocated in the program.  Each word in the heap and stack (or in some cases, a sequence of words) represent a term that has been allocated.

The heap contains all of the allocated terms in an execution context.  In some cases, the terms occupy more than one word of memory (e.g., a tuple), but in general, the heap contains a record of memory in use by the program.

The heap grows incrementally, as memory is allocated, and terms are allocated sequentially, in increasing memory addresses.  There is, therefore, no memory fragmentation, properly speaking, at least insofar as a portion of memory might be in use and then freed.  However, it is possible that previously allocated blocks of memory in the context heap are no longer referenced by the program.  In this case, the allocated blocks are "garbage", and are reclaimed at the next garbage collection. The actual growth of the heap is controlled by a heap growth strategy (`atomvm_heap_growth` spawn option) as described [below](#heap-growth-strategies).

>It is possible for the AtomVM heap, as provided by the underlying operating system, to become fragmented, as the execution context stack and heap are allocated via `malloc` or equiv.  But that is a different kind of fragmentation that does not refer to the allocated block used by an individual AtomVM process.

The stack grows from the top of the allocated block toward the heap in decreasing addresses.  Terms in the stack, as opposed to the heap, are either single-word terms, i.e., simple terms like small integers, process ids, etc, or _pointers_ to terms in the heap.  In either case, they only occupy one word of memory.

The region between the stack and heap is the free space available to the Erlang/Elixir process.

The following diagram illustrates an allocated block of memory that stores terms (or term pointers) in the heap and stack:

    +================================+ <- heap_start --
    |             word[0]            |      ^      ^
    +--------------------------------+      |      |
    |             word[1]            |      |      |
    +--------------------------------+      |      |
    |             word[2]            |      | heap |
    +--------------------------------+      |      |
    |               ...              |      |      |
    +--------------------------------+      |      |
    |                                |      v      |
    +--------------------------------+ <- heap_ptr |
    |                                |      ^      |
    |                                |      |      |
    |                                |      |      |
    |                                |      | free |
    |                                |      |      |
    |                                |      |      |
    |                                |      v      |
    +--------------------------------+ <- e ----   |
    |                                |      ^      |
    +--------------------------------+      |      |
    |                                |      |      |
    +--------------------------------+      | stack|
    |                                |      |      |
    +--------------------------------+      |      |
    |           word[n-1]            |      v      v
    +================================+ <- stack_base --

The initial size of the allocated block for the stack and heap in AtomVM is 8 words.  As heap and stack allocations grow, eventually, the amount of free space will decrease to the point where a garbage collection is required.  In this case, a new but larger block of memory is allocated by the AtomVM OS process, and terms are copied from the old stack and heap to the new stack and heap.  Garbage collection is described in more detail below.

### Heap growth strategies

AtomVM aims at minimizing memory footprint and several heap growth strategies are available. The heap is grown or shrunk when an allocation is required and the current execution context allows for a garbage collection (that will move data structures), allows for shrinking or forces shrinking (typically in the case of a call to [`erlang:garbage_collect/0,1`](./apidocs/erlang/estdlib/erlang.md#garbage_collect1)).

Each strategy is set at the process level.

Default strategy is bounded free (`{atomvm_heap_growth, bounded_free}`). In this strategy, when more memory is required, the allocator keeps the free amount between fixed boundaries (currently 16 and 32 terms). If no allocation is required but free space is larger than boundary, a garbage collection is triggered. After copying data to a new heap, if the free space is larger than the maximum, the heap is shrunk within the boundaries.

With minimum strategy (`{atomvm_heap_growth, minimum}`), when an allocation can happen, it is always adjusted to have the free space at 0.

With fibonacci strategy (`{atomvm_heap_growth, fibonacci}`), heap size grows following a variation of fibonacci until a large value and then grows by 20%. If free space is larger than 75% of heap size, the heap is shrunk. This strategy is inspired from Erlang/OTP's implementation.

### Registers

Registered are allocated in an array of 16 terms (words) and are referenced by the `x` field in the Context data structure:

    +---------+---------+---------+--------+
    |   x[0]  |   x[1]  |   ...   |  x[15] |
    +---------+---------+---------+--------+

Like terms in the stack, terms in registers are either single-word terms, i.e., simple terms like small integers, process ids, etc, or _pointers_ to terms in the heap, in a manner described in more detail below.  In either case, they only occupy one word of memory.

Registers are used as part of the BEAM instruction set to store and retrieve values that are passed between BEAM instruction opcodes.

### Process Dictionary

AtomVM processes support a process dictionary, or map of process-specific data, as supported via the [`erlang:put/2`](./apidocs/erlang/estdlib/erlang.md#put2) and [`erlang:get/1`](./apidocs/erlang/estdlib/erlang.md#get1) functions.

The Process Dictionary contains a list of key-value pairs, where each key and value is a single-word term, either a simple term like an atom or pid, or a reference to an allocated object in the process heap. (see below)

### Heap Fragments

AtomVM makes use of heap fragments in some edge cases, such as loading external terms from the literals table in a BEAM file.  Heap fragments are individually allocated blocks of memory that contain may contain multi-word term structures.  The data in heap fragments are copied into the heap during a garbage collection event, and then deleted, so heap fragments are generally short lived.  However, during execution of a program, there may be references to term structures in such fragments from the stack, registers, the process dictionary, or from nested terms in the process heap.

### Mailbox

Each Erlang process contains a process mailbox, which is a linked-list structure of messages.  Each message in this list contains a term structure, which is a copy of a term sent to it, e.g., via the [`erlang:send/2`](./apidocs/erlang/estdlib/erlang.md#send2) operation, or `!` operator.

The representation of terms in a message is identical to that in the heap and heap fragments.  Messages are allocated like fragments and they actually become heap fragments of the receiving process when the message is read off the mailbox (e.g., via `receive ... end`).  Messages (and their term contents) are moved to the main heap as part of regular garbage collection of the process, and the fragment is freed.

### Memory Graph

Memory is allocated in the execution context heap, and structured types, such as tuples and lists, generally include references to the blocks of memory that have been previously allocated.

For example, if we look at the memory allocated for the term

    {foo, [{bar, self()}]}

we would generally see something like the following in the execution context heap:

    |           ...             |
    |                           |
    +---------------------------+
    |          tuple            |<--+
    +---------------------------+   |
    |          bar              |   |
    +---------------------------+   |
    |          <0.1.0>          |   |
    +---------------------------+   |
    |          []               |<- | --+
    +---------------------------+   |   |
    |          tuple ptr        |---+   |
    +---------------------------+       |
    |          tuple            |       |
    +---------------------------+       |
    |          foo              |       |
    +---------------------------+       |
    |          list ptr         |-------+
    +---------------------------+
    |                           |
    |           ...             |
    01234567890123456789012345678901234567890123456789

The tuple `{bar, self()}` is allocated in a block, and the list `[{bar, self()}]` (or, technically, `[{bar, self()} | []]`) contains elements that _point_ to it elements (in this case, `[]` and `{bar, self()}` -- note that in general, in AtomVM, the address of the tail of a list occupies the first byte in the list -- more details on that below).  Finally, the tuple `{foo, [{bar, self()}]}` contains the atom `foo` and a _pointer_ to the list it contains.

In this way, the set of allocated blocks in the execution context heap forms a directed graph of objects, whose nodes are structured terms (lists, tuples, etc) and whose leaves are simple terms, like atoms, pids, and so forth.  Note that because BEAM-based languages such as Erlang and Elixir are true functional programming languages, these directed graphs have no cycles.

The stack, registers, and process dictionary contain pointers to terms in the heap.  We call these terms "root" nodes, and any term in the heap that is referenced by a root node, or any term that is so referenced by such a term, is in the path of a root node.  Some terms in the heap are not in the path of a root node.  We call these terms "garbage".

Note that the values in the stack and register root nodes change over time as the result of the execution of Erlang opcodes, and are dependent on the BEAM output of the Erlang compiler, along with inputs to the program being executed.  Thus, a term in the process heap may become garbage, once it is no longer reachable from the root set.  But once garbage, the term will always remain garbage, at least until it is reclaimed during a garbage collection event.  For more information about how the garbage collector works, see the Garbage Collection section, below.

## Simple Terms

The fundamental unit of memory in AtomVM is the `term` object, which is designed to fit either into a single machine work (single-word terms), or into multiple words (so called "boxed terms" and lists).

This section enumerates the AtomVM term types, and how they are represented in memory.

```{note}
The term type is overloaded in some cases to store raw pointers to memory addresses, but this is rare and well
controlled.
```

The following term types take up a single word, referred to as "immediates" in the BEAM documentation[1].  The low-order bits of the word are used to represent the type of the term, and the high order bits represent the term contents, in a manner described in the following sections.

### Atoms

An atom is represented as a single word, with the low-order 6 bits having the value `0xB` (`001011b`).  The high order word-size-6 bits are used to represent the index of the atom in the global atom table:

                              |< 6  >|
    +=========================+======+
    |       atom index        |001011| <- 0xB
    +=========================+======+
    |                                |
    |<---------- word-size --------->|

There may therefore only be `2^{word-size-6}` atoms in an AtomVM program (e.g., on a 32-bit platform, `67,108,864`).  Plenty to work with!

```{note}
The global atom table is a table of all allocated atoms, and is generally (at least in the limit, as modules are
loaded) a fixed size table.  Management of the global atom table is outside of the scope of this document.
```

### Integers

AtomVM supports integers up to 256 bits with an additional sign bit stored outside the numeric
payload. The representation strategy depends on the integer's size and uses canonicalization to
ensure each value has exactly one representation.

#### Immediate Integers

Small integers are represented as a single word, with the low-order 4 bits having the value `0xF`
(`1111b`).  The high order word-size-4 bits are used to represent the integer value using two's
complement:

                                |< 4>|
    +===========================+====+
    |       integer value       |1111| <- 0xF
    +===========================+====+
    |                                |
    |<---------- word-size --------->|

On 32-bit systems, immediate integers can represent signed values in the range `[-2^27, 2^27-1]` (28
bits + 4-bit tag = 32 bits).
On 64-bit systems, immediate integers can represent signed values in the range `[-2^59, 2^59-1]` (60
bits + 4-bit tag = 64 bits).

For integers outside these ranges, AtomVM uses boxed representations (see Boxed Integers section
below).

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

```{note}
Global process IDs are not currently supported, but they may be in the future, which may result in segmentation of
the high order `word-size - 4` bits.
```

## Boxed terms

Some term types cannot fit in a single word, and must therefore used a sequence of contiguous words to represent the term contents.  These terms are called "Boxed" terms.  Boxed terms use the low-order 6 bits of the first byte (`boxed[0]`) to represent the term type, and the high order `word-size - 6` bits to represent the remaining size (in words) of the boxed term, not including the first word.

### Boxed term pointers

Before discussing the different types of boxed terms in detail, let us first see how boxed terms are referenced from the stack, registers, process dictionary, and from embedded terms in the heap.  We call such references to boxed terms boxed term pointers.

A boxed term pointer is a single-word term that contains the address of the referenced term in the high-order `word-size - 2` bits, and `0x2` (`10b`) in the low-order 2 bits.

                                  |2 |
    +=============================+==+
    |       term address          |10| <- term pointer type (2 bits)
    +=============================+==+
    |                                |
    |<---------- word-size --------->|

Because terms (and hence the heap) are always aligned on boundaries that are divisible by the word size, the low-order 2 bits of a term address are always 0.  Consequently, the high-order word-size - 2 (`1,073,741,824`, on a 32-bit platform) are sufficient to address any term address in the AtomVM address space, for 32-bit and greater machine architectures.

### Boxed Integers

AtomVM uses boxed integers for values that exceed the immediate integer range. There are two types
of boxed integer representations: native integers (using int32_t or int64_t) and big integers (using
arrays of uint32_t digits).

#### Native Boxed Integers

For integers that don't fit in immediate representation but can be stored in native C integer
types, AtomVM uses boxed integers with two's complement encoding and a redundant sign bit in the
header.

**On 32-bit systems:**
- Integers in range `[-2^31, -2^27-1] ∪ [2^27, 2^31-1]` are stored as boxed int32_t (single word
payload)
- Integers in range `[-2^63, -2^31-1] ∪ [2^31, 2^63-1]` are stored as boxed int64_t (two word
payload)

**On 64-bit systems:**
- Integers in range `[-2^63, -2^59-1] ∪ [2^59, 2^63-1]` are stored as boxed int64_t (single word
payload)

The boxed header uses:
- `0x8` (`001000b`) for positive integers (TERM_BOXED_POSITIVE_INTEGER)
- `0xC` (`001100b`) for negative integers (TERM_BOXED_NEGATIVE_INTEGER)

                              |< 6  >|
    +=========================+======+
    |    boxed-size (1 or 2)  |001X00| boxed[0] (X=0 for positive, X=1 for negative)
    +-------------------------+------+
    |     native integer value       | boxed[1] (int32_t or int64_t low word)
    +--------------------------------+
    |     high word (if int64_t on   | boxed[2] (32-bit systems only)
    |         32-bit system)         |
    +================================+
    |                                |
    |<---------- word-size --------->|

#### Big Integers

For integers beyond the native int64_t range (up to ±(2^256 - 1)), AtomVM uses an array of uint32_t
digits representing the magnitude, with the sign stored as a flag in the boxed header. These big
integers do NOT use two's complement encoding.

The digits array:
- Stores the absolute value of the integer
- Uses little-endian ordering (digit[0] is least significant)
- Omits leading zero digits to save space
- Includes a dummy zero digit when necessary to avoid ambiguity with native boxed integers

                              |< 6  >|
    +=========================+======+
    |    boxed-size (n)       |001X00| boxed[0] (X=0 for positive, X=1 for negative)
    +-------------------------+------+
    |         digit[0] (lsb)         | boxed[1] (uint32_t)
    +--------------------------------+
    |         digit[1]               | boxed[2] (uint32_t)
    +--------------------------------+
    |               ...              | ...
    +--------------------------------+
    |         digit[k-1] (msb)       | boxed[k] (uint32_t)
    +--------------------------------+
    |     0 (dummy digit if needed)  | boxed[n] (uint32_t)
    +================================+
    |                                |
    |<---------- word-size --------->|

**Canonicalization Rules:**
- AtomVM ensures that integers are always stored in the most compact representation
- Operations that produce results fitting in a smaller representation automatically convert to that
representation
- A dummy digit mechanism ensures that the smallest big integer always has more words than the
largest native boxed integer. This is required when storing values such as `UINT64_MAX`
(`0xFFFFFFFFFFFFFFFF`), that would require only 2 digits, but boxed-size field must allow to
distinguish it from native boxed integers (such as `int64_t`)

**Examples:**
- The value 3 is always stored as an immediate integer (never as a boxed integer)
- On a 64-bit system, 2^60 would be stored as a boxed int64_t, not as a big integer
- The value 2^100 would be stored as a big integer with 4 uint32_t digits (plus potentially a dummy
digit)

### References

A reference (e.g., created via [`erlang:make_ref/0`](./apidocs/erlang/estdlib/erlang.md#make_ref0)) stores a 64-bit incrementing counter value (a "ref tick").  On 64 bit machines, a Reference takes up two words -- the boxed header and the 64-bit value, which of course can fit in a single word.  On 32-bit platforms, the high-order 28 bits are stored in `boxed[1]`, and the low-order 32 bits are stored in `boxed[2]`:

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

### Maps

Maps are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x2C` (`101100b`), followed by:

* a term pointer to a tuple of arity `n` containing the keys in the map;
* a sequence of `n`-many words, containing the values of the map corresponding (in order) to the keys in the reference tuple.

The keys and values are single word terms, i.e., either immediates or pointers to boxed terms or lists.

            +=========================+======+
    +-----> |    boxed-tuple (n)      |000000|
    |       +-------------------------+------+
    |       |             key-1              |
    |       +--------------------------------+
    |       |             key-2              |
    |       +--------------------------------+
    |       |               ...              |
    |       +--------------------------------+
    |       |             key-n              |
    |       +================================+
    |       |                                |
    |                       ...
    |       |                         |< 6  >|
    |       +=========================+======+
    |       |    boxed-size (n)       |101100| boxed[0]
    |       +-------------------------+------+
    +-----------------<   keys               | boxed[1]
            +--------------------------------+
            |             value-1            | boxed[2]
            +--------------------------------+
            |               ...              | ...
            +--------------------------------+
            |             value-n            | boxed[2 + n]
            +================================+
            |                                |
            |<---------- word-size --------->|

The tuple of keys may or may not be contiguous with the boxed term holding the map itself (and in general will not be, after garbage collection).  In addition, maps that are modified [sic] via the `:=` operator (or via `=>`, when the key already exists in the source map) share the keys tuple, for space efficiency.

### Binaries

Binaries are stored in several different ways, depending on their size and the kinds of data to which they refer.

Binary data less than 64 bytes in length are stored in the process heap, as so-called Heap Binaries.

Binary data greater or equal to 64 bytes is stored in two manners, depending on whether the data stored is constant data (e.g., literal binary data compiled directly into a BEAM file), or dynamically allocated data, e.g., as the result of a call to the [`erlang:list_to_binary/1`](./apidocs/erlang/estdlib/erlang.md#list_to_binary1) Nif.

Non-const binaries are stored outside of the heap in dynamically allocated memory and are reference-counted, whereby references to dynamically allocated blocks are tracked from pointers in heap storage.  This way, large blocks of binary data can be efficiently shared between processes; only a relatively small term that contains a reference to the dynamically allocated storage needs to be copied.  When the reference count of non-literal binary reaches 0, the dynamically allocated memory is free'd.

Const binaries share similar features to non-const binaries in the process heap; however, instead of pointing to dynamically allocated memory that requires reference counting and memory management, the boxed term in the process heap points directly to constant memory (e.g., a term literal stored in a memory-mapped BEAM file).  This is especially useful in memory constrained applications, such as the ESP32 micro-controller, where the BEAM file contents are not read into memory, but are instead directly mapped from flash storage.

Finally, a special kind of binary is used in the heap to maintain the state of a match context, when, for example, matching binary terms using Erlang bit syntax.  Like non-const binaries, creation and destruction of match context binaries will affect the reference count on the binaries to which they refer.

The following sub-sections describe these storage mechanisms and memory management in more detail.

#### Heap Binaries

Heap binaries are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x024` (`100100b`), followed by the size in bytes of the binary, and then a sequence of `n`-many words, which contains the sequence of `size`-many bytes (`<= word-size * n`):

                              |< 6  >|
    +=========================+======+
    |    boxed-size (n)       |100100| boxed[0]
    +-------------------------+------+
    |         size (in bytes)        | boxed[1]
    +--------------------------------+
    | byte-1, byte-2, byte-3,  ...   | boxed[2]
    +--------------------------------+
    |               ...              | boxed[i]
    +-------------------+------------+
    | ..., byte-{size-1}| -unused-   | boxed[n+1]
    +===================+============+
    |                                |
    |<---------- word-size --------->|

```{note}
If the number of bytes in a binary is not evenly divisible by the machine word size, then the remaining sequence
of bytes in the last word are unused.
```

#### Reference Counted Binaries

Reference counted binaries are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x020` (`100000b`), followed by the size in bytes of the binary data, a word containing a set of flags, and then a pointer to the off-heap data.

Currently, only the low-order bit of the flags field is used.  A `0` value of indicates that the referenced binary is non-literal.

The off-heap data is a block of allocated data, containing:

* a ListHead structure, used to maintain a list of dynamically allocated data (mostly for bookkeeping purposes);
* a reference count (unsigned integer);
* the size of the stored data;
* the stored data, itself.

All of the above data is allocated in a single block, so that it can be easily `free`'d when no longer referenced.

The reference count is initialized to 1, under the principle that that reference count is incremented for any occurrence of boxed terms that reference the same data in any heap space, including process heaps, mailbox messages, heap fragments, and so forth.  Decrementing reference counts and `free`'ing data in off-heap storage is discussed in more detail below, in the Garbage Collection section.

                                   |< 6  >|
    +--> +=========================+======+
    |    |    boxed-size (5)       |100000| boxed[0]
    |    +-------------------------+------+
    |    |         size (in bytes)        | boxed[1]
    |    +--------------------------------+
    |    |             flags             0| boxed[2]
    |    +--------------------------------+                   off-heap storage
    |    |              ptr >-------------- boxed[3] ---> +----------------------+ ------
    |    +--------------------------------+               |         prev         |     ^
    |    |              cdr               | boxed[4]      +----------------------+     | ListHead
    |    +--------------------------------+               |         next         |     v
    +-----------------< car               | boxed[5]      +----------------------+ ------
         +================================+               |   reference-count    |
         |<---------- word-size --------->|               +----------------------+
                                                          |         size         |
                                                          +----------------------+
                                                          |         data         |
                                                                     ...
                                                          |                      |
                                                          +----------------------+

```{note}
The size of a reference counted binary is stored both in the process heap (in the boxed term), as well as in the
off-heap storage.  The size count in the off-heap storage is needed in order to report the amount of data in use by
binaries (e.g., via [`erlang:memory/0,1`](./apidocs/erlang/estdlib/erlang.md#memory1)).
```

In addition, a reference-counted boxed term contains a cons-cell appended to the end of the boxed term, which is used by the garbage collector for tracking references.  The `car` of this cell points to the boxed term, itself, and the `cdr` points to the "previous" cons cell associated with a reference counted binary in the heap, if there is one, or the empty list (`nil`), otherwise.  The cons cell forms an element in the "Mark and Sweep Object" (MSO) list, used to reclaim unreferenced storage during a garbage collection event..  See the Garbage Collection section, below, for more information about the critical role of this structure in the process of reclaiming unused memory in the AtomVM virtual machine.

#### Const Binaries

Const binaries are stored in the same manner as Reference Counted binaries, with the following exceptions:

* The low order bit of the flags field (`boxed[2]`) is `1`, to indicate that the reference binary is constant;
* The ptr field (`boxed[3]`) points directly to the constant storage (e.g., literal data stored in a memory-mapped BEAM file);
* The trailing cons cell elements are unused, as dynamic memory management for static storage is unnecessary.  These values are initialized to `nil`.

This heap structure has the following representation:

                              |< 6  >|
    +=========================+======+
    |    boxed-size (5)       |100000| boxed[0]
    +-------------------------+------+
    |         size (in bytes)        | boxed[1]
    +--------------------------------+
    |             flags             1| boxed[2]
    +--------------------------------+                       static storage
    |              ptr >-------------- boxed[3] -------> +----------------------+
    +--------------------------------+                   |          data        |
    |             unused             | boxed[4]          |                      |
    +--------------------------------+                               ...
    |             unused             | boxed[5]          |                      |
    +================================+                   +----------------------+
    |<---------- word-size --------->|

#### Match Binaries

Match binaries are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x04` (`000100b`), and the following elements:

* a reference to either a binary or another match binary that refers to a binary;
* an offset in the referenced binary used by the match opcodes;
* a saved state used for backtracking unmatched clause heads;

Like a reference counted binary, a match binary includes a trailing cons cell, whose `car` element points to the actual referenced binary (if the referenced binary is a reference-counted binary), and whose `cdr` points to the "previous" cons cell associated with a reference counted binary in the heap.

```{note}
If the referenced binary is not reference-counted, the trailing cons cell elements are unused and are initialized
to `nil`.
```

    some
    binary                            |< 6  >|
    ^       +=========================+======+
    |       |    boxed-size (5)       |000100| boxed[0]
    |       +-------------------------+------+
    |       |      match-or-binary-ref       | boxed[1]
    |       +--------------------------------+
    |       |             offset             | boxed[2]
    |       +--------------------------------+
    |       |             saved              | boxed[3]
    |       +--------------------------------+
    |       |              cdr               | boxed[4]
    |       +--------------------------------+
    +--------------------< car               | boxed[5]
            +================================+
            |<---------- word-size --------->|

A reference to a reference-counted binary counts as a reference, in which case the creation or copying of a match binary results in the increment of the reference-counted binary's reference count, and the garbage collection of a match binary results in a decrement (and possible `free`ing) of a reference-counted binary.  The trailing cons cell becomes an element of the context (or message) MSO list, and plays a critical role in garbage collection.  See the garbage collection section below for more information about the role of this structure.

#### Sub-Binaries

Sub-binaries are represented as boxed terms containing a boxed header (`boxed[0]`), a type tag of `0x28` (`101000b`)

A sub-binary is a boxed term that points to a reference-counted binary, recording the offset into the binary and the length (in bytes) of the sub-binary.  An invariant for this term is that the `offset + length` is always less than or equal to the length of the referenced binary.

        some
        refc
        binary                            |< 6  >|
        ^       +=========================+======+
        |       |    boxed-size (3)       |101000| boxed[0]
        |       +-------------------------+------+
        |       |              len               | boxed[1]
        |       +--------------------------------+
        |       |             offset             | boxed[2]
        |       +--------------------------------+
        +----------------< binary-ref            | boxed[3]
                +================================+
                |<---------- word-size --------->|

Note than when a sub-binary is copied between processes (e.g., via [`erlang:send`](./apidocs/erlang/estdlib/erlang.md#send2), or `!`), the sub-binary boxed term, as well as the boxed-term that manages the reference-counted binary is copied, as well.  Thus, sending a sub-binary to another process will result in an increment of the reference count on the referenced binary, and similarly, garbage collection of the sub-binary will result in a decrement of the referenced binary's reference count.

A sub-binary may be created from both const (literal) and non-const reference-counted binaries.  For performance reasons, sub-binaries do not reference heap binaries.

Sub-binaries are created via the [`binary:part/3`](./apidocs/erlang/estdlib/binary.md#part3) and [`binary:split/2`](./apidocs/erlang/estdlib/binary.md#split2) Nifs, as well as via the `/binary` bit syntax specifier.

## Lists

A list is, very simply, a cons cell, i.e., a sequence of two words, whose first word is a term (single word or term pointer) representing the tail (`cdr`) of the list, and the second of which represents the head (`car`) of the list.

    +================================+
    |                tail            | list_elem[0]
    +--------------------------------+
    |                head            | list_elem[1]
    +================================+
    |                                |
    |<---------- word-size --------->|

```{note}
Lists are typically terminated with the empty list (`[]`), represented by the nil term, described above.  However,
nothing in Erlang requires that a sequence of cons cells is `nil`-terminated.
```

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

```{note}
String elements may not remain contiguous after a garbage collection event.
```

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
    +--------------------------------+
    |            closure_1           | boxed[3]
    +- - - - - - - - - - - - - - - - +
    |               ...              |
    +- - - - - - - - - - - - - - - - +
    |            closure_k           | boxed[n-1]
    += = = = = = = = = = = = = = = = +
    |                                |
    |<---------- word-size --------->|

## Special Stack Types

Some terms are only used in the stack.

### Continuation Pointer

A continuation pointer is a raw address.  Because words are aligned on word boundaries, the low order two bits of a continuation pointer are always `0x0` (`(00000000)b`):

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

Module and catch label indices are stored outside of the process heap and are outside of the scope of this document.

## Garbage Collection

Garbage collection refers to the process of removing no-longer referenced term data stored in the heap, making room for new storage, as the program requires.  AtomVM implements [Tracing Garbage Collection](https://en.wikipedia.org/wiki/Tracing_garbage_collection), as does [Erlang Garbage Collection](https://erlang.org/doc/apps/erts/GarbageCollection.html).  Unlike some garbage collection systems (e.g., as implemented by the Java Virtual Machine), garbage collection in Erlang-based systems, is performed independently on the heap allocated for each active Erlang process; there is no single shared heap for all running Erlang processes.

A given process heap and stack occupy a single region of malloc'd memory, and it is the job of the Erlang VM to manage memory within the allocated regions.  Because this region is fixed, every allocation in the heap or stack results in less free space for the Erlang process.  When free space reaches a limit, AtomVM will run a garbage collection event, which will allocate a new block of memory to hold the new heap and stack (the actual allocation depends on the heap growth strategy [as explained above](#heap-growth-strategies)), and then copy terms from the old heap and stack to the new heap and stack.  Any terms that no longer have references from term pointers in the old stack or registers are not copied to the new stack, and are therefore "collected" as garbage.  In addition, any objects in the old heap that reference objects in shared memory (see reference counted binaries, above) are also managed as part of this process, in a manner described below.

                                  +---------+ ------
                                  |   new   |    ^
                                  |   heap  |    |
                                  +---------+    |
           ---- +----------+      |         |    |
            ^   |          |      |         |    |
            |   |   old    |      |         |    |  new
    old     |   |   heap   |      |         |    |  malloc'd
    malloc'd|   |          | ===> |         |    |  region
    region  |   +==========+  gc  |  free   |    |
            |   |   old    |      |         |    |
            v   |   stack  |      |         |    |
           ---- +----------+      |         |    |
                                  +---------+    |
                                  |   new   |    |
                                  |   stack |    v
                                  +---------+ -----


    +---+---+---+-------------------+---+
    | 0 | 1 | 2 |                   | 15|
    +---+---+---+-------------------+---+
    registers


    process dictionary
    +--------+--------+
    |   k1   |   v1   |
    +--------+--------+
    |   k2   |   v2   |
    +--------+--------+
    |       ...       |

Terms stored in the stack, registers, and process dictionary are either single-word terms (like atoms or pids) or term references, i.e., single-word terms that point to boxed terms or list cells in the heap.  These terms constitute the "roots" of the memory graph of all "reachable" terms in the process.

Boxed integers, including both native boxed integers and big integers, are simple blob structures that are copied as-is during garbage collection. They do not contain any pointers or addresses that need to be updated during the garbage collection process.

### When does garbage collection happen?

Garbage collection typically occurs as the result of a request for an allocation of a multi-word term in the heap (e.g., a tuple, list, or binary, among other types), and when there is currently insufficient space in the free space between the current heap and the current stack to accommodate the allocation.

Garbage collection is a _synchronous_ operation in each Context (Erlang process), but conceptually no other execution contexts are impacted (i.e., no global locks, other than those required for memory allocation in the OS process heap).

### Garbage Collection Steps

Garbage collection in AtomVM can be broken down into the following phases:

* Allocation of a new block of memory to store the new heap and stack;
* A "shallow copy" of all root terms (from the stack, registers, and process dictionary) into the heap, as well as updates to the references in the stack, registers, and process dictionary;
* An iterative "scan and copy" of the new heap, until all "live" terms are copied to the new heap;
* A sweep of the "Mark Sweep Object" list;
* Deletion of the old heap.

The following subsections describe these phases in more detail.

#### Allocation

Garbage collection typically occurs as the result of a request for space on an Erlang process's heap.  The amount of space requested is dependent on the kind of term being allocated, but in general, AtomVM will check the amount of free space in the heap, and if it is below the amount of requested space plus some extra (currently, 16 words), then a garbage collection will occur, with the requested allocation space being the current size of the heap, plus the requested size, plus an extra 16 words.

Allocation is a straightforward `malloc` in the (operating system) process heap of the requested set of words.  This block of storage will become the "new heap", as opposed to the existing, or "old heap".

#### Shallow Copy

The garbage collector starts by traversing the current root set, i.e., the terms contained in the stack, registers, and keys and values in the process dictionary, and performs a "shallow copy" of the terms that are in or referenced from these root terms from the old heap to the new heap, while at the same time updating the values in the root set, as some of these values may be pointers into the old heap, and therefore need to be updated to pointers in the new heap.

A shallow copy of a term depends on the type of the term being copied.  If the term is a single-word term, like an atom or pid, then the term only resides in the root set, itself, and nothing needs to be copied from the old heap to the new heap.  (The term _may_ occur in the heap elsewhere, but as an element of another term, like a tuple, for example.)

On the other hand, if the term in the root set points to a boxed term in the old heap, then three things happen:

* The boxed term is copied from the old heap to the new heap.  Note that if the term being copied contains pointers to other boxed terms in the old heap, the pointers are _not_ updated (yet); they will be as part of the iterative scan and copy (see below);
* The first word of the existing boxed term that was copied is _over-written_ with a marker value (`0x2b`) in the old heap, and the second word is over-written with the address of the copied boxed term in the new heap.
* The term in the root set is updated with the address of the copied boxed term in the new heap.

This process is best illustrated with a motivating example:

    {foo, <<1,2,3,4,...,1024>>}

Suppose this term resides in the old heap, and some `register[i]` is a root term pointer to this tuple in the heap:

    +-> |              |        |             |
    |   |              |        |             |
    |   |              |        |             |  USED
    |   |              |        |             |
              ...                     ...
    |   |              |        |             |
    |   +--------------+        +=============+ <-- heap
    |   |     tuple    |<---+   |             |     addr
    |   +--------------+    |   |             |
    |   |   atom foo   |    |   |             |
    |   +--------------+    |   |             |
    +----< refc binary |    |   |             |  FREE
        +--------------+    |   |             |
        |              |    |   |             |
        |     ...      |    |   |     ...     |
        |              |    |   |             |
            old heap        |       new heap
                            |
                ---+----------------+---
               ... |     old-ptr    | ...
                ---+----------------+---
                      register[i]

The boxed term is copied to the new heap, overwritten with the marked header `0x2b`, along with a pointer to the new term, and the root term is updated with the same address:

    +-> |              | <--------+   |             |
    |   |              |          |   |             |
    |   |              |          |   |             |
    |   |              |          |   |             |
               ...                          ...
    |   |              |          |   |             |
    |   +--------------+ >>>>>>>>>|>> +-------------+
    |   |     0x2b     |   +--+---|-> |   tuple     | USED
    |   +--------------+   |  |   |   +-------------+
    |   |     ptr  --------+  |   |   |  atom foo   |
    |   +--------------+      |   |   +-------------+
    +----< refc binary |      |   +----< refc binary|
        +--------------+ >>>>>|>>>>>>>+=============+ <-- new
        |              |      | COPY  |             |   heap
        |     ...      |      |       |             |   addr
             old heap         |       |             |
                              |       |             | FREE
                              |
                              |
                  ---+----------------+---
                 ... |     new-ptr    | ...
                  ---+----------------+---
                        register[i]

Note that the first term of the tuple (atom `foo`) is copied to the new heap, but the pointer to the refc binary is out of date -- it still points to a value in the old heap.  This will be corrected in the iterative scan and copy phase, below.

After a shallow copy of the root set, all terms immediately reachable from the root set have been copied to the new heap, and any boxed terms they reference have been marked as being moved.  The new heap consists of a set of contiguous copied boxed terms from the old heap, starting from the base address of the heap, to some higher address in the heap, but less than or equal to the maximum heap size on the new heap.

#### Iterative Scan and Copy

The iterative scan and copy phase works as follows:

* Starting with the newly created region used in the shallow copy phase in the new heap, iterate over every term in the region  (call this the "scan&copy" region);
* If any term in this region is a reference to a term on the old heap that has _not_ been marked as copied, perform a shallow copy of it (as described above) to the new heap, but starting at the next free address below the region being iterated over;
* Note that after iterating over all such terms in the scan and copy region, all terms are "complete", in that there are no references to boxed terms in the old heap in that region.  We have, however, created a new region which may have references to boxed terms in the old heap;
* So we repeat the process on the new region, which will complete the current scan&copy region, but which in turn may create a new region of copied terms;
* The process is repeated until no new regions have been introduced.

The following sequence of iterative additions to the new heap illustrates this process:

    +---------------+ ===> +---------------+ ===> +---------------+ ...
    |   scan&copy   |      |    complete   |      |    complete   |
    |   region      |      |    region     |      |    region     |
    |               |      |               |      |               |
    |               |      |               |      |               |
    |               |      |               |      |               |
    |               |      |               |      |               |
    |               |      |               |      |               |
    +---------------+      +---------------+      +---------------+
                           |     newly     |      |   scan&copy   |
                           |    copied     |      |   region      |
                           |     terms     |      |               |
                           +---------------+      +---------------+
                                                  |newlycpiedterms|
                                                  +---------------+



     ... ===> +---------------+ ===> +---------------+
              |    complete   |      |    complete   |
              |    region     |      |    region     |
              |               |      |               |
              |               |      |               |
              |               |      |               |
              |               |      |               |
              |               |      |               |
              |               |      |               |
              |               |      |               |
              |               |      |               |
              |               |      |               |
              +---------------+      |               |
              |  scan&copy    |      |               |
              +---------------+      +---------------+

At the end of the iterative scan and copy, all reachable terms in the old heap will be copied to the new heap, and no boxed terms in the old heap will contain pointers to terms in the old heap.  Any terms that have not been copied to the new heap are "garbage", as there are no longer any paths to them from the root set, and can therefore be destroyed,

#### MSO Sweep

As mentioned in the section above on binaries, AtomVM supports reference-counted binaries, whereby binaries of a sufficiently large size (>64 bytes) are allocated outside of the process heap, and are instead referenced from boxed terms in the heap.  This way, binaries, which are immutable objects, can be shared between processes without incurring the time and space cost of a large data copy.

In order to manage the memory associated with such binaries, AtomVM tracks references to these off-heap binaries via the "Mark and Sweep Object" list, a list that keeps track of which boxed terms in the process heap have a reference to an off-heap binary.   When such a boxed term is copied (e.g., from a heap to a mailbox on a `send`, or from a mailbox to a heap on a `receive`), the reference count is incremented on the off-heap binary.

The MSO list is formed via the cons cells that are appended to reference counted binary boxed terms in the process heap.  The list is initially empty (nil), but as reference counted binaries are added to the process heap, they are pre-pended to the MSO list for the process (on the mailbox message, as reference-counted binaries in the mailbox need to be managed, as well).

The following diagram illustrates a set of two reference counted binaries in a process heap:

            |                   |
    +-----> +-------------------+
    |       |       refc        |
    |       |      binary       |
    |       |                   |
    |       |                   |
    |       +-------------------+ <----+
    |       |        nil        |      |
    |       +-------------------+      |
    +---------------< car       |      |
            +-------------------+      |
            |                   |      |
                    ...
            |                   |      |
    +-----> +-------------------+      |
    |       |       refc        |      |
    |       |      binary       |      |
    |       |                   |      |
    |       |                   |      |
    |       +-------------------+ <----|-------+
    |       |        cdr >-------------+       |
    |       +-------------------+              |
    +--------------< car        |              |
            +-------------------+              |
            |                   |              |
            |                   |              |
                     ...                       |
                                               |
                                       +-------^--------+
                                       |   mso_list     |
                                       +----------------+

After the new heap has been scanned and copied, as described above, the MSO list is traversed to determine if any reference-counted binaries are no longer referenced from the process heap.  If any reference counted binaries in the heap have not been marked as moved from the old heap, they are, effectively, no longer referenced from the root set, and the reference count on the corresponding off-heap binary can be decremented.  Furthermore, when the reference count reaches 0, the binaries can then be deleted.

```{note}
Const binaries, while they have slots for entry into the MSO list, nonetheless are never "stitched" into the MSO
list, as the binary data they point to is const, endures for the lifecycle of the program, and is never deleted.
Match binaries, on the other hand, do count as references, and can therefore be stitched into the MSO list.  However,
when they are, the reference counted binaries they point to are the actual binaries in the process heap, not the
match binaries, as with the case of refc binaries on the process heap.
```

#### Deletion

Once all terms have been copied from the old heap to the new heap, and once the MSO list has been swept for unreachable references, the old heap is simply discarded via the `free` function.
