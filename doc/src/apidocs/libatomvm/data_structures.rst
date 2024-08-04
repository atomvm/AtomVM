.. Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
..
.. SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

:orphan:

.. c:namespace:: libAtomVM
.. c:namespace-push:: structs

---------------------
Data Structures
---------------------

.. toctree::
   :maxdepth: 4
   :caption: Structs

.. doxygenstruct:: AtomsHashTable
   :allow-dot-graphs:
.. doxygenstruct:: AtomStringIntPair
   :allow-dot-graphs:
.. doxygenstruct:: AtomTable
   :allow-dot-graphs:
.. doxygenstruct:: AVMPackData
   :allow-dot-graphs:
.. doxygenstruct:: BuiltInAtomRequestSignal
   :allow-dot-graphs:
.. doxygenstruct:: BuiltInAtomSignal
   :allow-dot-graphs:
.. doxygenstruct:: CharDataToBytesAcc
   :allow-dot-graphs:
.. doxygenstruct:: CharDataToBytesSizeAcc
   :allow-dot-graphs:
.. doxygenstruct:: ConstAVMPack
   :allow-dot-graphs:
.. doxygenstruct:: Context
   :allow-dot-graphs:
.. doxygenstruct:: ContextAccumulator
   :allow-dot-graphs:
.. doxygenstruct:: DictEntry
   :allow-dot-graphs:
.. doxygenstruct:: EntropyContextResource
   :allow-dot-graphs:
.. doxygenstruct:: ErlNifEnv
   :allow-dot-graphs:
.. doxygenstruct:: ErlNifResourceTypeInit
   :allow-dot-graphs:
.. doxygenstruct:: ExportedFunction
   :allow-dot-graphs:
.. doxygenstruct:: FprintfFun
   :allow-dot-graphs:
.. doxygenstruct:: GenMessage
   :allow-dot-graphs:
.. doxygenstruct:: GlobalContext
   :allow-dot-graphs:
.. doxygenstruct:: Heap
   :allow-dot-graphs:
.. Doxygen mangles this structure when parsing atomshashtable.c.
.. c:struct:: HNode

   **Public Members**

   .. c:var:: struct HNode *next
   .. c:var:: AtomString key
   .. c:var:: unsigned long value

.. doxygenstruct:: HNodeGroup
   :allow-dot-graphs:
.. doxygenstruct:: IFFRecord
   :allow-dot-graphs:
.. doxygenstruct:: InMemoryAVMPack
   :allow-dot-graphs:
.. defined in excluded opcodesswitch.h
.. c:struct:: Int24

   .. c:var:: int32_t val24 : 24

.. c:struct:: Int40

   .. c:var:: int64_t val40 : 40

.. c:struct:: Int48

   .. c:var:: int64_t val48 : 48

.. c:struct:: Int56

   .. c:var:: int64_t val56 : 56

.. c:struct:: kv_pair

   **Public Members**

   .. c:var:: term key
   .. c:var:: term value

.. end of opcodesswitch.h structs
.. doxygenstruct:: LineRefOffset
   :allow-dot-graphs:
.. doxygenstruct:: ListHead
   :allow-dot-graphs:
.. doxygenstruct:: LiteralEntry
   :allow-dot-graphs:
.. doxygenstruct:: Mailbox
   :allow-dot-graphs:
.. doxygenstruct:: Message
   :allow-dot-graphs:
.. doxygenstruct:: Module
   :allow-dot-graphs:
.. doxygenstruct:: ModuleFilename
   :allow-dot-graphs:
.. doxygenstruct:: ModuleFunction
   :allow-dot-graphs:
.. doxygenstruct:: Monitor
   :allow-dot-graphs:
.. doxygenstruct:: Nif
   :allow-dot-graphs:
.. doxygenstruct:: PrinterFun
.. doxygenstruct:: RefcBinary
   :allow-dot-graphs:
.. doxygenstruct:: RefcBinaryAVMPack
   :allow-dot-graphs:
.. doxygenstruct:: RefSignal
   :allow-dot-graphs:
.. doxygenstruct:: RegisteredProcess
   :allow-dot-graphs:
.. doxygenstruct:: ResourceMonitor
   :allow-dot-graphs:
.. doxygenstruct:: ResourceType
   :allow-dot-graphs:
.. doxygenstruct:: SelectEvent
   :allow-dot-graphs:
.. doxygenstruct:: SnprintfFun
   :allow-dot-graphs:
.. doxygenstruct:: SSLConfigResource
   :allow-dot-graphs:
.. doxygenstruct:: SSLContextResource
   :allow-dot-graphs:
.. doxygenstruct:: SyncList
   :allow-dot-graphs:
.. doxygenstruct:: TermSignal
   :allow-dot-graphs:
.. doxygenstruct:: TimerList
   :allow-dot-graphs:
.. doxygenstruct:: TimerListItem
   :allow-dot-graphs:
.. doxygenstruct:: UnresolvedFunctionCall
   :allow-dot-graphs:
.. doxygenstruct:: ValuesHashTable
   :allow-dot-graphs:

.. c:namespace-pop::
.. c:namespace-push:: enums

---------------------
Enumerations
---------------------

.. toctree::
   :maxdepth: 3
   :caption: Enums

.. doxygenenum:: AtomTableCopyOpt
.. doxygenenum:: BitstringFlags
.. doxygenenum:: CharDataEncoding
.. doxygenenum:: ContextFlags
.. doxygenenum:: DictionaryFunctionResult
.. doxygenenum:: ErlNifResourceFlags
.. doxygenenum:: ErlNifSelectFlags
.. doxygenenum:: ExternalTermOpts
.. doxygenenum:: ExternalTermResult
.. doxygenenum:: FunctionType
.. doxygenenum:: GenMessageParseResult
.. doxygenenum:: HeapGrowthStrategy
.. doxygenenum:: inet_domain
.. doxygenenum:: inet_protocol
.. doxygenenum:: inet_type
.. doxygenenum:: InteropFunctionResult
.. doxygenenum:: MemoryAllocMode
.. doxygenenum:: MemoryGCResult
.. doxygenenum:: MessageType
.. doxygenenum:: ModuleLoadResult
.. doxygenenum:: NativeHandlerResult
.. doxygenenum:: OpenAVMResult
.. doxygenenum:: RefcBinaryFlags
.. doxygenenum:: SocketErrors
.. doxygenenum:: TermCompareOpts
.. doxygenenum:: TermCompareResult
.. doxygenenum:: UnicodeConversionResult
.. doxygenenum:: UnicodeTransformDecodeResult
