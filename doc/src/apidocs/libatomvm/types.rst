.. Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
..
.. SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

:orphan:

.. c:namespace:: libAtomVM
.. c:namespace-push:: types

---------------------
Types
---------------------

.. toctree::
   :maxdepth: 3
   :caption: Types

.. doxygentypedef:: atom_ref_t
.. doxygentypedef:: AtomString
.. doxygentypedef:: avm_float_t
.. doxygentypedef:: avm_int64_t
.. doxygentypedef:: avm_int_t
.. doxygentypedef:: avm_uint64_t
.. doxygentypedef:: avm_uint_t
.. doxygentypedef:: avmpack_fold_fun
.. from exclided opcodesshwitch.h
.. c:type:: term* dreg_t

.. c:type:: dreg_gc_safe_t

   .. c:struct:: _

      .. c:var:: term *base
      .. c:var:: int index

.. doxygentypedef:: ERL_NIF_TERM
.. doxygentypedef:: ErlNifEvent
.. doxygentypedef:: ErlNifMonitor
.. doxygentypedef:: ErlNifPid
.. doxygentypedef:: ErlNifResourceDown
.. doxygentypedef:: ErlNifResourceDtor
.. doxygentypedef:: ErlNifResourceStop
.. doxygentypedef:: ErlNifResourceType
.. doxygentypedef:: event_handler_t
.. doxygentypedef:: EventListener
.. TODO: find out why Doxygen can parse this from mailbox.h
.. c:type:: MailboxMessage MailboxMessage

   .. c:struct:: MailboxMessage

      .. c:var:: MailboxMessage *next
      .. c:union:: _

         .. c:var:: enum MessageType type
         .. c:var:: term *heap_fragment_end

.. doxygentypedef:: term
