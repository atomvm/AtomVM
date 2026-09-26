# PR Review: last three commits

**Scope:** commits `e6ca78649`, `fbc662828`, and `4db6cd43b` (aggregate diff `HEAD~3..HEAD`)
**Verdict:** **Request changes**

The host implementation builds and the existing TCP/UDP socket tests pass, but the new write-wait path has selector ownership, lwIP lifetime, and liveness defects that can strand processes or dereference freed resources. The three commits add no regression tests for backpressure or simultaneous read/write selection.

> **Remediation status:** The accompanying changes address all six findings. They add regression coverage for independent read/write selectors, zero-length UDP datagrams, and empty sends on closed sockets. The clean host build and full `test_estdlib.avm` suite pass; the changed Pico W/lwIP objects also compile successfully. Hardware-only reset, callback-queue saturation, SSL WANT_WRITE, and forced distribution backpressure scenarios remain unexecuted.

## Findings

### P1 — Read and write selections still share one owner and monitor

**File:** `src/libAtomVM/otp_socket.c:322-373, 767-800, 1002-1011, 1129-1157`

`SelectEvent` now stores separate read/write PIDs, but `SocketResource` still has only one `selecting_process_id` and one `selecting_process_monitor`. Registering write readiness from process B therefore demonitor/process-replaces an existing read selection from process A.

On lwIP, incoming data is then delivered using the shared PID at lines 980-981, so A's read reference is sent to B and A remains blocked. On BSD, readiness itself uses the direction-specific `SelectEvent` PID, but close and process-down handling still act on the shared owner: closing sends both abort references to the last selector, and the last selector exiting stops both directions.

Track the owner and monitor independently for read and write selection, and make close, stop, and `socket_down` direction-aware. The connect trap should have separate ownership from established-socket readiness. This is not safely fixable as a small local patch because monitoring, cancellation, and lwIP delivery all depend on the same fields.

### P1 — BSD accepted sockets use uninitialized select references

**File:** `src/libAtomVM/otp_socket.c:2021-2024`

The two new reference fields are initialized for sockets opened normally and for lwIP accepted sockets, but not for BSD accepted sockets. `enif_alloc_resource` ultimately uses `malloc`, so these values are indeterminate. A later close can treat garbage as an active reference and build a malformed abort notification.

```diff
diff --git a/src/libAtomVM/otp_socket.c b/src/libAtomVM/otp_socket.c
@@
         struct SocketResource *conn_rsrc_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketResource));
         conn_rsrc_obj->fd = fd;
         conn_rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
+        conn_rsrc_obj->read_select_ref_ticks = 0;
+        conn_rsrc_obj->write_select_ref_ticks = 0;
         conn_rsrc_obj->buf_size = DEFAULT_BUFFER_SIZE;
```

### P1 — Queued lwIP write callbacks can dereference a freed socket resource

**File:** `src/libAtomVM/otp_socket.c:264-292, 639-645, 899-903, 1002-1040`

`tcp_sent_cb` and `tcp_poll_cb` enqueue a raw `SocketResource *` without retaining the resource. The callbacks are installed for every TCP PCB, not only while a write selection is pending. If the last Erlang reference disappears, `socket_dtor` can close the PCB and free the resource while a previously queued sent/poll event still points to it; `tcp_sent_handler` then dereferences freed memory. A successful `tcp_close` may also leave a closing PCB invoking callbacks after the destructor returns.

Detach sent/poll callbacks before releasing their owner and give every queued event an explicit resource reference that is released after dispatch (including enqueue-failure handling). The handler must also synchronize with close before reading `socket_state` or `tcp_pcb`. The explicit-close finalizer does not protect destructor-driven close or events queued before it.

### P1 — lwIP terminal TCP errors do not complete write waiters

**File:** `src/libAtomVM/otp_socket.c:875-903, 1186-1200, 2808-2825`; `libs/estdlib/src/socket.erl:761-770`; `libs/estdlib/src/ssl.erl:345-354`

After `nif_select_write/2`, callers wait only for a matching `'$socket'` select/abort message. `tcp_err_cb`, however, clears the shared selecting PID and sends a `TrapAnswerSignal`, which is the connect trap protocol and cannot match either receive. A reset or TCP timeout during backpressure can therefore leave `socket:send/2` or SSL WANT_WRITE blocked forever. Accepted lwIP PCBs compound this by installing recv/sent/poll callbacks but no `tcp_err` callback.

Separate connect-trap state from readiness selection. For an established socket error, mark the PCB unavailable and send direction-specific `{'$socket', Socket, abort, {Ref, closed}}` notifications. Install the error callback on accepted PCBs as well. Also return `{error, closed}` rather than raising `badarg` when write selection races with close.

### P1 — Blocking distribution sends can deadlock bidirectional traffic

**File:** `libs/estdlib/src/socket_dist_controller.erl:160-172, 214-227`; `libs/estdlib/src/socket.erl:742-770`

The distribution controller owns both `send_data_loop/1` and `recv_data_loop/1`. `socket:send/2` now blocks inside a selective receive while waiting for output readiness. If two peers simultaneously fill their send buffers, both controllers wait for write readiness and neither returns to `handle_info/2` to drain readable peer data. Neither receive buffer drains, so neither write necessarily becomes ready; ticks and `getstat` are blocked too.

Keep distribution output event-driven: retain the unsent framed suffix and write reference in controller state, return to the gen_server loop on backpressure, and resume it on write readiness while continuing to service read readiness. Queue ticks behind any partial frame to preserve byte ordering. A linked writer is an alternative only after cross-process selector ownership is fixed.

### P2 — Empty `socket:send/2` bypasses the socket and drops zero-length UDP datagrams

**File:** `libs/estdlib/src/socket.erl:588-595, 742-749`; `src/libAtomVM/otp_socket.c:2603-2647`

The new empty-binary clauses return `ok` without validating or touching the socket. Consequently, `socket:send(InvalidOrClosedSocket, <<>>)` falsely succeeds. For a connected UDP socket, a valid zero-length datagram is silently dropped. Removing the shortcuts exposes a second lwIP issue: a successful zero-byte native send is currently classified as `SocketWouldBlock` solely because `sent_data == 0`.

```diff
diff --git a/libs/estdlib/src/socket.erl b/libs/estdlib/src/socket.erl
@@
-send(_Socket, <<>>) ->
-    ok;
 send(Socket, Data) when is_binary(Data) ->
     send_all_binary(Socket, Data);
@@
-send_all_binary(_Socket, <<>>) ->
-    ok;
 send_all_binary(Socket, Data) ->
     case ?MODULE:nif_send(Socket, Data) of
diff --git a/src/libAtomVM/otp_socket.c b/src/libAtomVM/otp_socket.c
@@
-    if (sent_data == 0) {
+    if (sent_data == 0 && err == ERR_MEM) {
         return SocketWouldBlock;
     }
```

## Missing regression coverage

Add focused tests that force the states introduced by this PR:

- fill a TCP send buffer, verify the writer blocks, then drain it and verify exact bytes without loss or duplication;
- close locally and reset remotely while a writer is blocked;
- register read and write selections from different processes in both orders, then test readiness, close, and one selector exiting;
- saturate distribution traffic in both directions with a bounded completion time;
- on lwIP, destroy/close a resource after a sent event is queued and reset both connected and accepted sockets during write wait;
- force SSL WANT_WRITE and verify success/error wakeup;
- verify a connected UDP peer receives exactly one zero-length datagram and that an empty send on a closed socket fails.

## Verification performed

- `git diff --check HEAD~3..HEAD` — passed.
- Debug host build with JIT disabled: `cmake --build .amp/in/review-build --target AtomVM test_estdlib -j4` — passed.
- `test_estdlib.avm` — `test_tcp_socket`, `test_udp_socket`, `test_gen_tcp`, and `test_gen_udp` passed. The aggregate run failed only at `test_net_kernel` because this local build lacks MbedTLS/crypto (`crypto:strong_rand_bytes/1` was unavailable); SSL was skipped for the same configuration reason.
- No lwIP target was executed, so the lwIP findings are based on direct callback/resource-lifetime tracing rather than runtime reproduction.
