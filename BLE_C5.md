<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# ESP32-C5 BLE controller on RTEMS

`RTEMS_BT=ON` adds ESP32-C5 controller support, the `AtomVM-BLE`
advertising smoke test and the `ble_vhci` Erlang transport. Build and hardware-test
instructions are in
[HW_TEST.md](HW_TEST.md#esp32-c3c5-bluetoothwi-fi-coexistence).
The port does not link a BLE host: GAP, GATT, ATT and pairing need a host.

## Why C5 needs more glue than C3

C3's `libbtdm_app.a` / `libbtdm_app_flash.a` controller supplies its own OS
abstraction. C5's NimBLE-derived `libble_app.a` instead expects an NPL
function table, stack orchestration and eight external mempool functions:

```
os_ext_mempool_register_cb  os_memblock_get  os_mempool_clear
os_memblock_put             os_mempool_ext_init
os_mempool_flags_set        os_mempool_init   os_mempool_unregister
```

The SDK builds only NimBLE's `os_mempool.c` to supply these functions. No
NimBLE host is linked. AtomVM provides the NPL implementation, controller
configuration, task/interrupt callbacks and HCI advertising commands.

The controller configuration and callback tables are binary interfaces to
closed vendor libraries. The port follows ESP-IDF commit
`9a97f6c54ec638111ce55cd36581b3c192f15207`. Cross-compiler assertions verified
all field types, offsets and structure sizes: 66 controller fields, 12 external
callbacks, six coexistence callbacks and 45 NPL callbacks. The latter includes
`eventq_put_to_front`. Magic/version constants and NPL error values also match.

The vendor configuration headers depend on FreeRTOS and an IDF include tree
that cannot be flattened into the BSP include directory. The configuration
structure is therefore declared in `bt/rtems_bt_c5.c`. The NPL headers in
`bt/include/nimble/` are also installed by SDK patch 0018; keep both copies
in sync when changing that interface.

## Resource profile

C5 shares 384 KiB SRAM between executable code, RTEMS, the VM and both radios.
The port keeps IDF's packet sizes and reduces queued packets and connections:

| Resource | C5 setting |
| --- | --- |
| Connections | 1 |
| ACL buffers | 4 x 517 bytes |
| HCI event buffers | 8 high-priority + 4 low-priority, 257 bytes each |
| MSYS pools | 4 x 256 bytes + 8 x 320 bytes |
| RTEMS timer limit | 64, shared by Wi-Fi and NPL |
| Executable SRAM reservation | 80 KiB (`0x14000`) |
| ROM-owned SRAM reservation | 16 KiB |
| CPU / APB clocks | 160 / 80 MHz |
| Bluetooth low-power clock | Actual 40/48 MHz XTAL divided to 100 kHz |
| Controller task priority | IDF 23 maps to RTEMS 2 |
| Controller sleep / retention | Disabled |

The controller's `.adv_fast_execute_code_iram1.*` and
`.high_perf_code_iram1.*` sections add about 25 KiB to the Wi-Fi fast paths.
The SDK linker fragment collects them into `.wifi_iram`. The build script
refreshes that section when upgrading an installed BSP and updates both SRAM
region sizes consistently. The initial IDF-sized buffer profile exhausted
heap when networking started; increasing the IRAM reservation further also
reduces the shared heap.

## Shared radio services and lifecycle

The SDK prepares shared modem state once, regardless of which radio starts
first. Bluetooth enables the MAC, BB, APB, security, BLE timer, ETM and
coexistence clocks and the four IDF peripheral resets. RTEMS patch 0034 routes
C5 Bluetooth, low-power timer, BLE timer/security and coexistence interrupts;
the shared C6 interrupt table otherwise has no Bluetooth routes. Interrupt
installation checks vector-enable failures.

PHY ownership keeps RF active while either radio uses it. The first active
radio starts periodic PLL tracking on the shared timer server; the last stops
it. Wi-Fi stop preserves the coexistence clock while Bluetooth is enabled.
Baseband initialization runs on every controller enable, matching IDF's
`esp_btbb_enable()` after its reference count returns to zero. This is needed
for RF operation after close/reopen, even when HCI commands still succeed.
Controller shutdown and failed init/enable unwind stack stages in reverse
order, release MSYS and unregister callbacks before releasing their services.

NPL queues use interrupt-protected lists and an ownerless binary semaphore as
a wakeup hint. Stale notifications retain the original finite deadline.
Callout stop/reset unlink queued expirations, and generation cookies reject
stale timer-server callbacks without dereferencing deleted memory. Callbacks
without an event queue run in task context and may delete themselves. Timer
deletion and task wakeups happen after restoring interrupts. Reinitialization
reuses OS objects; semaphore count queries read the RTEMS 7 core count under
its queue lock rather than maintaining a separate cache.

RX-buffer optimization and IDF's default scan duplicate settings are enabled.
P-256 key generation/ECDH use the SDK's software MbedTLS ECP routines with
Bluetooth little-endian coordinates/scalars. Controller randomness and ECC
use the independent C5 hardware RNG with IDF's startup configuration and a
sampling delay exceeding its minimum. They do not depend on the SDK's
standalone PSA generator, which has no entropy source before VM startup.

## HCI framing

The advertising commands use H4 framing. Controller command buffers begin at
the opcode, so the send path strips the `0x01` packet indicator. Controller
callbacks supply raw HCI events; the smoke-test ring prepends `0x04` before
parsing them. The advertiser sends one non-connectable legacy advertising
event at a time and updates its manufacturer-data counter after completion.
Initialization runs in a native task so the Erlang VM can keep running.

## Attaching a host through VHCI

The Erlang API matches `esp32-ble-vhci` (`8e4ac9e598`), including the
`Elixir.BleVhci` NIF aliases:

```erlang
{ok, Handle} = ble_vhci:init(),
ok = ble_vhci:send(Handle, self(), <<1, 3, 12, 0>>), % HCI Reset
receive {hci_event, <<4, 14, 4, _Credits, 3, 12, 0>>} -> ok end,
ok = ble_vhci:close(Handle).
```

Only the process that opened the handle may send or close it. Incoming H4
events and ACL packets arrive as `{hci_event, Binary}`. The owner also receives
`{ble_vhci, ready_to_send}` on startup and on transport availability callbacks.
The host must implement HCI command credits and ACL flow control; transport
availability alone does not establish either. Malformed H4 frames raise
`badarg`; controller allocation/transmit failures return `{error, send_failed}`.

Initialization and close suspend the calling Erlang process until a native
task completes the operation, allowing the VM to run other processes. Close
is idempotent, and old handles cannot close a newer session. Owner exit or
handle garbage collection requests asynchronous cleanup. A failed controller
disable leaves the session reserved so close can retry safely. The owner
monitor remains installed across failed close attempts, including when another
process retains the handle. Cleanup requested by GC, owner exit or overflow
retries transient disable failures without admitting a new session.

Controller callbacks only copy packets into a 24-packet queue. Its maximum
H4 frame is 522 bytes (517 ACL payload bytes plus headers), exceeding the
reference branch's 260-byte limit. The queue has 24 descriptors and
allocates each packet at its actual length, leaving SRAM available for Wi-Fi
startup. Queue storage is freed on close. Queue or receive-allocation exhaustion
closes the session and reports
`{ble_vhci, {error, overflow}}`; the host must reopen rather than continue with
missing HCI credits/events. One 4 KiB native worker remains until VM teardown,
where it is joined before freeing VM state. Native-task replies and HCI messages
use the VM's task-driver queue. This depends on the shared dead-recipient cleanup
fix [98ffe7bbc](https://github.com/petermm/AtomVM/commit/98ffe7bbcb62c7e8852c2fd14713cb336be340dc),
carried as a separate prerequisite commit. It releases messages and resource
references when their recipient exits before delivery.

`src/platforms/rtems/src/lib/rtems_bt_vhci.h` provides the native
`esp_vhci_host_*` interface. Start with `rtems_bt_vhci_start_async()` and wait
for `rtems_bt_is_started()` before registering host callbacks. This mode is
mutually exclusive with the advertising smoke test; repeated starts of the
same mode remain successful unless initialization failed.

Commands use `0x01 || command`, events `0x04 || event` and ACL packets
`0x02 || ACL`. Commands lose the H4 indicator before reaching the controller;
outbound ACL data is wrapped in an `os_mbuf`, and received chains are flattened
before calling the host. Receive callback results propagate to the controller.
Registration stores callbacks without proactively signaling send availability;
send availability is gated on the controller being enabled.

The BLE-only startup task can exit because PHY tracking belongs to the shared
radio service. A complete host, pairing and over-the-air ACL traffic still
need validation.

## SDK versions

`docker/dependencies.sh` pins `esp32c5-bt-lib` to
`0b5cb2d7cfb4078e951da36a695bc4fb37e52391`, matching the IDF reference.
The tested C5 `libble_app.a`, `libbtbb.a` and `libphy.a` are byte-identical to
that IDF checkout. Wi-Fi and coexistence retain the existing RTEMS SDK pins:

| Dependency | RTEMS pin | IDF reference pin |
| --- | --- | --- |
| Coexistence | `fa152b3b6b2e3c5e3a57e55fcf4b005a4e28cef9` | `c758e7b56e0fa22177a0539796e1df59978dc322` |
| Wi-Fi | `fe6666fcb138979fe4c54c8790331ac6a3b0f567` | `cce8a7328597181ae4e34962b93aae3b73508f33` |

Updating those pins is a separate SDK upgrade requiring regression checks
across the supported chips. The port does not claim complete IDF feature parity.

## Validation

The standalone NPL suite includes the production implementation with mocked
RTEMS directives. It covers queue ordering/deduplication, stale wakes and tick
rollover, callout cancellation/generations, callback self-deletion, resource
reuse/failure cleanup, semaphore preemption and RTEMS dispatch constraints.

```sh
cmake -S src/platforms/rtems/tests/npl -B /tmp/atomvm-npl-tests \
    -DNPL_SANITIZERS=ON -DCMAKE_BUILD_TYPE=Debug
cmake --build /tmp/atomvm-npl-tests
ctest --test-dir /tmp/atomvm-npl-tests --output-on-failure
```

Validation on 2026-10-22:

- NPL tests pass with AddressSanitizer and UndefinedBehaviorSanitizer.
- Shared task-driver mailbox regressions pass for FIFO ordering and dead
  recipients with normal messages and trap replies: host SMP, plus no-SMP
  with AddressSanitizer and UndefinedBehaviorSanitizer.
- `test_esp32c5_vhci` passes on the physical C5 and in esp-emu 0.49 without
  `--elf` BLE interception: HCI Reset/version/address/buffer-size commands,
  malformed frames, foreign owners, stale handles, repeated close/reopen,
  owner exit and garbage collection. Emulator logs also confirm VM teardown.
- A temporary controller fixture exercises the real NIF with a 522-byte ACL
  receive frame, 25–33-byte receive frames around the inline/refc binary
  boundary, queue overflow, init/send failures, close-failure retry and
  owner exit after a failed close with a handle retained by another process.
  Delayed open/close also verify that another Erlang process runs during each
  operation. The shared task-driver delivery path passes this fixture with
  `DEBUG_HEAP_ALLOC` bounds checks enabled. These are synthetic transport
  checks, not a BLE connection test.
- `test_esp32c5_vhci_coex` passes on the same C5: an Erlang owner closes/reopens
  the controller, then drives advertising through both Wi-Fi traffic phases
  and stop/restart. Two complete runs pass 24 DNS/TCP/HTTP checks and two
  DHCP associations in about 148 s. A Mac scanner receives increasing counters
  in both associated phases. Other runs fail at TCP connect with
  `EHOSTUNREACH`; a LAN HTTP check also times out receiving a response after
  the server receives the request. Network reliability remains unresolved.
  Hardware coexistence has not been rerun after switching delivery to the
  shared task-driver queue with the dead-recipient cleanup fix.
- C5 SRAM requires packet-sized receive allocations and a 4 KiB worker stack.
  Run the API and coexistence suites as separate applications; loading both
  test modules alongside HTTP traffic exhausted the shared heap.
- Both changed shared SDK translation units compile for C3 and C5 with
  `-Wall -Wextra -Werror` and the SDK's unused-parameter suppression.
- Linker fixtures cover fresh, upgraded, partially updated and repeated
  installations, plus rejection of malformed/duplicate sections.
- An additional emulator advertising check reaches HCI setup but asserts in
  `r_ble_hw_resolv_list_search`. The same assertion occurs with and without
  the VHCI bridge; this check does not establish emulator advertising support.
  RF advertising is covered by the physical-board test below.
- An ESP32-C5 v1.0 with a 48 MHz XTAL passes the UART coexistence test:
  BLE starts before Wi-Fi, both traffic phases obtain DHCP, Wi-Fi stops and
  restarts, 24 DNS/TCP/HTTP checks pass, and the app returns `ok` in 149.4 s.
  A separate Mac scanner receives increasing counters before and after
  restart, plus a counter during the stop/restart window.
- A temporary native probe passes four controller lifecycle cycles, 11
  injected init failures, eight enable failures and disable-failure recovery.
  After each cleanup, free heap and RTEMS object counts remain constant.
  BLE-only PHY tracking runs on every cycle. P-256 checks cover distinct
  generated keys, matching ECDH secrets, invalid-point rejection and a
  scalar-one/generator known-answer vector.

The tested SDK was a local overlay with changed glue/BSP objects rebuilt
against pinned sources; a complete fresh SDK image build was not repeated.
Physical 40 MHz C5 boards, connections, pairing/GATT, ACL traffic, sustained
load, stack/heap high-water marks and power-saving coexistence remain untested.
