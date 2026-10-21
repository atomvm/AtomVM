<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# RTEMS hardware testing

This guide covers flashing and testing AtomVM on physical ESP32-C3/C5/C6
boards, with additional checks for S31. Run commands from the repository root,
using the same shell throughout.
For GRiSP 2, see the final section. Track outstanding failures in [TODOS.md](TODOS.md).

## Current baseline

Results from 2026-10-19; these describe the boards tested, not every board revision.

| Check | ESP32-C3 | ESP32-C5 | C6 / S31 |
| --- | --- | --- | --- |
| Direct boot, RTC counter/storage, crypto smoke checks | Pass | Pass | Not tested physically |
| USB console | Pass before sleep | Unstable during boot; UART bridge works | Not tested physically |
| Active/passive scan, association, DHCP, RSSI, DNS, TCP/HTTP, stop/restart | Pass | Pass over UART | Not tested physically |
| Raw ADC and PWM register readback | Pass | Pass | Not tested physically |
| Timer light sleep | Returns, but expired Erlang timer delivery fails | Same failure | Not tested physically |
| ADC accuracy, PWM waveform, deep sleep, GPIO wakeup and power consumption | Not validated | Not validated | Not validated |

S31 startup and Wi-Fi have been compared with ESP-IDF, including CPLL320
calibration, modem clock dependencies, peripheral-access setup, PHY lifecycle
and baseband watchdog settings. The Wi-Fi emulator suite and a focused two-core
PHY/I2C/Wi-Fi lock test pass; the latter verifies nested locking, mutual
exclusion and restoration of both enabled and already-disabled interrupts.
Erlang SMP ping-pong and parallel-compute checks also pass.
S31 has only emulator coverage. A physical board is still needed to validate
PLL settling, clock accuracy, RF, DMA permissions and sleep behavior.

The C6 startup and Wi-Fi paths have been compared with ESP-IDF, including
PLL480 calibration, the 80 MHz flash divider, modem clock dependencies, DMA
access filters, combined-radio PHY calibration, PLL tracking and nested critical
sections. The C6 emulator Wi-Fi suite passes scan/cancellation, connection,
traffic and stop/restart checks; C3/C5/S31 emulator regressions also pass.
C6 has only emulator coverage. On the first physical board, verify
clock/timer accuracy, active/passive scans, WPA2/DHCP, traffic and stop/restart
using this guide; emulator success cannot establish PLL settling, RF behavior
or DMA permissions on silicon.

The C3 revision 0.4 board with a 40 MHz crystal was retested over native USB
Serial/JTAG after aligning its startup and PHY lifecycle with ESP-IDF. The
checks cover RTC timing/storage, crypto, active/passive scans, WPA2/DHCP,
measured RSSI, UDP loopback, DNS, TCP/HTTP and stop/restart. PHY and Wi-Fi
critical sections retain nested interrupt state; the PHY I2C hooks and critical
callbacks reside in internal instruction RAM. C3 also uses IDF's combined-radio
PHY setting and digital-register backup/restore across disable/wakeup. Existing analog and sleep gaps
listed above remain separate validation tasks.

The C5 DevKit V2.0 with a 48 MHz crystal passes active and passive scans,
finding three APs across 2.4 GHz and 5 GHz. WPA2 association on 2.4 GHz, DHCP,
measured RSSI, UDP loopback, DNS and TCP/HTTP pass with the default Wi-Fi
protocols. Two consecutive runs of the final image also pass stop/restart.
An earlier restart ran out of memory; matching ESP-IDF's temporary calibration
buffer lifetime recovers about 1.9 KB of RAM. Extended cycling under load and
a 5 GHz connection still need validation.

On 2026-10-21, the C5 also passes BLE advertising with Wi-Fi coexistence over
UART: BLE starts before Wi-Fi, 24 DNS/TCP/HTTP checks pass across two 60-second
traffic phases, and Wi-Fi stops/restarts and obtains DHCP again while BLE
remains active. The app returns `ok`; a separate BLE receiver sees increasing
advertising counters in both traffic phases. See [BLE_C5.md](BLE_C5.md) for
the required clock/interrupt patches and the measured SRAM/timer budget. This
uses a compact controller profile with one connection. On 2026-10-22, the C5
also passes four controller lifecycle cycles and injected init/enable/disable
failures with stable heap/object counts, standalone PHY tracking and P-256
known-answer/round-trip checks. The post-fix coexistence run also passes.
Connections, pairing, GATT, VHCI ACL traffic and coexistence with power saving remain untested.

Direct boot must calibrate the system PLL for the detected crystal before initializing peripherals; retaining
the ROM's 40 MHz PLL settings overclocks this board to about 191 MHz instead
of 160 MHz. The Wi-Fi adapter also preserves the caller's default radio
arbitration priorities, matching ESP-IDF with coexistence disabled.

The C5 Wi-Fi glue follows ESP-IDF's clock dependencies and first-enable PHY
calibration sequence, including the modem low-power clock, periodic PLL tracking,
baseband watchdog settings and RF shutdown on disable. These use the pinned vendor
LL headers with RTEMS locks and timers. Direct boot also applies ESP-IDF's DMA
access filter and master security-mode setup. Calibration scratch data is freed
after use, as in ESP-IDF. Emulator success still requires a separate hardware
connection and stop/restart check.

UART testing also exposed a cold-boot RTC watchdog reset: the BSP cleared bit 8
instead of the flash-boot enable at bit 12. The BSP patch now clears bit 12;
the C5 stays running through both scans. Emulator tests do not model this reset.

## 1. Prepare the board and a private workspace

You need Docker, Python 3, Erlang/OTP 28, CMake, Ninja, a C compiler, gperf,
rebar3 and the usual [host build dependencies](doc/src/build-instructions.md).
The host compiles BEAM files and packs the application; Docker cross-compiles
the RTEMS firmware. Use host OTP 28 rather than the Docker image's default OTP.

Connect one board with a data-capable USB cable. For Wi-Fi, use a nearby test
AP with DHCP; a 2.4 GHz WPA2-Personal network is a useful common baseline.
Record the board/module, antenna arrangement, chip revision, crystal and flash
size. Disconnect external circuits before the initial test.

```sh
export REPO_ROOT="$PWD"
export CHIP=esp32c3                 # esp32c5 or esp32c6
export HW_PORT=/dev/cu.usbmodem101  # select your port; Linux often /dev/ttyACM0
export HW_WORK="$(mktemp -d "${TMPDIR:-/tmp}/atomvm-${CHIP}-hw.XXXXXX")"
export HW_IMAGE="atomvm-rtems7-hw:${CHIP}"
chmod 700 "$HW_WORK"
umask 077

python3 -m venv "$HW_WORK/venv"
export HW_PYTHON="$HW_WORK/venv/bin/python"
"$HW_PYTHON" -m pip install 'esptool==5.1.0' pyserial
"$HW_PYTHON" -m serial.tools.list_ports -v
"$HW_PYTHON" -m esptool --chip "$CHIP" --port "$HW_PORT" flash-id
```

Match the detected chip to `CHIP` before writing anything. If automatic download
mode fails, hold BOOT, press and release RESET, then release BOOT. Close serial
monitors before running esptool. Native USB Serial/JTAG and an external USB/UART
bridge are different console paths; choose the matching BSP configuration below.
The example uses esptool 5.1.0, which does not recognize `esp32s31`. S31 requires
a flasher with explicit support for that chip before physical testing can proceed;
its BSP and test build use `CHIP=esp32s31`.

### Back up the entire flash first

The example assumes the detected flash is 4 MiB. Change `FLASH_BYTES` to the
actual size. Keep this backup until the board is restored; it may contain
credentials or other private data.

```sh
export FLASH_BYTES=0x400000
"$HW_PYTHON" -m esptool --chip "$CHIP" --port "$HW_PORT" --baud 460800 \
    read-flash 0 "$FLASH_BYTES" "$HW_WORK/original-flash.bin"
chmod 600 "$HW_WORK/original-flash.bin"
"$HW_PYTHON" -c 'import hashlib, pathlib, sys; p = pathlib.Path(sys.argv[1]); print(len(p.read_bytes()), hashlib.sha256(p.read_bytes()).hexdigest())' \
    "$HW_WORK/original-flash.bin" > "$HW_WORK/original-flash.sha256"
cat "$HW_WORK/original-flash.sha256"
git rev-parse HEAD > "$HW_WORK/revision.txt"
git diff > "$HW_WORK/source.diff"
```

Confirm the byte count matches the detected capacity before flashing. These
instructions replace the existing boot firmware. They do not change eFuses.

## 2. Build an SDK with the right console

The committed Docker BSP configurations use UART for emulation. For the native
USB Serial/JTAG connection, use `ESPRESSIF_USE_USB_CONSOLE = True`; for the
connector marked UART, use `False`. Copy the Docker context so the choice does
not affect Git or the emulator SDK:

```sh
cp -R src/platforms/rtems/docker "$HW_WORK/docker"
export USE_USB_CONSOLE=True          # False for the UART connector
"$HW_PYTHON" - "$HW_WORK/docker/${CHIP}-wifi-bsp.ini" "$USE_USB_CONSOLE" <<'PY'
from pathlib import Path
import sys
p = Path(sys.argv[1])
assert sys.argv[2] in ('True', 'False')
text = p.read_text()
assert 'ESPRESSIF_USE_USB_CONSOLE = False' in text
p.write_text(text.replace('ESPRESSIF_USE_USB_CONSOLE = False',
                          'ESPRESSIF_USE_USB_CONSOLE = ' + sys.argv[2]))
PY
docker build -t "$HW_IMAGE" \
    -f "$HW_WORK/docker/Dockerfile.rtems7-esp32" "$HW_WORK/docker"
```

Despite its name, this Dockerfile builds all four ESP BSPs and their Wi-Fi stacks
using the source pins and patches in this checkout. Docker caches the toolchain
layers. Recopy the context and rebuild after changing SDK patches or source pins.

For a UART bridge, use the board's UART0 console connection at 115200 baud.
The C5 DevKit V2.0 retest used its UART-marked connector and WCH bridge
(`/dev/cu.wchusbserial110` on this Mac); the native USB connection disappeared
during boot. A native SDK is also
usable; follow the [RTEMS build guide](src/platforms/rtems/README.md) and rebuild
the BSP with the appropriate console setting. A console change requires a BSP
rebuild, not just an AtomVM rebuild. Preserve the Wi-Fi stack's linker placement
if reinstalling a BSP over an existing SDK.

## 3. Create the hardware smoke application

Save the following as `$HW_WORK/hw_test.erl`. It tests basic timing, RTC storage
and crypto, then accepts credentials over a raw serial connection. It does not
enter sleep or run the destructive flash/NVS tests.

```erlang
-module(hw_test).
-export([start/0]).

start() ->
    erlang:display({hardware_boot, atomvm:platform(),
                    erlang:system_info(esp32_chip_info)}),
    T0 = erlang:monotonic_time(millisecond),
    C0 = atomvm_rtems:get_rtc_counter(),
    timer:sleep(1000),
    true = erlang:monotonic_time(millisecond) - T0 >= 1000,
    true = atomvm_rtems:get_rtc_counter() > C0,
    ok = esp:rtc_slow_set_binary(<<"hardware RTC">>),
    <<"hardware RTC">> = esp:rtc_slow_get_binary(),
    R0 = crypto:strong_rand_bytes(16),
    R1 = crypto:strong_rand_bytes(16),
    true = byte_size(R0) =:= 16 andalso R0 =/= R1,
    32 = byte_size(crypto:hash(sha256, <<"hardware">>)),
    erlang:display({basic_hardware, ok}),
    {ok, Console} = uart:init([{peripheral, "/dev/console"},
                               {speed, 115200}, {raw, true}]),
    erlang:display({wifi_config, ready}),
    <<SL, PL>> = read_exact(Console, 2, <<>>),
    true = SL > 0 andalso SL =< 32 andalso PL =< 63,
    Payload = read_exact(Console, SL + PL, <<>>),
    <<SSID:SL/binary, PSK:PL/binary>> = Payload,
    ok = uart:deinit(Console),
    {ok, _} = network:start([{sta, [{managed, true}]}]),
    {ok, {Active, _}} = network:wifi_scan([{results, 8}, {dwell, 120}]),
    erlang:display({wifi_scan, active, Active}),
    erlang:garbage_collect(),
    {ok, {Passive, _}} = network:wifi_scan(
        [{results, 8}, {passive, true}, {dwell, 120}]),
    erlang:display({wifi_scan, passive, Passive}),
    true = Active > 0 orelse Passive > 0,
    erlang:garbage_collect(),
    connect(SSID, PSK),
    udp_loopback(),
    {ok, _} = inet:getaddr("example.com", inet),
    erlang:display({dns, ok}),
    {ok, Sock} = gen_tcp:connect("example.com", 80,
        [binary, {active, false}, {inet_backend, socket}]),
    ok = gen_tcp:send(Sock,
        <<"GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n">>),
    {ok, <<"HTTP/", _/binary>>} = gen_tcp:recv(Sock, 0, 10000),
    ok = gen_tcp:close(Sock),
    erlang:display({tcp_http, ok}),
    ok = network:stop(),
    {ok, _} = network:start([{sta, [{managed, true}]}]),
    connect(SSID, PSK),
    ok = network:stop(),
    erlang:display({wifi_restart, ok}),
    ok.

read_exact(_Console, 0, Acc) -> Acc;
read_exact(Console, N, Acc) ->
    {ok, Data} = uart:read(Console, N, 60000),
    read_exact(Console, N - byte_size(Data), <<Acc/binary, Data/binary>>).

connect(SSID, PSK) ->
    ok = network:sta_connect([{ssid, SSID}, {psk, PSK}]),
    ok = atomvm_rtems:wait_wifi(30000),
    {ok, RSSI} = network:sta_rssi(),
    true = is_integer(RSSI) andalso RSSI >= -127 andalso RSSI =< 0,
    erlang:display({wifi_dhcp_rssi, RSSI, ok}).

udp_loopback() ->
    {ok, RX} = socket:open(inet, dgram, udp),
    ok = socket:bind(RX, #{family => inet, addr => {127,0,0,1}, port => 9091}),
    {ok, TX} = socket:open(inet, dgram, udp),
    ok = socket:sendto(TX, <<"hardware">>,
        #{family => inet, addr => {127,0,0,1}, port => 9091}),
    {ok, {_, <<"hardware">>}} = socket:recvfrom(RX, 0, 5000),
    ok = socket:close(TX),
    ok = socket:close(RX),
    erlang:display({udp_loopback, ok}).
```

The HTTP check needs Internet access. For an isolated lab, replace the hostname,
port and Host header with a reachable HTTP server, and test DNS separately
against a name served by the lab resolver. UDP loopback checks the socket path;
the HTTP request checks traffic through the AP.

The existing `test_esp32*_wifi` suites assume the emulator AP, fixed RSSI and
emulator network addresses. Use the application above on a real network.

## ESP32-C3/C5 Bluetooth/Wi-Fi coexistence

Use the minimal BLE test after selecting C3 or C5 and rebuilding its hardware SDK
from the current patches:

```sh
export TEST_MODULE=test_esp32c3_ble
# For C5, use TEST_MODULE=test_esp32c5_ble instead.
export HW_RTEMS_BT=ON
cp "$REPO_ROOT/src/platforms/rtems/tests/test_erl_sources/$TEST_MODULE.erl" \
    "$HW_WORK/$TEST_MODULE.erl"
```

Follow the build/flash steps below and supply Wi-Fi credentials using the same
private JSON/UART protocol. The test starts a non-connectable `AtomVM-BLE`
advertiser before Wi-Fi, joins the AP, runs HTTP requests, and stops/restarts
Wi-Fi while Bluetooth remains enabled. It requires Internet access to
`example.com`. Keep a BLE scanner running across both Wi-Fi phases and verify
advertisements after the restart. `Return value: ok` checks the application and
HCI responses; the receiver check is needed to establish over-the-air behavior.
The `{bt_vm_progress,ok}` marker also verifies that another Erlang process runs
while the native controller initializes. `bt_start/0` yields until HCI setup
completes; `bt_start_async/0` and `bt_status/0` expose the same operation without
waiting.
Manufacturer data with test company ID `0xffff` contains a little-endian
32-bit counter incremented after each controller-confirmed advertising event.
The controller is enabled for one event at a time, with about 1.28 seconds
between events. The three advertising-channel transmissions within one event
share a counter; different events use different counters. Check that received
counters continue increasing throughout the test, including after Wi-Fi restarts.
Connections, pairing and GATT are not implemented by this controller smoke test.

The C5 SDK also needs RTEMS patch `0034-esp32c5-route-bluetooth-interrupts.patch`
and Wi-Fi glue patch `0018-rtems-esp-wifi-c5-bluetooth.patch`. Rebuild an older
SDK: the C5's shared C6 interrupt table lacks the controller's BT_MAC and
LP_TIMER routes, and its Bluetooth security and timer clocks need enabling.
The C5 test runs each associated Wi-Fi phase for 60 seconds.

To compare experimental Wi-Fi modem power saving, export
`HW_WIFI_POWER_SAVE=MIN_MODEM` or `MAX_MODEM` before building; the default is
`NONE`. For maximum mode, `export HW_WIFI_LISTEN_INTERVAL=10` uses ten AP
beacon intervals between wakeups instead of the default three. Compare
received counters and HTTP traffic across the three modes.

For native stack measurements, add `-DCONFIGURE_STACK_CHECKER_ENABLED` to the
firmware's C flags and call `rtems_stack_checker_report_usage()` from a native
task after exercising both radios. A C3 diagnostic run measured 660 bytes used
by the 4 KiB controller task and 1584 bytes by the 4 KiB initialization/HCI task.
These are observed high-water marks, not bounds for other controller features.
ESP-IDF's FreeRTOS task stack argument is in bytes too; do not multiply it by
the native FreeRTOS word size.

## 4. Build and flash

Build the host libraries once:

```sh
cmake -S "$REPO_ROOT" -B "$HW_WORK/host" -G Ninja -DAVM_DISABLE_JIT=ON
cmake --build "$HW_WORK/host" -t atomvmlib-rtems
mkdir -p "$HW_WORK/beam"
export TEST_MODULE="${TEST_MODULE:-hw_test}"
```

Use this block for each test module. Keep the source at
`$HW_WORK/$TEST_MODULE.erl`; outputs use separate directories per module.
Pruning the pack reduces RAM pressure, especially on C3.

```sh
erlc +debug_info -o "$HW_WORK/beam" "$HW_WORK/$TEST_MODULE.erl"
"$HW_WORK/host/tools/packbeam/packbeam" create -p -r -s "$TEST_MODULE" \
    "$HW_WORK/$TEST_MODULE.avm" "$HW_WORK/beam/$TEST_MODULE.beam" \
    "$HW_WORK/host/libs/atomvmlib-rtems.avm"
docker run --rm -e CHIP -e TEST_MODULE -e HW_RTEMS_BT -e HW_WIFI_POWER_SAVE \
    -e HW_WIFI_LISTEN_INTERVAL \
    -v "$REPO_ROOT:/workspace:ro" -v "$HW_WORK:/hw" "$HW_IMAGE" bash -euc '
    cmake -S /workspace/src/platforms/rtems -B "/hw/$TEST_MODULE" -G Ninja \
        -DCMAKE_TOOLCHAIN_FILE=/workspace/src/platforms/rtems/cmake/rtems-toolchain.cmake \
        -DRTEMS_PREFIX=/opt/rtems/7 -DRTEMS_VERSION=7 \
        -DRTEMS_BSP="riscv/${CHIP}db" -DRTEMS_BOARD="$CHIP" \
        -DRTEMS_SLEEP_TEST_FIXTURE=OFF -DRTEMS_BT="${HW_RTEMS_BT:-OFF}" \
        -DRTEMS_WIFI_POWER_SAVE="${HW_WIFI_POWER_SAVE:-NONE}" \
        -DRTEMS_WIFI_LISTEN_INTERVAL="${HW_WIFI_LISTEN_INTERVAL:-3}" \
        -DAVM_PACK="/hw/$TEST_MODULE.avm"
    cmake --build "/hw/$TEST_MODULE" -j4
    '
export HW_BIN="$HW_WORK/$TEST_MODULE/AtomVM-${CHIP}db.bin"
"$HW_PYTHON" -m esptool --chip "$CHIP" --port "$HW_PORT" --baud 460800 \
    --after no-reset write-flash 0x0 "$HW_BIN"
```

Flash the CMake-produced raw `.bin` at **offset 0**. It has the RTEMS direct-boot
header already; do not run `elf2image` or add an ESP-IDF bootloader/partition
table. Flash on the host, where the serial device is available. Drop to 115200
baud if transfers are unreliable. Keep `RTEMS_SLEEP_TEST_FIXTURE=OFF` for every
physical test: the fixture replaces hardware behavior for CI.

## 5. Enter credentials privately and capture the result

Create a local file, then edit its values in your editor:

```sh
printf '%s\n' '{"ssid": "", "password": ""}' > "$HW_WORK/wifi.json"
chmod 600 "$HW_WORK/wifi.json"
```

Use a 1–32 byte SSID and an 8–63 byte WPA passphrase, or an empty password for
an open AP. Credentials are sent at runtime, not embedded in source or firmware.
Keep the workspace, flash backup and logs outside Git. Review logs before sharing:
redacting credentials does not remove every network identifier or crash detail.

Save this monitor as `$HW_WORK/monitor.py`:

```python
import json
from pathlib import Path
import sys
import time
import serial

port_name, config_path, log_path = sys.argv[1:]
config = json.loads(Path(config_path).read_text())
ssid = config['ssid'].encode()
password = config['password'].encode()
assert 1 <= len(ssid) <= 32
assert not password or 8 <= len(password) <= 63
packet = bytes((len(ssid), len(password))) + ssid + password
port = serial.Serial(port=None, baudrate=115200, timeout=0.2)
port.port = port_name
port.dtr = False
port.rts = False
port.open()
buffer = b''
sent = False
result = 2
print('Monitor open. Release BOOT and press RESET to start the test.', flush=True)
try:
    deadline = time.monotonic() + 180
    with Path(log_path).open('w') as log:
        while time.monotonic() < deadline:
            try:
                buffer += port.read(port.in_waiting or 1)
            except (OSError, serial.SerialException):
                port.close()
                time.sleep(0.3)
                try:
                    port.open()
                except (OSError, serial.SerialException):
                    pass
                continue
            while b'\n' in buffer:
                line, buffer = buffer.split(b'\n', 1)
                safe = line
                if password:
                    safe = safe.replace(password, b'<password>')
                safe = safe.replace(ssid, b'<ssid>')
                text = safe.decode('utf-8', errors='replace').rstrip('\r')
                print(text, flush=True)
                log.write(text + '\n')
                log.flush()
                if b'{wifi_config,ready}' in line and not sent:
                    port.write(packet)
                    port.flush()
                    sent = True
                if b'Return value:' in line:
                    result = 0 if b'Return value: ok' in line else 1
                    break
            if result != 2:
                break
finally:
    port.close()
print(f'Monitor result: {result} (0=pass, 1=failure, 2=timeout)')
sys.exit(result)
```

```sh
"$HW_PYTHON" "$HW_WORK/monitor.py" "$HW_PORT" "$HW_WORK/wifi.json" \
    "$HW_WORK/hardware-run.log"
```

Press RESET after the monitor opens, with BOOT released. A pass includes
`{basic_hardware,ok}`, scan counts, two DHCP/RSSI checks, UDP/DNS/HTTP success,
`{wifi_restart,ok}` and finally **`Return value: ok`**. Do not treat successful
PHY initialization or an open socket alone as a Wi-Fi pass. A shutdown/fatal
message after the completed application's successful return can be normal RTEMS
exit behavior; a crash before that result is a failure.

If the port disappears on reset, select its new name and rerun the monitor.
If scans find zero APs, compare active/passive scans against an ESP-IDF scan-only
image on the same board, antenna and AP placement. Back up first and restore the
RTEMS image afterward. Record both logs rather than assuming an antenna problem.

## 6. Peripheral and sleep tests

Use separate images so sleep failures do not hide the network result. To run the
existing peripheral test, copy it into the workspace, set the module, then repeat
the build/flash block in section 4:

```sh
cp src/platforms/rtems/tests/test_erl_sources/test_peripherals.erl "$HW_WORK/"
export TEST_MODULE=test_peripherals
```

Read its pin assignments before flashing: PWM drives GPIO3; ADC uses GPIO4 on
C3, GPIO6 on C5/C6 and GPIO49 on S31. Confirm these pins are accessible and free
on your module. Use a 115200 serial terminal with DTR/RTS reset control disabled,
capture the complete log and press RESET. This test does not request credentials.
It proceeds from RTC/PWM/ADC checks into light sleep.

- **ADC:** A floating-pin sample only proves conversion completed. The test also
  permits `{error, timeout}`, so inspect the actual result. For hardware acceptance,
  apply several known voltages within the chip's rated ADC input range, share
  ground and compare raw/voltage results with a meter. C3/C5/C6 voltage conversion
  lacks eFuse calibration; S31 voltage is `undefined`.
- **PWM:** `get_freq` and `get_duty` readback do not prove an output waveform.
  Measure GPIO3 with a scope or logic analyzer. The test uses 1 kHz at 12-bit
  resolution, briefly sets full duty, then 512/4096 duty (12.5%) and stops.
  Extend the hold time in the private test copy for manual measurements.
- **Light sleep:** Require timer wakeup, advancing monotonic time, delivery of an
  Erlang timer that expired while asleep, RTC data retention and scheduler/worker
  resumption. Current C3/C5 runs fail the expired-timer check (`timer_lost`) and
  USB console continuity is unreliable. A timeout or lost USB log is inconclusive
  about where execution stopped. Use UART or store a short result in RTC RAM and
  read it with JTAG, as in our isolated diagnostics.
- **Deep sleep:** Copy
  [test_deep_sleep.erl](src/platforms/rtems/tests/test_erl_sources/test_deep_sleep.erl)
  into the workspace, set `TEST_MODULE=test_deep_sleep`, rebuild and flash.
  Expect `{deep_sleep,enter}`, a reset after the 100 ms sleep, then
  `{deep_sleep_retention,...,ok}` and `Return value: ok`. Verify reset/wakeup
  reasons and RTC retention; returning from `esp:deep_sleep/1` is a failure.
- **GPIO wake and power:** Test separately with a supported wake pin configured
  as input and a defined idle level. Measure sleep current without an active
  debugger and account for USB, LEDs and board regulators. JTAG access or a timer
  wakeup alone does not establish a low-power transition. On S31, also verify both
  schedulers resume and exchange messages.

The general [test_boot.erl](src/platforms/rtems/tests/test_erl_sources/test_boot.erl)
is a broader, destructive test: it enters sleep, drives GPIOs, erases flash at
`0x3c0000`, reformats NVS and writes `/flash`. Run it only with a complete backup
and the default flash layout. For storage acceptance, additionally check that
written values/files survive reboot and power cycling.

## 7. Record results and restore the board

Keep a short report containing the source revision and local diff, SDK pins and
console setting, board/module and chip revision, crystal/flash size, antenna,
test image hash, wiring/instruments, test names, serial logs and measured values.
Mark each check pass, fail or untested. Keep emulator results separate from
physical measurements and distinguish register readback from measured behavior.

Close the monitor before flashing a known working test image again. To restore
the original firmware, recheck that the connected board matches the backup:

```sh
"$HW_PYTHON" -m esptool --chip "$CHIP" --port "$HW_PORT" --baud 460800 \
    write-flash 0x0 "$HW_WORK/original-flash.bin"
"$HW_PYTHON" -m esptool --chip "$CHIP" --port "$HW_PORT" \
    verify-flash 0x0 "$HW_WORK/original-flash.bin"
```

Release BOOT, reset and confirm the original application starts. Retain the
backup/report securely; remove the temporary credential file when finished.

## GRiSP 2

Use the [SD-card installation guide](src/platforms/rtems/boards/grisp2/INSTALL.md)
and the [hardware smoke-test instructions](src/platforms/rtems/README.md#grisp-2-hardware).
Back up the SD card and preserve the existing bootloader entry. Check boot/FDT,
serial console, LEDs, GPIO/interrupts, EEPROM reads, I2C/SPI and Ethernet DHCP,
DNS and traffic. Test valid `app.avm` replacement, missing application and
truncated application fallback. Record physical results separately; GRiSP 2
hardware validation is still pending.
