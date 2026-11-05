# ESP32-S3 Zephyr Wi-Fi crashes when IPv6 is enabled

## Summary

On ESP32-S3, Zephyr 4.5 Wi-Fi starts and passes an AP-only test, but crashes
once IPv6 is enabled and the interface carries traffic. The failure is not in
AtomVM's network driver: it happens in Zephyr's Wi-Fi/IPv6 path before the
AtomVM Wi-Fi callback runs. Disabling IPv6 for the S3 test image makes the
entire AtomVM Wi-Fi suite pass.

The failing configurations reproduce on:

- Wokwi CLI 0.29.1 with the `board-esp32-s3-devkitc-1` model.
- Espressif `esp-emu` 0.51.0.

C3 and classic ESP32 Wi-Fi pass with IPv6 enabled.

## Environment

- Zephyr **4.5.0** (`41d10b597304`), built from the Zephyr 4.5 CI image.
- `hal_espressif` from Zephyr 4.5.
- Wokwi CLI **0.29.1**.
- Espressif `esp-emulator` **0.51.0**, run on the same Zephyr firmware.
- AtomVM Zephyr test image for
  `esp32s3_devkitc/esp32s3/procpu`.
- Local Zephyr source inspected at
  `/Users/petermm/OSScontrib/zephyr` (`4.5.0-1672-g072fd9442fe`).

## Symptoms

With the default Zephyr IPv6 configuration:

- `test_wifi_ap` passes: the S3 softAP starts.
- `test_wifi_example` starts AP+STA, connects the station, acquires an IPv4
  address, and then crashes while processing received traffic.
- `test_wifi_managed` / `test_wifi_scan` fail or crash depending on timing.
- `test_wifi_z_devmode` crashes during AP startup.

Typical failures:

```text
E: ** FATAL EXCEPTION
E: ** CPU 0 EXCCAUSE 0 (illegal instruction)
E: ** PC 0x60a20 VADDR 0
E: >>> ZEPHYR FATAL ERROR 0: CPU exception on CPU 0
E: Current thread: ... (rx_q[0])
```

```text
E: ** FATAL EXCEPTION
E: ** CPU 0 EXCCAUSE 0 (illegal instruction)
E: ** PC 0xc32e5765 VADDR 0
E: >>> ZEPHYR FATAL ERROR 0: CPU exception on CPU 0
E: Current thread: ... (esp32_wifi_event_tid)
```

The `0xc32e5765` PC is not a valid code address; the exception backtrace is
reported as `CORRUPTED`. That indicates corrupted control flow rather than a
normal fault at a symbol.

The same `esp32_wifi_event_tid` / `0xc32e5765` failure occurs with
`esp-emu` on a fresh boot, so this is not Wokwi-specific.

## Reproduction

The current reproducer is the AtomVM Zephyr test image with the S3 Wi-Fi tests
enabled and IPv6 left at Zephyr's default:

```sh
west build -b esp32s3_devkitc/esp32s3/procpu \
  -d build-wokwi-test -p=always \
  src/platforms/zephyr/tests
```

Then stage `build-wokwi-test/zephyr/zephyr.bin` and `zephyr.elf` with the S3
`diagram.esp32s3.json` and run Wokwi:

```sh
wokwi-cli . --timeout 180000 \
  --expect-text "PROJECT EXECUTION SUCCESSFUL" \
  --fail-text "PROJECT EXECUTION FAILED"
```

The same image can be run on the Espressif emulator:

```sh
esp-emu --chip esp32s3 \
  --firmware build-wokwi-test/zephyr/zephyr.bin \
  --elf build-wokwi-test/zephyr/zephyr.elf \
  --net user --timeout 30s
```

`test_devmode` alone also crashes on a fresh boot, so this is not a
leftover-state or teardown issue.

## Results matrix

| S3 configuration | Result |
| --- | --- |
| Zephyr default IPv6 | `test_wifi_ap` passes; `test_wifi_example` crashes in `rx_q[0]`; devmode crashes in `esp32_wifi_event_tid` |
| `NET_IF_IPV6_NO_ND` on AP and STA | devmode passes; `test_wifi_example` still crashes in `rx_q[0]` |
| `CONFIG_NET_IPV6_MLD=n` | `test_wifi_example` still crashes in `rx_q[0]` |
| `CONFIG_NET_IPV6_MLD=n` + `CONFIG_NET_IPV6_DAD=n` | `test_wifi_example` still crashes in `rx_q[0]` |
| `CONFIG_NET_CONNECTION_MANAGER=n` | `test_wifi_example` still crashes in `rx_q[0]` |
| `CONFIG_ESP32_WIFI_AMPDU_RX_ENABLED=n` | `test_wifi_example` still crashes in `rx_q[0]` |
| Larger Wi-Fi event/RX/net-mgmt stacks | No change |
| `CONFIG_NET_IPV6=n` | Full S3 suite passes: 26/26 |

The full S3 suite with IPv6 disabled runs:

- `test_wifi_ap` — pass
- `test_wifi_example` — pass (AP+STA, SNTP, DNS)
- `test_wifi_managed` — pass
- `test_wifi_scan` — pass
- `test_wifi_z_devmode` — pass

The final boot reports 21 passes and 5 skips because the deep-sleep test
re-runs the suite and the Wi-Fi tests are intentionally skipped on the
post-DSLEEP boot.

## Why `NET_EVENT_IPV6_ADDR_ADD` is raised

`NET_EVENT_IPV6_ADDR_ADD` is not an AtomVM setting and it is not a standalone
Kconfig option. IPv6 is enabled by default in Zephyr:

- `subsys/net/ip/Kconfig.ipv6`: `menuconfig NET_IPV6`, `default y`.

When an IPv6 address is added, `subsys/net/ip/net_if.c` raises the event:

```c
net_mgmt_event_notify_with_info(
    NET_EVENT_IPV6_ADDR_ADD, iface,
    &ipv6->unicast[i].address.in6_addr,
    sizeof(struct net_in6_addr));
```

The event is handled because Zephyr's connection manager registers a static
IPv6 handler:

- `subsys/net/conn_mgr/events_handler.c`:
  `conn_mgr_ipv6_events_handler`, registered with
  `CONN_MGR_IPV6_EVENTS_MASK`.
- `CONN_MGR_IPV6_EVENTS_MASK` includes `NET_EVENT_IPV6_ADDR_ADD`,
  `NET_EVENT_IPV6_ADDR_DEL`, `NET_EVENT_IPV6_DAD_SUCCEED`, and
  `NET_EVENT_IPV6_DAD_FAILED`.
- `CONFIG_NET_CONNECTION_MANAGER` selects `NET_MGMT_EVENT_INFO`.

Disabling MLD or DAD changes which IPv6 packets are sent, but it does not
remove IPv6 from the interface and does not fix the S3 RX crash. Only
disabling the IPv6 stack itself avoids it.

## Is it already fixed in the local Zephyr tree?

The local checkout is `v4.5.0-1672-g072fd9442fe`. It contains no commits after
v4.5.0 touching `drivers/wifi/esp32/src/esp_wifi_drv.c`:

```sh
git log --oneline v4.5.0..HEAD -- drivers/wifi/esp32/src/esp_wifi_drv.c
# no output
```

The post-v4.5.0 `subsys/net/ip/net_if.c` changes are unrelated (link-address
assert removal, loopback fragmentation, and similar). The S3 RX path and the
IPv6 address-add event path are still present at these locations:

- `drivers/wifi/esp32/src/esp_wifi_drv.c:388` — `eth_esp32_rx()`
- `drivers/wifi/esp32/src/esp_wifi_drv.c:442` — `wifi_esp32_ap_iface_rx()`
- `subsys/net/ip/net_if.c:2570` — `NET_EVENT_IPV6_ADDR_ADD` notification from
  `net_if_ipv6_addr_add()`
- `subsys/net/ip/net_if.c:4233` — `iface_ipv6_start()`
- `subsys/net/conn_mgr/events_handler.c:56` — `conn_mgr_ipv6_events_handler()`

So this is not already fixed in the local Zephyr HEAD. A candidate fix has to
land in one of the paths above, or the S3 test image must remain IPv4-only.

## Comparison with C3 and classic ESP32

The AtomVM board configurations for all three targets are largely identical:

- `CONFIG_WIFI_USAGE_MODE_STA_AP=y`
- `CONFIG_WIFI_NM=y`
- `CONFIG_NET_IF_MAX_IPV4_COUNT=2`
- `CONFIG_NET_IF_MAX_IPV6_COUNT=2`
- `CONFIG_NET_DHCPV4_SERVER=y`
- the same Wi-Fi AP/STA and DHCP server settings

The Zephyr ESP32 Wi-Fi driver also has no S3-specific RX path. The only
architecture-specific blocks in `esp_wifi_drv.c` are for ESP32-S2/C3 ADC
calibration; the AP/STA RX callbacks and buffer settings are shared.

The notable differences are:

| Setting / property | C3 | Classic ESP32 | S3 |
| --- | --- | --- | --- |
| CPU | RISC-V | Xtensa | Xtensa |
| Wi-Fi AP/STA config | same | same | same |
| `CONFIG_NET_CONNECTION_MANAGER` | default | test image sets `n` | default |
| IPv6 | default enabled | default enabled | test image currently `n` |
| A-MPDU RX | default | default | default |

I tested the plausible S3-specific/different settings with IPv6 enabled:

- `CONFIG_NET_CONNECTION_MANAGER=n` (matching the classic test image):
  `test_wifi_example` still crashes in `rx_q[0]`.
- `CONFIG_ESP32_WIFI_AMPDU_RX_ENABLED=n`:
  `test_wifi_example` still crashes in `rx_q[0]`.

So neither the connection-manager difference nor A-MPDU RX is sufficient.
The local Zephyr HEAD also has no post-4.5.0 changes to the ESP32 Wi-Fi
driver. The remaining difference is the S3 Wi-Fi blob/HAL/emulator behavior,
not an S3-specific branch in the shared driver source.

## Root-cause assessment

The failure is below AtomVM:

- `test_devmode` crashes before any previous Wi-Fi state exists.
- `test_wifi_ap` passes, so AP bring-up by itself is not fatal.
- The failing AP+STA example gets through AP/STA startup, IPv4 address
  acquisition, and SNTP before crashing in the network RX thread.
- The crash thread is `rx_q[0]` or `esp32_wifi_event_tid`, not the AtomVM
  test thread.
- The invalid PC and `CORRUPTED` backtrace indicate memory/control-flow
  corruption, so the exception does not point at the responsible function.
- Disabling IPv6, which removes IPv6 ND/MLD frames and IPv6 RX processing,
  makes the entire suite pass.

This is consistent with a Zephyr ESP32-S3 Wi-Fi RX/IPv6 bug, or an S3 Wi-Fi
model/HAL limitation in the emulators. It is not fixed by larger stacks.

## Potential fixes

### 1. Zephyr `net_if`: notify after releasing the interface lock — candidate

`net_if_ipv6_addr_add()` raises `NET_EVENT_IPV6_ADDR_ADD` while holding the
interface lock. With direct or same-thread event dispatch, a callback such as
`conn_mgr_ipv6_events_handler()` can call `net_if_ipv6_get_global_addr()`,
which tries to lock interfaces again. Even with the queued event thread this
is unnecessarily fragile.

A candidate fix is to copy the address, release `net_if_lock()`, and then
raise the event:

```c
struct net_in6_addr added_addr;
bool notify = false;

/* inside the address slot setup */
net_ipaddr_copy(&added_addr, addr);
notify = true;

/* after net_if_unlock(iface) */
if (notify) {
    net_mgmt_event_notify_with_info(
        NET_EVENT_IPV6_ADDR_ADD, iface,
        &added_addr, sizeof(added_addr));
}
```

This is a defensible Zephyr cleanup and removes the callback-under-lock
hazard, but it is not proven to fix the `rx_q[0]` corruption. The AP/devmode
path is the part where this is most relevant.

### 2. Zephyr ESP32 Wi-Fi RX path

The remaining AP+STA crash is in the Wi-Fi/network receive thread. The next
investigation should focus on:

- `drivers/wifi/esp32/src/esp_wifi_drv.c`
  - `eth_esp32_rx()` at line 388
  - `wifi_esp32_ap_iface_rx()` at line 442
  - `net_pkt_rx_alloc_with_buffer()` / `net_pkt_write()` /
    `net_recv_data()` for IPv6 frames.
- Whether the S3 HAL reports IPv6 frame lengths or RX descriptors
  differently from C3/classic ESP32.
- Whether AP and STA share an RX callback incorrectly in AP+STA mode.
- Whether the S3 Wi-Fi blob/model can handle IPv6 multicast (ND/MLD) frames
  at all.

A minimal Zephyr Wi-Fi + IPv6 sample, separate from AtomVM, should be built
next. The AtomVM test image is useful for confirming the regression, but a
standalone sample will isolate whether the bug is in the Zephyr driver, HAL,
or emulator.

### 3. AtomVM test-image workaround

The S3 test image currently sets:

```conf
CONFIG_NET_IPV6=n
```

in
`src/platforms/zephyr/tests/boards/esp32s3_devkitc_esp32s3_procpu.conf`.

That is a **test-only** workaround. It restores S3 Wi-Fi coverage and keeps
C3/classic ESP32 IPv6 coverage. It does not make IPv6 work on S3. An AtomVM
runtime fix is not appropriate here because the crash occurs in Zephyr's
Wi-Fi RX/event path, not in AtomVM code.

## Related AtomVM change

During this investigation, `network_driver_destroy()` was also found not to
call `stop_network()` when a VM exited without `network:stop()`. That is a
separate lifecycle bug and is fixed by calling `stop_network()` from the
Zephyr network port destroy callback. It does not fix the S3 IPv6 crash.

## References

- Zephyr 4.5.0: <https://github.com/zephyrproject-rtos/zephyr/releases/tag/v4.5.0>
- Zephyr IPv6 Kconfig:
  `subsys/net/ip/Kconfig.ipv6`
- Zephyr IPv6 address event:
  `subsys/net/ip/net_if.c`, `net_if_ipv6_addr_add()`
- Zephyr conn_mgr IPv6 handler:
  `subsys/net/conn_mgr/events_handler.c`
- Zephyr ESP32 Wi-Fi driver:
  `drivers/wifi/esp32/src/esp_wifi_drv.c`
- Espressif esp-emulator:
  <https://github.com/espressif/esp-emulator>
- Wokwi ESP32 guide:
  <https://docs.wokwi.com/guides/esp32>
- AtomVM failing S3 CI job:
  <https://github.com/petermm/AtomVM/actions/runs/41864563791/job/126726694341>
- Local evidence logs (workspace scratch):
  `.amp/in/s3-wifi-run.log`, `.amp/in/s3-ipv6-nomld-run.log`,
  `.amp/in/s3-ipv6-nomld-nodad-run.log`, `.amp/in/s3-esp-emu-devmode-only.log`,
  `.amp/in/s3-noipv6-devmode-run.log`, `.amp/in/s3-final-run.log`
