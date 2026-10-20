<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM on RTEMS

This directory contains the RTEMS port of AtomVM, supporting RTEMS 6.2 as well as RTEMS 7.0 (`main`).

Remaining work and physical validation are tracked in the
[RTEMS TODOs](../../../TODOS.md).
For board setup, flash backups, private Wi-Fi configuration and physical test
procedures, see [hardware testing](../../../HW_TEST.md).

Supported bring-up targets:

- SPARC `erc32` on SIS (`rtems-run --rtems-bsps=erc32-sis`)
- ARM `realview_pbx_a9_qemu` on QEMU (`qemu-system-arm -M realview-pbx-a9 -smp 2`)
- ARM `imx7` on QEMU (`qemu-system-arm -M mcimx7d-sabre -smp 2`)
- GRiSP 2 (`arm/imx7` with `RTEMS_BOARD=grisp2`; hardware validation pending)
- RISC-V `esp32c3db` on ESP32-C3 (`esp-emu --chip esp32c3` or QEMU `qemu-system-riscv32 -M esp32c3`)
- RISC-V `esp32c5db` on ESP32-C5 (`esp-emu --chip esp32c5`)
- RISC-V `esp32c6db` on ESP32-C6 (`esp-emu --chip esp32c6`)
- RISC-V `esp32s31db` on ESP32-S31 dual-core SMP (`esp-emu --chip esp32s31`)

Other RTEMS 6 and 7 BSPs can be selected at configure time once a matching toolchain
and BSP are installed. `imx7` is the NXP i.MX 7Dual BSP; QEMU models the
SABRE board, not GRiSP 2 (same BSP family, different pinmux, PHY, and FDT).
The GRiSP 2 build selects the board-specific LibBSD nexus and checks that the
bootloader supplied an FDT with the `embeddedbrains,grisp2` compatible string.

SMP multi-core BEAM execution is supported on dual-core and multi-core targets
(`esp32s31db`, `realview_pbx_a9_qemu`, and `imx7`), dynamically scaling
schedulers with `rtems_scheduler_get_processor_maximum()`. Uniprocessor targets
(`erc32`, `esp32c3db`, `esp32c5db`, `esp32c6db`, `grisp2`) build with uniprocessor
optimizations (`AVM_DISABLE_SMP=ON`). Applications are compiled on the host
and embedded in the RTEMS executable. GRiSP 2 can instead load `app.avm`
from its SD card, using the embedded application as a fallback.
Pack `atomvmlib-rtems.avm` (estdlib + eavmlib + `avm_network` +
`avm_rtems`) together with the application so `init:boot/1` can start the
entry module. JIT is currently disabled.

ESP32 builds default to `MinSizeRel` (`-Os -DNDEBUG`) and use function/data
sections with linker garbage collection. Explicit build types, such as
`-DCMAKE_BUILD_TYPE=Debug`, are respected. Other BSP defaults are unchanged.
`AVM_MINIMAL_OPCODES` remains off: existing BEAM files can require the excluded
opcodes. Enable it only with a compatible application and library pack.
The BSP's fixed flash code/data split can leave padding in the `.bin` even
when the linked code becomes much smaller. TLS and 64-bit floats remain enabled.

Sockets are provided on `arm/imx7` when `rtems-libbsd` is installed. Without
LibBSD, and on SPARC `erc32` and ARM `realview_pbx_a9_qemu`,
`atomvm_rtems:wait_dhcp/1` returns `{error, enotsup}`.

UART implements `uart_hal` over RTEMS Termios (`/dev/console`, erc32
`/dev/console_a` / `/dev/console_b`, imx7 `/dev/ttyS0` … `/dev/ttyS6`).
I2C implements `i2c_hal` with Linux-style `I2C_RDWR` on imx7
(`/dev/i2c-0`, registered from FDT alias `i2c0` on first open). Supports
both 7-bit (`0..127`) and 10-bit (`128..1023`, `I2C_M_TEN`) addressing,
bus probing (`is_device_ready/2,3,4`), bus scanning (`scan/1,3`), atomic
multi-message transfers (`transfer/2,3`), configurable bus retries
(`{retries, N}`), and explicit register address width (`{mem_add_size, 8 | 16}`).
SPARC erc32 and ARM RealView have no supported I2C controller; `i2c:init/1` returns
`{error, enotsup}`.
SPI implements `spi_hal` with Linux-style `SPI_IOC_MESSAGE` on imx7
(`/dev/spi-0`, registered from FDT alias `spi0` on first open). SPARC erc32
and ARM RealView have no supported SPI controller; `spi:init/1` returns
`{error, enotsup}`.
GPIO implements `gpio_hal` using the RTEMS i.MX GPIO driver on imx7. Pins can
be addressed as `{Bank, Pin}` (`1..7`, `0..31`) or through an FDT property.
Supports configurable pull resistors (`up`, `down`, `floating`), open-drain mode
(`output_od`), hardware edge/level interrupts delivering `{gpio_interrupt, Pin}`
notifications, and dynamic pinmuxing (`set_pin_mux/2`). GPIO on erc32 and
RealView returns `{error, enotsup}`.

ESP32-C3/C5/C6/S31 support station RSSI and active/passive scanning through
`network:sta_rssi/0` and `network:wifi_scan/0,1`. Managed station mode permits
scanning before selecting an access point:

```erlang
{ok, _} = network:start([{sta, [{managed, true}]}]),
{ok, {Found, AccessPoints}} = network:wifi_scan(),
ok = network:sta_connect([{ssid, "my-network"}, {psk, "my-password"}]),
ok = atomvm_rtems:wait_wifi(15000),
{ok, Dbm} = network:sta_rssi().
```

The existing `scan_done` callback option is supported. LibBSD supplies station
RSSI; scanning on LibBSD and other RTEMS backends returns `{error, enotsup}`.

The ESP lwIP configuration disables DHCP address-conflict checks because AP
proxy ARP can otherwise cause a valid lease to be declined. Addresses are
reported only after DHCP binds the lease.

The C3/C5/C6/S31 glue uses ESP-IDF clock dependencies and PHY lifecycle: calibration on
first enable, wakeup on later enables, periodic PLL tracking and RF shutdown on
disable. Vendor LL helpers preserve unrelated register fields instead of enabling
whole clock registers. RTEMS supplies locks and timer callbacks.

C3 direct boot also initializes modem clocks and the Wi-Fi low-power clock
source using ESP-IDF's `esp_perip_clk_init()` sequence. The BSP selects the PLL
with both source bits cleared and publishes the 80 MHz APB frequency for ROM
clients. PHY calibration runs on first enable and releases its scratch buffer;
C3 selects the combined-radio PHY mode and saves/restores its 84-byte digital
register state across disable/wakeup. Subsequent enables wake the PHY, and
disable closes RF and temperature sensing before removing the common clock. C3 critical sections preserve nested interrupt
levels, and PHY I2C transactions use the same nesting protection. These critical
callbacks are placed in internal instruction RAM.

C6 direct boot calibrates PLL480 from its 40 MHz crystal before selecting the
160 MHz CPU clock, and limits the flash source clock to 80 MHz, following
ESP-IDF's `rtc_clk_init()` and clock LL sequence. C6 modem initialization uses
the ACTIVE clock-domain maps, the system slow-clock source (including RC32K),
and the Wi-Fi MAC/APB/baseband/coexistence dependencies. It also applies IDF's
DMA access-filter setup. Its combined-radio PHY calibrates on first enable,
frees the scratch buffer, tracks PLL parameters and closes RF/temperature sensing
before gating analog I2C. Wi-Fi clocks remain enabled while initialized; the
common/private FE clock disable operations intentionally leave those clocks on.
C6 critical callbacks preserve nested interrupt state in internal RAM. With
modem retention disabled, C6 uses `phy_wakeup_init()` rather than C3's backup-DMA
register restore. C6 validation is emulator-only; physical PLL, RF, DMA and
sleep behavior still need testing.

S31 direct boot switches to XTAL40, configures and calibrates CPLL320, then
selects CPU/MEM/SYS/APB dividers of 1/2/3/2. Clock-update and calibration
failures stop boot explicitly. S31 modem initialization uses its separate
Wi-Fi MAC/APB/baseband/44 MHz/80X1 and PLL-source dependencies, ACTIVE clock
maps and RC_SLOW/XTAL32K low-power selection. PHY calibration runs on first
enable with temporary scratch storage; subsequent enables wake the PHY.
Disable closes RF and temperature sensing before gating analog I2C. The
baseband watchdog follows ESP-IDF's S31 workaround. S31 does not use the C3
USB PHY workaround or backup-DMA retention path.

S31 PHY, Wi-Fi and analog-I2C critical sections nest on each hart while an
RTEMS SMP lock excludes the other hart. Their interrupt and lock operations
execute in internal RAM. Wi-Fi initialization preserves the BSP's per-hart
PMA mappings and applies ESP-IDF's S31 peripheral-access workaround. Modem PLL
routing is shared between Wi-Fi and PHY; the underlying BBPLL/reference supply
stays enabled for direct-boot flash/USB consumers outside IDF's clock manager.
S31 validation is emulator-only; physical PLL, RF, DMA and sleep behavior
remain unverified.

RTEMS retains its own scheduler, queues and interrupt controller integration.
Wi-Fi buffers are smaller than IDF's defaults to fit available RAM, calibration
runs fully at boot without IDF's NVS calibration cache, and station power saving
is disabled by default. `-DRTEMS_WIFI_POWER_SAVE=MIN_MODEM` or `MAX_MODEM`
selects experimental station modem sleep; `NONE` keeps it disabled.
`-DRTEMS_WIFI_LISTEN_INTERVAL=10` sets maximum-mode wakeups to ten AP beacon
intervals; its default is three, matching IDF. `ON`/`OFF` remain aliases for
`MIN_MODEM`/`NONE`.
The Wi-Fi sleep-clock callback uses IDF's Q12 period format and the selected
C3 Bluetooth LP clock; modem sleep also supplies a measured RTC period.
Measuring that period does not install the RTC as RTEMS's system timecounter.
Physical sleep and analog validation remain tracked in the TODOs.

On ESP32-C3, `RTEMS_BT=ON` adds a minimal non-connectable legacy BLE
advertiser named `AtomVM-BLE`. Shared modem startup supports either radio
starting first, and PHY ownership keeps RF enabled while either radio uses it.
Radio power follows `rtc_init()` and `esp_bt_power_domain_on()`: clear the
force-up/force-no-isolation bits and release BT isolation before preparing
the shared modem domain. The BSP's additional BT power/reset sequence is
not used. The controller uses ESP-IDF's BT peripheral reset, 40 MHz XTAL
divided to 1 MHz for LPCLK, Q19 clock conversion, external PLL tracking and
default +9 dBm transmit power. CPU/APB clocks remain fixed at 160/80 MHz;
controller sleep is disabled. The configured scan cache has 20 entries
instead of IDF's default 100 to leave heap space for Wi-Fi. HCI command
completion and status are checked before `bt_start/0`
returns success. Initialization runs in a native task and the Erlang wrapper
yields while waiting; `bt_start_async/0` and `bt_status/0` expose that state
directly. This is controller bring-up; scanning, connections, GATT and
a BLE host API remain future work. BLE scheduler errors and sparse Mac scanner
reception are still under investigation. See [hardware testing](../../../HW_TEST.md).

The smoke-test configuration intentionally differs from IDF's defaults: one
activity instead of six, a 20-entry scan cache instead of 100, and connections,
scanning, DTM and encryption disabled. BLE 5 controller commands enable one
advertising event at a time, retaining legacy non-connectable packets on air.
The manufacturer counter advances after the controller reports that event
complete; the next event gets a new counter. These choices keep the C3 heap
available for Wi-Fi. Task stack sizes remain in bytes, as in ESP-IDF's FreeRTOS
port. The ROM-ready callback retains
IDF's default minimum-connection-interval setting even though this test does
not accept connections.

Wi-Fi software coexistence follows ESP-IDF: shared C3 modem startup (and
`esp_wifi_init()` on other chips) registers the coexistence OS adapter and
runs `coex_pre_init()` before
`esp_wifi_init_internal()`, because the Wi-Fi blob registers its scheme
callback before it calls the OS table's `_coex_init()`. The OS table then
forwards `coex_init()`, `coex_enable()` and the request/release/channel/scheme
entry points to `libcoexist.a`. External-wire coexistence and the
flexible-period power-management hooks stay off, matching IDF with those
options disabled. This arbitrates the shared radio once a BLE controller is
linked; it is not a BLE stack.

With esp-emu 0.49, C5/C6/S31 lose the virtual AP association after scanning
while connected. Their emulator tests check scan results and cancellation,
then restart the station before testing traffic. C3 also tests traffic
immediately after an associated scan. CI Wi-Fi runs use `--batch-size 1000` on C3
and `--batch-size 5000` on C5/C6/S31 to service radio events frequently enough
during authentication. Physical Wi-Fi coverage and remaining failures are
documented in the [hardware testing guide](../../../HW_TEST.md).

## Prerequisites

Install RTEMS 6.2 tools and the BSP you want to run. The official release
sources are at <https://ftp.rtems.org/pub/rtems/releases/6/6.2/>.

Example for SPARC / erc32:

```sh
export RTEMS_PREFIX=$HOME/development/rtems/6.2
wget https://ftp.rtems.org/pub/rtems/releases/6/6.2/sources/rtems-source-builder-6.2.tar.xz
tar xJf rtems-source-builder-6.2.tar.xz
cd rtems-source-builder-6.2/rtems
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" 6/rtems-sparc
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=sparc-rtems6 --with-rtems-bsp=sparc/erc32 --with-rtems-tests=no \
    6/rtems-kernel
export PATH="$RTEMS_PREFIX/bin:$PATH"
```

You also need a host AtomVM build (Erlang/OTP 28, CMake, gperf) to compile BEAM
and pack `.avm` files. OTP 25 can emit opcodes this VM no longer decodes by
default (`bs_test_tail2`); CI uses OTP 28.

## Build

From the repository root, build a host AtomVM and a boot AVM:

```sh
cmake -S . -B build-host -G Ninja
cmake --build build-host -t rtems_boot_test atomvmlib-rtems
mkdir -p src/platforms/rtems/build
./build-host/tools/packbeam/packbeam create -s test_boot \
    src/platforms/rtems/build/rtems_boot.avm \
    build-host/src/platforms/rtems/tests/test_erl_sources/test_boot.beam \
    build-host/libs/atomvmlib-rtems.avm
```

Cross-compile the RTEMS image, embedding that AVM:

```sh
cmake -S src/platforms/rtems -B src/platforms/rtems/build -G Ninja \
    -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
    -DRTEMS_PREFIX="$RTEMS_PREFIX" \
    -DRTEMS_BSP=sparc/erc32 \
    -DRTEMS_VERSION=6 \
    -DAVM_PACK="$PWD/src/platforms/rtems/build/rtems_boot.avm"
cmake --build src/platforms/rtems/build
```

## Run

```sh
rtems-run --rtems-bsps=erc32-sis \
    src/platforms/rtems/build/AtomVM-erc32.exe
```

A successful boot of `rtems_boot_test` prints `{atomvm_rtems_boot,rtems}`
and `Return value: ok` before the RTEMS exit fatal. SIS then traps (`ta 0x0`);
that shutdown is expected. There must be no `Failed load module` line.

## ARM RealView PBX-A9 / QEMU

Install ARM tools and the `realview_pbx_a9_qemu` BSP:

```sh
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" 6/rtems-arm
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=arm-rtems6 \
    --with-rtems-bsp=arm/realview_pbx_a9_qemu \
    --with-rtems-tests=no \
    6/rtems-kernel
```

Cross-compile as above with `-DRTEMS_BSP=arm/realview_pbx_a9_qemu`. Run the
resulting ELF directly in QEMU; this BSP does not require a DTB or debugger
bootstrap:

```sh
qemu-system-arm -net none -nographic -no-reboot -M realview-pbx-a9 -m 256M -smp 2 \
    -kernel src/platforms/rtems/build/AtomVM-realview_pbx_a9_qemu.exe
```

`-no-reboot` makes QEMU exit when RTEMS resets the board during shutdown. The
console UART is supported. I2C, GPIO, and networking currently return
`{error, enotsup}` on this target.

## i.MX 7 / QEMU SABRE

Install ARM tools and the `imx7` BSP (separate prefix or the same `RTEMS_PREFIX`):

```sh
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" 6/rtems-arm
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=arm-rtems6 --with-rtems-bsp=arm/imx7 --with-rtems-tests=no \
    6/rtems-kernel
```

Cross-compile with `-DRTEMS_BSP=arm/imx7`. The ELF is `AtomVM-imx7.exe`.

The imx7 BSP does not initialize clocks or pins; it expects a bootloader FDT
pointer in ARM `r2`. QEMU `-kernel` of an ELF leaves `r2=0` and ignores `-dtb`,
so the helper raw-loads the DTB at `0xb0000000` (above the ELF workspace) and
sets `r2` via gdb.

Build Linux `imx7d-sdb.dtb` from the v6.1 DTS (do not enable armhf apt on
amd64 hosts). Sparse-checkout must include `include/uapi/linux` because
`linux-event-codes.h` is a symlink there. Patch the DTB for QEMU's 1 GiB
RAM and an 8 MHz ARM generic-timer frequency so RTEMS does not touch
unimplemented system-counter MMIO:

```sh
fdtput -t x imx7d-sdb-qemu.dtb /memory@80000000 reg 0x80000000 0x40000000
fdtput -t x imx7d-sdb-qemu.dtb /timer clock-frequency 0x7a1200
src/platforms/rtems/tools/run-imx7-qemu.sh \
    -d imx7d-sdb-qemu.dtb -s 2 \
    src/platforms/rtems/build/AtomVM-imx7.exe
```

QEMU default RAM is 128 MiB, which is smaller than this ELF; the helper uses
`-m 1024M`. This is SoC-level UART bring-up, not GRiSP 2.

Open a second UART with `{peripheral, "/dev/ttyS1"}` (imx7) or
`"/dev/console_b"` (erc32). I2C on imx7:

```erlang
%% Low-level NIF interface with bus retries and probing
{ok, Bus} = i2c:init([{peripheral, "/dev/i2c-0"}, {fdt_alias, "i2c0"}, {retries, 2}]),
ok = i2c:is_device_ready(Bus, 16#57),
{ok, Devices} = i2c:scan(Bus),
{ok, [ok, RegVal]} = i2c:transfer(Bus, [{write, 16#57, <<0>>}, {read, 16#57, 1}]),
ok = i2c:deinit(Bus).

%% High-level i2c_hal interface with 10-bit address and explicit register width
I2C = i2c:open([{peripheral, "/dev/i2c-0"}, {fdt_alias, "i2c0"}]),
{ok, Data} = i2c:read_bytes(I2C, 16#57, 0, 4, [{mem_add_size, 8}]),
ok = i2c:close(I2C).
```

SPI on imx7 (such as the GRiSP 2 Pmod SPI connector):

```erlang
SPI = spi:open([
    {bus_config, [{peripheral, "/dev/spi-0"}, {fdt_alias, "spi0"}]},
    {device_config, [
        {sensor, [
            {cs, #{path => "/pmod-spi", property => "grisp,gpios", index => 0}},
            {clock_speed_hz, 1000000},
            {mode, 0}
        ]}
    ]}
]),
{ok, Value} = spi:read_at(SPI, sensor, 16#0F, 8),
ok = spi:close(SPI).
```

GPIO direct bank/pin access addresses `{Bank, Pin}` (`1..7`, `0..31`), with optional dynamic pinmuxing via `gpio:set_pin_mux/2`:

```erlang
ok = gpio:set_pin_mux({1, 3}, 5), % ALT5 = GPIO
ok = gpio:set_pin_mode({1, 3}, output),
ok = gpio:digital_write({1, 3}, high).
```

An FDT consumer property can instead identify a pin and preserve its active
polarity:

```erlang
Pin = #{path => "/leds/status", property => "gpios", index => 0},
ok = gpio:set_pin_mode(Pin, output_od),
ok = gpio:digital_write(Pin, high).
```

Pull resistors, open-drain output mode, and hardware interrupts are supported on i.MX:

```erlang
ok = gpio:set_pin_pull(Pin, up),
GPIO = gpio:start(),
ok = gpio:set_int(GPIO, Pin, falling),
receive
    {gpio_interrupt, Pin} ->
        io:format("GPIO triggered: ~p~n", [Pin])
end,
ok = gpio:remove_int(GPIO, Pin),
ok = gpio:close(GPIO).
```

## Host startup regression test

On Linux, exercise the RTEMS boot fallback using the native VM and stubbed
board I/O:

```sh
cmake -S . -B build-host -G Ninja -DAVM_DISABLE_JIT=ON
cmake --build build-host -t test-rtems-startup
./build-host/tests/rtems-startup/test-rtems-startup
```

The test caps VM allocations at 256 KiB, forces failures late in module
loading and partway through import construction, and checks that the embedded
app starts and all VM allocations are released. It does not require an RTEMS
toolchain or board.

## Networking (imx7 / LibBSD)

Build LibBSD with the official RTEMS 6.2 Source Builder set. The `imx7` BSP
set uses `6/rtems-libbsd` (FreeBSD 12 `rtems-libbsd-6.2.tar.xz`), which is
internally consistent for i.MX7. The FreeBSD 14 6.2 tarball is not.

```sh
cd rtems-source-builder-6.2/rtems
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=arm-rtems6 --with-rtems-bsp=arm/imx7 \
    6/rtems-libbsd
```

CMake links `libbsd.a` when it is present at
`$RTEMS_PREFIX/arm-rtems6/imx7/lib/libbsd.a`. The image initializes LibBSD,
brings up `lo0`, starts dhcpcd on `ffec0`, and installs the DHCP-provided name
servers atomically in `/etc/resolv.conf`. `atomvm_rtems:wait_resolver/1` waits
until that resolver configuration is installed. QEMU user networking attaches
to the onboard ENET with:

```sh
src/platforms/rtems/tools/run-imx7-qemu.sh \
    -d imx7d-sdb-qemu.dtb \
    -n user,model=imx.enet,hostfwd=tcp::8080-:8080 \
    src/platforms/rtems/build/AtomVM-imx7.exe
```

The Linux v6.1 `imx7d-sdb` DTB already describes `fsl,imx7d-fec`, PHY, MDIO,
interrupts, and clocks used by QEMU and the LibBSD `ffec` driver. Pack a
network test with `rtems_net_test` / `test_net.erl` to cover DHCP, DNS, outbound
TCP, a guest echo server on port 8080, and host-to-guest forwarding.

## GRiSP 2 hardware

For a prebuilt image, download the `atomvm-grisp2-sd-card-<commit>` artifact
from the RTEMS Build workflow. GitHub downloads it as a `.zip` file. Follow
the [SD-card installation guide](boards/grisp2/INSTALL.md), also included as
`README.md` at the top level of the ZIP. No RTEMS toolchain is needed to
install the prebuilt image.

GRiSP 2 uses an i.MX6ULL with RTEMS's shared `arm/imx7` BSP. Reuse the
RTEMS 6.2 ARM compiler, `imx7` BSP, and LibBSD from the QEMU build above;
LibBSD already includes the KSZ8091 PHY driver and i.MX nexus devices.
There is no additional AtomVM board driver or `libgrisp` dependency.
The GRiSP toolchain repository's RTEMS 5 prefix is not compatible with this
port, and building that entire repository is unnecessary.

A generated board DTB is bundled in
[`boards/grisp2`](boards/grisp2/README.md), with its source revision and
regeneration instructions. CMake validates its header and board compatible
string and copies it to `oftree` beside the image. Override it with
`-DRTEMS_DTB=/absolute/path/to/custom.dtb` for a different board
revision. The bootloader passes this DTB separately; it is not embedded in
the executable.

Install `u-boot-tools` and `device-tree-compiler` (`dtc` on Homebrew) for
image packaging and DTB validation.

### Single-Command Docker Build & SD-Card Packaging (Recommended)

If you have Docker (or OrbStack/Podman) installed, build the entire GRiSP 2 firmware and package the SD-card bundle with a single command without installing the RTEMS 6.2 ARM toolchain or LibBSD on your host:

```sh
./src/platforms/rtems/tools/docker-build-grisp2.sh --zip
```

This uses [`Dockerfile.rtems6-grisp2`](docker/Dockerfile.rtems6-grisp2) with cached multi-stage builds (`6/rtems-arm`, `arm/imx7` kernel, and `rtems-libbsd`), cross-compiles AtomVM for `RTEMS_BOARD=grisp2`, and generates the ready-to-copy `sd-card-bundle/` and `atomvm-grisp2-sd-card.zip`.

### Native Host Build

Build and package a hardware smoke test natively as follows:

```sh
cmake -S . -B build-host -G Ninja
cmake --build build-host -t rtems_grisp2_test atomvmlib-rtems
mkdir -p src/platforms/rtems/build
./build-host/tools/packbeam/packbeam create -s test_grisp2 \
    src/platforms/rtems/build/rtems_grisp2.avm \
    build-host/src/platforms/rtems/tests/test_erl_sources/test_grisp2.beam \
    build-host/libs/atomvmlib-rtems.avm

cmake -S src/platforms/rtems -B src/platforms/rtems/build-grisp2 -G Ninja \
    -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
    -DRTEMS_PREFIX="$RTEMS_PREFIX" \
    -DRTEMS_BSP=arm/imx7 \
    -DRTEMS_BOARD=grisp2 \
    -DAVM_PACK="$PWD/src/platforms/rtems/build/rtems_grisp2.avm"
cmake --build src/platforms/rtems/build-grisp2
```

`RTEMS_BOARD=grisp2` also enables creation of `AtomVM-grisp2.zImage`. The
image is converted to a flat binary, gzip-compressed, and wrapped with U-Boot
`mkimage` at load/entry address `0x80200000`. Inspect it with:

```sh
mkimage -l src/platforms/rtems/build-grisp2/AtomVM-grisp2.zImage
```

For the first boot, copy `AtomVM-grisp2.zImage` to the first FAT SD-card
partition as `zImage` and copy the build's `oftree` beside it. Copy
`src/platforms/rtems/build/rtems_grisp2.avm` as `app.avm` to exercise the SD
loader. Install `boards/grisp2/atomvm.conf` as `loader/entries/atomvm.conf`
on the same partition so barebox can find the image and device tree. See
the [installation guide](boards/grisp2/INSTALL.md) for serial console setup
and SD boot selection.

The smoke test prints `{atomvm_grisp2,rtems}`, `{grisp2,uart,ok}`,
`{grisp2,led,ok}`, `{grisp2,gpio,high}` or `{grisp2,gpio,low}`,
`{grisp2,gpio_int,ok}`, `{grisp2,eeprom,ok}`, `{grisp2,spi,ok}`, and `{grisp2,grisp_module,ok}`, followed by `Return value: ok`. The first RGB LED's
red channel lights for half a second. The EEPROM check reads eight bytes
at I²C address `0x57` and does not write EEPROM contents.
Use `rtems_net_test` separately to verify `ffec0` DHCP/DNS/TCP on the
physical Ethernet link.

### GRiSP 2 Board API (`grisp` module)

The `grisp` module (packed into `atomvmlib-rtems.avm`) provides high-level APIs and pre-configured peripheral mappings for the GRiSP 2 hardware:

```erlang
%% RGB LEDs (LED 1 and LED 2)
ok = grisp:led(1, red),
ok = grisp:led(2, {0, 255, 128}),
ok = grisp:led_off(1).

%% Jumper Keys (1..5)
State = grisp:jumper(1),               % 'high' or 'low'
[J1, J2, J3, J4, J5] = grisp:jumpers(). % Read all 5 jumpers

%% Board Identity & EEPROM
{ok, Info} = grisp:eeprom(),           % Decodes serial, batch, PCB version, CRC16
{ok, Serial} = grisp:serial_number(),
{ok, {PcbVar, PcbRev}} = grisp:hw_rev(),
{ok, MacBin} = grisp:mac_address(),    % 48-bit factory EUI MAC
{ok, MacStr} = grisp:mac_address_string().

%% Pmod SPI Connector (X1402)
SPI = grisp:spi_open(sensor, [{cs, cs1}, {clock_speed_hz, 1000000}, {mode, 0}]),
{ok, Val} = spi:read_at(SPI, sensor, 16#0F, 8),
ok = grisp:spi_close(SPI).

%% I2C Buses
InternalI2C = grisp:i2c_open(internal), % /dev/i2c-0 (EEPROM 0x57, EUI 0x5F, ATECC608A 0x36, 1-Wire 0x18)
PmodI2C = grisp:i2c_open(pmod),         % /dev/i2c-1 (X1403 Pmod connector)
ok = grisp:i2c_close(InternalI2C).

%% UART Ports
ConsoleUART = grisp:uart_open(console), % /dev/console at 115200 baud
PmodUART = grisp:uart_open(pmod, 9600), % /dev/ttyS3 (X1405 Pmod connector)
ok = uart:write(PmodUART, <<"Hello Pmod\n">>),
ok = grisp:uart_close(PmodUART).

%% Pmod GPIO Connector (X1404)
Pin1 = grisp:pmod_gpio_pin(1),          % FDT pin map for Pmod GPIO pin 1
ok = gpio:set_pin_mode(Pin1, output),
ok = gpio:digital_write(Pin1, high).

%% Wi-Fi (Realtek RTL8188EU 802.11b/g/n)
ok = grisp:wifi_connect(<<"MyWiFiSSID">>, <<"MyWiFiPassword">>),
{ok, Status} = grisp:wifi_status(),     % #{state => got_ip, ip => {192,168,1,42}, ...}
ok = grisp:wifi_disconnect().

%% Or portable cross-platform AtomVM network API:
network:start([{sta, [{ssid, "MyWiFiSSID"}, {psk, "MyWiFiPassword"}]}]),
network:wait_for_sta().
```

CI cross-builds the hardware image in the existing `arm/imx7` job and uploads
the `atomvm-grisp2-sd-card-<commit>` artifact. It contains ready-to-copy
`zImage`, `oftree` and `app.avm` files, a boot-loader entry, installation
instructions, source revision details, device-tree sources and licenses.
The debug ELF is named `debug/AtomVM-grisp2.elf` in the bundle. Physical
boot, UART, GPIO, I²C, SPI and Ethernet remain manual checks; a successful cross-build does not establish
that they work on a board.

### Updating the application on GRiSP 2

Install the firmware containing the SD loader once. Then applications can be
changed by replacing `app.avm` in the root of the existing SD-card FAT
partition and rebooting. No additional partition or firmware rebuild is
needed for application-only changes:

```text
zImage                      RTEMS + AtomVM + embedded fallback application
oftree                      Board device tree
app.avm                     Replaceable application, including its libraries
loader/entries/atomvm.conf   Boot image and device-tree selection
```

For example, from the repository root, build and pack `hello_world` using
the existing host build:

```sh
cmake --build build-host -t hello_world atomvmlib-rtems
./build-host/tools/packbeam/packbeam create -s hello_world \
    build-host/app.avm \
    build-host/examples/erlang/hello_world.beam \
    build-host/libs/atomvmlib-rtems.avm
```

With the board powered off, replace the card's `app.avm` with
`build-host/app.avm`, safely eject the card, and reboot. Keep `zImage`,
`oftree` and the boot-loader entry. A native driver or runtime change still
requires a new `zImage`.
The boot log identifies the selected file:

```text
AtomVM: Loaded /media/mmcsd-0-0/app.avm (... bytes)
Starting: hello_world.beam...
```

The media server starts before LibBSD attaches SD devices and mounts FAT
filesystems. The loader waits up to five seconds for the configured file,
then reads the complete pack into RAM (maximum 32 MiB). Missing files,
read/allocation failures, invalid pack or BEAM chunk structure, and startup
module load failures select the embedded application. The checks do not
validate all bytecode semantics: build the pack with the supported OTP and
libraries. An error after an application starts is reported normally; it
does not silently run the fallback. Remove `app.avm` to boot the embedded
hardware smoke test.

The default RTEMS path assumes the SD card is `mmcsd-0` and uses its first
partition, `/media/mmcsd-0-0`. If device numbering or the card layout differs,
set `-DRTEMS_APP_PATH=/media/your-volume/app.avm` when building the
firmware. For an unpartitioned FAT volume on `mmcsd-0`, use
`/media/mmcsd-0/app.avm`. The loader selects this path explicitly; it does
not infer which volume barebox booted.

The CI artifact includes the smoke-test pack as `app.avm`. On hardware,
check a valid replacement, a missing file, and a truncated copy: the latter
two must print the fallback message and run the embedded example.

## RTEMS 7 and ESP32-C3 (RISC-V)

RTEMS `main` (upcoming 7.0 release) introduces first-class support for Espressif's RISC-V microcontrollers under `bsps/riscv/esp32`, starting with the `esp32c3db` direct-boot BSP.

Key characteristics of AtomVM on ESP32-C3:
- **Architecture**: RISC-V RV32IMC (`-march=rv32imc -mabi=ilp32`), target triple `riscv-rtems7`.
- **Direct-Boot**: Bootrom begins execution directly from flash using the 8-byte direct boot header at offset `0x0`. The flat binary (`.bin`) is produced automatically by CMake via `${RTEMS_OBJCOPY} -O binary`.
- **Console & Peripherals**: Polled console is active over UART0 or USB-serial-JTAG (`/dev/console`). Dedicated RTEMS hardware drivers provide native support for **I2C** (`/dev/i2c-0`), general-purpose **SPI** (`/dev/spi-0`), **GPIO** (pins 0..21 with pin claiming, input/output/open-drain modes, pull-up/down resistors, and edge-triggered interrupts), and **RTC Slow/Fast Memory** (persistent binary storage across deep sleep and resets at `0x5000_0000` via `atomvm_rtems:rtc_slow_*` and `esp:rtc_slow_*`).

ADC, low-speed LEDC PWM, RTC storage/counters, and timer/GPIO wakeup have per-chip
implementations on C3, C5, C6 and S31, following ESP-IDF's register definitions:

| Chip | ADC GPIOs | PWM channels / maximum resolution | Sleep wakeup GPIOs |
| --- | --- | --- | --- |
| C3 | 0–4 (ADC1), 5 (ADC2) | 6 / 14 bits | 0–5 |
| C5 | 1–6 (ADC1) | 6 / 20 bits | 0–6 |
| C6 | 0–6 (ADC1) | 6 / 20 bits | 0–7 |
| S31 | 42–49 (ADC1), 50–57 (ADC2) | LEDC0: 8 / 20 bits | 0–7 |

ADC conversions have a bounded timeout; missing hardware completion returns
`{error, timeout}`. Readings use the existing 9–12-bit API (S31's native 17-bit
result is reduced to 12 bits). Voltage values remain estimates without eFuse
calibration on C3/C5/C6. S31 has no programmable attenuation or voltage
calibration in ESP-IDF, so its voltage result is `undefined`. PWM uses the XTAL
clock (40 MHz, or 48 MHz on C5 boards with that crystal) and rejects
frequency/resolution combinations outside the divider's range. The existing
fade API still performs immediate duty changes.

Light sleep retains SRAM, flash, peripheral state, XTAL and PLL supplies while
using the RTC/PMU to suspend the CPUs. An RTC timecounter preserves monotonic and
wall time; missed RTEMS ticks and timer queues are advanced after wakeup. S31's
second CPU parks through an RTEMS per-CPU job and resumes after the transition.
Deep sleep powers down the HP domains and restarts through ROM, preserving the
RTC binary and timekeeper (S31 RTC RAM starts at `0x2e000000`). Stop networking and
finish peripheral transfers before explicitly suspending the whole system.
Configure GPIO wakeup pads as inputs with appropriate pulls before sleeping.
The BSP must reserve SRAM for `.fast_text` (`ESP32C_IRAM_REGION_SIZE`); the
provided Wi-Fi BSP configurations already do so.
`esp:deep_sleep/1` takes milliseconds; timer-wakeup configuration takes microseconds.
Global deep-sleep pad hold is C3-specific; newer chips use individual pad holds
and return `{error, enotsup}` for the global hold calls.

**Validation:** `rtems_peripheral_test` builds `test_peripherals.beam` for ADC/PWM,
RTC storage, light sleep, expired timers and scheduler resumption. The pinned
esp-emulator does not complete hardware sleep transitions, and its newer RTC
counters read zero. CI therefore tests scheduler/clock integration separately
with `-DRTEMS_SLEEP_TEST_FIXTURE=ON`, which replaces only the RTC counter and PMU
entry point. Never use that option in firmware for a board. Physical power
transitions, analog accuracy and GPIO wakeup still need board validation. Build
`rtems_deep_sleep_test` and pack `test_deep_sleep.beam` for a real-board test of
timer wakeup, deep-sleep reset reason and RTC retention across the reset.

Initial physical C3/C5 testing verified direct boot with a USB console, an
advancing RTC counter, RTC binary storage, crypto, raw ADC conversion and PWM register
readback. C3 Wi-Fi passes active/passive scans (two APs), association, DHCP,
measured RSSI, DNS, UDP loopback, TCP/HTTP and stop/restart. C5 passes scanning,
association and traffic checks over UART with default Wi-Fi protocols and WPA2 on 2.4 GHz; its scans
find three APs across 2.4 GHz and 5 GHz. C5 direct boot calibrates the system
PLL for its detected crystal and follows ESP-IDF's modem clock, PHY lifecycle
and DMA access initialization. A 5 GHz connection and physical C6/S31 testing
remain pending. Two consecutive final-image runs also pass C5 stop/restart
after matching ESP-IDF's calibration-buffer lifetime to recover about 1.9 KB
of RAM. See [hardware testing](../../../HW_TEST.md) for the baseline.

Timer light sleep returns on C3/C5, but recovery of an expired Erlang timer fails
and USB console continuity is unreliable. These remain open hardware issues;
ADC accuracy, PWM waveforms, deep sleep and GPIO wakeup are unverified.

### 1. Build RTEMS 7 Tools and BSP

You can build the toolchain and BSP using Docker, the automated build script, or manually via RSB.

CI, both RTEMS Dockerfiles, and the native build scripts share the commit pins in
[`docker/dependencies.sh`](docker/dependencies.sh), taken from successful CI run
[39153602941](https://github.com/petermm/AtomVM/actions/runs/39153602941). This pins
RSB, the RTEMS 7 kernel, LibBSD, lwIP, the Wi-Fi sources and blobs, MbedTLS, and
the emulator installer. RSB pins its compiler and RTEMS 6 kernel archives by
revision and checksum; lwIP and LibBSD pin their submodules through gitlinks.
The workflow also pins the Erlang/OTP version.

To update sources, edit the pins, rebuild the Docker image with `--rebuild`,
and run the relevant boot, Wi-Fi and SMP tests. CI cache keys include the pins.
Native builders reject a cached checkout at another commit; use `--clean`
to fetch the new pins. Explicit local RTEMS trees remain available through
`--rtems-src`; Docker's `RSB_REF` and `RTEMS_REF` overrides require full commit IDs.
Ubuntu packages continue to come from the configured distribution repositories.

#### Option A: Single-Command Docker Build & Emulation (Recommended)

If you have Docker (or OrbStack/Podman) installed, build the entire firmware and run the boot test in `esp-emulator` with a single command:

```sh
./src/platforms/rtems/tools/docker-build-esp32c3.sh --simulate
```

This uses the dedicated [`Dockerfile.rtems7-esp32c3`](docker/Dockerfile.rtems7-esp32c3) to compile the RTEMS 7 RISC-V tools, build the `riscv/esp32c3db` BSP, cross-compile AtomVM, and run `esp-emu`.

#### Option B: Automated Local Build Script

To build the native RTEMS 7 toolchain and BSP on your local Linux or macOS machine without Docker:

```sh
./src/platforms/rtems/tools/build-rtems7-toolchain.sh --prefix "$HOME/development/rtems/7"
```

This script clones RSB and RTEMS `main`, builds `7/rtems-riscv`, and configures and installs the `riscv/esp32c3db` BSP into the requested prefix.

#### Option C: Manual RSB & Waf Build

Alternatively, execute the build steps manually:

```sh
export RTEMS_PREFIX=$HOME/development/rtems/7
git clone https://github.com/RTEMS/rtems-source-builder.git rsb
cd rsb/rtems
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" 7/rtems-riscv
export PATH="$RTEMS_PREFIX/bin:$PATH"

git clone https://github.com/RTEMS/rtems.git rtems-src
cd rtems-src
./waf configure --prefix="$RTEMS_PREFIX" --rtems-bsps=riscv/esp32c3db
./waf
./waf install
```

#### Option D: Developing with a Local or Patched RTEMS Tree

If you are developing or testing custom patches against RTEMS `main` or `bsps/riscv/esp32`:

- **Via Docker (`--rtems-src`)**:
  Pass your local RTEMS checkout to `docker-build-esp32c3.sh`:
  ```sh
  ./src/platforms/rtems/tools/docker-build-esp32c3.sh --rtems-src /path/to/my/rtems --simulate
  ```
  Docker reuses the cached RSB toolchain and re-compiles only the ~30-second BSP layer using your local RTEMS tree.

- **Via Docker (`patches/` directory)**:
  Drop any `*.patch` or `*.diff` files into `src/platforms/rtems/docker/patches/`. They will be automatically applied with `git apply` before configuring and compiling the BSP.

- **Via Native Script (`--rtems-src`)**:
  ```sh
  ./src/platforms/rtems/tools/build-rtems7-toolchain.sh --rtems-src /path/to/my/rtems
  ```

### 2. Cross-Compile AtomVM

Cross-compile AtomVM targeting the ESP32-C3:

```sh
cmake -S src/platforms/rtems -B src/platforms/rtems/build-esp32c3 -G Ninja \
    -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
    -DRTEMS_PREFIX="$RTEMS_PREFIX" \
    -DRTEMS_BSP=riscv/esp32c3db \
    -DRTEMS_BOARD=esp32c3 \
    -DRTEMS_VERSION=7 \
    -DAVM_PACK="$PWD/build-host/libs/atomvmlib-rtems.avm"
cmake --build src/platforms/rtems/build-esp32c3
```

This generates `AtomVM-esp32c3db.exe` and the flash-ready direct-boot image `AtomVM-esp32c3db.bin`.

### 3. Flashing to Physical Board

The flash-ready direct-boot image is written to flash at offset `0x0` with
`esptool.py`, for any of the ESP32 boards (`AtomVM-esp32c3db.bin`,
`AtomVM-esp32c5db.bin`, `AtomVM-esp32c6db.bin`, `AtomVM-esp32s31db.bin`).  CMake
also exposes a `flash` target for the board you configured:

```sh
esptool.py --chip esp32c3 --port /dev/ttyACM0 --baud 921600 \
    write_flash 0x0 AtomVM-esp32c3db.bin
# or
ninja -C src/platforms/rtems/build-esp32c3 flash
```

CI publishes these images as `atomvm-rtems-<bsp_name>-<run_id>` artifacts: the
direct-boot `.bin` for the ESP32 boards and the `.exe` (loadable with QEMU or
`rtems-run`) for the emulated boards.

### 4. Fast Emulation with esp-emulator (Recommended)

You can emulate the ESP32-C3 build locally or in CI without physical hardware using Espressif's high-performance Rust-based [`esp-emulator`](https://github.com/espressif/esp-emulator) (`esp-emu`):

```sh
# Install esp-emulator
curl -fsSL https://raw.githubusercontent.com/espressif/esp-emulator/main/install.sh | sh -s -- --version 0.49.0
export PATH="$HOME/.local/bin:$PATH"

# Run emulation with automatic exit on completion
esp-emu --chip esp32c3 --firmware src/platforms/rtems/build-esp32c3/AtomVM-esp32c3db.bin \
    --timeout 30s --exit-on "Return value: ok"
```

If `esp-emu` is detected in `PATH` during CMake configure, you can also run:
```sh
cmake --build src/platforms/rtems/build-esp32c3 -t simulate
```

Alternatively, run in QEMU:

```sh
dd if=/dev/zero of=flash.bin bs=1M count=4
dd if=AtomVM-esp32c3db.bin of=flash.bin conv=notrunc
qemu-system-riscv32 -nographic -machine esp32c3 \
    -drive file=flash.bin,if=mtd,format=raw -serial mon:stdio
```

### 5. Testing with Wokwi CLI

The ESP32-C3/C5/C6/S31 Docker build scripts accept `--simulate-wokwi` to
run the built test with Wokwi instead of esp-emulator. Install
[`wokwi-cli`](https://docs.wokwi.com/wokwi-ci/cli-installation) on the host and
export `WOKWI_CLI_TOKEN`, as used by `.github/workflows/esp32-simtest.yaml`.
The runner also inherits `WOKWI_CLI_SERVER` if you use a local CI server.
The token stays in the environment; it is not written to generated files.

```sh
./src/platforms/rtems/tools/docker-build-esp32c3.sh --simulate-wokwi

# Select the S31 SMP test with the Wokwi backend:
./src/platforms/rtems/tools/docker-build-esp32s31.sh --test-smp --simulate-wokwi

# Test Wi-Fi against Wokwi-GUEST (open network, empty password):
./src/platforms/rtems/tools/docker-build-esp32c3.sh --test-wifi --simulate-wokwi

# Re-run an existing build without compiling:
./src/platforms/rtems/tools/run-wokwi.sh esp32c3 src/platforms/rtems/build-esp32c3
```

The runner generates `wokwi.toml` and a UART-connected `diagram.json` in
`<build-directory>/wokwi/`, loading the RTEMS direct-boot image at flash offset
zero. It saves UART output to `wokwi/serial.log`, requires `Return value: ok`,
and returns a failure for a fatal exception or a 60-second simulated-time
timeout. Missing CLI/token errors are reported before starting a Docker build.

`--test-wifi --simulate-wokwi` selects a shared Wokwi Wi-Fi test on all four
chips. It uses SSID `Wokwi-GUEST` and password `""`, checking active/passive
scanning, scan callbacks/cancellation, connection, DHCP and RSSI.
It requires DNS resolution and an HTTP response from `example.com` through
Wokwi's gateway, including traffic after a connected scan. Gateway or Internet
outages therefore fail the test. The esp-emulator backend keeps its existing
AP, fixed RSSI and local HTTP/SNTP fixtures.
Rebuild the Docker toolchain image to include the RTEMS interrupt fixes:
four-byte trap slots through interrupt ID 47, corrected C3/C6 register
addressing, and preservation of CLIC interrupt priority on C5/S31.
The S31 boot and SMP tests pass with Wokwi CLI 0.29.0 and these fixes.
Simulator tests do not establish compatibility with physical hardware.

## SMP Multi-Core Support

AtomVM on RTEMS supports symmetric multiprocessing (SMP) on targets with multi-core BSPs:
- **RISC-V ESP32-S31**: Dual-core RV32IMAFC microcontroller simulated in `esp-emulator` (`--chip esp32s31`).
- **ARM RealView PBX-A9**: Multi-core ARM Cortex-A9 simulated in QEMU (`-M realview-pbx-a9 -smp 2` or `-smp 4`).
- **ARM i.MX 7Dual SABRE**: Dual-core ARM Cortex-A7 simulated in QEMU (`-M mcimx7d-sabre -smp 2`).

### Architecture & Schedulers

When built with an SMP-enabled RTEMS kernel (`RTEMS_SMP` defined in `<rtems/score/cpuopts.h>`), AtomVM detects the number of available cores at startup:
- Queries `rtems_scheduler_get_processor_maximum()` (or `CONFIGURE_MAXIMUM_PROCESSORS`).
- Spawns background scheduler threads (`smp_scheduler_thread`) using RTEMS POSIX threads (`pthread_create`).
- Synchronizes using fine-grained POSIX mutexes and condition variables with bounded deadline waits (`pthread_cond_timedwait`) for scheduler sleep and wakeups.
- Schedulers run BEAM processes concurrently across all available cores. In Erlang, `erlang:system_info(schedulers)` and `erlang:system_info(schedulers_online)` report the detected number of cores.

Uniprocessor targets (such as `erc32`, `esp32c3`, `esp32c5`, `esp32c6`, and `grisp2`) automatically default to `AVM_DISABLE_SMP=ON`, eliminating lock overhead and ensuring uniprocessor execution.

### Emulating ESP32-S31 Dual-Core with esp-emulator
 
```sh
esp-emu --chip esp32s31 \
    --firmware src/platforms/rtems/build-esp32s31/AtomVM-esp32s31db.bin \
    --timeout 30s --exit-on "Return value: ok"
```

Or using the automated docker script:
```sh
./src/platforms/rtems/tools/docker-build-esp32s31.sh --test-smp
```

### Running SMP on QEMU RealView PBX-A9

Run with `-smp 2` (or `-smp 4`):

```sh
qemu-system-arm -net none -nographic -no-reboot -M realview-pbx-a9 -m 256M -smp 2 \
    -kernel src/platforms/rtems/build/AtomVM-realview_pbx_a9_qemu.exe
```

### Running SMP on QEMU i.MX 7 SABRE

Pass `-s <cores>` (default is 2) to `run-imx7-qemu.sh`:

```sh
src/platforms/rtems/tools/run-imx7-qemu.sh \
    -d imx7d-sdb-qemu.dtb -s 2 \
    src/platforms/rtems/build/AtomVM-imx7.exe
```

## Benchmark suite

[`run-atomvm-benchmark.sh`](tools/run-atomvm-benchmark.sh) builds and runs
[pguyot/atomvm_benchmark](https://github.com/pguyot/atomvm_benchmark) on an
RTEMS ESP32 board under esp-emulator.  It builds the benchmark (rebar3 +
`atomvm_rebar3_plugin`) inside the Docker image, packs a runnable with
`atomvmlib-rtems`, cross-compiles the firmware for the requested board and runs
it.  It needs the Docker image and `esp-emu` on `PATH`.

```sh
# Build and run the suite on ESP32-C5
./src/platforms/rtems/tools/run-atomvm-benchmark.sh --board esp32c5

# Pick a board and a longer emulator timeout (the suite is heavy)
./src/platforms/rtems/tools/run-atomvm-benchmark.sh --board esp32s31 --timeout 3600s
```

`--board` is one of `esp32c3`, `esp32c5`, `esp32c6`, `esp32s31`.  The suite
times itself with `erlang:system_time/1`, which follows the emulated CPU clock
that `bsp_start()` programs, so the reported numbers are deterministic and
independent of host speed -- useful for comparing builds.

## CMake options

| Option | Default | Description |
| --- | --- | --- |
| `RTEMS_PREFIX` | (required) | RTEMS installation prefix (e.g. 6.2 or 7) |
| `RTEMS_BSP` | `sparc/erc32` | Architecture/BSP (`arch/bsp`, e.g. `riscv/esp32c3db`) |
| `RTEMS_BOARD` | `generic` (`qemu` for `arm/imx7`, `esp32c3` for `riscv/esp32`) | Board integration (`generic`, `qemu`, `grisp2`, or `esp32c3`) |
| `RTEMS_VERSION` | `6` (or `7` for `riscv/esp32`) | Major toolchain version (`sparc-rtems6-gcc`, `riscv-rtems7-gcc`) |
| `AVM_PACK` | empty | Host-built `.avm` to embed |
| `RTEMS_CREATE_ZIMAGE` | `ON` for `grisp2` | Create a GRiSP 2 bootloader-ready image |
| `RTEMS_IMAGE_LOAD_ADDRESS` | `0x80200000` | GRiSP 2 image load and entry address |
| `RTEMS_DTB` | bundled `boards/grisp2/imx6ul-grisp2.dtb` | Board DTB to validate and copy as `oftree` when packaging |
| `RTEMS_APP_PATH` | `/media/mmcsd-0-0/app.avm` | RTEMS filesystem path for the replaceable application (GRiSP 2 only) |
| `AVM_DISABLE_JIT` | `ON` | JIT is not supported yet |
| `AVM_DISABLE_SMP` | `OFF` on SMP BSPs, `ON` otherwise | Disable SMP multi-core support |
