#!/usr/bin/env bash
#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M. <petermm@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

set -euo pipefail

DEFAULT_PREFIX="${HOME}/development/rtems/7"
PREFIX="${DEFAULT_PREFIX}"
BUILD_DIR="${HOME}/.cache/rtems7-wifi-build"
CLEAN=0

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPENDENCIES="${SCRIPT_DIR}/dependencies.sh"
if [ ! -f "${DEPENDENCIES}" ]; then
    DEPENDENCIES="${SCRIPT_DIR}/../docker/dependencies.sh"
fi
. "${DEPENDENCIES}"

DEFAULT_PATCH_DIR="${SCRIPT_DIR}/patches"
if [ ! -d "${DEFAULT_PATCH_DIR}" ] && [ -d "${SCRIPT_DIR}/../docker/patches" ]; then
    DEFAULT_PATCH_DIR="${SCRIPT_DIR}/../docker/patches"
fi
PATCH_DIR="${DEFAULT_PATCH_DIR}"

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Builds and installs rtems-lwip and rtems-esp-wifi stack for riscv/esp32c3db.

Options:
  --prefix <path>     Target RTEMS prefix (default: ${DEFAULT_PREFIX})
  --build-dir <path>  Temporary build directory (default: ${BUILD_DIR})
  --patch-dir <path>  Path to patches directory (default: ${DEFAULT_PATCH_DIR})
  --clean             Wipe the build directory before building
  -h, --help          Show this help message

EOF
    exit "${1:-0}"
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --prefix)
            PREFIX="$2"
            shift 2
            ;;
        --prefix=*)
            PREFIX="${1#*=}"
            shift
            ;;
        --build-dir)
            BUILD_DIR="$2"
            shift 2
            ;;
        --build-dir=*)
            BUILD_DIR="${1#*=}"
            shift
            ;;
        --patch-dir)
            PATCH_DIR="$2"
            shift 2
            ;;
        --patch-dir=*)
            PATCH_DIR="${1#*=}"
            shift
            ;;
        --clean)
            CLEAN=1
            shift
            ;;
        -h|--help)
            usage 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage 1
            ;;
    esac
done

echo "================================================================"
echo "RTEMS 7 ESP32-C3 Wi-Fi & lwIP Stack Builder"
echo "  Target Prefix: ${PREFIX}"
echo "  Build Dir:     ${BUILD_DIR}"
echo "  Patch Dir:     ${PATCH_DIR}"
echo "================================================================"

command -v riscv-rtems7-gcc >/dev/null 2>&1 || {
    export PATH="${PREFIX}/bin:${PATH}"
    command -v riscv-rtems7-gcc >/dev/null 2>&1 || {
        echo "Error: riscv-rtems7-gcc not found in PATH or ${PREFIX}/bin" >&2
        exit 1
    }
}

command -v git >/dev/null 2>&1 || { echo "Error: git is required" >&2; exit 1; }
command -v python3 >/dev/null 2>&1 || { echo "Error: python3 is required" >&2; exit 1; }

BSP_LIB="${PREFIX}/riscv-rtems7/esp32c3db/lib"
BSP_INC="${PREFIX}/riscv-rtems7/esp32c3db/lib/include"

if [ ! -d "${BSP_LIB}" ]; then
    echo "Error: RTEMS BSP library directory not found: ${BSP_LIB}" >&2
    echo "Please build the riscv/esp32c3db BSP first." >&2
    exit 1
fi

if [ "${CLEAN}" -eq 1 ] && [ -d "${BUILD_DIR}" ]; then
    echo "==> Cleaning build directory: ${BUILD_DIR}"
    rm -rf "${BUILD_DIR}"
fi

mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# -----------------------------------------------------------------------------
# Step 1: Clone and build rtems-lwip
# -----------------------------------------------------------------------------
echo "==> [1/4] Building rtems-lwip for riscv/esp32c3db..."
if [ ! -d "rtems-lwip/.git" ]; then
    rtems_clone_pinned https://github.com/RTEMS/rtems-lwip.git "${RTEMS_LWIP_COMMIT}" rtems-lwip
    # Fetch the pinned lwIP commit from its mirror to avoid Savannah TLS/HTTP failures.
    git -C rtems-lwip config submodule.lwip-upstream.url https://github.com/lwip-tcpip/lwip.git
    git -C rtems-lwip submodule update --init --recursive --depth 1 lwip-upstream
    # The upstream Waf gitlink is not available in the GitHub mirror.
    # Use a compatible, pinned mirror revision instead.
    rtems_clone_pinned https://github.com/RTEMS/rtems_waf.git "${RTEMS_LWIP_WAF_COMMIT}" rtems-lwip/rtems_waf
    if [ -n "${PATCH_DIR}" ] && [ -d "${PATCH_DIR}/rtems-lwip" ]; then
        for p in "${PATCH_DIR}"/rtems-lwip/*.patch; do
            if [ -f "$p" ]; then
                echo "==> Applying rtems-lwip patch: $(basename "$p")..."
                git -C rtems-lwip apply "$p" || patch -d rtems-lwip -p1 < "$p"
            fi
        done
    fi
fi

rtems_clone_pinned https://github.com/RTEMS/rtems-lwip.git "${RTEMS_LWIP_COMMIT}" rtems-lwip
rtems_clone_pinned https://github.com/lwip-tcpip/lwip.git \
    "$(git -C rtems-lwip rev-parse HEAD:lwip-upstream)" rtems-lwip/lwip-upstream
rtems_clone_pinned https://github.com/RTEMS/rtems_waf.git "${RTEMS_LWIP_WAF_COMMIT}" rtems-lwip/rtems_waf

cd "${BUILD_DIR}/rtems-lwip"
./waf configure --prefix="${PREFIX}" --rtems-bsps=riscv/esp32c3db
./waf
./waf install

echo "==> rtems-lwip installed successfully:"
ls -lh "${BSP_LIB}/liblwip.a"

# -----------------------------------------------------------------------------
# Step 2: Fetch esp32-wifi-lib, esp-hal-3rdparty, mbedtls, and rtems-esp-wifi
# -----------------------------------------------------------------------------
echo "==> [2/4] Fetching Wi-Fi components and blobs..."
cd "${BUILD_DIR}"

rtems_clone_pinned https://github.com/espressif/esp32-wifi-lib.git "${ESP_WIFI_COMMIT}" esp32-wifi-lib

rtems_clone_pinned https://github.com/espressif/esp-phy-lib.git "${ESP_PHY_COMMIT}" esp-phy-lib

rtems_clone_pinned https://github.com/espressif/esp-coex-lib.git "${ESP_COEX_COMMIT}" esp-coex-lib

rtems_clone_pinned https://github.com/espressif/esp32c3-bt-lib.git "${ESP32C3_BT_COMMIT}" esp32c3-bt-lib

rtems_clone_pinned https://github.com/espressif/esp-hal-3rdparty.git "${ESP_HAL_COMMIT}" esp-hal-3rdparty

rtems_clone_pinned https://github.com/Mbed-TLS/mbedtls.git "${RTEMS_MBEDTLS_COMMIT}" mbedtls

if [ ! -d "rtems-esp-wifi/.git" ]; then
    rtems_clone_pinned https://github.com/thesamprice/rtems-esp-wifi.git "${RTEMS_ESP_WIFI_COMMIT}" rtems-esp-wifi
    if [ -n "${PATCH_DIR}" ] && [ -d "${PATCH_DIR}/rtems-esp-wifi" ]; then
        for p in "${PATCH_DIR}"/rtems-esp-wifi/*.patch; do
            if [ -f "$p" ]; then
                echo "==> Applying rtems-esp-wifi patch: $(basename "$p")..."
                git -C rtems-esp-wifi apply "$p" || patch -d rtems-esp-wifi -p1 < "$p"
            fi
        done
    fi
fi

rtems_clone_pinned https://github.com/thesamprice/rtems-esp-wifi.git "${RTEMS_ESP_WIFI_COMMIT}" rtems-esp-wifi

# -----------------------------------------------------------------------------
# Step 3: Build mbedtls and wpa_supplicant
# -----------------------------------------------------------------------------
echo "==> [3/4] Compiling and installing MbedTLS 3.6.3 and WPA Supplicant..."
GLUE="${BUILD_DIR}/rtems-esp-wifi"
HAL="${BUILD_DIR}/esp-hal-3rdparty/components"
WPA="${HAL}/wpa_supplicant"
MBED="${BUILD_DIR}/mbedtls"

# Build & install MbedTLS 3.6.3
cd "${MBED}"
python3 scripts/config.py set MBEDTLS_NO_PLATFORM_ENTROPY
python3 scripts/config.py unset MBEDTLS_TIMING_C
python3 scripts/config.py unset MBEDTLS_NET_C
cmake -B build-mbedtls -G Ninja \
  -DCMAKE_SYSTEM_NAME=Generic \
  -DCMAKE_SYSTEM_PROCESSOR=riscv \
  -DCMAKE_C_COMPILER=riscv-rtems7-gcc \
  -DCMAKE_C_FLAGS="-march=rv32imc -mabi=ilp32 -Os" \
  -DCMAKE_INSTALL_PREFIX="${PREFIX}/riscv-rtems7/esp32c3db" \
  -DENABLE_TESTING=OFF \
  -DENABLE_PROGRAMS=OFF \
  -DMBEDTLS_FATAL_WARNINGS=OFF
ninja -C build-mbedtls install
cd "${BUILD_DIR}"

# Build WPA Supplicant
mkdir -p "${BUILD_DIR}/wpa-obj"
WPA_INC="-isystem ${BSP_INC} -I ${GLUE}/include -I ${GLUE}/include/rtems-esp \
         -I ${WPA}/src -I ${WPA}/src/utils -I ${WPA}/src/common -I ${WPA}/src/crypto \
         -I ${WPA}/include -I ${WPA}/port/include \
         -I ${WPA}/esp_supplicant/include -I ${WPA}/esp_supplicant/include/esp_private \
         -I ${WPA}/esp_supplicant/src \
         -I ${MBED}/include \
         -I ${HAL}/esp_wifi/include -I ${HAL}/esp_wifi/wifi_apps/roaming_app/include \
         -I ${HAL}/esp_wifi/wifi_apps/roaming_app/src \
         -I ${HAL}/esp_wifi/wifi_apps/include/apps_private \
         -I ${HAL}/esp_common/include -I ${HAL}/log/include -I ${HAL}/esp_event/include \
         -I ${HAL}/esp_rom/include -I ${HAL}/esp_rom/esp32c3/include \
         -I ${HAL}/esp_rom/esp32c3/include/esp32c3 \
         -I ${HAL}/soc/include -I ${HAL}/soc/esp32c3/include \
         -I ${HAL}/esp_hw_support/include -I ${HAL}/esp_timer/include \
         -I ${HAL}/esp_system/include"

WPA_DEFS="-D__ets__ -DESP_SUPPLICANT -DIEEE8021X_EAPOL -DEAP_PEER_METHOD \
          -DEAP_MSCHAPv2 -DEAP_TTLS -DEAP_TLS -DEAP_PEAP -DUSE_WPA2_TASK \
          -DCONFIG_WPS -DESPRESSIF_USE -DCONFIG_ECC -DCONFIG_IEEE80211W \
          -DCONFIG_SHA256 -DCONFIG_NO_RADIUS -DCONFIG_CRYPTO_INTERNAL"

for c in "${WPA}"/src/*/*.c "${WPA}"/esp_supplicant/src/*.c \
         "${WPA}"/esp_supplicant/src/*/*.c "${WPA}"/port/*.c; do
    [ -f "$c" ] || continue
    o="${BUILD_DIR}/wpa-obj/$(echo "${c#${WPA}/}" | tr '/' '_' | sed 's/\.c$/.o/')"
    riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 -std=gnu17 -DESP_PLATFORM ${WPA_DEFS} \
        -DMBEDTLS_CONFIG_FILE='<rtems-esp/mbedtls_config.h>' \
        ${WPA_INC} -Os -ffunction-sections -fdata-sections \
        -c -o "$o" "$c" 2>/dev/null || true
done
riscv-rtems7-ar rcs "${BSP_LIB}/libwpa.a" "${BUILD_DIR}"/wpa-obj/*.o

# -----------------------------------------------------------------------------
# Step 4: Build rtems-esp-wifi glue objects and install blobs & linker scripts
# -----------------------------------------------------------------------------
echo "==> [4/4] Building rtems-esp-wifi glue and packaging..."
mkdir -p "${BUILD_DIR}/glue-obj"

COMMON_INC="-isystem ${BSP_INC} -I ${GLUE}/include -I ${GLUE}/include/rtems-esp \
            -I ${HAL}/esp_wifi/include -I ${HAL}/esp_wifi/include/local \
            -I ${HAL}/esp_common/include -I ${HAL}/esp_event/include \
            -I ${HAL}/log/include -I ${HAL}/esp_rom/include -I ${HAL}/soc/include \
            -I ${HAL}/soc/esp32c3/include -I ${HAL}/soc/esp32c3/register \
            -I ${HAL}/esp_hal_clock/esp32c3/include -I ${HAL}/hal/platform_port/include \
            -I ${HAL}/esp_hal_regi2c/esp32c3/include -I ${HAL}/esp_hw_support/include \
            -I ${HAL}/esp_timer/include -I ${HAL}/esp_phy/include \
            -I ${HAL}/esp_system/include -I ${HAL}/heap/include -I ${HAL}/hal/include \
            -I ${HAL}/esp_coex/include -I ${HAL}/esp_hal_gpio/include \
            -I ${HAL}/esp_hal_gpio/esp32c3/include -I ${HAL}/esp_wifi/wifi_apps/include \
            -I ${HAL}/esp_hw_support/include/soc"

# 1. rtems_esp_glue.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -Wall -Wextra -c -o "${BUILD_DIR}/glue-obj/rtems_esp_glue.o" "${GLUE}/src/rtems_esp_glue.c"

# 2. rtems_wifi_os_adapter.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -Wall -Wextra -c -o "${BUILD_DIR}/glue-obj/rtems_wifi_os_adapter.o" "${GLUE}/src/rtems_wifi_os_adapter.c"

# 3. rtems_esp_wifi_init.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -Wall -Wextra -c -o "${BUILD_DIR}/glue-obj/rtems_esp_wifi_init.o" "${GLUE}/src/rtems_esp_wifi_init.c"

# 4. rtems_esp_event.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -Wall -Wextra -c -o "${BUILD_DIR}/glue-obj/rtems_esp_event.o" "${GLUE}/src/rtems_esp_event.c"

# 5. rtems_esp_netif.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -Wall -Wextra -c -o "${BUILD_DIR}/glue-obj/rtems_esp_netif.o" "${GLUE}/src/rtems_esp_netif.c"

# 6. ftm_load_calibration.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -c -o "${BUILD_DIR}/glue-obj/ftm.o" "${HAL}/esp_wifi/src/ftm_load_calibration.c"

# 7. esp_wifi_regulatory.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -c -o "${BUILD_DIR}/glue-obj/reg.o" "${HAL}/esp_wifi/regulatory/esp_wifi_regulatory.c"

# 8. phy_init_data.c
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 ${COMMON_INC} \
    -I "${HAL}/esp_phy/esp32c3/include" \
    -c -o "${BUILD_DIR}/glue-obj/phy_init_data.o" "${HAL}/esp_phy/esp32c3/phy_init_data.c"

# 9. wpa common.c (hexstr2bin)
riscv-rtems7-gcc -march=rv32imc -mabi=ilp32 -DESP_PLATFORM ${COMMON_INC} \
    -I "${WPA}/src" -I "${WPA}/src/utils" -I "${WPA}/include" -I "${WPA}/port/include" \
    -c -o "${BUILD_DIR}/glue-obj/wpa_common.o" "${WPA}/src/utils/common.c"

# Archive into librtems_esp_wifi.a
riscv-rtems7-ar rcs "${BSP_LIB}/librtems_esp_wifi.a" "${BUILD_DIR}"/glue-obj/*.o

# Copy precompiled Wi-Fi and PHY blobs to BSP lib directory
echo "==> Installing Wi-Fi and PHY binary blobs into ${BSP_LIB}..."
cp "${BUILD_DIR}/esp32-wifi-lib/esp32c3/"lib*.a "${BSP_LIB}/"
cp "${BUILD_DIR}/esp-phy-lib/esp32c3/"lib*.a "${BSP_LIB}/"
cp "${BUILD_DIR}/esp-coex-lib/esp32c3/libcoexist.a" "${BSP_LIB}/"
cp "${BUILD_DIR}/esp32c3-bt-lib/esp32c3/libbtdm_app_flash.a" "${BSP_LIB}/"

# Copy Mask ROM linker scripts to BSP lib directory
echo "==> Installing Mask ROM linker scripts into ${BSP_LIB}..."
ROM_LD="${HAL}/esp_rom/esp32c3/ld"
cp "${ROM_LD}/esp32c3.rom.ld" \
   "${ROM_LD}/esp32c3.rom.api.ld" \
   "${ROM_LD}/esp32c3.rom.version.ld" \
   "${ROM_LD}/esp32c3.rom.eco3.ld" \
   "${BSP_LIB}/"

# Copy headers to BSP include directory
echo "==> Installing Wi-Fi headers into ${BSP_INC}..."
mkdir -p "${BSP_INC}/rtems-esp" "${BSP_INC}/esp_wifi" "${BSP_INC}/esp_event" "${BSP_INC}/freertos"
cp -r "${GLUE}/include/"* "${BSP_INC}/"
cp "${GLUE}/include/rtems-esp/sdkconfig.h" "${BSP_INC}/sdkconfig.h"
cp "${HAL}/esp_wifi/include/"*.h "${BSP_INC}/" 2>/dev/null || true
cp "${HAL}/esp_event/include/"*.h "${BSP_INC}/" 2>/dev/null || true
cp "${HAL}/esp_phy/include/"*.h "${BSP_INC}/" 2>/dev/null || true
cp "${HAL}/esp_common/include/"*.h "${BSP_INC}/" 2>/dev/null || true
cp "${HAL}/esp_rom/include/"*.h "${BSP_INC}/" 2>/dev/null || true
cp -r "${HAL}/soc/include/"* "${BSP_INC}/" 2>/dev/null || true
cp -r "${HAL}/soc/esp32c3/include/"* "${BSP_INC}/" 2>/dev/null || true
cp -r "${HAL}/esp_hw_support/include/"* "${BSP_INC}/" 2>/dev/null || true

# Insert before RAM load images advance the flash offset independently of rodata.
# Otherwise Wi-Fi log strings get different mapped and load offsets.
if ! grep -q 'wifi_iram' "${BSP_LIB}/linkcmds"; then
    echo "==> Splicing Wi-Fi IRAM/DRAM sections into ${BSP_LIB}/linkcmds..."
    awk -v base="${BSP_LIB}/linkcmds.base" -v frag="${GLUE}/ld/esp32c3-wifi-sections.ld" '
      /^[[:space:]]*INCLUDE[[:space:]]+linkcmds\.base/ {
        while ((getline line < base) > 0) {
          if (line ~ /^[[:space:]]*\.rwbarrier[[:space:]]*:/ && !spliced) {
            while ((getline f < frag) > 0) print f
            spliced = 1
          }
          print line
        }
        next
      }
      { print }
    ' "${BSP_LIB}/linkcmds" > "${BSP_LIB}/linkcmds.wifi"
    mv "${BSP_LIB}/linkcmds.wifi" "${BSP_LIB}/linkcmds"
fi

echo "================================================================"
echo "RTEMS 7 ESP32-C3 Wi-Fi & lwIP stack installed successfully!"
echo "  Libraries in: ${BSP_LIB}"
echo "    - liblwip.a"
echo "    - librtems_esp_wifi.a"
echo "    - libwpa.a"
echo "    - libmbedtls.a"
echo "    - libnet80211.a, libpp.a, libcore.a, libphy.a, etc."
echo "    - libbtdm_app_flash.a (C3 BLE controller)"
echo "================================================================"
