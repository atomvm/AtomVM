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
DEFAULT_BSP="riscv/esp32c3db"
BUILD_DIR="${HOME}/.cache/rtems7-build"
PREFIX="${DEFAULT_PREFIX}"
BSP="${DEFAULT_BSP}"
RTEMS_SRC_DIR="${RTEMS_SRC_DIR:-}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "${SCRIPT_DIR}/../docker/dependencies.sh"
CLEAN=0

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Builds RTEMS 7 RISC-V cross-compiler tools and BSP from pinned upstream commits.

Options:
  --prefix <path>     Target installation directory (default: ${DEFAULT_PREFIX})
  --bsp <arch/bsp>    RTEMS BSP to build (default: ${DEFAULT_BSP})
  --build-dir <path>  Temporary build directory (default: ${BUILD_DIR})
  --rtems-src <path>  Path to local RTEMS source tree (or env RTEMS_SRC_DIR)
  --clean             Wipe the build directory before building
  -h, --help          Show this help message

Example:
  $(basename "$0") --prefix /opt/rtems/7 --bsp riscv/esp32c3db
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --prefix)
            PREFIX="$2"
            shift 2
            ;;
        --bsp)
            BSP="$2"
            shift 2
            ;;
        --build-dir)
            BUILD_DIR="$2"
            shift 2
            ;;
        --rtems-src)
            RTEMS_SRC_DIR="$2"
            shift 2
            ;;
        --clean)
            CLEAN=1
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            ;;
    esac
done

echo "================================================================"
echo "RTEMS 7 Toolchain & BSP Builder"
echo "  Target Prefix: ${PREFIX}"
echo "  Target BSP:    ${BSP}"
echo "  Build Dir:     ${BUILD_DIR}"
echo "================================================================"

# Check host dependencies
command -v git >/dev/null 2>&1 || { echo "Error: git is required" >&2; exit 1; }
command -v curl >/dev/null 2>&1 || { echo "Error: curl is required" >&2; exit 1; }
command -v python3 >/dev/null 2>&1 || { echo "Error: python3 is required" >&2; exit 1; }

if [ "${CLEAN}" -eq 1 ] && [ -d "${BUILD_DIR}" ]; then
    echo "==> Cleaning build directory: ${BUILD_DIR}"
    rm -rf "${BUILD_DIR}"
fi

mkdir -p "${BUILD_DIR}" "${PREFIX}"

RSB_REPO="https://github.com/RTEMS/rtems-source-builder.git"
RTEMS_REPO="https://github.com/RTEMS/rtems.git"

# Step 1: Fetch the pinned RSB revision
cd "${BUILD_DIR}"
rtems_clone_pinned "${RSB_REPO}" "${RTEMS_RSB7_COMMIT}" rsb

# Step 2: Build RISC-V tools via RSB
echo "==> Building RTEMS 7 RISC-V toolchain with RSB (this may take 15-30 mins)..."
cd "${BUILD_DIR}/rsb/rtems"

# On macOS, Homebrew python provides full libpython flags needed by RSB/GDB
RSB_PYTHON_ARGS=""
if [[ "$(uname -s)" == "Darwin" ]]; then
    if [ -x "/opt/homebrew/bin/python3" ]; then
        RSB_PYTHON_ARGS="--with-python-version=/opt/homebrew/bin/python3"
    elif command -v brew >/dev/null 2>&1 && [ -x "$(brew --prefix)/bin/python3" ]; then
        RSB_PYTHON_ARGS="--with-python-version=$(brew --prefix)/bin/python3"
    fi
fi

# Strip obsolete sis simulator, gdb, and broken gitlab multilibs patch from RSB configs
sed -i.bak '/sis/d' config/7/rtems-riscv.bset && rm -f config/7/rtems-riscv.bset.bak
sed -i.bak '/with_rtems_gdb/d' config/tools/rtems-default-tools.bset && rm -f config/tools/rtems-default-tools.bset.bak
sed -i.bak -e '/0001-RTEMS-Add-riscv-multilibs/d' -e '/uCjG1KIhPuCR3H5G/d' config/tools/rtems-gcc-15.2-newlib-head.cfg && rm -f config/tools/rtems-gcc-15.2-newlib-head.cfg.bak

# gmplib.org is not reachable from every environment, and dl.rtems.org has
# been unreliable for MPFR. CI prefetches both from GNU's mirror; do the same.
sed -i.bak 's|https://gmplib.org/download/gmp|https://ftp.gnu.org/gnu/gmp|g' ../source-builder/config/gmp.cfg && rm -f ../source-builder/config/gmp.cfg.bak
mkdir -p sources
curl -fSL --retry 3 --retry-all-errors https://ftp.gnu.org/gnu/gmp/gmp-6.3.0.tar.bz2 -o sources/gmp-6.3.0.tar.bz2
curl -fSL --retry 3 --retry-all-errors https://ftp.gnu.org/gnu/mpfr/mpfr-4.2.1.tar.bz2 -o sources/mpfr-4.2.1.tar.bz2

../source-builder/sb-set-builder --prefix="${PREFIX}" ${RSB_PYTHON_ARGS} 7/rtems-riscv

export PATH="${PREFIX}/bin:${PATH}"

echo "==> Toolchain installed successfully:"
"${PREFIX}/bin/riscv-rtems7-gcc" --version | head -n 1

# Step 3: RTEMS kernel source (local checkout or clone)
if [ -n "${RTEMS_SRC_DIR}" ]; then
    if [ ! -d "${RTEMS_SRC_DIR}" ]; then
        echo "Error: Specified RTEMS source directory does not exist: ${RTEMS_SRC_DIR}" >&2
        exit 1
    fi
    RTEMS_SRC_DIR="$(cd "${RTEMS_SRC_DIR}" && pwd)"
    echo "==> Using local RTEMS kernel source: ${RTEMS_SRC_DIR}"
    RTEMS_BUILD_DIR="${RTEMS_SRC_DIR}"
else
    cd "${BUILD_DIR}"
    rtems_clone_pinned "${RTEMS_REPO}" "${RTEMS_KERNEL7_COMMIT}" rtems
    RTEMS_BUILD_DIR="${BUILD_DIR}/rtems"
fi

# Step 4: Configure and build BSP via waf
echo "==> Configuring and building RTEMS BSP: ${BSP}..."
cd "${RTEMS_BUILD_DIR}"
./waf configure --prefix="${PREFIX}" --rtems-bsps="${BSP}"
./waf
./waf install

echo "================================================================"
echo "RTEMS 7 RISC-V build complete!"
echo "  Prefix: ${PREFIX}"
echo "  Tools:  ${PREFIX}/bin"
echo ""
echo "To use with AtomVM:"
echo "  export PATH=\"${PREFIX}/bin:\$PATH\""
echo "  cmake -S src/platforms/rtems -B src/platforms/rtems/build-esp32c3 -G Ninja \\"
echo "      -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \\"
echo "      -DRTEMS_PREFIX=\"${PREFIX}\" \\"
echo "      -DRTEMS_BSP=${BSP} \\"
echo "      -DRTEMS_BOARD=esp32c3 \\"
echo "      -DAVM_PACK=/path/to/atomvmlib-rtems.avm"
echo "  cmake --build src/platforms/rtems/build-esp32c3"
echo "================================================================"
