# Copyright 2026 Peter M. <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

# Source revisions used by the successful RTEMS CI run 39153602941.
# Keep CI, Docker and native builds on the same sources. RSB also pins its
# compiler, binutils, newlib and RTEMS 6 kernel archives by revision and hash.
RTEMS_RSB6_COMMIT=84b3f5660ebaa96e1e405ca12f67b538b48636d6
RTEMS_RSB7_COMMIT=74fffa09a3df68b7e6cacabbde243ea7828edb34
RTEMS_KERNEL7_COMMIT=4bca987369df73718cdee1576323c9384fc728b2
RTEMS_LIBBSD_COMMIT=96319dbdbbd4a38244453e351a132bd728b27eb1
RTEMS_LWIP_COMMIT=f7962bdca9f5dbbfc8bf21b38896596acc911cfb
RTEMS_LWIP_WAF_COMMIT=8bd39e6047a93aca82d28638644b92cc7a141286
ESP_WIFI_COMMIT=fe6666fcb138979fe4c54c8790331ac6a3b0f567
ESP_PHY_COMMIT=20f1db053a0e6cb9f1c09d255c43bf42483041d0
ESP_HAL_COMMIT=f9f3710f964e536b4fa05d82776c09ed231268a4
# esp-hal-3rdparty pins this as its esp_coex/lib gitlink. The adapter-header
# (6b18c39) and Wi-Fi basic (6691b5d) MD5s match that HAL and esp32-wifi-lib.
ESP_COEX_COMMIT=fa152b3b6b2e3c5e3a57e55fcf4b005a4e28cef9
# ESP-IDF pins this submodule; the flash-only controller keeps its IRAM
# section to the ESP-IDF size instead of linking ~28 KiB more code.
ESP32C3_BT_COMMIT=0a08c4b32f3666003080b662a1a61794da24ff0f
RTEMS_ESP_WIFI_COMMIT=95129edf3086ae48ad61ef57b371129e2823ec45
RTEMS_MBEDTLS_COMMIT=22098d41c6620ce07cf8a0134d37302355e1e5ef
RTEMS_LINUX_DTS_COMMIT=830b3c68c1fb1e9176028d02ef86f3cf76aa2476
ESP_EMU_INSTALL_COMMIT=06102dc8a043e4f2ba6b2e3cabcf516c4ea84576
RTEMS_ESP_EMU_VERSION=0.49.0

# Erlang/OTP and rebar3 versions used by the CI workflow
# (.github/workflows/rtems-build.yaml). Keep CI, Docker and native builds on the
# same toolchain, so locally built AVM packs match the ones produced by CI.
OTP_VERSION=29.1.2
REBAR3_VERSION=3.27.3

# Fetch a full commit ID directly: git clone --branch does not accept a SHA.
# Keep patched cached sources when they match; reject stale caches rather than
# silently building a different revision or discarding local modifications.
rtems_clone_pinned() (
    set -eu
    rtems_url=$1
    rtems_commit=$2
    rtems_directory=$3
    case "$rtems_commit" in
        ''|*[!0-9a-f]*) echo "Expected a full Git commit ID: $rtems_commit" >&2; exit 1 ;;
    esac
    if [ "${#rtems_commit}" -ne 40 ]; then
        echo "Expected a full Git commit ID: $rtems_commit" >&2
        exit 1
    fi
    if [ ! -e "$rtems_directory/.git" ]; then
        git init -q "$rtems_directory" &&
            git -C "$rtems_directory" remote add origin "$rtems_url" || exit 1
    fi
    if ! git -C "$rtems_directory" rev-parse --verify HEAD >/dev/null 2>&1; then
        git -C "$rtems_directory" fetch --depth 1 origin "$rtems_commit" &&
            git -C "$rtems_directory" checkout --detach FETCH_HEAD || exit 1
    fi
    rtems_actual=$(git -C "$rtems_directory" rev-parse HEAD) || exit 1
    if [ "$rtems_actual" != "$rtems_commit" ]; then
        echo "$rtems_directory is at $rtems_actual; expected $rtems_commit. Clean the build cache before rebuilding." >&2
        exit 1
    fi
    echo "==> Using $rtems_url @ $rtems_commit"
)

# Install the pinned Erlang/OTP and rebar3.
# The OTP tarball is the very same prebuilt artifact that erlef/setup-beam
# installs in CI, so BEAM files are always produced by the same compiler.
rtems_install_otp() (
    set -eu
    otp_root=/opt/erlang
    case "$(dpkg --print-architecture)" in
        amd64) otp_arch=amd64 ;;
        arm64) otp_arch=arm64 ;;
        *) echo "Unsupported architecture: $(dpkg --print-architecture)" >&2; exit 1 ;;
    esac
    if ! command -v curl >/dev/null 2>&1; then
        apt-get update -qq
        apt-get install -y -qq curl
    fi
    mkdir -p "${otp_root}"
    curl -fsSL --retry 3 "https://builds.hex.pm/builds/otp/${otp_arch}/ubuntu-24.04/OTP-${OTP_VERSION}.tar.gz" \
        | tar -xz --strip-components=1 -C "${otp_root}"
    "${otp_root}/Install" -minimal "${otp_root}"
    rm -rf "${otp_root}"/erts-*/src "${otp_root}"/erts-*/man
    # Expose the toolchain through /usr/local/bin, which is already in PATH.
    for binary in "${otp_root}"/bin/*; do
        ln -sf "${binary}" "/usr/local/bin/$(basename "${binary}")"
    done
    curl -fsSL --retry 3 "https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" \
        -o /usr/local/bin/rebar3
    chmod +x /usr/local/bin/rebar3
    erl -noshell -eval 'io:format("Erlang/OTP ~s~n", [erlang:system_info(otp_release)]), halt().'
    rebar3 version
)
