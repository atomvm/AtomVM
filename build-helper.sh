#!/bin/bash
# Build helper for jit-edge cherry-pick workflow
# Usage: ./build-helper.sh [nojit|jit]
# Builds AtomVM and runs tests, returns 0 on success

set -e

BUILD_TYPE="${1:-both}"
BUILD_DIR_NOJIT="/home/user/AtomVM/build-nojit"
BUILD_DIR_JIT="/home/user/AtomVM/build-jit"
ATOMVM_DIR="/home/user/AtomVM"

# Common cmake args
COMMON_CMAKE_ARGS=(
    -G Ninja
    -DPACKBEAM_PATH=/tmp/packbeam_work/atomvm_packbeam-0.8.2
    -DUF2TOOL_PATH=/tmp/uf2tool_src
)

setup_checkouts() {
    local BUILD_DIR="$1"
    mkdir -p "$BUILD_DIR/tools/packbeam/_checkouts"
    ln -sfn /tmp/packbeam_work/atomvm_packbeam-0.8.2 "$BUILD_DIR/tools/packbeam/_checkouts/atomvm_packbeam"
    mkdir -p "$BUILD_DIR/tools/uf2tool/_checkouts"
    ln -sfn /tmp/uf2tool_src "$BUILD_DIR/tools/uf2tool/_checkouts/uf2tool"
}

build_nojit() {
    echo "=== Building without JIT ==="
    rm -rf "$BUILD_DIR_NOJIT"
    mkdir -p "$BUILD_DIR_NOJIT"
    setup_checkouts "$BUILD_DIR_NOJIT"

    cmake "${COMMON_CMAKE_ARGS[@]}" \
        "$ATOMVM_DIR" -B "$BUILD_DIR_NOJIT" 2>&1 | grep -E "Error|Warning.*MbedTLS|Configuring done" || true

    ninja -C "$BUILD_DIR_NOJIT" -j4

    echo "=== Running test-erlang (non-JIT) ==="
    timeout 120 "$BUILD_DIR_NOJIT/tests/test-erlang" -s prime_smp

    echo "=== Running dialyzer (non-JIT) ==="
    ninja -C "$BUILD_DIR_NOJIT" dialyzer
}

build_jit() {
    echo "=== Building with JIT ==="
    rm -rf "$BUILD_DIR_JIT"
    mkdir -p "$BUILD_DIR_JIT"
    setup_checkouts "$BUILD_DIR_JIT"

    cmake "${COMMON_CMAKE_ARGS[@]}" \
        -DAVM_DISABLE_JIT=OFF \
        "$ATOMVM_DIR" -B "$BUILD_DIR_JIT" 2>&1 | grep -E "Error|Warning.*MbedTLS|Configuring done" || true

    ninja -C "$BUILD_DIR_JIT" -j4

    echo "=== Running test-erlang -b (JIT with beams) ==="
    # test_code_load_binary uses AtomVM-specific beam format, skip in OTP beam mode
    timeout 600 "$BUILD_DIR_JIT/tests/test-erlang" -b -s prime_smp,test_code_load_binary

    echo "=== Running test-erlang (JIT) ==="
    timeout 120 "$BUILD_DIR_JIT/tests/test-erlang" -s prime_smp

    echo "=== Running JIT tests ==="
    timeout 120 "$BUILD_DIR_JIT/src/AtomVM" "$BUILD_DIR_JIT/tests/libs/jit/test_jit.avm"

    echo "=== Running dialyzer (JIT) ==="
    ninja -C "$BUILD_DIR_JIT" dialyzer
}

run_erlfmt() {
    echo "=== Checking formatting with erlfmt ==="
    find "$ATOMVM_DIR" -name "*.erl" \
        -not -path "*/build*" \
        -not -path "*/_build*" \
        | xargs erlfmt -c
    echo "erlfmt: all files OK"
}

case "$BUILD_TYPE" in
    nojit)
        build_nojit
        ;;
    jit)
        build_jit
        ;;
    erlfmt)
        run_erlfmt
        ;;
    both)
        run_erlfmt
        build_nojit
        build_jit
        ;;
    *)
        echo "Usage: $0 [nojit|jit|erlfmt|both]"
        exit 1
        ;;
esac

echo "=== All done ==="
