#!/usr/bin/env bash
# Copyright 2026 Peter M. <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPENDENCIES="${SCRIPT_DIR}/../docker/dependencies.sh"
TEST_DIR="$(mktemp -d)"
trap 'rm -rf "$TEST_DIR"' EXIT

git init -q "$TEST_DIR/upstream"
printf 'first\n' > "$TEST_DIR/upstream/source"
git -C "$TEST_DIR/upstream" add source
git -C "$TEST_DIR/upstream" -c user.name=Test -c user.email=test@example.org commit -qm first
FIRST="$(git -C "$TEST_DIR/upstream" rev-parse HEAD)"
printf 'second\n' > "$TEST_DIR/upstream/source"
git -C "$TEST_DIR/upstream" add source
git -C "$TEST_DIR/upstream" -c user.name=Test -c user.email=test@example.org commit -qm second
SECOND="$(git -C "$TEST_DIR/upstream" rev-parse HEAD)"

clone_pinned() {
    "$TEST_SHELL" -eu -c '. "$1"; rtems_clone_pinned "$2" "$3" "$4"' \
        source-pins "$DEPENDENCIES" "$1" "$2" "$3"
}

# Docker RUN uses /bin/sh; the native builders and Actions use Bash.
for TEST_SHELL in sh bash; do
    CHECKOUT="$TEST_DIR/checkout-$TEST_SHELL"
    clone_pinned "$TEST_DIR/upstream" "$FIRST" "$CHECKOUT"
    test "$(git -C "$CHECKOUT" rev-parse HEAD)" = "$FIRST"
    test "$(cat "$CHECKOUT/source")" = first
    if git -C "$CHECKOUT" symbolic-ref -q HEAD; then
        echo 'Pinned checkout must have a detached HEAD' >&2
        exit 1
    fi

    printf 'patched\n' > "$CHECKOUT/source"
    clone_pinned "$TEST_DIR/upstream" "$FIRST" "$CHECKOUT"
    test "$(cat "$CHECKOUT/source")" = patched
    if clone_pinned "$TEST_DIR/upstream" "$SECOND" "$CHECKOUT" > "$TEST_DIR/expected-error.log" 2>&1; then
        echo 'A stale cached checkout must be rejected' >&2
        exit 1
    fi
    test "$(git -C "$CHECKOUT" rev-parse HEAD)" = "$FIRST"
    test "$(cat "$CHECKOUT/source")" = patched

    for INVALID in main "${FIRST:0:7}"; do
        if clone_pinned "$TEST_DIR/upstream" "$INVALID" "$TEST_DIR/invalid-$TEST_SHELL" > "$TEST_DIR/expected-error.log" 2>&1; then
            echo 'Branches and abbreviated commit IDs must be rejected' >&2
            exit 1
        fi
    done
    test ! -e "$TEST_DIR/invalid-$TEST_SHELL"
    if clone_pinned "$TEST_DIR/missing-$TEST_SHELL" "$FIRST" "$TEST_DIR/failed-$TEST_SHELL" > "$TEST_DIR/expected-error.log" 2>&1; then
        echo 'A failed fetch must fail the checkout' >&2
        exit 1
    fi
    git clone --bare -q "$TEST_DIR/upstream" "$TEST_DIR/missing-$TEST_SHELL"
    clone_pinned "$TEST_DIR/missing-$TEST_SHELL" "$FIRST" "$TEST_DIR/failed-$TEST_SHELL"
    test "$(git -C "$TEST_DIR/failed-$TEST_SHELL" rev-parse HEAD)" = "$FIRST"
done

echo 'Pinned source checkout tests passed'
