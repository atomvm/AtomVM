#!/bin/sh
#
# This file is part of AtomVM.
#
# Copyright 2026 Davide Bettio <davide@uninstall.it>
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

# Golden tests for the supported API tools. Run from anywhere:
#
#     tools/supported_api/test/run-tests.sh
#
# Tools are invoked from this directory so that the file locations they print
# stay relative, and therefore stable.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
tools=$(CDPATH= cd -- "$here/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$here"

failures=0

pass() {
    echo "ok: $1"
}

fail() {
    echo "FAIL: $1" >&2
    failures=$((failures + 1))
}

expect_output() {
    description=$1
    expected=$2
    shift 2
    if ! escript "$@" > "$tmp/out" 2> "$tmp/err"; then
        cat "$tmp/err" >&2
        fail "$description: exited non-zero"
        return 0
    fi
    if diff -u "$expected" "$tmp/out"; then
        pass "$description"
    else
        fail "$description: output differs from $expected"
    fi
}

expect_failure() {
    description=$1
    shift
    if escript "$@" > "$tmp/out" 2> "$tmp/err"; then
        fail "$description: exited zero"
    else
        pass "$description"
    fi
}

expect_output "extract_gperf_registrations.erl" expected/registrations.txt \
    "$tools/extract_gperf_registrations.erl" fixtures/sample.gperf

expect_failure "extract_gperf_registrations.erl rejects a non-registration line" \
    "$tools/extract_gperf_registrations.erl" fixtures/unparseable.gperf

if [ "$failures" -ne 0 ]; then
    echo "$failures test(s) failed" >&2
    exit 1
fi
echo "all tests passed"
