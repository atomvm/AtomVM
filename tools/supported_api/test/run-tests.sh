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

# One expected file serves both input modes: the output has no file names in it.
erlc -o "$tmp" fixtures/fixture_exports.erl

expect_output "extract_beam_exports.erl" expected/fixture_exports.funcs \
    "$tools/extract_beam_exports.erl" "$tmp/fixture_exports.beam"

echo "$tmp/fixture_exports.beam" > "$tmp/beams.txt"
expect_output "extract_beam_exports.erl --files-from" expected/fixture_exports.funcs \
    "$tools/extract_beam_exports.erl" --files-from "$tmp/beams.txt"

escript "$tools/extract_beam_exports.erl" -o "$tmp/fixture_exports.funcs" \
    "$tmp/fixture_exports.beam"

escript "$tools/extract_gperf_registrations.erl" -o "$tmp/check.txt" fixtures/check.gperf
expect_output "check_native_stubs.erl" expected/check_native_stubs.txt \
    "$tools/check_native_stubs.erl" --registrations "$tmp/check.txt" \
    "$tmp/fixture_exports.funcs"

escript "$tools/extract_gperf_registrations.erl" -o "$tmp/check_missing.txt" \
    fixtures/check_missing.gperf
expect_failure "check_native_stubs.erl reports a registration without an export" \
    "$tools/check_native_stubs.erl" --registrations "$tmp/check_missing.txt" \
    "$tmp/fixture_exports.funcs"

expect_output "extract_instructions.erl" expected/instructions.txt \
    "$tools/extract_instructions.erl" fixtures/sample_opcodes.def

expect_failure "extract_instructions.erl rejects an unknown X macro" \
    "$tools/extract_instructions.erl" fixtures/unknown_macro_opcodes.def

if [ "$failures" -ne 0 ]; then
    echo "$failures test(s) failed" >&2
    exit 1
fi
echo "all tests passed"
