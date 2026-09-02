#!/usr/bin/env python3
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

"""Probe QEMU user-mode hostfwd until the guest echo server answers."""

from __future__ import annotations

import argparse
import socket
import sys
import time


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8080)
    parser.add_argument("--timeout", type=float, default=60.0)
    parser.add_argument("--payload", default="ping")
    args = parser.parse_args()

    deadline = time.monotonic() + args.timeout
    payload = args.payload.encode()
    last_error = "no attempt"
    while time.monotonic() < deadline:
        try:
            with socket.create_connection((args.host, args.port), timeout=2.0) as sock:
                sock.sendall(payload)
                data = sock.recv(len(payload))
                if data == payload:
                    print(f"hostfwd-ok {args.host}:{args.port}")
                    return 0
                last_error = f"unexpected reply {data!r}"
        except OSError as exc:
            last_error = str(exc)
        time.sleep(0.5)

    print(f"hostfwd probe failed: {last_error}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
