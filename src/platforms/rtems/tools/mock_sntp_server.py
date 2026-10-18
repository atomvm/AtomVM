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

import select
import socket
import struct
import sys
import time

NTP_DELTA = 2208988800

ports = [12300]
if len(sys.argv) > 1:
    try:
        ports = [int(p) for p in sys.argv[1:]]
    except ValueError:
        pass

sockets = []
for p in ports:
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind(("0.0.0.0", p))
        sockets.append(s)
        print(f"mock_sntp_server: listening on UDP port {p}", flush=True)
    except Exception as e:
        print(f"mock_sntp_server: note: could not bind {p}: {e}", file=sys.stderr, flush=True)

if not sockets:
    print("mock_sntp_server: error: no UDP ports could be bound", file=sys.stderr, flush=True)
    sys.exit(1)

while True:
    try:
        readable, _, _ = select.select(sockets, [], [])
        for s in readable:
            data, addr = s.recvfrom(1024)
            if len(data) >= 48:
                now = time.time()
                sec = int(now) + NTP_DELTA
                frac = int((now - int(now)) * (2**32))

                resp = bytearray(48)
                resp[0] = 0x24  # LI=0, VN=4, Mode=4 (Server)
                resp[1] = 1     # Stratum=1
                resp[2] = 4     # Poll=4
                resp[3] = 0xEC  # Precision=-20
                resp[12:16] = b"LOCL"
                struct.pack_into("!II", resp, 16, sec, frac)
                resp[24:32] = data[40:48]
                struct.pack_into("!II", resp, 32, sec, frac)
                struct.pack_into("!II", resp, 40, sec, frac)

                s.sendto(resp, addr)
                print(f"mock_sntp_server: replied to {addr} with timestamp {int(now)}", flush=True)
    except Exception as e:
        print(f"mock_sntp_server error: {e}", file=sys.stderr, flush=True)
        break
