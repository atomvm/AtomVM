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

import socket
import time
import sys


def checksum(data):
    return f"{sum(data.encode()) % 256:02x}"

def send_packet(s, payload):
    pkt = f"${payload}#{checksum(payload)}"
    s.sendall(pkt.encode())
    # read ack '+'
    ack = s.recv(1)
    # read response
    resp = b""
    while True:
        ch = s.recv(1)
        if ch == b"$":
            break
    while True:
        ch = s.recv(1)
        if ch == b"#":
            s.recv(2) # cksum
            break
        resp += ch
    s.sendall(b"+")
    return resp.decode()

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("127.0.0.1", 1234))
print("Connected to GDB stub")

# Stop target with interrupt 0x03
s.sendall(b"\x03")
time.sleep(0.1)
try:
    resp = s.recv(1024)
    print("Interrupt response:", resp)
except Exception as e:
    print("Recv err:", e)

# Read registers
regs_hex = send_packet(s, "g")
print(f"Registers ({len(regs_hex)} chars): {regs_hex}")

# Unpack 32-bit registers (little endian)
regs = []
for i in range(0, len(regs_hex), 8):
    chunk = regs_hex[i:i+8]
    if len(chunk) == 8:
        val = int.from_bytes(bytes.fromhex(chunk), 'little')
        regs.append(val)

reg_names = [
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
    "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
    "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6",
    "pc"
]

for idx, val in enumerate(regs[:len(reg_names)]):
    print(f"{reg_names[idx]:>5}: 0x{val:08x}")

def read_mem(addr, length):
    hex_data = send_packet(s, f"m{addr:x},{length:x}")
    return bytes.fromhex(hex_data)

tasks_info = read_mem(0x4081806c, 0x3c)
print(f"_RTEMS_tasks_Information: {tasks_info.hex()}")
# Objects_Information layout:
# maximum: uint32 at offset 0 (or Objects_Maximum_type at offset 8?)
# local_table: Objects_Control ** at offset 16 or 24 or 32
for off in range(0, 0x3c, 4):
    val = int.from_bytes(tasks_info[off:off+4], 'little')
    print(f"  +0x{off:02x}: 0x{val:08x}")

table_ptr = int.from_bytes(tasks_info[0x18:0x1c], 'little')
print(f"local_table pointer: 0x{table_ptr:08x}")
table_mem = read_mem(table_ptr, 64)
for i in range(0, len(table_mem), 4):
    tcb = int.from_bytes(table_mem[i:i+4], 'little')
    if tcb != 0:
        # read first 64 bytes of TCB (Object name is usually at +0x08 or +0x0c)
        tcb_mem = read_mem(tcb, 64)
        name_bytes = tcb_mem[12:16]
        name_str = "".join([chr(b) if 32 <= b <= 126 else f"\\x{b:02x}" for b in name_bytes])
        print(f"  Task #{i//4}: TCB=0x{tcb:08x}, Name={name_str!r}, raw={tcb_mem[:16].hex()}")
        # print stack and state: state is at offset in TCB (e.g. current_state)
        # In RTEMS 7 RISC-V, let's dump first 0x100 of TCB
        # Wait_information / states


