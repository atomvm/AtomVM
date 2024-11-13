/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

import * as fs from "fs";
import { RP2040, ConsoleLogger, LogLevel } from "rp2040js";

import { decodeBlock } from "uf2";
import fetch from "sync-fetch";

const FLASH_START_ADDRESS = 0x10000000;

function loadBootrom(rp2040: RP2040) {
  const ROM_ELF_URL =
    "https://github.com/raspberrypi/pico-bootrom/releases/download/b1/b1.elf";
  const response = fetch(ROM_ELF_URL);
  if (!response.ok) {
    throw new Error(`Error fetching bootrom: ${response.statusText}`);
  }
  // Skip ELF header and only keep 16 KB
  const body = response.arrayBuffer();
  const bootrom: Uint32Array = new Uint32Array(body.slice(0x10000, 0x14000));
  rp2040.loadBootrom(bootrom);
}

function loadUF2(filename: string, rp2040: RP2040) {
  const file = fs.openSync(filename, "r");
  const buffer = new Uint8Array(512);
  while (fs.readSync(file, buffer) === buffer.length) {
    const block = decodeBlock(buffer);
    const { flashAddress, payload } = block;
    rp2040.flash.set(payload, flashAddress - FLASH_START_ADDRESS);
  }
  fs.closeSync(file);
}

function loadAvm(filename: string, rp2040: RP2040) {
  const file = fs.openSync(filename, "r");
  const buffer = new Uint8Array(512);
  let addr = 0x100a0000;
  while (fs.readSync(file, buffer) > 0) {
    rp2040.flash.set(buffer, addr - FLASH_START_ADDRESS);
    addr += buffer.length;
  }
  fs.closeSync(file);
}

const mcu = new RP2040();
loadBootrom(mcu);

for (let arg of process.argv.slice(2)) {
  if (arg.endsWith(".uf2")) {
    loadUF2(arg, mcu);
  } else if (arg.endsWith(".avm")) {
    loadAvm(arg, mcu);
  } else {
    console.error(`Unknown argument ${arg}`);
    console.error(
      `Syntax: ${process.argv[0]} ${process.argv[1]} [file.uf2 [file2.uf2 ...]]`,
    );
  }
}

mcu.logger = new ConsoleLogger(LogLevel.Error);

let currentLine = "";
mcu.uart[0].onByte = (value) => {
  process.stdout.write(new Uint8Array([value]));

  const char = String.fromCharCode(value);
  if (char === "\n") {
    if (currentLine === "OK" || currentLine === "OK\r") {
      process.exit(0);
    } else if (currentLine === "FAIL" || currentLine === "FAIL\r") {
      process.exit(1);
    }
    currentLine = "";
  } else {
    currentLine += char;
  }
};

mcu.core.PC = 0x10000000;
mcu.execute();
