//
// This file is part of AtomVM.
//
// Copyright 2025 Paul Guyot <pguyot@kallisys.net>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
//

import gleam/io
import gleam_avm/atomvm

pub fn main() {
  io.println("Hello from Gleam !")
  io.println("The AtomVM platform we are running on is:")
  io.debug(atomvm.platform())
}

// AtomVM currently requires a start/0 function instead of main/0
pub fn start() {
  main()
}
