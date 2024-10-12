#
# This file is part of AtomVM.
#
# Copyright 2018 Davide Bettio <davide@uninstall.it>
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

defmodule HelloWorld do
  # this compiler option is to suppress warnings when compiling the VM
  # it is not needed or recommended for user apps.
  @compile {:no_warn_undefined, [Console]}
  def start() do
    Console.print("Hello World\n")
    Console.puts("Console.puts() and Console.print() work with binary ")
    Console.puts(~c"or charlist strings.\n")
    Console.flush()
  end
end
