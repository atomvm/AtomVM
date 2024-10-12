#
# This file is part of AtomVM.
#
# Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
# Copyright 2022 Winford (Uncle Grumpy) <dwinford@proton.me>
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

defmodule AVMPort do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}
  @moduledoc """
  This module provides an interface to communicate with AtomVM port processes.
  The functionality of AVMPort.call is identical to the eavmlib :port.call/2 and
  :port.call/3 functions. AVMPort.open provides elixir native functionality of
  :erlang.open_port/2.
  """

  @spec call(pid(), term()) :: term()
  def call(pid, message) do
    :port.call(pid, message)
  end

  @spec call(pid(), term(), non_neg_integer()) :: term()
  def call(pid, message, timeoutMs) do
    :port.call(pid, message, timeoutMs)
  end

  @spec open(term(), list()) :: pid()
  def open(port_name, options) do
    :erlang.open_port(port_name, options)
  end
end
