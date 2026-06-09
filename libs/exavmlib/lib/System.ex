#
# This file is part of elixir-lang.
#
# Copyright 2012-2024 Elixir Contributors
# https://github.com/elixir-lang/elixir/blob/v1.17/lib/elixir/lib/system.ex
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
# SPDX-License-Identifier: Apache-2.0
#

defmodule System do
  @compile {:autoload, false}
  @type time_unit ::
          :second
          | :millisecond
          | :microsecond
          | :nanosecond
          | pos_integer()

  @doc """
  Returns the current monotonic time in the `:native` time unit.

  This time is monotonically increasing and starts in an unspecified
  point in time. This is not strictly monotonically increasing. Multiple
  sequential calls of the function may return the same value.

  Inlined by the compiler.
  """
  @spec monotonic_time() :: integer
  def monotonic_time do
    :erlang.monotonic_time()
  end

  @doc """
  Returns the current monotonic time in the given time unit.

  This time is monotonically increasing and starts in an unspecified
  point in time.
  """
  @spec monotonic_time(time_unit) :: integer
  def monotonic_time(unit) do
    :erlang.monotonic_time(unit)
  end

  @doc """
  Returns the current system time in the `:native` time unit.

  It is the VM view of the `os_time/0`. They may not match in
  case of time warps although the VM works towards aligning
  them. This time is not monotonic.

  Inlined by the compiler.
  """
  @spec system_time() :: integer
  def system_time do
    :erlang.system_time()
  end

  @doc """
  Returns the current system time in the given time unit.

  It is the VM view of the `os_time/0`. They may not match in
  case of time warps although the VM works towards aligning
  them. This time is not monotonic.
  """
  @spec system_time(time_unit) :: integer
  def system_time(unit) do
    :erlang.system_time(unit)
  end

  @doc """
  Returns the current operating system (OS) time.

  The result is returned in the `:native` time unit.

  This time may be adjusted forwards or backwards in time
  with no limitation and is not monotonic.

  Inlined by the compiler.
  """
  @spec os_time() :: integer
  def os_time do
    :os.system_time()
  end

  @doc """
  Returns the current operating system (OS) time in the given time `unit`.

  This time may be adjusted forwards or backwards in time
  with no limitation and is not monotonic.
  """
  @spec os_time(time_unit | :native) :: integer
  def os_time(unit) do
    :os.system_time(unit)
  end
end
