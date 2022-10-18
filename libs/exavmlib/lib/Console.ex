#
# This file is part of AtomVM.
#
# Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
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

defmodule Console do
  @compile {:no_warn_undefined, [:console]}
  @compile {:no_warn_undefined, [AVMPort]}
  @moduledoc """
  Functions for writing to the console.
  """
  @doc """
  Print a string to the console.

  This is the preferred method for writing a string to the console
  when a micro controller (i.e. ESP32). This function uses less
  system resources, and should be faster in most cases.

  This operation will only write string data.
  The output is not suffixed with a newline character or sequence.
  To print an elixir/erlang term, use :erlang.display/1

  returns :ok if the data was written, or {:error, reason}, if there was
  an error.
  """
  @spec print(charlist() | binary()) :: :ok | {:error, term()}
  def print(string),
    do: :console.print(string)

  @doc """
  Print a string to the console.

  This operation will only write string data.
  The output is not suffixed with a newline character or sequence.
  To print an elixir/erlang term, use :erlang.display/1

  The first parameter is optional, and if supplied it will be used as
  the registered name for the console process, otherwise :stdio is used.

  returns :ok if the data was written, or {:error, reason}, if there was
  an error.
  """
  @spec puts(pid(), charlist() | binary()) :: :ok | {:error, term()}
  def puts(device \\ :stdio, item) do
    pid =
      case :erlang.whereis(device) do
        :undefined ->
          case device do
            :stdio ->
              new_pid = AVMPort.open({:spawn, "console"}, [])
              :erlang.register(:stdio, new_pid)
              new_pid

            _ ->
              :error
          end

        pid ->
          pid
      end

    write(pid, item)
  end

  defp write(console, string),
    do: AVMPort.call(console, {:puts, string})

  @doc """
  Flush any previously written data.

  Flush any data previously written using the `puts/1` function.

  Supplying the pid is only necessary if `puts/2` was used with an
  optional name other than :stdio.

  returns :ok if the data was written, or {:error, reason}, if there was
  an error.
  """
  @spec flush(pid()) :: :ok | {:error, term()}
  def flush(console \\ :erlang.whereis(:stdio)),
    do: AVMPort.call(console, :flush)
end
