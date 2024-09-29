#
# This file is part of AtomVM.
#
# Copyright 2024 Davide Bettio <davide@uninstall.it>
# Copyright 2012-2017 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.7.4/lib/elixir/lib/io.ex
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

defmodule IO do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  # Taken from Elixir io.ex
  @type chardata :: String.t() | maybe_improper_list(char | chardata, String.t() | [])

  # Taken from Elixir io.ex
  @doc """
  Converts chardata into a string.

  For more information about chardata, see the ["Chardata"](#module-chardata)
  section in the module documentation.

  In case the conversion fails, it raises an `UnicodeConversionError`.
  If a string is given, it returns the string itself.

  ## Examples

      iex> IO.chardata_to_string([0x00E6, 0x00DF])
      "æß"

      iex> IO.chardata_to_string([0x0061, "bc"])
      "abc"

      iex> IO.chardata_to_string("string")
      "string"

  """
  @spec chardata_to_string(chardata) :: String.t()
  def chardata_to_string(chardata)

  def chardata_to_string(string) when is_binary(string) do
    string
  end

  def chardata_to_string(list) when is_list(list) do
    List.to_string(list)
  end

  # Taken from Elixir io.ex
  @doc """
  Converts iodata (a list of integers representing bytes, lists
  and binaries) into a binary.
  The operation is Unicode unsafe.

  Notice that this function treats lists of integers as raw bytes
  and does not perform any kind of encoding conversion. If you want
  to convert from a charlist to a string (UTF-8 encoded), please
  use `chardata_to_string/1` instead.

  If this function receives a binary, the same binary is returned.

  Inlined by the compiler.

  ## Examples

      iex> bin1 = <<1, 2, 3>>
      iex> bin2 = <<4, 5>>
      iex> bin3 = <<6>>
      iex> IO.iodata_to_binary([bin1, 1, [2, 3, bin2], 4 | bin3])
      <<1, 2, 3, 1, 2, 3, 4, 5, 4, 6>>

      iex> bin = <<1, 2, 3>>
      iex> IO.iodata_to_binary(bin)
      <<1, 2, 3>>

  """
  @spec iodata_to_binary(iodata) :: binary
  def iodata_to_binary(item) do
    :erlang.iolist_to_binary(item)
  end

  def puts(string) do
    :io.put_chars([to_chardata(string), ?\n])
  end

  defp to_chardata(list) when is_list(list), do: list
  defp to_chardata(other), do: to_string(other)

  def inspect(t) do
    Kernel.inspect(t)
    |> puts()

    t
  end
end
