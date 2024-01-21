#
# This file is part of AtomVM.
#
# Copyright 2020 Davide Bettio <davide@uninstall.it>
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

defmodule Kernel do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @doc """
  Returns an integer which is the arithmetical absolute value of `number`.

  ## Examples
    iex> abs(3)
    3
    iex> abs(-3)
    3
  """
  def abs(number), do: :erlang.abs(number)

  def div(dividend, divisor), do: :erlang.div(dividend, divisor)

  def rem(dividend, divisor), do: :erlang.rem(dividend, divisor)

  def inspect(term, opts \\ []) when is_list(opts) do
    case term do
      t when is_atom(t) ->
        [?:, atom_to_string(t)]

      t when is_integer(t) ->
        :erlang.integer_to_binary(t)

      t when is_list(t) ->
        # TODO: escape unprintable lists
        :erlang.list_to_binary(t)

      t when is_pid(t) ->
        :erlang.pid_to_list(t)
        |> :erlang.list_to_binary()

      t when is_function(t) ->
        :erlang.fun_to_list(t)
        |> :erlang.list_to_binary()

      t when is_tuple(t) ->
        [?{ | t |> :erlang.tuple_to_list() |> inspect_join(?})]
        |> :erlang.list_to_binary()

      t when is_binary(t) ->
        # TODO: escape unprintable binaries
        t

      t when is_reference(t) ->
        :erlang.ref_to_list(t)
        |> :erlang.list_to_binary()

      t when is_float(t) ->
        :erlang.float_to_binary(t)

      t when is_map(t) ->
        [?%, ?{ | t |> inspect_kv() |> join(?})]
        |> :erlang.list_to_binary()
    end
  end

  defp inspect_join([], last) do
    [last]
  end

  defp inspect_join([e], last) do
    [inspect(e), last]
  end

  defp inspect_join([h | t], last) do
    [inspect(h), ?,, ?\s | inspect_join(t, last)]
  end

  defp join([], last) do
    [last]
  end

  defp join([e], last) do
    [e, last]
  end

  defp join([h | t], last) do
    [h, ?,, ?\s | join(t, last)]
  end

  defp inspect_kv(t) do
    :maps.fold(
      fn
        k, v, acc_in when is_atom(k) ->
          [[atom_to_string(k), ?:, ?\s, inspect(v)] | acc_in]

        k, v, acc_in ->
          [[inspect(k), " => ", inspect(v)] | acc_in]
      end,
      [],
      t
    )
  end

  defp atom_to_string(atom) do
    # TODO: use unicode rather than plain latin1
    # handle spaces and special characters
    :erlang.atom_to_binary(atom, :latin1)
  end
end
