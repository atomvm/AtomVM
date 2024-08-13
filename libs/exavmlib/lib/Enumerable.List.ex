#
# This file is part of elixir-lang.
#
# Copyright 2012-2017 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.10.1/lib/elixir/lib/enumerable.ex
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

defimpl Enumerable, for: List do
  def count(_list), do: {:error, __MODULE__}
  def member?(_list, _value), do: {:error, __MODULE__}
  def slice(_list), do: {:error, __MODULE__}

  def reduce(_list, {:halt, acc}, _fun), do: {:halted, acc}
  def reduce(list, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
  def reduce([], {:cont, acc}, _fun), do: {:done, acc}
  def reduce([head | tail], {:cont, acc}, fun), do: reduce(tail, fun.(head, acc), fun)

  @doc false
  def slice(_list, _start, 0, _size), do: []
  def slice(list, start, count, size) when start + count == size, do: list |> drop(start)
  def slice(list, start, count, _size), do: list |> drop(start) |> take(count)

  defp drop(list, 0), do: list
  defp drop([_ | tail], count), do: drop(tail, count - 1)

  defp take(_list, 0), do: []
  defp take([head | tail], count), do: [head | take(tail, count - 1)]
end
