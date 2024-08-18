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

defimpl Enumerable, for: Map do
  def count(map) do
    {:ok, map_size(map)}
  end

  def member?(map, {key, value}) do
    {:ok, match?(%{^key => ^value}, map)}
  end

  def member?(_map, _other) do
    {:ok, false}
  end

  def slice(map) do
    size = map_size(map)
    {:ok, size, &Enumerable.List.slice(:maps.to_list(map), &1, &2, size)}
  end

  def reduce(map, acc, fun) do
    reduce_list(:maps.to_list(map), acc, fun)
  end

  defp reduce_list(_list, {:halt, acc}, _fun), do: {:halted, acc}
  defp reduce_list(list, {:suspend, acc}, fun), do: {:suspended, acc, &reduce_list(list, &1, fun)}
  defp reduce_list([], {:cont, acc}, _fun), do: {:done, acc}
  defp reduce_list([head | tail], {:cont, acc}, fun), do: reduce_list(tail, fun.(head, acc), fun)
end
