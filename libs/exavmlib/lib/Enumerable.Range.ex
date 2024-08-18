#
# This file is part of elixir-lang.
#
# Copyright 2012-2020 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.10.4/lib/elixir/lib/enum.ex
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

defimpl Enumerable, for: Range do
  def reduce(first..last, acc, fun) do
    reduce(first, last, acc, fun, _up? = last >= first)
  end

  defp reduce(_first, _last, {:halt, acc}, _fun, _up?) do
    {:halted, acc}
  end

  defp reduce(first, last, {:suspend, acc}, fun, up?) do
    {:suspended, acc, &reduce(first, last, &1, fun, up?)}
  end

  defp reduce(first, last, {:cont, acc}, fun, _up? = true) when first <= last do
    reduce(first + 1, last, fun.(first, acc), fun, _up? = true)
  end

  defp reduce(first, last, {:cont, acc}, fun, _up? = false) when first >= last do
    reduce(first - 1, last, fun.(first, acc), fun, _up? = false)
  end

  defp reduce(_, _, {:cont, acc}, _fun, _up) do
    {:done, acc}
  end

  def member?(first..last, value) when is_integer(value) do
    if first <= last do
      {:ok, first <= value and value <= last}
    else
      {:ok, last <= value and value <= first}
    end
  end

  def member?(_.._, _value) do
    {:ok, false}
  end

  def count(first..last) do
    if first <= last do
      {:ok, last - first + 1}
    else
      {:ok, first - last + 1}
    end
  end

  def slice(first..last) do
    if first <= last do
      {:ok, last - first + 1, &slice_asc(first + &1, &2)}
    else
      {:ok, first - last + 1, &slice_desc(first - &1, &2)}
    end
  end

  defp slice_asc(current, 1), do: [current]
  defp slice_asc(current, remaining), do: [current | slice_asc(current + 1, remaining - 1)]

  defp slice_desc(current, 1), do: [current]
  defp slice_desc(current, remaining), do: [current | slice_desc(current - 1, remaining - 1)]
end
