#
# This file is part of elixir-lang.
#
# Copyright 2012-2024 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.10.1/lib/elixir/lib/keyword.ex
#
# merge/2 take/2 pop/2/3 pop!/2 keyword?/1 has_key?/2 split/2 from:
# https://github.com/elixir-lang/elixir/blob/v1.16/lib/elixir/lib/keyword.ex
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

defmodule Keyword do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  def fetch(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> {:ok, value}
      false -> :error
    end
  end

  def fetch!(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> raise(KeyError, key: key, term: keywords)
    end
  end

  def get(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> default
    end
  end

  def get_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> fun.()
    end
  end

  def merge(keywords1, []) when is_list(keywords1), do: keywords1
  def merge([], keywords2) when is_list(keywords2), do: keywords2

  def merge(keywords1, keywords2) when is_list(keywords1) and is_list(keywords2) do
    if keyword?(keywords2) do
      fun = fn
        {key, _value} when is_atom(key) ->
          not has_key?(keywords2, key)

        _ ->
          raise ArgumentError,
                "expected a keyword list as the first argument, got: #{inspect(keywords1)}"
      end

      :lists.filter(fun, keywords1) ++ keywords2
    else
      raise ArgumentError,
            "expected a keyword list as the second argument, got: #{inspect(keywords2)}"
    end
  end

  def pop(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case fetch(keywords, key) do
      {:ok, value} -> {value, delete(keywords, key)}
      :error -> {default, keywords}
    end
  end

  def pop!(keywords, key) when is_list(keywords) and is_atom(key) do
    case fetch(keywords, key) do
      {:ok, value} -> {value, delete(keywords, key)}
      :error -> raise KeyError, key: key, term: keywords
    end
  end

  def put(keywords, key, value) when is_list(keywords) and is_atom(key) do
    [{key, value} | delete(keywords, key)]
  end

  def delete(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keymember(key, 1, keywords) do
      true -> delete_key(keywords, key)
      _ -> keywords
    end
  end

  def split(keywords, keys) when is_list(keywords) and is_list(keys) do
    fun = fn {k, v}, {take, drop} ->
      case k in keys do
        true -> {[{k, v} | take], drop}
        false -> {take, [{k, v} | drop]}
      end
    end

    acc = {[], []}
    {take, drop} = :lists.foldl(fun, acc, keywords)
    {:lists.reverse(take), :lists.reverse(drop)}
  end

  def take(keywords, keys) when is_list(keywords) and is_list(keys) do
    :lists.filter(fn {k, _} -> :lists.member(k, keys) end, keywords)
  end

  def keyword?([{key, _value} | rest]) when is_atom(key), do: keyword?(rest)
  def keyword?([]), do: true
  def keyword?(_other), do: false

  def has_key?(keywords, key) when is_list(keywords) and is_atom(key) do
    :lists.keymember(key, 1, keywords)
  end

  defp delete_key([{key, _} | tail], key), do: delete_key(tail, key)
  defp delete_key([{_, _} = pair | tail], key), do: [pair | delete_key(tail, key)]
  defp delete_key([], _key), do: []
end
