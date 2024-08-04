#
# This file is part of elixir-lang.
#
# Copyright 2012-2019 Elixir Contributors
# https://github.com/elixir-lang/elixir/blob/v1.11.4/lib/elixir/lib/map.ex
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

defmodule Map do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @type key :: any
  @type value :: any

  def new(list) when is_list(list), do: :maps.from_list(list)
  def new(%{} = map), do: map

  def has_key?(map, key), do: :maps.is_key(key, map)

  def fetch(map, key), do: :maps.find(key, map)

  def fetch!(map, key) do
    :maps.get(key, map)
  end

  def get(map, key, default \\ nil) do
    case map do
      %{^key => value} ->
        value

      %{} ->
        default

      other ->
        :erlang.error({:badmap, other}, [map, key, default])
    end
  end

  def put(map, key, value) do
    :maps.put(key, value, map)
  end

  def delete(map, key), do: :maps.remove(key, map)

  defdelegate merge(map1, map2), to: :maps

  def from_struct(struct) when is_atom(struct) do
    delete(struct.__struct__(), :__struct__)
  end

  def from_struct(%_{} = struct) do
    delete(struct, :__struct__)
  end

  def equal?(%{} = map1, %{} = map2), do: map1 === map2
  def equal?(%{} = map1, map2), do: :erlang.error({:badmap, map2}, [map1, map2])
  def equal?(term, other), do: :erlang.error({:badmap, term}, [term, other])

  @doc """
  Puts a value under `key` only if the `key` already exists in `map`.

  ## Examples

      iex> Map.replace(%{a: 1, b: 2}, :a, 3)
      %{a: 3, b: 2}

      iex> Map.replace(%{a: 1}, :b, 2)
      %{a: 1}

  """
  @doc since: "1.11.0"
  @spec replace(map, key, value) :: map
  def replace(map, key, value) do
    case map do
      %{^key => _value} ->
        %{map | key => value}

      %{} ->
        map

      other ->
        :erlang.error({:badmap, other})
    end
  end

  @doc """
  Puts a value under `key` only if the `key` already exists in `map`.

  If `key` is not present in `map`, a `KeyError` exception is raised.

  Inlined by the compiler.

  ## Examples

      iex> Map.replace!(%{a: 1, b: 2}, :a, 3)
      %{a: 3, b: 2}

      iex> Map.replace!(%{a: 1}, :b, 2)
      ** (KeyError) key :b not found in: %{a: 1}

  """
  @doc since: "1.5.0"
  @spec replace!(map, key, value) :: map
  def replace!(map, key, value) do
    :maps.update(key, value, map)
  end
end
