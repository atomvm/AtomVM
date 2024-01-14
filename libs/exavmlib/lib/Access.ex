#
# This file is part of elixir-lang.
#
# Copyright 2012-2019 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.10.1/lib/elixir/lib/access.ex
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

defmodule Access do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  def fetch(map, key) when is_map(map) do
    case map do
      %{^key => value} -> {:ok, value}
      _ -> :error
    end
  end

  def fetch(list, key) when is_list(list) and is_atom(key) do
    case :lists.keyfind(key, 1, list) do
      {_, value} -> {:ok, value}
      false -> :error
    end
  end

  def fetch(list, key) when is_list(list) do
    raise ArgumentError,
          "the Access calls for keywords expect the key to be an atom, got: " <> inspect(key)
  end

  def fetch(nil, _key) do
    :error
  end

  def fetch!(container, key) do
    case fetch(container, key) do
      {:ok, value} -> value
      :error -> raise(KeyError, key: key, term: container)
    end
  end

  def get(container, key, default \\ nil)

  def get(map, key, default) when is_map(map) do
    case map do
      %{^key => value} -> value
      _ -> default
    end
  end

  def get(list, key, default) when is_list(list) and is_atom(key) do
    case :lists.keyfind(key, 1, list) do
      {_, value} -> value
      false -> default
    end
  end

  def get(list, key, _default) when is_list(list) do
    raise ArgumentError,
          "the Access calls for keywords expect the key to be an atom, got: " <> inspect(key)
  end

  def get(nil, _key, default) do
    default
  end
end
