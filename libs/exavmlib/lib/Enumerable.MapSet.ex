#
# This file is part of elixir-lang.
#
# Copyright 2012-2024 Elixir Contributors
# https://github.com/elixir-lang/elixir/tree/v1.17.2/lib/elixir/lib/map_set.ex
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

defimpl Enumerable, for: MapSet do
  def count(map_set) do
    {:ok, MapSet.size(map_set)}
  end

  def member?(map_set, val) do
    {:ok, MapSet.member?(map_set, val)}
  end

  def reduce(map_set, acc, fun) do
    Enumerable.List.reduce(MapSet.to_list(map_set), acc, fun)
  end

  def slice(map_set) do
    size = MapSet.size(map_set)
    {:ok, size, &Enumerable.List.slice(MapSet.to_list(map_set), &1, &2, size)}
  end
end
