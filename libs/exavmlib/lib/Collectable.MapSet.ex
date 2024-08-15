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

defimpl Collectable, for: MapSet do
  def into(%@for{map: set} = map_set) do
    fun = fn
      list, {:cont, x} -> [x | list]
      list, :done -> %{map_set | map: :sets.union(set, :sets.from_list(list, version: 2))}
      _, :halt -> :ok
    end

    {[], fun}
  end
end
