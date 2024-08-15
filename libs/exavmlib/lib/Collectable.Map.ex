#
# This file is part of elixir-lang.
#
# Copyright 2012-2018 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.7.4/lib/elixir/lib/collectable.ex
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

defimpl Collectable, for: Map do
  def into(original) do
    fun = fn
      map, {:cont, {k, v}} -> :maps.put(k, v, map)
      map, :done -> map
      _, :halt -> :ok
    end

    {original, fun}
  end
end
