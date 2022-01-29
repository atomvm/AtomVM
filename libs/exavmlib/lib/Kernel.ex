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

  def inspect(term, opts \\ []) when is_list(opts) do
    case term do
      t when is_atom(t) ->
        :erlang.atom_to_binary(t, :latin1)

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
end
