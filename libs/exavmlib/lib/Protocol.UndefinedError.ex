#
# This file is part of elixir-lang.
#
# Copyright 2012-2018 Elixir Contributors
# https://github.com/elixir-lang/elixir/tree/v1.7.4/lib/elixir/lib/exception.ex
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

defmodule Protocol.UndefinedError do
  defexception [:protocol, :value, description: ""]

  @impl true
  def message(%{protocol: protocol, value: value, description: description}) do
    "protocol #{inspect(protocol)} not implemented for #{inspect(value)}" <>
      maybe_description(description) <> maybe_available(protocol)
  end

  defp maybe_description(""), do: ""
  defp maybe_description(description), do: ", " <> description

  defp maybe_available(protocol) do
    case protocol.__protocol__(:impls) do
      {:consolidated, []} ->
        ". There are no implementations for this protocol."

      {:consolidated, types} ->
        ". This protocol is implemented for: #{Enum.map_join(types, ", ", &inspect/1)}"

      :not_consolidated ->
        ""
    end
  end
end
