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

defmodule FunctionClauseError do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  defexception [:module, :function, :arity, :kind, :args, :clauses]

  @impl true
  def message(exception) do
    case exception do
      %{function: nil} ->
        "no function clause matches"

      %{module: module, function: function, arity: arity} ->
        formatted = Exception.format_mfa(module, function, arity)
        "no function clause matching in #{formatted}"
    end
  end
end
