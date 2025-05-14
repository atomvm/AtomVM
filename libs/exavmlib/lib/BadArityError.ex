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

defmodule BadArityError do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  defexception [:function, :args]

  @impl true
  def message(exception) do
    fun = exception.function
    args = exception.args
    insp = Enum.map_join(args, ", ", &inspect/1)

    {:arity, arity} = Function.info(fun, :arity)
    "#{inspect(fun)} with arity #{arity} called with #{count(length(args), insp)}"
    "#{inspect(fun)} called with #{count(length(args), insp)}"
  end

  defp count(0, _insp), do: "no arguments"
  defp count(1, insp), do: "1 argument (#{insp})"
  defp count(x, insp), do: "#{x} arguments (#{insp})"
end
