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

defmodule ArgumentError do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  defexception message: "argument error"

  @impl true
  def blame(
        %{message: "argument error"} = exception,
        [{:erlang, :apply, [module, function, args], _} | _] = stacktrace
      ) do
    message =
      cond do
        # Note that args may be an empty list even if they were supplied
        not is_atom(module) and is_atom(function) and args == [] ->
          "you attempted to apply #{inspect(function)} on #{inspect(module)}. " <>
            "If you are using apply/3, make sure the module is an atom. " <>
            "If you are using the dot syntax, such as map.field or module.function, " <>
            "make sure the left side of the dot is an atom or a map"

        not is_atom(module) ->
          "you attempted to apply a function on #{inspect(module)}. " <>
            "Modules (the first argument of apply) must always be an atom"

        not is_atom(function) ->
          "you attempted to apply #{inspect(function)} on module #{inspect(module)}. " <>
            "Functions (the second argument of apply) must always be an atom"

        not is_list(args) ->
          "you attempted to apply #{inspect(function)} on module #{inspect(module)} " <>
            "with arguments #{inspect(args)}. Arguments (the third argument of apply) must always be a list"
      end

    {%{exception | message: message}, stacktrace}
  end

  def blame(exception, stacktrace) do
    {exception, stacktrace}
  end
end
