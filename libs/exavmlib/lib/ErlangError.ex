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

defmodule ErlangError do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}
  @compile {:no_warn_undefined, BadStructError}

  defexception [:original]

  @impl true
  def message(exception) do
    "Erlang error: #{inspect(exception.original)}"
  end

  @doc false
  def normalize(:badarg, _stacktrace) do
    %ArgumentError{}
  end

  def normalize(:badarith, _stacktrace) do
    %ArithmeticError{}
  end

  def normalize(:system_limit, _stacktrace) do
    %SystemLimitError{}
  end

  def normalize(:cond_clause, _stacktrace) do
    %CondClauseError{}
  end

  def normalize({:badarity, {fun, args}}, _stacktrace) do
    %BadArityError{function: fun, args: args}
  end

  def normalize({:badfun, term}, _stacktrace) do
    %BadFunctionError{term: term}
  end

  def normalize({:badstruct, struct, term}, _stacktrace) do
    # starting from v1.19.0, BadStructError is not available
    # still older Elixir versions might need it
    BadStructError.__struct__(%{struct: struct, term: term})
  end

  def normalize({:badmatch, term}, _stacktrace) do
    %MatchError{term: term}
  end

  def normalize({:badmap, term}, _stacktrace) do
    %BadMapError{term: term}
  end

  def normalize({:badbool, op, term}, _stacktrace) do
    %BadBooleanError{operator: op, term: term}
  end

  def normalize({:badkey, key}, stacktrace) do
    term =
      case stacktrace do
        [{Map, :get_and_update!, [map, _, _], _} | _] -> map
        [{Map, :update!, [map, _, _], _} | _] -> map
        [{:maps, :update, [_, _, map], _} | _] -> map
        [{:maps, :get, [_, map], _} | _] -> map
        [{:erlang, :map_get, [_, map], _} | _] -> map
        _ -> nil
      end

    %KeyError{key: key, term: term}
  end

  def normalize({:badkey, key, map}, _stacktrace) do
    %KeyError{key: key, term: map}
  end

  def normalize({:case_clause, term}, _stacktrace) do
    %CaseClauseError{term: term}
  end

  def normalize({:with_clause, term}, _stacktrace) do
    %WithClauseError{term: term}
  end

  def normalize({:try_clause, term}, _stacktrace) do
    %TryClauseError{term: term}
  end

  def normalize(:undef, stacktrace) do
    {mod, fun, arity} = from_stacktrace(stacktrace)
    %UndefinedFunctionError{module: mod, function: fun, arity: arity}
  end

  def normalize(:function_clause, stacktrace) do
    {mod, fun, arity} = from_stacktrace(stacktrace)
    %FunctionClauseError{module: mod, function: fun, arity: arity}
  end

  def normalize({:badarg, payload}, _stacktrace) do
    %ArgumentError{message: "argument error: #{inspect(payload)}"}
  end

  def normalize(other, _stacktrace) do
    %ErlangError{original: other}
  end

  defp from_stacktrace([{module, function, args, _} | _]) when is_list(args) do
    {module, function, length(args)}
  end

  defp from_stacktrace([{module, function, arity, _} | _]) do
    {module, function, arity}
  end

  defp from_stacktrace(_) do
    {nil, nil, nil}
  end
end
