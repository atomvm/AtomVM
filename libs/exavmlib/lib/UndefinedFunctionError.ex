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

defmodule UndefinedFunctionError do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  defexception [:module, :function, :arity, :reason, :message]

  @impl true
  def message(%{message: nil} = exception) do
    %{reason: reason, module: module, function: function, arity: arity} = exception
    {message, _loaded?} = message(reason, module, function, arity)
    message
  end

  def message(%{message: message}) do
    message
  end

  defp message(nil, module, function, arity) do
    cond do
      is_nil(function) or is_nil(arity) ->
        {"undefined function", false}

      is_nil(module) ->
        formatted_fun = Exception.format_mfa(module, function, arity)
        {"function #{formatted_fun} is undefined", false}

      function_exported?(module, :module_info, 0) ->
        message(:"function not exported", module, function, arity)

      true ->
        message(:"module could not be loaded", module, function, arity)
    end
  end

  defp message(:"module could not be loaded", module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined (module #{inspect(module)} is not available)", false}
  end

  defp message(:"function not exported", module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined or private", true}
  end

  defp message(reason, module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined (#{reason})", false}
  end

  @impl true
  def blame(exception, stacktrace) do
    %{reason: reason, module: module, function: function, arity: arity} = exception
    {message, _loaded?} = message(reason, module, function, arity)
    {%{exception | message: message}, stacktrace}
  end
end
