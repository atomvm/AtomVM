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

defmodule KeyError do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  defexception [:key, :term, :message]

  @impl true
  def message(exception = %{message: nil}), do: message(exception.key, exception.term)
  def message(%{message: message}), do: message

  def message(key, term) do
    message = "key #{inspect(key)} not found"

    if term != nil do
      message <> " in: #{inspect(term)}"
    else
      message
    end
  end

  @impl true
  def blame(exception = %{term: nil}, stacktrace) do
    message = message(exception.key, exception.term)
    {%{exception | message: message}, stacktrace}
  end

  def blame(exception, stacktrace) do
    %{term: term, key: key} = exception
    message = message(key, term)
    {%{exception | message: message}, stacktrace}
  end
end
