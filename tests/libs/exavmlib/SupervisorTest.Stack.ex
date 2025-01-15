#
# This file is part of elixir-lang.
#
# Copyright 2012-2024 Elixir Contributors
# https://github.com/elixir-lang/elixir/blob/v1.17/lib/elixir/test/elixir/supervisor_test.exs
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

defmodule SupervisorTest.Stack do
  use GenServer

  def start_link({state, opts}) do
    GenServer.start_link(__MODULE__, state, opts)
  end

  def init(args) do
    {:ok, args}
  end

  def handle_call(:pop, _from, [h | t]) do
    {:reply, h, t}
  end

  def handle_call(:stop, _from, stack) do
    # There is a race condition between genserver terminations.
    # So we will explicitly unregister it here.
    try do
      case self() |> Process.info(:registered_name) |> elem(1) do
        [] -> :ok
        pid -> Process.unregister(pid)
      end
    rescue
      _ -> :ok
    end

    {:stop, :normal, :ok, stack}
  end

  def handle_cast({:push, h}, t) do
    {:noreply, [h | t]}
  end
end
