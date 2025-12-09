#
# This file is part of elixir-lang.
#
# Copyright 2012-2024 Elixir Contributors
# https://github.com/elixir-lang/elixir/blob/v1.17/lib/elixir/test/elixir/gen_server_test.exs
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

defmodule GenServerTest.Stack do
  use GenServer

  def init(args) do
    {:ok, args}
  end

  def handle_call(:pop, _from, [h | t]) do
    {:reply, h, t}
  end

  def handle_call(:noreply, _from, h) do
    {:noreply, h}
  end

  def handle_call(:stop_self, _from, state) do
    # catch_exit(GenServer.stop(self()))
    reason = "stop_self"
    {:reply, reason, state}
  end

  def handle_cast({:push, element}, state) do
    {:noreply, [element | state]}
  end

  def terminate(_reason, _state) do
    # There is a race condition if the agent is
    # restarted too fast and it is registered.

    # need extra protection AtomVM!
    try do
      case self() |> Process.info(:registered_name) |> elem(1) do
        [] -> :ok
        pid -> Process.unregister(pid)
      end
    rescue
      _ -> :ok
    end

    :ok
  end
end
