#
# This file is part of elixir-lang.
#
# Copyright 2012-2019 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.10.1/lib/elixir/lib/process.ex
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

defmodule Process do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @moduledoc """
  Conveniences for working with processes and the process dictionary.

  Besides the functions available in this module, the `Kernel` module
  exposes and auto-imports some basic functionality related to processes
  available through the following functions:

    * `Kernel.spawn/1` and `Kernel.spawn/3`
    * `Kernel.spawn_link/1` and `Kernel.spawn_link/3`
    * `Kernel.spawn_monitor/1` and `Kernel.spawn_monitor/3`
    * `Kernel.self/0`
    * `Kernel.send/2`

  While this module provides low-level conveniences to work with processes,
  developers typically use abstractions such as `Agent`, `GenServer`,
  `Registry`, `Supervisor` and `Task` for building their systems and
  resort to this module for gathering information, trapping exits, links
  and monitoring.
  """

  @typedoc """
  A process destination.

  A remote or local PID, a local port, a locally registered name, or a tuple in
  the form of `{registered_name, node}` for a registered name at another node.
  """
  @type dest :: pid | port | (registered_name :: atom) | {registered_name :: atom, node}

  @doc """
  Tells whether the given process is alive on the local node.

  If the process identified by `pid` is alive (that is, it's not exiting and has
  not exited yet) than this function returns `true`. Otherwise, it returns
  `false`.

  `pid` must refer to a process running on the local node or `ArgumentError` is raised.

  Inlined by the compiler.
  """
  @spec alive?(pid) :: boolean
  defdelegate alive?(pid), to: :erlang, as: :is_process_alive

  @doc """
  Sleeps the current process for the given `timeout`.

  `timeout` is either the number of milliseconds to sleep as an
  integer or the atom `:infinity`. When `:infinity` is given,
  the current process will sleep forever, and not
  consume or reply to messages.

  **Use this function with extreme care**. For almost all situations
  where you would use `sleep/1` in Elixir, there is likely a
  more correct, faster and precise way of achieving the same with
  message passing.

  For example, if you are waiting for a process to perform some
  action, it is better to communicate the progress of such action
  with messages.

  In other words, **do not**:

      Task.start_link(fn ->
        do_something()
        ...
      end)

      # Wait until work is done
      Process.sleep(2000)

  But **do**:

      parent = self()

      Task.start_link(fn ->
        do_something()
        send(parent, :work_is_done)
        ...
      end)

      receive do
        :work_is_done -> :ok
      after
        # Optional timeout
        30_000 -> :timeout
      end

  For cases like the one above, `Task.async/1` and `Task.await/2` are
  preferred.

  Similarly, if you are waiting for a process to terminate,
  monitor that process instead of sleeping. **Do not**:

      Task.start_link(fn ->
        ...
      end)

      # Wait until task terminates
      Process.sleep(2000)

  Instead **do**:

      {:ok, pid} =
        Task.start_link(fn ->
          ...
        end)

      ref = Process.monitor(pid)

      receive do
        {:DOWN, ^ref, _, _, _} -> :task_is_down
      after
        # Optional timeout
        30_000 -> :timeout
      end

  """
  @spec sleep(timeout) :: :ok
  def sleep(timeout)
      when is_integer(timeout) and timeout >= 0
      when timeout == :infinity do
    receive after: (timeout -> :ok)
  end

  @spec send(dest, msg) :: :ok | :noconnect | :nosuspend
        when dest: dest(),
             msg: any
  defdelegate send(dest, msg), to: :erlang

  @spec send_after(pid | atom, term, non_neg_integer, [option]) :: reference
        when option: {:abs, boolean}
  def send_after(dest, msg, time, _opts \\ []) do
    :erlang.send_after(time, dest, msg)
  end

  @spec cancel_timer(reference) :: non_neg_integer | false | :ok
  defdelegate cancel_timer(timer_ref), to: :erlang

  @type spawn_opt ::
          :link
          | :monitor
          | {:priority, :low | :normal | :high}
          | {:fullsweep_after, non_neg_integer}
          | {:min_heap_size, non_neg_integer}
          | {:min_bin_vheap_size, non_neg_integer}
  @type spawn_opts :: [spawn_opt]

  @doc """
  Spawns the given function according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  More options are available; for the comprehensive list of available options
  check `:erlang.spawn_opt/4`.

  Inlined by the compiler.

  ## Examples

      Process.spawn(fn -> 1 + 2 end, [:monitor])
      #=> {#PID<0.93.0>, #Reference<0.18808174.1939079169.202418>}
      Process.spawn(fn -> 1 + 2 end, [:link])
      #=> #PID<0.95.0>

  """
  @spec spawn((-> any), spawn_opts) :: pid | {pid, reference}
  defdelegate spawn(fun, opts), to: :erlang, as: :spawn_opt

  @doc """
  Spawns the given function `fun` from module `mod`, passing the given `args`
  according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  It also accepts extra options, for the list of available options
  check `:erlang.spawn_opt/4`.

  Inlined by the compiler.
  """
  @spec spawn(module, atom, list, spawn_opts) :: pid | {pid, reference}
  defdelegate spawn(mod, fun, args, opts), to: :erlang, as: :spawn_opt

  @doc """
  Returns a list of PIDs corresponding to all the
  processes currently existing on the local node.

  Note that if a process is exiting, it is considered to exist but not be
  alive. This means that for such process, `alive?/1` will return `false` but
  its PID will be part of the list of PIDs returned by this function.

  See `:erlang.processes/0` for more information.

  Inlined by the compiler.

  ## Examples

      Process.list()
      #=> [#PID<0.0.0>, #PID<0.1.0>, #PID<0.2.0>, #PID<0.3.0>, ...]

  """
  @spec list() :: [pid]
  defdelegate list(), to: :erlang, as: :processes

  @spec link(pid | port) :: true
  defdelegate link(pid_or_port), to: :erlang

  @spec unlink(pid | port) :: true
  defdelegate unlink(pid_or_port), to: :erlang

  @doc """
  Register a PID or port identifier under `name`.

  See `:erlang.register/2` for more information.

  ## Examples

    Process.register(self(), :test)

  """
  def register(pid_or_port, name) when is_atom(name) do
    :erlang.register(name, pid_or_port)
  end

  @doc """
  Unregister a registered process by `name`.

  See `:erlang.unregister/1` for more information.

  ## Examples

    Process.unregister(:test)

  """
  def unregister(name) when is_atom(name) do
    :erlang.unregister(name)
  end

  @doc """
  Returns the PID or port identifier registered under `name` or `nil` if the
  name is not registered.

  See `:erlang.whereis/1` for more information.

  ## Examples

      Process.register(self(), :test)
      Process.whereis(:test)
      #=> #PID<0.84.0>
      Process.whereis(:wrong_name)
      #=> nil

  """
  @spec whereis(atom) :: pid | port | nil
  def whereis(name) do
    nillify(:erlang.whereis(name))
  end

  @typep heap_size ::
           non_neg_integer
           | %{size: non_neg_integer, kill: boolean, error_logger: boolean}

  @typep priority_level :: :low | :normal | :high | :max

  @doc """
  Sets the given `flag` to `value` for the calling process.

  Returns the old value of `flag`.

  See `:erlang.process_flag/2` for more information.

  Inlined by the compiler.
  """
  @spec flag(:error_handler, module) :: module
  @spec flag(:max_heap_size, heap_size) :: heap_size
  @spec flag(:message_queue_data, :erlang.message_queue_data()) :: :erlang.message_queue_data()
  @spec flag(:min_bin_vheap_size, non_neg_integer) :: non_neg_integer
  @spec flag(:min_heap_size, non_neg_integer) :: non_neg_integer
  @spec flag(:priority, priority_level) :: priority_level
  @spec flag(:save_calls, 0..10000) :: 0..10000
  @spec flag(:sensitive, boolean) :: boolean
  @spec flag(:trap_exit, boolean) :: boolean
  defdelegate flag(flag, value), to: :erlang, as: :process_flag

  @doc """
  Sets the given `flag` to `value` for the given process `pid`.

  Returns the old value of `flag`.

  It raises `ArgumentError` if `pid` is not a local process.

  The allowed values for `flag` are only a subset of those allowed in `flag/2`,
  namely `:save_calls`.

  See `:erlang.process_flag/3` for more information.

  Inlined by the compiler.
  """
  @spec flag(pid, :save_calls, 0..10000) :: 0..10000
  defdelegate flag(pid, flag, value), to: :erlang, as: :process_flag

  @doc """
  Returns information about the process identified by `pid`, or returns `nil` if the process
  is not alive.

  Use this only for debugging information.

  See `:erlang.process_info/1` for more information.
  """
  @spec info(pid) :: keyword | nil
  def info(pid) do
    nillify(:erlang.process_info(pid))
  end

  @doc """
  Returns information about the process identified by `pid`,
  or returns `nil` if the process is not alive.

  See `:erlang.process_info/2` for more information.
  """
  @spec info(pid, atom | [atom]) :: {atom, term} | [{atom, term}] | nil
  def info(pid, spec)

  def info(pid, :registered_name) do
    case :erlang.process_info(pid, :registered_name) do
      :undefined -> nil
      [] -> {:registered_name, []}
      other -> other
    end
  end

  def info(pid, spec) when is_atom(spec) or is_list(spec) do
    nillify(:erlang.process_info(pid, spec))
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other), do: other
end
