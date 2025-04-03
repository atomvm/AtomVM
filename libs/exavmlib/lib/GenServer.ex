#
# This file is part of elixir-lang.
#
# Copyright 2012-2024 Elixir Contributors
# https://github.com/elixir-lang/elixir/blob/v1.17/lib/elixir/lib/gen_server.ex
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

defmodule GenServer do
  @compile {:autoload, false}
  @callback init(init_arg :: term) ::
              {:ok, state}
              | {:ok, state, timeout | :hibernate | {:continue, continue_arg :: term}}
              | :ignore
              | {:stop, reason :: any}
            when state: any

  @callback handle_call(request :: term, from, state :: term) ::
              {:reply, reply, new_state}
              | {:reply, reply, new_state,
                 timeout | :hibernate | {:continue, continue_arg :: term}}
              | {:noreply, new_state}
              | {:noreply, new_state, timeout | :hibernate | {:continue, continue_arg :: term}}
              | {:stop, reason, reply, new_state}
              | {:stop, reason, new_state}
            when reply: term, new_state: term, reason: term

  @callback handle_cast(request :: term, state :: term) ::
              {:noreply, new_state}
              | {:noreply, new_state, timeout | :hibernate | {:continue, continue_arg :: term}}
              | {:stop, reason :: term, new_state}
            when new_state: term

  @callback handle_info(msg :: :timeout | term, state :: term) ::
              {:noreply, new_state}
              | {:noreply, new_state, timeout | :hibernate | {:continue, continue_arg :: term}}
              | {:stop, reason :: term, new_state}
            when new_state: term

  @callback handle_continue(continue_arg, state :: term) ::
              {:noreply, new_state}
              | {:noreply, new_state, timeout | :hibernate | {:continue, continue_arg}}
              | {:stop, reason :: term, new_state}
            when new_state: term, continue_arg: term

  @callback terminate(reason, state :: term) :: term
            when reason: :normal | :shutdown | {:shutdown, term} | term

  @callback code_change(old_vsn, state :: term, extra :: term) ::
              {:ok, new_state :: term}
              | {:error, reason :: term}
            when old_vsn: term | {:down, term}

  @optional_callbacks code_change: 3,
                      terminate: 2,
                      handle_info: 2,
                      handle_cast: 2,
                      handle_call: 3,
                      handle_continue: 2

  @typedoc "Return values of `start*` functions"
  @type on_start :: {:ok, pid} | :ignore | {:error, {:already_started, pid} | term}

  @typedoc "The GenServer name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Options used by the `start*` functions"
  @type options :: [option]

  @typedoc "Option values used by the `start*` functions"
  @type option ::
          {:debug, debug}
          | {:name, name}
          | {:timeout, timeout}
          | {:spawn_opt, [Process.spawn_opt()]}
          | {:hibernate_after, timeout}

  @typedoc "Debug options supported by the `start*` functions"
  @type debug :: [:trace | :log | :statistics | {:log_to_file, Path.t()}]

  @typedoc """
  The server reference.

  This is either a plain PID or a value representing a registered name.
  See the "Name registration" section of this document for more information.
  """
  @type server :: pid | name | {atom, node}

  @typedoc """
  Tuple describing the client of a call request.

  `pid` is the PID of the caller and `tag` is a unique term used to identify the
  call.
  """
  @type from :: {pid, tag :: term}

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      @behaviour GenServer

      unless Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start this module under a supervisor.

        See `Supervisor`.
        """
      end

      def child_spec(init_arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [init_arg]}
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end

      defoverridable child_spec: 1

      # TODO: Remove this on v2.0
      @before_compile GenServer

      @doc false
      def handle_call(msg, _from, state) do
        proc =
          case Process.info(self(), :registered_name) do
            {_, []} -> self()
            {_, name} -> name
          end

        # We do this to trick Dialyzer to not complain about non-local returns.
        case 0 do
          0 ->
            raise "attempted to call GenServer #{inspect(proc)} but no handle_call/3 clause was provided"

          1 ->
            {:stop, {:bad_call, msg}, state}
        end
      end

      @doc false
      def handle_info(msg, state) do
        proc =
          case Process.info(self(), :registered_name) do
            {_, []} -> self()
            {_, name} -> name
          end

        {:noreply, state}
      end

      @doc false
      def handle_cast(msg, state) do
        proc =
          case Process.info(self(), :registered_name) do
            {_, []} -> self()
            {_, name} -> name
          end

        # We do this to trick Dialyzer to not complain about non-local returns.
        case 0 do
          0 ->
            raise "attempted to cast GenServer #{inspect(proc)} but no handle_cast/2 clause was provided"

          1 ->
            {:stop, {:bad_cast, msg}, state}
        end
      end

      @doc false
      def terminate(_reason, _state) do
        :ok
      end

      @doc false
      def code_change(_old, state, _extra) do
        {:ok, state}
      end

      defoverridable code_change: 3, terminate: 2, handle_info: 2, handle_cast: 2, handle_call: 3
    end
  end

  defmacro __before_compile__(env) do
    unless Module.defines?(env.module, {:init, 1}) do
      message = """
      function init/1 required by behaviour GenServer is not implemented \
      (in module #{inspect(env.module)}).

      We will inject a default implementation for now:

          def init(init_arg) do
            {:ok, init_arg}
          end

      You can copy the implementation above or define your own that converts \
      the arguments given to GenServer.start_link/3 to the server state.
      """

      IO.warn(message, env)

      quote do
        @doc false
        def init(init_arg) do
          {:ok, init_arg}
        end

        defoverridable init: 1
      end
    end
  end

  @spec start_link(module, any, options) :: on_start
  def start_link(module, init_arg, options \\ []) when is_atom(module) and is_list(options) do
    do_start(:link, module, init_arg, options)
  end

  @spec start(module, any, options) :: on_start
  def start(module, init_arg, options \\ []) when is_atom(module) and is_list(options) do
    do_start(:nolink, module, init_arg, options)
  end

  defp do_start(:nolink, module, init_arg, options) do
    case Keyword.pop(options, :name) do
      {nil, opts} ->
        # :gen.start(:gen_server, link, module, init_arg, opts)

        # start(Module, Args, Options) ->
        :gen_server.start(module, init_arg, opts)

      {atom, opts} when is_atom(atom) ->
        # :gen.start(:gen_server, link, {:local, atom}, module, init_arg, opts)

        # start({local, Name}, Module, Args, Options) when is_atom(Name) ->
        :gen_server.start({:local, atom}, module, init_arg, opts)

      {other, _} ->
        raise ArgumentError, """
        expected :name option to be one of the following:

          * nil
          * atom
          * {:global, term}
          * {:via, module, term}

        Got: #{inspect(other)}
        """
    end
  end

  defp do_start(:link, module, init_arg, options) do
    case Keyword.pop(options, :name) do
      {nil, opts} ->
        # :gen.start(:gen_server, link, module, init_arg, opts)
        # start(Module, Args, Options) ->
        :gen_server.start_link(module, init_arg, opts)

      {atom, opts} when is_atom(atom) ->
        # :gen.start(:gen_server, link, {:local, atom}, module, init_arg, opts)

        # start({local, Name}, Module, Args, Options) when is_atom(Name) ->
        :gen_server.start_link({:local, atom}, module, init_arg, opts)

      {other, _} ->
        raise ArgumentError, """
        expected :name option to be one of the following:

          * nil
          * atom
          * {:global, term}
          * {:via, module, term}

        Got: #{inspect(other)}
        """
    end
  end

  @spec stop(server, reason :: term, timeout) :: :ok
  def stop(server, reason \\ :normal, timeout \\ :infinity) do
    :gen_server.stop(server, reason, timeout)
    # case whereis(server) do
    #   nil ->
    #     exit({:noproc, {__MODULE__, :stop, [server, reason, timeout]}})

    #   pid when pid == self() ->
    #     exit({:calling_self, {__MODULE__, :stop, [server, reason, timeout]}})

    #   pid ->
    #     try do
    #       :proc_lib.stop(pid, reason, timeout)
    #     catch
    #       :exit, err ->
    #         exit({err, {__MODULE__, :stop, [server, reason, timeout]}})
    #     end
    # end
  end

  @spec call(server, term, timeout) :: term
  def call(server, request, timeout \\ 5000)
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    :gen_server.call(server, request, timeout)
    # case :erlang.whereis(server) do
    #   :undefined ->
    #     exit({:noproc, {__MODULE__, :call, [server, request, timeout]}})

    #   pid ->
    #     try do
    #       :gen_server.call(pid, request, timeout)
    #     catch
    #       :exit, reason ->
    #         exit({reason, {__MODULE__, :call, [server, request, timeout]}})
    #     else
    #       {:ok, res} -> res
    #     end
    # end
  end

  @spec cast(server, term) :: :ok
  def cast(server, request)

  def cast({:global, name}, request) do
    try do
      :global.send(name, cast_msg(request))
      :ok
    catch
      _, _ -> :ok
    end
  end

  def cast({:via, mod, name}, request) do
    try do
      mod.send(name, cast_msg(request))
      :ok
    catch
      _, _ -> :ok
    end
  end

  def cast({name, node}, request) when is_atom(name) and is_atom(node),
    do: do_send({name, node}, cast_msg(request))

  def cast(dest, request) when is_atom(dest) or is_pid(dest), do: do_send(dest, cast_msg(request))

  defp cast_msg(req) do
    {:"$gen_cast", req}
  end

  defp do_send(dest, msg) do
    try do
      send(dest, msg)
      :ok
    catch
      _, _ -> :ok
    end
  end

  @spec reply(from, term) :: :ok
  def reply(client, reply) do
    :gen_server.reply(client, reply)
  end

  @spec whereis(server) :: pid | {atom, node} | nil
  def whereis(server)

  def whereis(pid) when is_pid(pid), do: pid

  def whereis(name) when is_atom(name) do
    Process.whereis(name)
  end

  # def whereis({:global, name}) do
  #   case :global.whereis_name(name) do
  #     pid when is_pid(pid) -> pid
  #     :undefined -> nil
  #   end
  # end

  # def whereis({:via, mod, name}) do
  #   case apply(mod, :whereis_name, [name]) do
  #     pid when is_pid(pid) -> pid
  #     :undefined -> nil
  #   end
  # end

  # def whereis({name, local}) when is_atom(name) and local == node() do
  #   Process.whereis(name)
  # end

  # def whereis({name, node} = server) when is_atom(name) and is_atom(node) do
  #   server
  # end

  @doc false
  def format_report(%{
        label: {GenServer, :no_handle_info},
        report: %{module: mod, message: msg, name: proc}
      }) do
    {~c"~p ~p received unexpected message in handle_info/2: ~p~n", [mod, proc, msg]}
  end
end
