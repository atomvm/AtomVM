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

defmodule Exception do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @moduledoc """
  Functions to format throw/catch/exit and exceptions.

  Note that stacktraces in Elixir are only available inside
  catch and rescue by using the `__STACKTRACE__/0` variable.

  Do not rely on the particular format returned by the `format*`
  functions in this module. They may be changed in future releases
  in order to better suit Elixir's tool chain. In other words,
  by using the functions in this module it is guaranteed you will
  format exceptions as in the current Elixir version being used.
  """

  @typedoc "The exception type"
  @type t :: %{
          required(:__struct__) => module,
          required(:__exception__) => true,
          optional(atom) => any
        }

  @typedoc "The kind handled by formatting functions"
  @type kind :: :error | non_error_kind
  @typep non_error_kind :: :exit | :throw | {:EXIT, pid}

  @type stacktrace :: [stacktrace_entry]
  @type stacktrace_entry ::
          {module, atom, arity_or_args, location}
          | {(... -> any), arity_or_args, location}

  @typep arity_or_args :: non_neg_integer | list
  @typep location :: keyword

  @callback exception(term) :: t
  @callback message(t) :: String.t()

  @doc """
  Called from `Exception.blame/3` to augment the exception struct.

  Can be used to collect additional information about the exception
  or do some additional expensive computation.
  """
  @callback blame(t, stacktrace) :: {t, stacktrace}
  @optional_callbacks [blame: 2]

  @doc """
  Returns `true` if the given `term` is an exception.
  """
  def exception?(term)
  def exception?(%_{__exception__: true}), do: true
  def exception?(_), do: false

  @doc """
  Gets the message for an `exception`.
  """
  def message(%module{__exception__: true} = exception) do
    try do
      module.message(exception)
    rescue
      caught_exception ->
        "got #{inspect(caught_exception.__struct__)} with message " <>
          "#{inspect(message(caught_exception))} while retrieving Exception.message/1 " <>
          "for #{inspect(exception)}"
    else
      result when is_binary(result) ->
        result

      result ->
        "got #{inspect(result)} " <>
          "while retrieving Exception.message/1 for #{inspect(exception)} " <>
          "(expected a string)"
    end
  end

  @doc """
  Normalizes an exception, converting Erlang exceptions
  to Elixir exceptions.

  It takes the `kind` spilled by `catch` as an argument and
  normalizes only `:error`, returning the untouched payload
  for others.

  The third argument is the stacktrace which is used to enrich
  a normalized error with more information. It is only used when
  the kind is an error.
  """
  @spec normalize(:error, any, stacktrace) :: t
  @spec normalize(non_error_kind, payload, stacktrace) :: payload when payload: var
  def normalize(kind, payload, stacktrace \\ [])
  def normalize(:error, %_{__exception__: true} = payload, _stacktrace), do: payload
  def normalize(:error, payload, stacktrace), do: ErlangError.normalize(payload, stacktrace)
  def normalize(_kind, payload, _stacktrace), do: payload

  @doc """
  Normalizes and formats any throw/error/exit.

  The message is formatted and displayed in the same
  format as used by Elixir's CLI.

  The third argument is the stacktrace which is used to enrich
  a normalized error with more information. It is only used when
  the kind is an error.
  """
  @spec format_banner(kind, any, stacktrace) :: String.t()
  def format_banner(kind, exception, stacktrace \\ [])

  def format_banner(:error, exception, stacktrace) do
    exception = normalize(:error, exception, stacktrace)
    "** (" <> inspect(exception.__struct__) <> ") " <> message(exception)
  end

  def format_banner(:throw, reason, _stacktrace) do
    "** (throw) " <> inspect(reason)
  end

  def format_banner(:exit, reason, _stacktrace) do
    "** (exit) " <> format_exit(reason, <<"\n    ">>)
  end

  def format_banner({:EXIT, pid}, reason, _stacktrace) do
    "** (EXIT from #{inspect(pid)}) " <> format_exit(reason, <<"\n    ">>)
  end

  @doc """
  Normalizes and formats throw/errors/exits and stacktraces.

  It relies on `format_banner/3` and `format_stacktrace/1`
  to generate the final format.

  If `kind` is `{:EXIT, pid}`, it does not generate a stacktrace,
  as such exits are retrieved as messages without stacktraces.
  """
  @spec format(kind, any, stacktrace) :: String.t()
  def format(kind, payload, stacktrace \\ [])

  def format({:EXIT, _} = kind, any, _) do
    format_banner(kind, any)
  end

  def format(kind, payload, stacktrace) do
    message = format_banner(kind, payload, stacktrace)

    case stacktrace do
      [] -> message
      _ -> message <> "\n" <> format_stacktrace(stacktrace)
    end
  end

  @doc """
  Attaches information to exceptions for extra debugging.

  This operation is potentially expensive, as it reads data
  from the filesystem, parses beam files, evaluates code and
  so on.

  If the exception module implements the optional `c:blame/2`
  callback, it will be invoked to perform the computation.
  """
  @doc since: "1.5.0"
  @spec blame(:error, any, stacktrace) :: {t, stacktrace}
  @spec blame(non_error_kind, payload, stacktrace) :: {payload, stacktrace} when payload: var
  def blame(kind, error, stacktrace)

  def blame(:error, error, stacktrace) do
    %module{} = struct = normalize(:error, error, stacktrace)

    # TODO: Code.ensure_loaded?(module) is not supported right now
    # original code: Code.ensure_loaded?(module) and function_exported?(module, :blame, 2)
    if function_exported?(module, :blame, 2) do
      module.blame(struct, stacktrace)
    else
      {struct, stacktrace}
    end
  end

  def blame(_kind, reason, stacktrace) do
    {reason, stacktrace}
  end

  @doc """
  Formats an exit. It returns a string.

  Often there are errors/exceptions inside exits. Exits are often
  wrapped by the caller and provide stacktraces too. This function
  formats exits in a way to nicely show the exit reason, caller
  and stacktrace.
  """
  @spec format_exit(any) :: String.t()
  def format_exit(reason) do
    format_exit(reason, <<"\n    ">>)
  end

  # 2-Tuple could be caused by an error if the second element is a stacktrace.
  defp format_exit({exception, maybe_stacktrace} = reason, joiner)
       when is_list(maybe_stacktrace) and maybe_stacktrace !== [] do
    try do
      Enum.map(maybe_stacktrace, &format_stacktrace_entry/1)
    catch
      :error, _ ->
        # Not a stacktrace, was an exit.
        format_exit_reason(reason)
    else
      formatted_stacktrace ->
        # Assume a non-empty list formattable as stacktrace is a
        # stacktrace, so exit was caused by an error.
        message =
          "an exception was raised:" <>
            joiner <> format_banner(:error, exception, maybe_stacktrace)

        Enum.join([message | formatted_stacktrace], joiner <> <<"    ">>)
    end
  end

  # :supervisor.start_link returns this error reason when it fails to init
  # because a child's start_link raises.
  defp format_exit({:shutdown, {:failed_to_start_child, child, {:EXIT, reason}}}, joiner) do
    format_start_child(child, reason, joiner)
  end

  # :supervisor.start_link returns this error reason when it fails to init
  # because a child's start_link returns {:error, reason}.
  defp format_exit({:shutdown, {:failed_to_start_child, child, reason}}, joiner) do
    format_start_child(child, reason, joiner)
  end

  # 2-Tuple could be an exit caused by mfa if second element is mfa, args
  # must be a list of arguments - max length 255 due to max arity.
  defp format_exit({reason2, {mod, fun, args}} = reason, joiner)
       when length(args) < 256 do
    try do
      format_mfa(mod, fun, args)
    catch
      :error, _ ->
        # Not an mfa, was an exit.
        format_exit_reason(reason)
    else
      mfa ->
        # Assume tuple formattable as an mfa is an mfa,
        # so exit was caused by failed mfa.
        "exited in: " <>
          mfa <> joiner <> "** (EXIT) " <> format_exit(reason2, joiner <> <<"    ">>)
    end
  end

  defp format_exit(reason, _joiner) do
    format_exit_reason(reason)
  end

  defp format_exit_reason(:normal), do: "normal"
  defp format_exit_reason(:shutdown), do: "shutdown"

  defp format_exit_reason({:shutdown, reason}) do
    "shutdown: #{inspect(reason)}"
  end

  defp format_exit_reason(:calling_self), do: "process attempted to call itself"
  defp format_exit_reason(:timeout), do: "time out"
  defp format_exit_reason(:killed), do: "killed"
  defp format_exit_reason(:noconnection), do: "no connection"

  defp format_exit_reason(:noproc) do
    "no process: the process is not alive or there's no process currently associated with the given name, possibly because its application isn't started"
  end

  defp format_exit_reason({:nodedown, node_name}) when is_atom(node_name) do
    "no connection to #{node_name}"
  end

  # :gen_server exit reasons

  defp format_exit_reason({:already_started, pid}) do
    "already started: " <> inspect(pid)
  end

  defp format_exit_reason({:bad_return_value, value}) do
    "bad return value: " <> inspect(value)
  end

  defp format_exit_reason({:bad_call, request}) do
    "bad call: " <> inspect(request)
  end

  defp format_exit_reason({:bad_cast, request}) do
    "bad cast: " <> inspect(request)
  end

  # :supervisor.start_link error reasons

  # If value is a list will be formatted by mfa exit in format_exit/1
  defp format_exit_reason({:bad_return, {mod, :init, value}})
       when is_atom(mod) do
    format_mfa(mod, :init, 1) <> " returned a bad value: " <> inspect(value)
  end

  defp format_exit_reason({:bad_start_spec, start_spec}) do
    "bad child specification, invalid children: " <> inspect(start_spec)
  end

  defp format_exit_reason({:start_spec, start_spec}) do
    "bad child specification, " <> format_sup_spec(start_spec)
  end

  defp format_exit_reason({:supervisor_data, data}) do
    "bad supervisor configuration, " <> format_sup_data(data)
  end

  defp format_exit_reason(reason), do: inspect(reason)

  defp format_start_child(child, reason, joiner) do
    "shutdown: failed to start child: " <>
      inspect(child) <> joiner <> "** (EXIT) " <> format_exit(reason, joiner <> <<"    ">>)
  end

  defp format_sup_data({:invalid_type, type}) do
    "invalid type: " <> inspect(type)
  end

  defp format_sup_data({:invalid_strategy, strategy}) do
    "invalid strategy: " <> inspect(strategy)
  end

  defp format_sup_data({:invalid_intensity, intensity}) do
    "invalid max_restarts (intensity): " <> inspect(intensity)
  end

  defp format_sup_data({:invalid_period, period}) do
    "invalid max_seconds (period): " <> inspect(period)
  end

  defp format_sup_data({:invalid_max_children, max_children}) do
    "invalid max_children: " <> inspect(max_children)
  end

  defp format_sup_data({:invalid_extra_arguments, extra}) do
    "invalid extra_arguments: " <> inspect(extra)
  end

  defp format_sup_data(other), do: "got: #{inspect(other)}"

  defp format_sup_spec({:duplicate_child_name, id}) do
    """
    more than one child specification has the id: #{inspect(id)}.
    If using maps as child specifications, make sure the :id keys are unique.
    If using a module or {module, arg} as child, use Supervisor.child_spec/2 to change the :id, for example:

        children = [
          Supervisor.child_spec({MyWorker, arg}, id: :my_worker_1),
          Supervisor.child_spec({MyWorker, arg}, id: :my_worker_2)
        ]
    """
  end

  defp format_sup_spec({:invalid_child_spec, child_spec}) do
    "invalid child specification: #{inspect(child_spec)}"
  end

  defp format_sup_spec({:invalid_child_type, type}) do
    "invalid child type: #{inspect(type)}. Must be :worker or :supervisor."
  end

  defp format_sup_spec({:invalid_mfa, mfa}) do
    "invalid mfa: #{inspect(mfa)}"
  end

  defp format_sup_spec({:invalid_restart_type, restart}) do
    "invalid restart type: #{inspect(restart)}. Must be :permanent, :transient or :temporary."
  end

  defp format_sup_spec({:invalid_shutdown, shutdown}) do
    "invalid shutdown: #{inspect(shutdown)}. Must be an integer >= 0, :infinity or :brutal_kill."
  end

  defp format_sup_spec({:invalid_module, mod}) do
    "invalid module: #{inspect(mod)}. Must be an atom."
  end

  defp format_sup_spec({:invalid_modules, modules}) do
    "invalid modules: #{inspect(modules)}. Must be a list of atoms or :dynamic."
  end

  defp format_sup_spec(other), do: "got: #{inspect(other)}"

  @doc """
  Receives a stacktrace entry and formats it into a string.
  """
  @spec format_stacktrace_entry(stacktrace_entry) :: String.t()
  def format_stacktrace_entry(entry)

  # From Macro.Env.stacktrace
  def format_stacktrace_entry({module, :__MODULE__, 0, location}) do
    format_location(location) <> inspect(module) <> " (module)"
  end

  # From :elixir_compiler_*
  def format_stacktrace_entry({_module, :__MODULE__, 1, location}) do
    format_location(location) <> "(module)"
  end

  # From :elixir_compiler_*
  def format_stacktrace_entry({_module, :__FILE__, 1, location}) do
    format_location(location) <> "(file)"
  end

  def format_stacktrace_entry({module, fun, arity, location}) do
    format_application(module) <> format_location(location) <> format_mfa(module, fun, arity)
  end

  def format_stacktrace_entry({fun, arity, location}) do
    format_location(location) <> format_fa(fun, arity)
  end

  defp format_application(module) do
    # We cannot use Application due to bootstrap issues
    case :application.get_application(module) do
      {:ok, app} -> "(" <> Atom.to_string(app) <> ") "
      :undefined -> ""
    end
  end

  @doc """
  Formats the stacktrace.

  A stacktrace must be given as an argument. If not, the stacktrace
  is retrieved from `Process.info/2`.
  """
  def format_stacktrace(trace \\ nil) do
    trace =
      if trace do
        trace
      else
        case Process.info(self(), :current_stacktrace) do
          {:current_stacktrace, t} -> Enum.drop(t, 3)
        end
      end

    case trace do
      [] -> "\n"
      _ -> "    " <> Enum.map_join(trace, "\n    ", &format_stacktrace_entry(&1)) <> "\n"
    end
  end

  @doc """
  Receives an anonymous function and arity and formats it as
  shown in stacktraces. The arity may also be a list of arguments.

  ## Examples

      Exception.format_fa(fn -> nil end, 1)
      #=> "#Function<...>/1"

  """
  def format_fa(fun, arity) when is_function(fun) do
    "#{inspect(fun)}#{format_arity(arity)}"
  end

  @doc """
  Receives a module, fun and arity and formats it
  as shown in stacktraces. The arity may also be a list
  of arguments.

  ## Examples

      iex> Exception.format_mfa(Foo, :bar, 1)
      "Foo.bar/1"

      iex> Exception.format_mfa(Foo, :bar, [])
      "Foo.bar()"

      iex> Exception.format_mfa(nil, :bar, [])
      "nil.bar()"

  Anonymous functions are reported as -func/arity-anonfn-count-,
  where func is the name of the enclosing function. Convert to
  "anonymous fn in func/arity"
  """
  def format_mfa(module, fun, arity) when is_atom(module) and is_atom(fun) do
    # Original code:
    # case Code.Identifier.extract_anonymous_fun_parent(fun) do
    #  {outer_name, outer_arity} ->
    #    "anonymous fn#{format_arity(arity)} in " <>
    #      "#{Code.Identifier.inspect_as_atom(module)}." <>
    #      "#{Code.Identifier.inspect_as_function(outer_name)}/#{outer_arity}"
    #
    #  :error ->
    #    "#{Code.Identifier.inspect_as_atom(module)}." <>
    #      "#{Code.Identifier.inspect_as_function(fun)}#{format_arity(arity)}"
    # end
    #
    # Here we use a super simplified version:
    "#{inspect(module)}.#{inspect(fun)}#{format_arity(arity)}"
  end

  defp format_arity(arity) when is_list(arity) do
    inspected = for x <- arity, do: inspect(x)
    "(#{Enum.join(inspected, ", ")})"
  end

  defp format_arity(arity) when is_integer(arity) do
    "/" <> Integer.to_string(arity)
  end

  @doc """
  Formats the given `file` and `line` as shown in stacktraces.
  If any of the values are `nil`, they are omitted.

  ## Examples

      iex> Exception.format_file_line("foo", 1)
      "foo:1:"

      iex> Exception.format_file_line("foo", nil)
      "foo:"

      iex> Exception.format_file_line(nil, nil)
      ""

  """
  def format_file_line(file, line, suffix \\ "") do
    if file do
      if line && line != 0 do
        "#{file}:#{line}:#{suffix}"
      else
        "#{file}:#{suffix}"
      end
    else
      ""
    end
  end

  defp format_location(opts) when is_list(opts) do
    format_file_line(Keyword.get(opts, :file), Keyword.get(opts, :line), " ")
  end
end
