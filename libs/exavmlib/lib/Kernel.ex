#
# This file is part of AtomVM.
#
# Copyright 2020 Davide Bettio <davide@uninstall.it>
# Copyright 2012-2022 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/main/lib/elixir/lib/kernel.ex
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
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule Kernel do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @doc """
  Returns an integer which is the arithmetical absolute value of `number`.

  ## Examples
    iex> abs(3)
    3
    iex> abs(-3)
    3
  """
  def abs(number), do: :erlang.abs(number)

  def div(dividend, divisor), do: :erlang.div(dividend, divisor)

  def rem(dividend, divisor), do: :erlang.rem(dividend, divisor)

  def inspect(term, opts \\ []) when is_list(opts) do
    case term do
      t when is_atom(t) ->
        atom_to_string(t, ":")

      t when is_integer(t) ->
        :erlang.integer_to_binary(t)

      t when is_list(t) ->
        if is_printable_list(t) do
          str = :erlang.list_to_binary(t)
          <<?'::utf8, str::binary, ?'::utf8>>
        else
          [?[ | t |> inspect_join(?])]
          |> :erlang.list_to_binary()
        end

      t when is_pid(t) ->
        :erlang.pid_to_list(t)
        |> :erlang.list_to_binary()

      t when is_port(t) ->
        :erlang.port_to_list(t)
        |> :erlang.list_to_binary()

      t when is_function(t) ->
        :erlang.fun_to_list(t)
        |> :erlang.list_to_binary()

      t when is_tuple(t) ->
        [?{ | t |> :erlang.tuple_to_list() |> inspect_join(?})]
        |> :erlang.list_to_binary()

      t when is_binary(t) ->
        if is_printable_binary(t) do
          <<?"::utf8, t::binary, ?"::utf8>>
        else
          ["<<" | t |> :erlang.binary_to_list() |> inspect_join(">>")]
          |> :erlang.list_to_binary()
        end

      t when is_reference(t) ->
        :erlang.ref_to_list(t)
        |> :erlang.list_to_binary()

      t when is_float(t) ->
        :erlang.float_to_binary(term, [{:decimals, 17}, :compact])

      t when is_map(t) ->
        [?%, ?{ | t |> inspect_kv() |> join(?})]
        |> :erlang.list_to_binary()
    end
  end

  defp inspect_join([], last) do
    [last]
  end

  defp inspect_join([e], last) do
    [inspect(e), last]
  end

  defp inspect_join([h | e], last) when not is_list(e) do
    [inspect(h), " | ", inspect(e), last]
  end

  defp inspect_join([h | t], last) do
    [inspect(h), ?,, ?\s | inspect_join(t, last)]
  end

  defp join([], last) do
    [last]
  end

  defp join([e], last) do
    [e, last]
  end

  defp join([h | t], last) do
    [h, ?,, ?\s | join(t, last)]
  end

  defp inspect_kv(t) do
    :maps.fold(
      fn
        k, v, acc_in when is_atom(k) ->
          [[atom_to_string(k), ?:, ?\s, inspect(v)] | acc_in]

        k, v, acc_in ->
          [[inspect(k), " => ", inspect(v)] | acc_in]
      end,
      [],
      t
    )
  end

  defp atom_to_string(atom, prefix \\ "") do
    case atom do
      true ->
        "true"

      false ->
        "false"

      nil ->
        "nil"

      any_atom ->
        case :erlang.atom_to_binary(any_atom) do
          <<"Elixir.", displayable::binary>> ->
            displayable

          other ->
            <<prefix::binary, other::binary>>
        end
    end
  end

  defp is_printable_list([]), do: false

  defp is_printable_list([char]) do
    is_printable_ascii(char)
  end

  defp is_printable_list([char | t]) do
    if is_printable_ascii(char) do
      is_printable_list(t)
    else
      false
    end
  end

  defp is_printable_list(_any), do: false

  defp is_printable_ascii(char) do
    is_integer(char) and char >= 32 and char < 127 and char != ?'
  end

  defp is_printable_binary(<<>>), do: true

  defp is_printable_binary(<<char::utf8, rest::binary>>) when char >= 32 do
    is_printable_binary(rest)
  end

  defp is_printable_binary(_any), do: false

  @doc """
  Returns the biggest of the two given terms according to
  Erlang's term ordering.

  If the terms compare equal, the first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> max(1, 2)
      2
      iex> max(:a, :b)
      :b

  Using Erlang's term ordering means that comparisons are
  structural and not semantic. For example, when comparing dates:

      iex> max(~D[2017-03-31], ~D[2017-04-01])
      ~D[2017-03-31]

  In the example above, `max/1` returned March 31st instead of April 1st
  because the structural comparison compares the day before the year. In
  such cases it is common for modules to provide functions such as
  `Date.compare/2` that perform semantic comparison.
  """
  @spec max(first, second) :: first | second when first: term, second: term
  def max(first, second) do
    :erlang.max(first, second)
  end

  @doc """
  Returns the smallest of the two given terms according to
  Erlang's term ordering.

  If the terms compare equal, the first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> min(1, 2)
      1
      iex> min("foo", "bar")
      "bar"

  Using Erlang's term ordering means that comparisons are
  structural and not semantic. For example, when comparing dates:

      iex> min(~D[2017-03-31], ~D[2017-04-01])
      ~D[2017-04-01]

  In the example above, `min/1` returned April 1st instead of March 31st
  because the structural comparison compares the day before the year. In
  such cases it is common for modules to provide functions such as
  `Date.compare/2` that perform semantic comparison.
  """
  @spec min(first, second) :: first | second when first: term, second: term
  def min(first, second) do
    :erlang.min(first, second)
  end

  # Taken from Elixir kernel.ex
  @doc """
  Creates and updates structs.

  The `struct` argument may be an atom (which defines `defstruct`)
  or a `struct` itself. The second argument is any `Enumerable` that
  emits two-element tuples (key-value pairs) during enumeration.

  Keys in the `Enumerable` that don't exist in the struct are automatically
  discarded. Note that keys must be atoms, as only atoms are allowed when
  defining a struct.

  This function is useful for dynamically creating and updating structs, as
  well as for converting maps to structs; in the latter case, just inserting
  the appropriate `:__struct__` field into the map may not be enough and
  `struct/2` should be used instead.

  ## Examples

      defmodule User do
        defstruct name: "john"
      end

      struct(User)
      #=> %User{name: "john"}

      opts = [name: "meg"]
      user = struct(User, opts)
      #=> %User{name: "meg"}

      struct(user, unknown: "value")
      #=> %User{name: "meg"}

      struct(User, %{name: "meg"})
      #=> %User{name: "meg"}

      # String keys are ignored
      struct(User, %{"name" => "meg"})
      #=> %User{name: "john"}

  """
  @spec struct(module | struct, Enum.t()) :: struct
  def struct(struct, fields \\ []) do
    struct(struct, fields, fn
      {:__struct__, _val}, acc ->
        acc

      {key, val}, acc ->
        case acc do
          %{^key => _} -> %{acc | key => val}
          _ -> acc
        end
    end)
  end

  # Taken from Elixir kernel.ex
  @doc """
  Similar to `struct/2` but checks for key validity.

  The function `struct!/2` emulates the compile time behaviour
  of structs. This means that:

    * when building a struct, as in `struct!(SomeStruct, key: :value)`,
      it is equivalent to `%SomeStruct{key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      If the struct is enforcing any key via `@enforce_keys`, those will
      be enforced as well;

    * when updating a struct, as in `struct!(%SomeStruct{}, key: :value)`,
      it is equivalent to `%SomeStruct{struct | key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      However, updating structs does not enforce keys, as keys are enforced
      only when building;

  """
  @spec struct!(module | struct, Enum.t()) :: struct | no_return
  def struct!(struct, fields \\ [])

  def struct!(struct, fields) when is_atom(struct) do
    struct.__struct__(fields)
  end

  def struct!(struct, fields) when is_map(struct) do
    struct(struct, fields, fn
      {:__struct__, _}, acc ->
        acc

      {key, val}, acc ->
        Map.replace!(acc, key, val)
    end)
  end

  defp struct(struct, [], _fun) when is_atom(struct) do
    struct.__struct__()
  end

  defp struct(struct, fields, fun) when is_atom(struct) do
    struct(struct.__struct__(), fields, fun)
  end

  defp struct(%_{} = struct, [], _fun) do
    struct
  end

  defp struct(%_{} = struct, fields, fun) do
    Enum.reduce(fields, struct, fun)
  end
end
