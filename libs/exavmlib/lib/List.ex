#
# This file is part of elixir-lang.
#
# Copyright 2012-2019 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.10.1/lib/elixir/lib/list.ex
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

defmodule List do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @moduledoc """
  Linked lists hold zero, one, or more elements in the chosen order.

  Lists in Elixir are specified between square brackets:

      iex> [1, "two", 3, :four]
      [1, "two", 3, :four]

  Two lists can be concatenated and subtracted using the
  `Kernel.++/2` and `Kernel.--/2` operators:

      iex> [1, 2, 3] ++ [4, 5, 6]
      [1, 2, 3, 4, 5, 6]
      iex> [1, true, 2, false, 3, true] -- [true, false]
      [1, 2, 3, true]

  Lists in Elixir are effectively linked lists, which means
  they are internally represented in pairs containing the
  head and the tail of a list:

      iex> [head | tail] = [1, 2, 3]
      iex> head
      1
      iex> tail
      [2, 3]

  Similarly, we could write the list `[1, 2, 3]` using only
  such pairs (called cons cells):

      iex> [1 | [2 | [3 | []]]]
      [1, 2, 3]

  Some lists, called improper lists, do not have an empty list as
  the second element in the last cons cell:

      iex> [1 | [2 | [3 | 4]]]
      [1, 2, 3 | 4]

  Although improper lists are generally avoided, they are used in some
  special circumstances like iodata and chardata entities (see the `IO` module).

  Due to their cons cell based representation, prepending an element
  to a list is always fast (constant time), while appending becomes
  slower as the list grows in size (linear time):

      iex> list = [1, 2, 3]
      iex> [0 | list] # fast
      [0, 1, 2, 3]
      iex> list ++ [4] # slow
      [1, 2, 3, 4]

  Most of the functions in this module work in linear time. This means that,
  that the time it takes to perform an operation grows at the same rate as the
  length of the list. For example `length/1` and `last/1` will run in linear
  time because they need to iterate through every element of the list, but
  `first/1` will run in constant time because it only needs the first element.

  Lists also implement the `Enumerable` protocol, so many functions to work with
  lists are found in the `Enum` module. Additionally, the following functions and
  operators for lists are found in `Kernel`:

    * `++/2`
    * `--/2`
    * `hd/1`
    * `tl/1`
    * `in/2`
    * `length/1`

  ## Charlists

  If a list is made of non-negative integers, where each integer represents a
  Unicode code point, the list can also be called a charlist. These integers
  must:

    * be within the range `0..0x10FFFF` (`0..1_114_111`);
    * and be out of the range `0xD800..0xDFFF` (`55_296..57_343`), which is
      reserved in Unicode for UTF-16 surrogate pairs.

  Elixir uses single quotes to define charlists:

      iex> 'héllo'
      [104, 233, 108, 108, 111]

  In particular, charlists will be printed back by default in single
  quotes if they contain only printable ASCII characters:

      iex> 'abc'
      'abc'

  Even though the representation changed, the raw data does remain a list of
  numbers, which can be handled as such:

      iex> inspect('abc', charlists: :as_list)
      "[97, 98, 99]"
      iex> Enum.map('abc', fn num -> 1000 + num end)
      [1097, 1098, 1099]

  You can use the `IEx.Helpers.i/1` helper to get a condensed rundown on
  charlists in IEx when you encounter them, which shows you the type, description
  and also the raw representation in one single summary.

  The rationale behind this behaviour is to better support
  Erlang libraries which may return text as charlists
  instead of Elixir strings. In Erlang, charlists are the default
  way of handling strings, while in Elixir it's binaries. One
  example of such functions is `Application.loaded_applications/0`:

      Application.loaded_applications()
      #=>  [
      #=>    {:stdlib, 'ERTS  CXC 138 10', '2.6'},
      #=>    {:compiler, 'ERTS  CXC 138 10', '6.0.1'},
      #=>    {:elixir, 'elixir', '1.0.0'},
      #=>    {:kernel, 'ERTS  CXC 138 10', '4.1'},
      #=>    {:logger, 'logger', '1.0.0'}
      #=>  ]

  A list can be checked if it is made of only printable ASCII
  characters with `ascii_printable?/2`.

  Improper lists are never deemed as charlists.
  """

  @compile :inline_list_funcs

  @doc """
  Duplicates the given element `n` times in a list.

  `n` is an integer greater than or equal to `0`.

  If `n` is `0`, an empty list is returned.

  ## Examples

      iex> List.duplicate("hello", 0)
      []

      iex> List.duplicate("hi", 1)
      ["hi"]

      iex> List.duplicate("bye", 2)
      ["bye", "bye"]

      iex> List.duplicate([1, 2], 3)
      [[1, 2], [1, 2], [1, 2]]

  """
  @spec duplicate(any, 0) :: []
  @spec duplicate(elem, pos_integer) :: [elem, ...] when elem: var
  def duplicate(elem, n) do
    :lists.duplicate(n, elem)
  end

  @doc """
  Deletes the given `element` from the `list`. Returns a new list without
  the element.

  If the `element` occurs more than once in the `list`, just
  the first occurrence is removed.

  ## Examples

      iex> List.delete([:a, :b, :c], :a)
      [:b, :c]

      iex> List.delete([:a, :b, :c], :d)
      [:a, :b, :c]

      iex> List.delete([:a, :b, :b, :c], :b)
      [:a, :b, :c]

      iex> List.delete([], :b)
      []

  """
  @spec delete([], any) :: []
  @spec delete([...], any) :: list
  def delete(list, element)
  def delete([element | list], element), do: list
  def delete([other | list], element), do: [other | delete(list, element)]
  def delete([], _element), do: []

  @doc """
  Flattens the given `list` of nested lists.

  Empty list elements are discarded.

  ## Examples

      iex> List.flatten([1, [[2], 3]])
      [1, 2, 3]

      iex> List.flatten([[], [[], []]])
      []

  """
  @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
  def flatten(list) do
    :lists.flatten(list)
  end

  @doc """
  Folds (reduces) the given list from the left with
  a function. Requires an accumulator.

  ## Examples

      iex> List.foldl([5, 5], 10, fn x, acc -> x + acc end)
      20

      iex> List.foldl([1, 2, 3, 4], 0, fn x, acc -> x - acc end)
      2

  """
  @spec foldl([elem], acc, (elem, acc -> acc)) :: acc when elem: var, acc: var
  def foldl(list, acc, fun) when is_list(list) and is_function(fun) do
    :lists.foldl(fun, acc, list)
  end

  @doc """
  Folds (reduces) the given list from the right with
  a function. Requires an accumulator.

  ## Examples

      iex> List.foldr([1, 2, 3, 4], 0, fn x, acc -> x - acc end)
      -2

  """
  @spec foldr([elem], acc, (elem, acc -> acc)) :: acc when elem: var, acc: var
  def foldr(list, acc, fun) when is_list(list) and is_function(fun) do
    :lists.foldr(fun, acc, list)
  end

  @doc """
  Returns the first element in `list` or `nil` if `list` is empty.

  ## Examples

      iex> List.first([])
      nil

      iex> List.first([1])
      1

      iex> List.first([1, 2, 3])
      1

  """
  @spec first([]) :: nil
  @spec first([elem, ...]) :: elem when elem: var
  def first([]), do: nil
  def first([head | _]), do: head

  @doc """
  Returns the last element in `list` or `nil` if `list` is empty.

  ## Examples

      iex> List.last([])
      nil

      iex> List.last([1])
      1

      iex> List.last([1, 2, 3])
      3

  """
  @spec last([]) :: nil
  @spec last([elem, ...]) :: elem when elem: var
  def last([]), do: nil
  def last([head]), do: head
  def last([_ | tail]), do: last(tail)

  @doc """
  Receives a list of tuples and returns the first tuple
  where the element at `position` in the tuple matches the
  given `key`.

  If no matching tuple is found, `default` is returned.

  ## Examples

      iex> List.keyfind([a: 1, b: 2], :a, 0)
      {:a, 1}

      iex> List.keyfind([a: 1, b: 2], 2, 1)
      {:b, 2}

      iex> List.keyfind([a: 1, b: 2], :c, 0)
      nil

  """
  @spec keyfind([tuple], any, non_neg_integer, any) :: any
  def keyfind(list, key, position, default \\ nil) do
    :lists.keyfind(key, position + 1, list) || default
  end

  @doc """
  Receives a list of tuples and returns `true` if there is
  a tuple where the element at `position` in the tuple matches
  the given `key`.

  ## Examples

      iex> List.keymember?([a: 1, b: 2], :a, 0)
      true

      iex> List.keymember?([a: 1, b: 2], 2, 1)
      true

      iex> List.keymember?([a: 1, b: 2], :c, 0)
      false

  """
  @spec keymember?([tuple], any, non_neg_integer) :: boolean
  def keymember?(list, key, position) do
    :lists.keymember(key, position + 1, list)
  end

  @doc """
  Receives a `list` of tuples and deletes the first tuple
  where the element at `position` matches the
  given `key`. Returns the new list.

  ## Examples

      iex> List.keydelete([a: 1, b: 2], :a, 0)
      [b: 2]

      iex> List.keydelete([a: 1, b: 2], 2, 1)
      [a: 1]

      iex> List.keydelete([a: 1, b: 2], :c, 0)
      [a: 1, b: 2]

  """
  @spec keydelete([tuple], any, non_neg_integer) :: [tuple]
  def keydelete(list, key, position) do
    :lists.keydelete(key, position + 1, list)
  end

  @doc """
  Wraps `term` in a list if this is not list.

  If `term` is already a list, it returns the list.
  If `term` is `nil`, it returns an empty list.

  ## Examples

      iex> List.wrap("hello")
      ["hello"]

      iex> List.wrap([1, 2, 3])
      [1, 2, 3]

      iex> List.wrap(nil)
      []

  """
  @spec wrap(term) :: maybe_improper_list()
  def wrap(term)

  def wrap(list) when is_list(list) do
    list
  end

  def wrap(nil) do
    []
  end

  def wrap(other) do
    [other]
  end

  @doc ~S"""
  Checks if `list` is a charlist made only of printable ASCII characters.

  Takes an optional `limit` as a second argument. `ascii_printable?/2` only
  checks the printability of the list up to the `limit`.

  A printable charlist in Elixir contains only the printable characters in the
  standard seven-bit ASCII character encoding, which are characters ranging from
  32 to 126 in decimal notation, plus the following control characters:

    * `?\a` - Bell
    * `?\b` - Backspace
    * `?\t` - Horizontal tab
    * `?\n` - Line feed
    * `?\v` - Vertical tab
    * `?\f` - Form feed
    * `?\r` - Carriage return
    * `?\e` - Escape

  For more information read the [Character groups](https://en.wikipedia.org/wiki/ASCII#Character_groups)
  section in the Wikipedia article of the [ASCII](https://en.wikipedia.org/wiki/ASCII) standard.

  ## Examples

      iex> List.ascii_printable?('abc')
      true

      iex> List.ascii_printable?('abc' ++ [0])
      false

      iex> List.ascii_printable?('abc' ++ [0], 2)
      true

  Improper lists are not printable, even if made only of ASCII characters:

      iex> List.ascii_printable?('abc' ++ ?d)
      false

  """
  @spec ascii_printable?(list, 0) :: true
  @spec ascii_printable?([], limit) :: true
        when limit: :infinity | pos_integer
  @spec ascii_printable?([...], limit) :: boolean
        when limit: :infinity | pos_integer
  def ascii_printable?(list, limit \\ :infinity)
      when is_list(list) and (limit == :infinity or (is_integer(limit) and limit >= 0)) do
    ascii_printable_guarded?(list, limit)
  end

  defp ascii_printable_guarded?(_, 0) do
    true
  end

  defp ascii_printable_guarded?([char | rest], counter)
       # 7..13 is the range '\a\b\t\n\v\f\r'. 32..126 are ASCII printables.
       when is_integer(char) and
              ((char >= 7 and char <= 13) or char == ?\e or (char >= 32 and char <= 126)) do
    ascii_printable_guarded?(rest, decrement(counter))
  end

  defp ascii_printable_guarded?([], _counter), do: true
  defp ascii_printable_guarded?(_, _counter), do: false

  @compile {:inline, decrement: 1}
  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1

  @doc """
  Returns `true` if `list` is an improper list. Otherwise returns `false`.

  ## Examples

      iex> List.improper?([1, 2 | 3])
      true

      iex> List.improper?([1, 2, 3])
      false

  """
  @spec improper?(maybe_improper_list) :: boolean
  def improper?(list) when is_list(list) and length(list) >= 0, do: false
  def improper?(list) when is_list(list), do: true

  @doc """
  Returns a list with `value` inserted at the specified `index`.

  Note that `index` is capped at the list length. Negative indices
  indicate an offset from the end of the `list`.

  ## Examples

      iex> List.insert_at([1, 2, 3, 4], 2, 0)
      [1, 2, 0, 3, 4]

      iex> List.insert_at([1, 2, 3], 10, 0)
      [1, 2, 3, 0]

      iex> List.insert_at([1, 2, 3], -1, 0)
      [1, 2, 3, 0]

      iex> List.insert_at([1, 2, 3], -10, 0)
      [0, 1, 2, 3]

  """
  @spec insert_at(list, integer, any) :: list
  def insert_at(list, index, value) when is_list(list) and is_integer(index) do
    case index do
      -1 ->
        list ++ [value]

      _ when index < 0 ->
        case length(list) + index + 1 do
          index when index < 0 -> [value | list]
          index -> do_insert_at(list, index, value)
        end

      _ ->
        do_insert_at(list, index, value)
    end
  end

  @doc """
  Returns a list with a replaced value at the specified `index`.

  Negative indices indicate an offset from the end of the `list`.
  If `index` is out of bounds, the original `list` is returned.

  ## Examples

      iex> List.replace_at([1, 2, 3], 0, 0)
      [0, 2, 3]

      iex> List.replace_at([1, 2, 3], 10, 0)
      [1, 2, 3]

      iex> List.replace_at([1, 2, 3], -1, 0)
      [1, 2, 0]

      iex> List.replace_at([1, 2, 3], -10, 0)
      [1, 2, 3]

  """
  @spec replace_at(list, integer, any) :: list
  def replace_at(list, index, value) when is_list(list) and is_integer(index) do
    if index < 0 do
      case length(list) + index do
        index when index < 0 -> list
        index -> do_replace_at(list, index, value)
      end
    else
      do_replace_at(list, index, value)
    end
  end

  @doc """
  Returns a list with an updated value at the specified `index`.

  Negative indices indicate an offset from the end of the `list`.
  If `index` is out of bounds, the original `list` is returned.

  ## Examples

      iex> List.update_at([1, 2, 3], 0, &(&1 + 10))
      [11, 2, 3]

      iex> List.update_at([1, 2, 3], 10, &(&1 + 10))
      [1, 2, 3]

      iex> List.update_at([1, 2, 3], -1, &(&1 + 10))
      [1, 2, 13]

      iex> List.update_at([1, 2, 3], -10, &(&1 + 10))
      [1, 2, 3]

  """
  @spec update_at([elem], integer, (elem -> any)) :: list when elem: var
  def update_at(list, index, fun) when is_list(list) and is_function(fun) and is_integer(index) do
    if index < 0 do
      case length(list) + index do
        index when index < 0 -> list
        index -> do_update_at(list, index, fun)
      end
    else
      do_update_at(list, index, fun)
    end
  end

  @doc """
  Returns `true` if `list` starts with the given `prefix` list; otherwise returns `false`.

  If `prefix` is an empty list, it returns `true`.

  ### Examples

      iex> List.starts_with?([1, 2, 3], [1, 2])
      true

      iex> List.starts_with?([1, 2], [1, 2, 3])
      false

      iex> List.starts_with?([:alpha], [])
      true

      iex> List.starts_with?([], [:alpha])
      false

  """
  @spec starts_with?(nonempty_list, nonempty_list) :: boolean
  @spec starts_with?(list, []) :: true
  @spec starts_with?([], nonempty_list) :: false
  def starts_with?(list, prefix)

  def starts_with?([head | tail], [head | prefix_tail]), do: starts_with?(tail, prefix_tail)
  def starts_with?(list, []) when is_list(list), do: true
  def starts_with?(list, [_ | _]) when is_list(list), do: false

  @doc """
  Converts a charlist to an atom.

  Elixir supports conversions from charlists which contains any Unicode
  code point.

  Inlined by the compiler.

  ## Examples

      iex> List.to_atom('Elixir')
      :Elixir

      iex> List.to_atom('🌢 Elixir')
      :"🌢 Elixir"

  """
  @spec to_atom(charlist) :: atom
  def to_atom(charlist) do
    :erlang.list_to_atom(charlist)
  end

  @doc """
  Converts a charlist to an existing atom. Raises an `ArgumentError`
  if the atom does not exist.

  Elixir supports conversions from charlists which contains any Unicode
  code point.

  Inlined by the compiler.

  ## Examples

      iex> _ = :my_atom
      iex> List.to_existing_atom('my_atom')
      :my_atom

      iex> _ = :"🌢 Elixir"
      iex> List.to_existing_atom('🌢 Elixir')
      :"🌢 Elixir"

      iex> List.to_existing_atom('this_atom_will_never_exist')
      ** (ArgumentError) argument error

  """
  @spec to_existing_atom(charlist) :: atom
  def to_existing_atom(charlist) do
    :erlang.list_to_existing_atom(charlist)
  end

  @doc """
  Returns the float whose text representation is `charlist`.

  Inlined by the compiler.

  ## Examples

      iex> List.to_float('2.2017764e+0')
      2.2017764

  """
  @spec to_float(charlist) :: float
  def to_float(charlist) do
    :erlang.list_to_float(charlist)
  end

  @doc """
  Returns an integer whose text representation is `charlist`.

  Inlined by the compiler.

  ## Examples

      iex> List.to_integer('123')
      123

  """
  @spec to_integer(charlist) :: integer
  def to_integer(charlist) do
    :erlang.list_to_integer(charlist)
  end

  @doc """
  Returns an integer whose text representation is `charlist` in base `base`.

  Inlined by the compiler.

  The base needs to be between `2` and `36`.

  ## Examples

      iex> List.to_integer('3FF', 16)
      1023

  """
  @spec to_integer(charlist, 2..36) :: integer
  def to_integer(charlist, base) do
    :erlang.list_to_integer(charlist, base)
  end

  @doc """
  Converts a list to a tuple.

  Inlined by the compiler.

  ## Examples

      iex> List.to_tuple([:share, [:elixir, 163]])
      {:share, [:elixir, 163]}

  """
  @spec to_tuple(list) :: tuple
  def to_tuple(list) do
    :erlang.list_to_tuple(list)
  end

  # Minimal implementation with no unicode support
  def to_string(list) when is_list(list) do
    :erlang.list_to_binary(list)
  end

  ## Helpers

  # replace_at

  defp do_replace_at([], _index, _value) do
    []
  end

  defp do_replace_at([_old | rest], 0, value) do
    [value | rest]
  end

  defp do_replace_at([head | tail], index, value) do
    [head | do_replace_at(tail, index - 1, value)]
  end

  # insert_at

  defp do_insert_at([], _index, value) do
    [value]
  end

  defp do_insert_at(list, 0, value) do
    [value | list]
  end

  defp do_insert_at([head | tail], index, value) do
    [head | do_insert_at(tail, index - 1, value)]
  end

  # update_at

  defp do_update_at([value | list], 0, fun) do
    [fun.(value) | list]
  end

  defp do_update_at([head | tail], index, fun) do
    [head | do_update_at(tail, index - 1, fun)]
  end

  defp do_update_at([], _index, _fun) do
    []
  end
end
