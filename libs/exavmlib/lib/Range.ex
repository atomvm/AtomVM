#
# This file is part of elixir-lang.
#
# Copyright 2012-2020 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.10.4/lib/elixir/lib/enum.ex
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

defmodule Range do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @moduledoc """
  Ranges represent a sequence of one or many, ascending
  or descending, consecutive integers.

  Ranges can be either increasing (`first <= last`) or
  decreasing (`first > last`). Ranges are also always
  inclusive.

  A range is represented internally as a struct. However,
  the most common form of creating and matching on ranges
  is via the `../2` macro, auto-imported from `Kernel`:

      iex> range = 1..3
      1..3
      iex> first..last = range
      iex> first
      1
      iex> last
      3

  A range implements the `Enumerable` protocol, which means
  functions in the `Enum` module can be used to work with
  ranges:

      iex> range = 1..10
      1..10
      iex> Enum.reduce(range, 0, fn i, acc -> i * i + acc end)
      385
      iex> Enum.count(range)
      10
      iex> Enum.member?(range, 11)
      false
      iex> Enum.member?(range, 8)
      true

  Such function calls are efficient memory-wise no matter the
  size of the range. The implementation of the `Enumerable`
  protocol uses logic based solely on the endpoints and does
  not materialize the whole list of integers.
  """

  defstruct first: nil, last: nil

  @type t :: %__MODULE__{first: integer, last: integer}
  @type t(first, last) :: %__MODULE__{first: first, last: last}

  @doc """
  Creates a new range.

  ## Examples

      iex> Range.new(-100, 100)
      -100..100

  """
  @spec new(integer, integer) :: t
  def new(first, last) when is_integer(first) and is_integer(last) do
    %Range{first: first, last: last}
  end

  def new(first, last) do
    raise ArgumentError,
          "ranges (first..last) expect both sides to be integers, " <>
            "got: #{inspect(first)}..#{inspect(last)}"
  end

  if Version.match?(System.version(), "~> 1.12") do
    @doc """
    Returns the size of `range`. Available Elixir 1.12 and later.

    ## Examples

        iex> Range.size(1..10)
        10
        iex> Range.size(1..10//2)
        5
        iex> Range.size(1..10//3)
        4
        iex> Range.size(1..10//-1)
        0

        iex> Range.size(10..1//-1)
        10
        iex> Range.size(10..1//-2)
        5
        iex> Range.size(10..1//-3)
        4
        iex> Range.size(10..1//1)
        0

    """
    @doc since: "0.7.0"
    @spec size(t) :: non_neg_integer
    def size(range)
    def size(first..last//step) when step > 0 and first > last, do: 0
    def size(first..last//step) when step < 0 and first < last, do: 0
    def size(first..last//step), do: abs(div(last - first, step)) + 1
  end

  @doc """
  Checks if two ranges are disjoint.

  ## Examples

      iex> Range.disjoint?(1..5, 6..9)
      true
      iex> Range.disjoint?(5..1, 6..9)
      true
      iex> Range.disjoint?(1..5, 5..9)
      false
      iex> Range.disjoint?(1..5, 2..7)
      false

  """
  @doc since: "1.8.0"
  @spec disjoint?(t, t) :: boolean
  def disjoint?(first1..last1 = _range1, first2..last2 = _range2) do
    {first1, last1} = normalize(first1, last1)
    {first2, last2} = normalize(first2, last2)
    last2 < first1 or last1 < first2
  end

  @compile inline: [normalize: 2]
  defp normalize(first, last) when first > last, do: {last, first}
  defp normalize(first, last), do: {first, last}
end
