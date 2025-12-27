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

  defstruct first: nil, last: nil, step: nil

  @type limit :: integer
  @type step :: pos_integer | neg_integer
  @type t :: %__MODULE__{first: limit, last: limit, step: step}
  @type t(first, last) :: %__MODULE__{first: first, last: last, step: step}

  @doc """
  Creates a new range.

  ## Examples

      iex> Range.new(-100, 100)
      -100..100

  """
  @spec new(integer, integer) :: t
  def new(first, last) when is_integer(first) and is_integer(last) do
    step =
      if first <= last do
        1
      else
        -1
      end

    %Range{first: first, last: last, step: step}
  end

  def new(first, last) do
    raise ArgumentError,
          "ranges (first..last) expect both sides to be integers, " <>
            "got: #{inspect(first)}..#{inspect(last)}"
  end

  @doc """
  Creates a new range with `step`.

  ## Examples

      iex> Range.new(-100, 100, 2)
      -100..100//2

  """
  @doc since: "1.12.0"
  @spec new(limit, limit, step) :: t
  def new(first, last, step)
      when is_integer(first) and is_integer(last) and is_integer(step) and step != 0 do
    %Range{first: first, last: last, step: step}
  end

  def new(first, last, step) do
    raise ArgumentError,
          "ranges (first..last//step) expect both sides to be integers and the step to be a " <>
            "non-zero integer, got: #{inspect(first)}..#{inspect(last)}//#{inspect(step)}"
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
  else
    @spec size(t) :: non_neg_integer
    def size(range)
    def size(%Range{first: first, last: last, step: step}) when step > 0 and first > last, do: 0
    def size(%Range{first: first, last: last, step: step}) when step < 0 and first < last, do: 0
    def size(%Range{first: first, last: last, step: step}), do: abs(div(last - first, step)) + 1
  end

  @doc """
  Checks if two ranges are disjoint.

  ## Examples

      iex> Range.disjoint?(1..5, 6..9)
      true
      iex> Range.disjoint?(5..1//-1, 6..9)
      true
      iex> Range.disjoint?(1..5, 5..9)
      false
      iex> Range.disjoint?(1..5, 2..7)
      false

  Steps are also considered when computing the ranges to be disjoint:

      iex> Range.disjoint?(1..10//2, 2..10//2)
      true

      # First element in common is 29
      iex> Range.disjoint?(1..100//14, 8..100//21)
      false
      iex> Range.disjoint?(57..-1//-14, 8..100//21)
      false
      iex> Range.disjoint?(1..100//14, 50..8//-21)
      false
      iex> Range.disjoint?(1..28//14, 8..28//21)
      true

      # First element in common is 14
      iex> Range.disjoint?(2..28//3, 9..28//5)
      false
      iex> Range.disjoint?(26..2//-3, 29..9//-5)
      false

      # Starting from the back without alignment
      iex> Range.disjoint?(27..11//-3, 30..0//-7)
      true

  """
  @doc since: "1.8.0"
  @spec disjoint?(t, t) :: boolean
  def disjoint?(
        %Range{first: first1, last: last1, step: step1} = range1,
        %Range{first: first2, last: last2, step: step2} = range2
      ) do
    # TODO: change function head back to this, as soon as we update Elixir
    # def disjoint?(first1..last1//step1 = range1, first2..last2//step2 = range2) do

    if size(range1) == 0 or size(range2) == 0 do
      true
    else
      {first1, last1, step1} = normalize(first1, last1, step1)
      {first2, last2, step2} = normalize(first2, last2, step2)

      cond do
        last2 < first1 or last1 < first2 ->
          true

        abs(step1) == 1 and abs(step2) == 1 ->
          false

        true ->
          # We need to find the first intersection of two arithmetic
          # progressions and see if they belong within the ranges
          # https://math.stackexchange.com/questions/1656120/formula-to-find-the-first-intersection-of-two-arithmetic-progressions
          {gcd, u, v} = Integer.extended_gcd(-step1, step2)

          if rem(first2 - first1, gcd) == 0 do
            c = first1 - first2 + step2 - step1
            t1 = -c / step2 * u
            t2 = -c / step1 * v
            t = max(floor(t1) + 1, floor(t2) + 1)
            x = div(c * u + t * step2, gcd) - 1
            y = div(c * v + t * step1, gcd) - 1

            x < 0 or first1 + x * step1 > last1 or
              y < 0 or first2 + y * step2 > last2
          else
            true
          end
      end
    end
  end

  @compile inline: [normalize: 3]
  defp normalize(first, last, step) when first > last,
    do: {first - abs(div(first - last, step) * step), first, -step}

  defp normalize(first, last, step), do: {first, last, step}
end
