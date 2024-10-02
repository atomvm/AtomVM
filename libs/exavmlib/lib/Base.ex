#
# This file is part of elixir-lang.
#
# Copyright 2014-2023 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.17.3/lib/elixir/lib/base.ex
#
# Copyright 2024 Yuto Oguchi <oguchiyuto@realglobe.jp>, Realglobe Inc.
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

defmodule Base do
  @moduledoc """
  This module provides data encoding and decoding functions
  according to [RFC 4648](https://tools.ietf.org/html/rfc4648).

  This document defines the commonly used base 16, base 32, and base
  64 encoding schemes.

  ## Base 16 alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | 0        |     4 | 4        |     8 | 8        |    12 | C        |
  |     1 | 1        |     5 | 5        |     9 | 9        |    13 | D        |
  |     2 | 2        |     6 | 6        |    10 | A        |    14 | E        |
  |     3 | 3        |     7 | 7        |    11 | B        |    15 | F        |

  ## Base 32 alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | A        |     9 | J        |    18 | S        |    27 | 3        |
  |     1 | B        |    10 | K        |    19 | T        |    28 | 4        |
  |     2 | C        |    11 | L        |    20 | U        |    29 | 5        |
  |     3 | D        |    12 | M        |    21 | V        |    30 | 6        |
  |     4 | E        |    13 | N        |    22 | W        |    31 | 7        |
  |     5 | F        |    14 | O        |    23 | X        |       |          |
  |     6 | G        |    15 | P        |    24 | Y        | (pad) | =        |
  |     7 | H        |    16 | Q        |    25 | Z        |       |          |
  |     8 | I        |    17 | R        |    26 | 2        |       |          |


  ## Base 32 (extended hex) alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | 0        |     9 | 9        |    18 | I        |    27 | R        |
  |     1 | 1        |    10 | A        |    19 | J        |    28 | S        |
  |     2 | 2        |    11 | B        |    20 | K        |    29 | T        |
  |     3 | 3        |    12 | C        |    21 | L        |    30 | U        |
  |     4 | 4        |    13 | D        |    22 | M        |    31 | V        |
  |     5 | 5        |    14 | E        |    23 | N        |       |          |
  |     6 | 6        |    15 | F        |    24 | O        | (pad) | =        |
  |     7 | 7        |    16 | G        |    25 | P        |       |          |
  |     8 | 8        |    17 | H        |    26 | Q        |       |          |

  ## Base 64 alphabet

  | Value |  Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:----------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | A         |    17 | R        |    34 | i        |    51 | z        |
  |     1 | B         |    18 | S        |    35 | j        |    52 | 0        |
  |     2 | C         |    19 | T        |    36 | k        |    53 | 1        |
  |     3 | D         |    20 | U        |    37 | l        |    54 | 2        |
  |     4 | E         |    21 | V        |    38 | m        |    55 | 3        |
  |     5 | F         |    22 | W        |    39 | n        |    56 | 4        |
  |     6 | G         |    23 | X        |    40 | o        |    57 | 5        |
  |     7 | H         |    24 | Y        |    41 | p        |    58 | 6        |
  |     8 | I         |    25 | Z        |    42 | q        |    59 | 7        |
  |     9 | J         |    26 | a        |    43 | r        |    60 | 8        |
  |    10 | K         |    27 | b        |    44 | s        |    61 | 9        |
  |    11 | L         |    28 | c        |    45 | t        |    62 | +        |
  |    12 | M         |    29 | d        |    46 | u        |    63 | /        |
  |    13 | N         |    30 | e        |    47 | v        |       |          |
  |    14 | O         |    31 | f        |    48 | w        | (pad) | =        |
  |    15 | P         |    32 | g        |    49 | x        |       |          |
  |    16 | Q         |    33 | h        |    50 | y        |       |          |

  ## Base 64 (URL and filename safe) alphabet

  | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
  |------:|:---------|------:|:---------|------:|:---------|------:|:---------|
  |     0 | A        |    17 | R        |    34 | i        |    51 | z        |
  |     1 | B        |    18 | S        |    35 | j        |    52 | 0        |
  |     2 | C        |    19 | T        |    36 | k        |    53 | 1        |
  |     3 | D        |    20 | U        |    37 | l        |    54 | 2        |
  |     4 | E        |    21 | V        |    38 | m        |    55 | 3        |
  |     5 | F        |    22 | W        |    39 | n        |    56 | 4        |
  |     6 | G        |    23 | X        |    40 | o        |    57 | 5        |
  |     7 | H        |    24 | Y        |    41 | p        |    58 | 6        |
  |     8 | I        |    25 | Z        |    42 | q        |    59 | 7        |
  |     9 | J        |    26 | a        |    43 | r        |    60 | 8        |
  |    10 | K        |    27 | b        |    44 | s        |    61 | 9        |
  |    11 | L        |    28 | c        |    45 | t        |    62 | -        |
  |    12 | M        |    29 | d        |    46 | u        |    63 | _        |
  |    13 | N        |    30 | e        |    47 | v        |       |          |
  |    14 | O        |    31 | f        |    48 | w        | (pad) | =        |
  |    15 | P        |    32 | g        |    49 | x        |       |          |
  |    16 | Q        |    33 | h        |    50 | y        |       |          |

  """

  @type encode_case :: :upper | :lower
  @type decode_case :: :upper | :lower | :mixed

  @doc """
  Decodes a base 16 encoded string into a binary string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding

  The values for `:case` can be:

    * `:upper` - only allows upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  ## Examples

      iex> Base.decode16("666F6F626172")
      {:ok, "foobar"}

      iex> Base.decode16("666f6f626172", case: :lower)
      {:ok, "foobar"}

      iex> Base.decode16("666f6F626172", case: :mixed)
      {:ok, "foobar"}

  """
  @spec decode16(binary, case: decode_case) :: {:ok, binary} | :error
  def decode16(string, ops \\ []) do
    {:ok, decode16!(string, ops)}
  rescue
    ArgumentError -> :error
  end

  @doc """
  Decodes a base 16 encoded string into a binary string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to accept when decoding

  The values for `:case` can be:

    * `:upper` - only allows upper case characters (default)
    * `:lower` - only allows lower case characters
    * `:mixed` - allows mixed case characters

  An `ArgumentError` exception is raised if the padding is incorrect or
  a non-alphabet character is present in the string.

  ## Examples

      iex> Base.decode16!("666F6F626172")
      "foobar"

      iex> Base.decode16!("666f6f626172", case: :lower)
      "foobar"

      iex> Base.decode16!("666f6F626172", case: :mixed)
      "foobar"

  """
  @spec decode16!(binary, case: decode_case) :: binary
  def decode16!(string, opts \\ [])

  def decode16!(string, _ops) when is_binary(string) and rem(byte_size(string), 2) == 0 do
    # TODO: support :case option
    :binary.decode_hex(string)
  end

  def decode16!(string, _opts) when is_binary(string) do
    raise ArgumentError,
          "string given to decode has wrong length. An even number of bytes was expected, got: #{byte_size(string)}. " <>
            "Double check your string for unwanted characters or pad it accordingly"
  end

  @doc """
  Encodes a binary string into a base 16 encoded string.

  ## Options

  The accepted options are:

    * `:case` - specifies the character case to use when encoding

  The values for `:case` can be:

    * `:upper` - uses upper case characters (default)
    * `:lower` - uses lower case characters

  ## Examples

      iex> Base.encode16("foobar")
      "666F6F626172"

      iex> Base.encode16("foobar", case: :lower)
      "666f6f626172"

  """
  @spec encode16(binary, case: encode_case) :: binary
  def encode16(data, opts \\ []) do
    case Keyword.get(opts, :case, :upper) do
      :upper -> :binary.encode_hex(data, :uppercase)
      :lower -> :binary.encode_hex(data, :lowercase)
    end
  end
end
