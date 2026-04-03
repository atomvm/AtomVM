#
# This file is part of elixir-lang.
#
# Copyright 2021 The Elixir Team
# https://github.com/elixir-lang/elixir/blob/03b9fde6/lib/elixir/lib/json.ex
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
# Adapted for AtomVM:
# - Removed __deriving__ macro (not supported)
# - Removed Duration, Date, Time, NaiveDateTime, DateTime encoders
#   (Calendar modules not available)
# - Simplified error offset extraction (error_info not supported)
#

defprotocol JSON.Encoder do
  # Elixir < 1.18.0 doesn't have a built-in JSON.Encoder protocol,
  # so autoload must remain enabled for defimpl to find it.
  if Version.match?(System.version(), ">= 1.18.0") do
    @compile {:autoload, false}
  end

  @moduledoc """
  A protocol for custom JSON encoding of data structures.
  """

  @doc """
  A function invoked to encode the given term to `t:iodata/0`.
  """
  def encode(term, encoder)
end

defimpl JSON.Encoder, for: Atom do
  def encode(value, encoder) do
    case value do
      nil -> "null"
      true -> "true"
      false -> "false"
      _ -> encoder.(Atom.to_string(value), encoder)
    end
  end
end

defimpl JSON.Encoder, for: BitString do
  def encode(value, _encoder) do
    :json.encode_binary(value)
  end
end

defimpl JSON.Encoder, for: List do
  def encode(value, encoder) do
    :json.encode_list(value, encoder)
  end
end

defimpl JSON.Encoder, for: Integer do
  def encode(value, _encoder) do
    :json.encode_integer(value)
  end
end

defimpl JSON.Encoder, for: Float do
  def encode(value, _encoder) do
    :json.encode_float(value)
  end
end

defimpl JSON.Encoder, for: Map do
  def encode(value, encoder) do
    case :maps.next(:maps.iterator(value)) do
      :none ->
        "{}"

      {key, value, iterator} ->
        [?{, key(key, encoder), ?:, encoder.(value, encoder) | next(iterator, encoder)]
    end
  end

  defp next(iterator, encoder) do
    case :maps.next(iterator) do
      :none ->
        "}"

      {key, value, iterator} ->
        [?,, key(key, encoder), ?:, encoder.(value, encoder) | next(iterator, encoder)]
    end
  end

  defp key(key, encoder) when is_atom(key),
    do: encoder.(Atom.to_string(key), encoder)

  defp key(key, encoder) when is_binary(key),
    do: encoder.(key, encoder)

  defp key(key, encoder),
    do: encoder.(String.Chars.to_string(key), encoder)
end

defmodule JSON.DecodeError do
  @compile {:autoload, false}

  @moduledoc """
  The exception raised by `JSON.decode!/1`.
  """
  defexception [:message, :offset, :data]
end

defmodule JSON do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @moduledoc ~S"""
  JSON encoding and decoding.

  Both encoder and decoder fully conform to
  [RFC 8259](https://tools.ietf.org/html/rfc8259) and
  [ECMA 404](https://ecma-international.org/publications-and-standards/standards/ecma-404/)
  standards.

  > **AtomVM limitation:** Decode error offsets are not tracked.
  > `{:error, {:invalid_byte, offset, byte}}` and similar tuples
  > always report offset `0`.

  ## Encoding

  | **Elixir**                 | **JSON** |
  |----------------------------|----------|
  | `integer() \| float()`     | Number   |
  | `true \| false `           | Boolean  |
  | `nil`                      | Null     |
  | `binary()`                 | String   |
  | `atom()`                   | String   |
  | `list()`                   | Array    |
  | `%{String.Chars.t() => _}` | Object   |

  ## Decoding

  | **JSON** | **Elixir**             |
  |----------|------------------------|
  | Number   | `integer() \| float()` |
  | Boolean  | `true \| false`        |
  | Null     | `nil`                  |
  | String   | `binary()`             |
  | Object   | `%{binary() => _}`     |
  """

  @doc ~S"""
  Decodes the given JSON.

  Returns `{:ok, decoded}` or `{:error, reason}`.

  ## Examples

      iex> JSON.decode("[null,123,\"string\",{\"key\":\"value\"}]")
      {:ok, [nil, 123, "string", %{"key" => "value"}]}
  """
  @spec decode(binary()) :: {:ok, term()} | {:error, term()}
  def decode(binary) when is_binary(binary) do
    with {decoded, :ok, rest} <- decode(binary, :ok, []) do
      if rest == "" do
        {:ok, decoded}
      else
        {:error, {:invalid_byte, byte_size(binary) - byte_size(rest), :binary.at(rest, 0)}}
      end
    end
  end

  @doc ~S"""
  Decodes the given JSON with the given decoders.

  Returns `{decoded, acc, rest}` or `{:error, reason}`.

  All decoders are optional. If not provided, they will fall back to
  implementations used by the `decode/1` function.
  """
  @spec decode(binary(), term(), keyword()) ::
          {term(), term(), binary()} | {:error, term()}
  def decode(binary, acc, decoders)
      when is_binary(binary) and is_list(decoders) do
    decoders = Keyword.put_new(decoders, :null, nil)

    try do
      :json.decode(binary, acc, Map.new(decoders))
    catch
      :error, :unexpected_end ->
        {:error, {:unexpected_end, byte_size(binary)}}

      :error, {:invalid_byte, byte} ->
        {:error, {:invalid_byte, 0, byte}}

      :error, {:unexpected_sequence, bytes} ->
        {:error, {:unexpected_sequence, 0, bytes}}
    end
  end

  @doc ~S"""
  Decodes the given JSON but raises an exception in case of errors.

  Returns the decoded content. See `decode/1` for possible errors.

  ## Examples

      iex> JSON.decode!("[null,123,\"string\",{\"key\":\"value\"}]")
      [nil, 123, "string", %{"key" => "value"}]
  """
  @spec decode!(binary()) :: term()
  def decode!(binary) when is_binary(binary) do
    case decode(binary) do
      {:ok, decoded} ->
        decoded

      {:error, {:unexpected_end, offset}} ->
        raise JSON.DecodeError,
          message:
            "unexpected end of JSON binary at position " <>
              "(byte offset) #{offset}",
          data: binary,
          offset: offset

      {:error, {:invalid_byte, offset, byte}} ->
        raise JSON.DecodeError,
          message:
            "invalid byte #{byte} at position " <>
              "(byte offset) #{offset}",
          data: binary,
          offset: offset

      {:error, {:unexpected_sequence, offset, bytes}} ->
        raise JSON.DecodeError,
          message:
            "unexpected sequence #{inspect(bytes)} at position " <>
              "(byte offset) #{offset}",
          data: binary,
          offset: offset
    end
  end

  @doc ~S"""
  Encodes the given term to JSON as a binary.

  ## Examples

      iex> JSON.encode!([123, "string", %{key: "value"}])
      "[123,\"string\",{\"key\":\"value\"}]"
  """
  @spec encode!(term(), (term(), (... -> iodata()) -> iodata())) ::
          binary()
  def encode!(term, encoder \\ &protocol_encode/2) do
    IO.iodata_to_binary(encoder.(term, encoder))
  end

  @doc ~S"""
  Encodes the given term to JSON as iodata.

  This is the most efficient format if the JSON is going to be
  used for IO purposes.

  ## Examples

      iex> data = JSON.encode_to_iodata!([123, "string", %{key: "value"}])
      iex> IO.iodata_to_binary(data)
      "[123,\"string\",{\"key\":\"value\"}]"
  """
  @spec encode_to_iodata!(
          term(),
          (term(), (... -> iodata()) -> iodata())
        ) :: iodata()
  def encode_to_iodata!(term, encoder \\ &protocol_encode/2) do
    encoder.(term, encoder)
  end

  @doc """
  Default encode implementation passed to `encode!/1`.

  Optimized dispatch to the `JSON.Encoder` protocol.
  """
  def protocol_encode(value, encoder) when is_atom(value) do
    case value do
      nil -> "null"
      true -> "true"
      false -> "false"
      _ -> encoder.(Atom.to_string(value), encoder)
    end
  end

  def protocol_encode(value, _encoder) when is_binary(value),
    do: :json.encode_binary(value)

  def protocol_encode(value, _encoder) when is_integer(value),
    do: :json.encode_integer(value)

  def protocol_encode(value, _encoder) when is_float(value),
    do: :json.encode_float(value)

  def protocol_encode(value, encoder) when is_list(value),
    do: :json.encode_list(value, encoder)

  def protocol_encode(%{} = value, encoder)
      when not is_map_key(value, :__struct__),
      do: JSON.Encoder.Map.encode(value, encoder)

  def protocol_encode(value, encoder),
    do: JSON.Encoder.encode(value, encoder)
end
