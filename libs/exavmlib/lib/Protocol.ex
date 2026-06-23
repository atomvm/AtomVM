# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec
# SPDX-FileCopyrightText: 2026 Peter Madsen-Mygdal <petermm@gmail.com>

defmodule Protocol do
  @moduledoc false
  @compile {:autoload, false}

  # Runtime-only shim for AtomVM. exavmlib ships only the protocol-name
  # concatenation used by generated protocol dispatch code; protocol
  # definition, derivation, and consolidation must happen on the host
  # compiler.
  # ExAtomVM precompiled, unconsolidated protocols continue to dispatch
  # normally at runtime.
  @doc false
  def __concat__(left, right) do
    left = :erlang.atom_to_binary(left)
    right = :erlang.atom_to_binary(right)

    left =
      case left do
        <<"Elixir.", _::binary>> -> left
        _ -> <<"Elixir.", left::binary>>
      end

    right =
      case right do
        <<"Elixir.", right::binary>> -> right
        _ -> right
      end

    :erlang.binary_to_atom(<<left::binary, ".", right::binary>>)
  end
end
