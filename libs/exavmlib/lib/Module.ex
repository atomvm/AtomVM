defmodule Module do
  @compile {:autoload, false}

  def concat(a, b) when is_atom(a) and is_atom(b) do
    a_string = :erlang.atom_to_binary(a, :latin1)
    <<"Elixir.", b_string::binary>> = :erlang.atom_to_binary(b, :latin1)

    :erlang.binary_to_atom(a_string <> "." <> b_string, :latin1)
  end
end
