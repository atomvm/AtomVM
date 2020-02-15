defmodule Kernel do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  def inspect(term, opts \\ []) when is_list(opts) do
    case term do
      t when is_atom(t) ->
        :erlang.atom_to_binary(t, :latin1)

      t when is_integer(t) ->
        :erlang.integer_to_binary(t)

      t when is_list(t) ->
        # TODO: escape unprintable lists
        :erlang.list_to_binary(t)

      t when is_pid(t) ->
        :erlang.pid_to_list(t)
        |> :erlang.list_to_binary()

      t when is_function(t) ->
        :erlang.fun_to_list(t)
        |> :erlang.list_to_binary()

      t when is_tuple(t) ->
        [?{ | t |> :erlang.tuple_to_list() |> inspect_join(?})]
        |> :erlang.list_to_binary()

      t when is_binary(t) ->
        # TODO: escape unprintable binaries
        t

      t when is_reference(t) ->
        :erlang.ref_to_list(t)
        |> :erlang.list_to_binary()

      t when is_float(t) ->
        :erlang.float_to_binary(t)
    end
  end

  defp inspect_join([], last) do
    [last]
  end

  defp inspect_join([e], last) do
    [inspect(e), last]
  end

  defp inspect_join([h | t], last) do
    [inspect(h), ?, , ?\s | inspect_join(t, last)]
  end
end
