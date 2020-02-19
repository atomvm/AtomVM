defmodule Keyword do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  def fetch(keywords, key) when is_list(keywords) and is_atom(key) do
    case :avm_lists.keyfind(key, 1, keywords) do
      {^key, value} -> {:ok, value}
      false -> :error
    end
  end

  def fetch!(keywords, key) when is_list(keywords) and is_atom(key) do
    case :avm_lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> raise(KeyError, key: key, term: keywords)
    end
  end

  def get(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case :avm_lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> default
    end
  end

  def get_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case :avm_lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> fun.()
    end
  end

  def put(keywords, key, value) when is_list(keywords) and is_atom(key) do
    [{key, value} | delete(keywords, key)]
  end

  def delete(keywords, key) when is_list(keywords) and is_atom(key) do
    case :avm_lists.keymember(key, 1, keywords) do
      true -> delete_key(keywords, key)
      _ -> keywords
    end
  end

  defp delete_key([{key, _} | tail], key), do: delete_key(tail, key)
  defp delete_key([{_, _} = pair | tail], key), do: [pair | delete_key(tail, key)]
  defp delete_key([], _key), do: []

end
