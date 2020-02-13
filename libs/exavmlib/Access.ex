defmodule Access do
  def fetch(list, key) when is_list(list) and is_atom(key) do
    case :avm_lists.keyfind(key, 1, list) do
      {_, value} -> {:ok, value}
      false -> :error
    end
  end

  def fetch(list, key) when is_list(list) do
    raise ArgumentError,
          "the Access calls for keywords expect the key to be an atom, got: " <> inspect(key)
  end

  def fetch(nil, _key) do
    :error
  end

  def fetch!(container, key) do
    case fetch(container, key) do
      {:ok, value} -> value
      :error -> raise(KeyError, key: key, term: container)
    end
  end

  def get(list, key, default) when is_list(list) and is_atom(key) do
    case :avm_lists.keyfind(key, 1, list) do
      {_, value} -> value
      false -> default
    end
  end

  def get(list, key, _default) when is_list(list) do
    raise ArgumentError,
          "the Access calls for keywords expect the key to be an atom, got: " <> inspect(key)
  end

  def get(nil, _key, default) do
    default
  end
end
