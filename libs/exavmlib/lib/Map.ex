defmodule Map do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  def new(list) when is_list(list), do: :maps.from_list(list)
  def new(%{} = map), do: map

  def has_key?(map, key), do: :maps.is_key(key, map)

  def fetch(map, key), do: :maps.find(key, map)

  def fetch!(map, key) do
    :maps.get(key, map)
  end

  def get(map, key, default \\ nil) do
    case map do
      %{^key => value} ->
        value

      %{} ->
        default

      other ->
        :erlang.error({:badmap, other}, [map, key, default])
    end
  end

  def put(map, key, value) do
    :maps.put(key, value, map)
  end

  def delete(map, key), do: :maps.remove(key, map)

  defdelegate merge(map1, map2), to: :maps

  def from_struct(struct) when is_atom(struct) do
    delete(struct.__struct__(), :__struct__)
  end

  def from_struct(%_{} = struct) do
    delete(struct, :__struct__)
  end

  def equal?(%{} = map1, %{} = map2), do: map1 === map2
  def equal?(%{} = map1, map2), do: :erlang.error({:badmap, map2}, [map1, map2])
  def equal?(term, other), do: :erlang.error({:badmap, term}, [term, other])
end
