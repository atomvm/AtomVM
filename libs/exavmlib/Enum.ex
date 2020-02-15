defmodule Enum do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  def reduce(enumerable, acc, fun) when is_list(enumerable) do
    :avm_lists.foldl(fun, acc, enumerable)
  end

  def all?(enumerable, fun) when is_list(enumerable) do
    all_list(enumerable, fun)
  end

  def any?(enumerable, fun) when is_list(enumerable) do
    any_list(enumerable, fun)
  end

  def count(enumerable) when is_list(enumerable) do
    length(enumerable)
  end

  def each(enumerable, fun) when is_list(enumerable) do
    :avm_lists.foreach(fun, enumerable)
    :ok
  end

  def filter(enumerable, fun) when is_list(enumerable) do
    filter_list(enumerable, fun)
  end

  def find(enumerable, default, fun) when is_list(enumerable) do
    find_list(enumerable, default, fun)
  end

  def find_index(enumerable, fun) when is_list(enumerable) do
    find_index_list(enumerable, 0, fun)
  end

  def find_value(enumerable, default, fun) when is_list(enumerable) do
    find_value_list(enumerable, default, fun)
  end

  def map(enumerable, fun) when is_list(enumerable) do
    :avm_lists.map(fun, enumerable)
  end

  def member?(enumerable, element) when is_list(enumerable) do
    :avm_lists.member(element, enumerable)
  end

  def reject(enumerable, fun) when is_list(enumerable) do
    reject_list(enumerable, fun)
  end

  ## all?

  defp all_list([h | t], fun) do
    if fun.(h) do
      all_list(t, fun)
    else
      false
    end
  end

  defp all_list([], _) do
    true
  end

  ## any?

  defp any_list([h | t], fun) do
    if fun.(h) do
      true
    else
      any_list(t, fun)
    end
  end

  defp any_list([], _) do
    false
  end

  ## filter

  defp filter_list([head | tail], fun) do
    if fun.(head) do
      [head | filter_list(tail, fun)]
    else
      filter_list(tail, fun)
    end
  end

  defp filter_list([], _fun) do
    []
  end

  ## find

  defp find_list([head | tail], default, fun) do
    if fun.(head) do
      head
    else
      find_list(tail, default, fun)
    end
  end

  defp find_list([], default, _) do
    default
  end

  ## find_index

  defp find_index_list([head | tail], counter, fun) do
    if fun.(head) do
      counter
    else
      find_index_list(tail, counter + 1, fun)
    end
  end

  defp find_index_list([], _, _) do
    nil
  end

  ## find_value

  defp find_value_list([head | tail], default, fun) do
    fun.(head) || find_value_list(tail, default, fun)
  end

  defp find_value_list([], default, _) do
    default
  end

  ## reject

  defp reject_list([head | tail], fun) do
    if fun.(head) do
      reject_list(tail, fun)
    else
      [head | reject_list(tail, fun)]
    end
  end

  defp reject_list([], _fun) do
    []
  end
end
