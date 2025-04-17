#
# This file is part of AtomVM.
#
# Copyright 2024 Davide Bettio <davide@uninstall.it>
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
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule Tests do
  #  defstruct [
  #    :field1,
  #    field2: 42
  #  ]

  @compile {:no_warn_undefined, :undef}

  def start() do
    :ok = IO.puts("Running Elixir tests")
    :ok = test_enum()
    :ok = test_exception()
    :ok = test_chars_protocol()
    :ok = test_inspect()
    :ok = test_gen_server()
    :ok = test_supervisor()
    :ok = IO.puts("Finished Elixir tests")
  end

  defp test_enum() do
    # list
    3 = Enum.count([1, 2, 3])
    true = Enum.member?([1, 2, 3], 1)
    false = Enum.member?([1, 2, 3], 4)
    [0, 2, 4] = Enum.map([0, 1, 2], fn x -> x * 2 end)
    6 = Enum.reduce([1, 2, 3], 0, fn x, acc -> acc + x end)
    [2, 3] = Enum.slice([1, 2, 3], 1, 2)
    :test = Enum.at([0, 1, :test, 3], 2)
    :atom = Enum.find([1, 2, :atom, 3, 4], -1, fn item -> not is_integer(item) end)
    1 = Enum.find_index([:a, :b, :c], fn item -> item == :b end)
    true = Enum.find_value([-2, -3, -1, 0, 1], fn item -> item >= 0 end)
    true = Enum.all?([1, 2, 3], fn n -> n >= 0 end)
    true = Enum.any?([1, -2, 3], fn n -> n < 0 end)
    [2] = Enum.filter([1, 2, 3], fn n -> rem(n, 2) == 0 end)
    [1, 3] = Enum.reject([1, 2, 3], fn n -> rem(n, 2) == 0 end)
    :ok = Enum.each([1, 2, 3], fn n -> true = is_integer(n) end)

    # map
    2 = Enum.count(%{a: 1, b: 2})
    true = Enum.member?(%{a: 1}, {:a, 1})
    false = Enum.member?(%{a: 1}, {:b, 1})
    kw = Enum.map(%{a: 1, b: 2}, fn x -> x end)
    1 = kw[:a]
    2 = kw[:b]
    kw2 = Enum.reduce(%{a: :A, b: :B}, [], fn {k, v}, acc -> [{v, k} | acc] end)
    :a = kw2[:A]
    :b = kw2[:B]
    kw3 = Enum.slice(%{a: 1, b: 2}, 0, 1)
    true = length(kw3) == 1 and (kw3[:a] == 1 or kw3[:b] == 2)
    at_0 = Enum.at(%{a: 1, b: 2}, 0)
    at_1 = Enum.at(%{a: 1, b: 2}, 1)
    true = at_0 == {:a, 1} or at_0 == {:b, 2}
    true = at_1 == {:a, 1} or at_1 == {:b, 2}
    true = at_0 != at_1
    {:c, :atom} = Enum.find(%{a: 1, b: 2, c: :atom, d: 3}, fn {_k, v} -> not is_integer(v) end)
    {:d, 3} = Enum.find(%{a: 1, b: 2, c: :atom, d: 3}, fn {k, _v} -> k == :d end)
    true = Enum.find_value(%{"a" => 1, b: 2}, fn {k, _v} -> is_atom(k) end)
    true = Enum.all?(%{a: 1, b: 2}, fn {_k, v} -> v >= 0 end)
    true = Enum.any?(%{a: 1, b: -2}, fn {_k, v} -> v < 0 end)
    [b: 2] = Enum.filter(%{a: 1, b: 2, c: 3}, fn {_k, v} -> rem(v, 2) == 0 end)
    [] = Enum.reject(%{a: 1, b: 2, c: 3}, fn {_k, v} -> v > 0 end)
    :ok = Enum.each(%{a: 1, b: 2}, fn {_k, v} -> true = is_integer(v) end)

    # map set
    3 = Enum.count(MapSet.new([0, 1, 2]))
    true = Enum.member?(MapSet.new([1, 2, 3]), 1)
    false = Enum.member?(MapSet.new([1, 2, 3]), 4)
    [0, 2, 4] = Enum.map(MapSet.new([0, 1, 2]), fn x -> x * 2 end)
    6 = Enum.reduce(MapSet.new([1, 2, 3]), 0, fn x, acc -> acc + x end)
    [] = Enum.slice(MapSet.new([1, 2]), 1, 0)
    ms_at_0 = Enum.at(MapSet.new([1, 2]), 0)
    ms_at_1 = Enum.at(MapSet.new([1, 2]), 1)
    true = ms_at_0 == 1 or ms_at_0 == 2
    true = ms_at_1 == 1 or ms_at_1 == 2
    :atom = Enum.find(MapSet.new([1, 2, :atom, 3, 4]), fn item -> not is_integer(item) end)
    nil = Enum.find_value([-2, -3, -1, 0, 1], fn item -> item > 100 end)
    true = Enum.all?(MapSet.new([1, 2, 3]), fn n -> n >= 0 end)
    true = Enum.any?(MapSet.new([1, -2, 3]), fn n -> n < 0 end)
    [2] = Enum.filter(MapSet.new([1, 2, 3]), fn n -> rem(n, 2) == 0 end)
    [1] = Enum.reject(MapSet.new([1, 2, 3]), fn n -> n > 1 end)
    :ok = Enum.each(MapSet.new([1, 2, 3]), fn n -> true = is_integer(n) end)

    # range
    4 = Enum.count(1..4)
    true = Enum.member?(1..4, 2)
    false = Enum.member?(1..4, 5)
    [1, 2, 3, 4] = Enum.map(1..4, fn x -> x end)
    55 = Enum.reduce(1..10, 0, fn x, acc -> x + acc end)
    [6, 7, 8, 9, 10] = Enum.slice(1..10, 5, 100)
    7 = Enum.at(1..10, 6)
    8 = Enum.find(-10..10, fn item -> item >= 8 end)
    true = Enum.find_value(-10..10, fn item -> item >= 0 end)
    true = Enum.all?(0..10, fn n -> n >= 0 end)
    true = Enum.any?(-1..10, fn n -> n < 0 end)
    [0, 1, 2] = Enum.filter(-10..2, fn n -> n >= 0 end)
    [-1] = Enum.reject(-1..10, fn n -> n >= 0 end)
    :ok = Enum.each(-5..5, fn n -> true = is_integer(n) end)

    # into
    %{a: 1, b: 2} = Enum.into([a: 1, b: 2], %{})
    %{a: 2} = Enum.into([a: 1, a: 2], %{})
    expected_mapset = MapSet.new([1, 2, 3])
    ^expected_mapset = Enum.into([1, 2, 3], MapSet.new())

    # Enum.flat_map
    [:a, :a, :b, :b, :c, :c] = Enum.flat_map([:a, :b, :c], fn x -> [x, x] end)
    [1, 2, 3, 4, 5, 6] = Enum.flat_map([{1, 3}, {4, 6}], fn {x, y} -> Range.new(x, y) end)
    [[:a], [:b], [:c]] = Enum.flat_map([:a, :b, :c], fn x -> [[x]] end)

    # Enum.join
    "1, 2, 3" = Enum.join(["1", "2", "3"], ", ")
    "1, 2, 3" = Enum.join([1, 2, 3], ", ")
    "123" = Enum.join([1, 2, 3], "")

    # Enum.reverse
    [4, 3, 2] = Enum.reverse([2, 3, 4])

    # other enum functions
    test_enum_chunk_while()

    undef =
      try do
        Enum.map({1, 2}, fn x -> x end)
      rescue
        e -> e
      end

    case undef do
      %Protocol.UndefinedError{description: "", protocol: Enumerable, value: {1, 2}} ->
        :ok

      %UndefinedFunctionError{arity: 3, function: :reduce, module: Enumerable} ->
        # code compiled with OTP != 25 doesn't raise Protocol.UndefinedError
        :ok
    end

    :ok
  end

  defp test_enum_chunk_while() do
    initial_col = 4
    lines_list = '-1234567890\nciao\n12345\nabcdefghijkl\n12'
    columns = 5

    chunk_fun = fn char, {count, rchars} ->
      cond do
        char == ?\n -> {:cont, Enum.reverse(rchars), {0, []}}
        count == columns -> {:cont, Enum.reverse(rchars), {1, [char]}}
        true -> {:cont, {count + 1, [char | rchars]}}
      end
    end

    after_fun = fn
      {_count, []} -> {:cont, [], []}
      {_count, rchars} -> {:cont, Enum.reverse(rchars), []}
    end

    ['-', '12345', '67890', 'ciao', '12345', 'abcde', 'fghij', 'kl', '12'] =
      Enum.chunk_while(lines_list, {initial_col, []}, chunk_fun, after_fun)
  end

  defp test_exception() do
    ex1 =
      try do
        raise "This is a test"
      rescue
        e -> e
      end

    %RuntimeError{message: "This is a test"} = ex1

    ex2 =
      try do
        :undef.ined(1, 2)
      rescue
        e -> e
      end

    # TODO: match for arity: 2, function: :ined, module: :undef
    %UndefinedFunctionError{} = ex2

    ex3 =
      try do
        fact(5) + fact(-2)
      rescue
        e -> e
      end

    %ArithmeticError{} = ex3

    ex4 =
      try do
        :erlang.integer_to_list(fact(-2))
      rescue
        e -> e
      end

    %ArgumentError{} = ex4

    ex5 =
      try do
        a = fact(-1)
        b = fact(3)
        ^a = b
      rescue
        e -> e
      end

    %MatchError{} = ex5

    :ok
  end

  def test_chars_protocol() do
    "" = String.Chars.to_string(nil)
    "hello" = String.Chars.to_string(:hello)
    "hellø" = String.Chars.to_string(:hellø)
    "123" = String.Chars.to_string(123)
    "1.0" = String.Chars.to_string(1.0)
    "abc" = String.Chars.to_string(~c"abc")
    "test" = String.Chars.to_string("test")
    :ok
  end

  def test_inspect() do
    "true" = inspect(true)
    "false" = inspect(false)
    "nil" = inspect(nil)

    ":test" = inspect(:test)
    ":アトム" = inspect(:アトム)
    "Test" = inspect(Test)

    "5" = inspect(5)
    "5.0" = inspect(5.0)

    ~s[""] = inspect("")
    ~s["hello"] = inspect("hello")
    ~s["アトム"] = inspect("アトム")

    "<<10>>" = inspect("\n")
    "<<0, 1, 2, 3>>" = inspect(<<0, 1, 2, 3>>)
    "<<195, 168, 0>>" = inspect(<<195, 168, 0>>)

    "[]" = inspect([])
    "[0]" = inspect([0])
    "[9, 10]" = inspect([9, 10])
    ~s'["test"]' = inspect(["test"])
    "'hello'" = inspect('hello')
    "[127]" = inspect([127])
    "[104, 101, 108, 108, 248]" = inspect('hellø')

    ~s([5 | "hello"]) = inspect([5 | "hello"])

    "{}" = inspect({})
    "{1, 2}" = inspect({1, 2})
    "{:test, 1}" = inspect({:test, 1})

    "%{}" = inspect(%{})
    either("%{a: 1, b: 2}", "%{b: 2, a: 1}", inspect(%{a: 1, b: 2}))
    either(~s[%{"a" => 1, "b" => 2}], ~s[%{"b" => 2, "a" => 1}], inspect(%{"a" => 1, "b" => 2}))

    # TODO: structs are not yet supported
    # either(
    #   ~s[%#{__MODULE__}{field1: nil, field2: 42}],
    #   ~s[%#{__MODULE__}{field2: 42, field1: nil}],
    #   inspect(%__MODULE__{})
    # )

    :ok
  end

  def test_supervisor do
    # TODO fix Supervisor!
    # https://github.com/elixir-lang/elixir/blob/v1.17/lib/elixir/test/elixir/supervisor_test.exs

    # Task not available.
    # Supervisor.child_spec(Task, []) ==
    #   %{id: Task, restart: :temporary, start: {Task, :start_link, [[]]}}

    # test "start_link/3 with local"
    # which_children missing AtomVM
    #
    {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one, name: :my_sup)
    # true = Supervisor.which_children(:my_sup) == []
    Supervisor.stop(pid)

    # test "start_link/2"
    children = [{SupervisorTest.Stack, {[:hello], [name: :dyn_stack]}}]
    {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

    wait_until_registered(:dyn_stack)
    true = GenServer.call(:dyn_stack, :pop) == :hello
    true = GenServer.call(:dyn_stack, :stop) == :ok

    wait_until_registered(:dyn_stack)
    true = GenServer.call(:dyn_stack, :pop) == :hello
    Supervisor.stop(pid)

    # test "start_link/2 with old and new specs"
    children = [
      {SupervisorTest.Stack, {[:hello], []}},
      {:old_stack, {SupervisorTest.Stack, :start_link, [{[:hello], []}]}, :permanent, 5000,
       :worker, [SupervisorTest.Stack]}
    ]

    {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)
    Supervisor.stop(pid)

    # test "start_link/3"
    {:ok, pid} = Supervisor.start_link(SupervisorTest.Stack.Sup, {[:hello], [name: :stat_stack]})
    wait_until_registered(:stat_stack)
    true = GenServer.call(:stat_stack, :pop) == :hello
    Supervisor.stop(pid)

    :ok
  end

  def test_gen_server do
    # Tests are from:
    # https://github.com/elixir-lang/elixir/blob/v1.17/lib/elixir/test/elixir/gen_server_test.exs

    # test "generates child_spec/1"
    true =
      GenServerTest.Stack.child_spec([:hello]) == %{
        id: GenServerTest.Stack,
        start: {GenServerTest.Stack, :start_link, [[:hello]]}
      }

    true =
      GenServerTest.CustomStack.child_spec([:hello]) == %{
        id: :id,
        restart: :temporary,
        shutdown: :infinity,
        start: {:foo, :bar, []}
      }

    # test "start_link/3 with local"
    {:ok, pid} = GenServer.start_link(GenServerTest.Stack, [:hello], name: :stack)
    true = GenServer.call(:stack, :pop) == :hello
    :ok = GenServer.stop(pid)

    # test "start_link/2, call/2 and cast/2"
    {:ok, pid} = GenServer.start_link(GenServerTest.Stack, [:hello])

    {:links, links} = Process.info(self(), :links)
    true = pid in links

    true = GenServer.call(pid, :pop) == :hello
    true = GenServer.cast(pid, {:push, :world}) == :ok
    :timer.sleep(500)
    true = GenServer.call(pid, :pop) == :world
    :timer.sleep(50)
    true = GenServer.stop(pid) == :ok

    # true = GenServer.cast({:global, :foo}, {:push, :world}) == :ok
    # true = GenServer.cast({:via, :foo, :bar}, {:push, :world}) == :ok
    # true = GenServer.cast(:foo, {:push, :world}) == :ok

    # test "nil name"
    {:ok, pid} = GenServer.start_link(GenServerTest.Stack, [:hello], name: nil)
    # registered_name not supported on release-0.6
    Process.info(pid, :registered_name) == {:registered_name, []}
    true = GenServer.stop(pid) == :ok

    # test "start/2"
    {:ok, pid} = GenServer.start(GenServerTest.Stack, [:hello])
    {:links, links} = Process.info(self(), :links)
    false = pid in links
    GenServer.stop(pid)

    # test "whereis/1"
    name = :whereis_server

    {:ok, pid} = GenServer.start_link(GenServerTest.Stack, [], name: name)
    true = GenServer.whereis(name) == pid

    # Bus error:
    # true = GenServer.whereis({name, node()}) == pid

    # error:
    # true = GenServer.whereis({name, :another_node}) == {name, :another_node}
    true = GenServer.whereis(pid) == pid
    true = GenServer.whereis(:whereis_bad_server) == nil

    true = GenServer.stop(pid, :normal) == :ok

    # {:ok, pid} = GenServer.start_link(GenServerTest.Stack, [], name: {:global, name})
    # true = GenServer.whereis({:global, name}) == pid
    # true = GenServer.whereis({:global, :whereis_bad_server}) == nil
    # true = GenServer.whereis({:via, :global, name}) == pid
    # true = GenServer.whereis({:via, :global, :whereis_bad_server}) == nil

    # test "stop/3", %{test: name}
    name = :test
    {:ok, pid} = GenServer.start(GenServerTest.Stack, [])
    true = GenServer.stop(pid, :normal) == :ok

    # stopped_pid = pid

    # true =
    #   catch_exit(GenServer.stop(stopped_pid)) ==
    #     {:noproc, {GenServer, :stop, [stopped_pid, :normal, :infinity]}}

    # true =
    #   catch_exit(GenServer.stop(nil)) ==
    #     {:noproc, {GenServer, :stop, [nil, :normal, :infinity]}}

    # {:ok, pid} = GenServer.start(Stack, [])

    # true =
    #   GenServer.call(pid, :stop_self) ==
    #     {:calling_self, {GenServer, :stop, [pid, :normal, :infinity]}}

    {:ok, _} = GenServer.start(GenServerTest.Stack, [], name: name)
    true = GenServer.stop(name, :normal) == :ok

    :ok
  end

  defp fact(n) when n < 0, do: :test
  defp fact(0), do: 1
  defp fact(n), do: fact(n - 1) * n

  def either(a, b, value) do
    case value do
      ^a -> a
      ^b -> b
    end
  end

  defp wait_until_registered(name) do
    unless Process.whereis(name) do
      wait_until_registered(name)
    end
  end
end
