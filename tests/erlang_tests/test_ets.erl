%
% This file is part of AtomVM.
%
% Copyright 2024 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_ets).

-export([start/0]).

start() ->
    ok = test_basic(),
    ok = test_named_table(),
    ok = test_keypos(),
    ok = test_key_types(),
    ok = test_private_access(),
    ok = test_protected_access(),
    ok = test_public_access(),
    ok = test_lookup_element(),
    ok = test_insert_list(),
    ok = test_update_counter(),
    ok = test_delete_table(),
    0.

test_basic() ->
    test_basic([]).

test_named_table() ->
    test_basic([named_table]).

test_basic(Options) ->
    {ok, Tid} = run_test(fun test_basic_fun/2, Options),

    %%
    %% The table should no longer exist
    %%
    sleep(25),
    ok = expect_failure(
        fun() ->
            ets:lookup(Tid, foo)
        end
    ),
    ok.

test_basic_fun(Pid, Options) ->
    expect_failure(fun() -> ets:new([isnt, an, atom], []) end),
    expect_failure(fun() -> ets:new(bad_options, not_a_list) end),

    Tid = ets:new(test, Options),

    %% check name in use
    case member(named_table, Options) of
        true ->
            expect_failure(fun() -> ets:new(test, Options) end),
            %% you can still create an un-named table with the same name
            _Tid = ets:new(test, []);
        _ ->
            _Tid = ets:new(test, [])
    end,

    [] = ets:lookup(Tid, foo),
    true = ets:insert(Tid, {foo, bar}),
    [{foo, bar}] = ets:lookup(Tid, foo),

    [] = ets:lookup(Tid, does_not_exist),

    true = ets:insert(Tid, {foo, tapas}),
    [{foo, tapas}] = ets:lookup(Tid, foo),

    true = ets:delete(Tid, does_not_exist),
    [] = ets:lookup(Tid, does_not_exist),
    true = ets:delete(Tid, foo),
    [] = ets:lookup(Tid, foo),

    true = ets:insert(Tid, {foo, bar}),
    true = ets:insert(Tid, {gnu, gnat}),
    true = ets:insert(Tid, {bar, tapas}),

    true = ets:delete(Tid, foo),
    [{gnu, gnat}] = ets:lookup(Tid, gnu),
    [{bar, tapas}] = ets:lookup(Tid, bar),
    true = ets:delete(Tid, gnu),
    [{bar, tapas}] = ets:lookup(Tid, bar),
    [] = ets:lookup(Tid, gnu),
    true = ets:delete(Tid, bar),
    [] = ets:lookup(Tid, bar),

    [] = ets:lookup(Tid, #{some => structured, key => [a, b, c]}),
    true = ets:insert(Tid, {#{some => structured, key => [a, b, c]}, bar}),
    [{#{some := structured, key := [a, b, c]}, bar}] = ets:lookup(Tid, #{
        some => structured, key => [a, b, c]
    }),

    expect_failure(fun() -> ets:insert(Tid, {}) end),
    expect_failure(fun() -> ets:insert(Tid, not_a_tuple) end),
    expect_failure(fun() -> ets:insert([isnt, a, table, reference], {foo, bar}) end),
    expect_failure(fun() -> ets:lookup([isnt, a, table, reference], foo) end),
    expect_failure(fun() -> ets:delete([isnt, a, table, reference], foo) end),

    Pid ! {ok, Tid}.

test_keypos() ->
    ok = run_test(fun test_keypos_fun/2, []),
    ok.

test_keypos_fun(Pid, _Options) ->
    expect_failure(fun() -> ets:new(bad_keypos, -1) end),

    Tid = ets:new(test, [{keypos, 2}]),

    true = ets:insert(Tid, {foo, bar}),
    true = ets:insert(Tid, {gnu, gnat}),
    true = ets:insert(Tid, {bar, tapas}),

    [{foo, bar}] = ets:lookup(Tid, bar),
    [{gnu, gnat}] = ets:lookup(Tid, gnat),
    [{bar, tapas}] = ets:lookup(Tid, tapas),

    expect_failure(fun() -> ets:insert(Tid, {}) end),
    expect_failure(fun() -> ets:insert(Tid, {arity_1}) end),

    Pid ! ok.

test_key_types() ->
    ok = run_test(fun test_key_types_fun/2, []),
    ok.

test_key_types_fun(Pid, _Options) ->
    Tid = ets:new(test, []),
    EchoServer = spawn_opt(fun echo_server/0, []),
    register(echo, EchoServer),

    ok = test_key_insert_lookup(
        Tid,
        some_atom
    ),
    ok = test_key_insert_lookup(
        Tid,
        12345
    ),
    ok = test_key_insert_lookup(
        Tid,
        0
    ),
    ok = test_key_insert_lookup(
        Tid,
        -12345
    ),
    ok = test_key_insert_lookup(
        Tid,
        3.14159365
    ),
    ok = test_key_insert_lookup(
        Tid,
        self()
    ),
    ok = test_key_insert_lookup(
        Tid,
        erlang:make_ref()
    ),
    ok = test_key_insert_lookup(
        Tid,
        <<"fubar">>
    ),
    ok = test_key_insert_lookup(
        Tid,
        <<"">>
    ),
    ok = test_key_insert_lookup(
        Tid,
        {some_atom, 1234}
    ),
    ok = test_key_insert_lookup(
        Tid,
        [a, b, c, self(), 3.1415265]
    ),
    ok = test_key_insert_lookup(
        Tid,
        [a | b]
    ),
    ok = test_key_insert_lookup(
        Tid,
        #{
            some_atom => {a, b, c},
            #{another => "map"} => erlang:make_ref(),
            <<1, 2, 3, 4>> => <<-4, -3, -2, -1>>
        }
    ),

    EchoServer ! halt,

    Pid ! ok.

test_key_insert_lookup(Tid, Key) ->
    true = ets:insert(Tid, {Key, value}),
    [{Key, value}] = ets:lookup(Tid, echo(Key)),
    ok.

test_private_access() ->
    Self = self(),
    Pid = spawn_opt(fun() -> test_access_fun(Self, [private]) end, []),

    Pid ! get_table,
    Tid =
        receive
            {table, T} ->
                T
        after 1000 ->
            error(timeout_wait_for_table)
        end,

    ok = expect_failure(
        fun() -> ets:insert(Tid, {gnu, gnat}) end
    ),
    ok = expect_failure(
        fun() -> ets:lookup(Tid, foo) end
    ),

    Pid ! halt,
    ok.

test_protected_access() ->
    Self = self(),
    Pid = spawn_opt(fun() -> test_access_fun(Self, [protected]) end, []),

    Pid ! get_table,
    Tid =
        receive
            {table, T} ->
                T
        after 1000 ->
            error(timeout_wait_for_table)
        end,

    ok = expect_failure(
        fun() -> ets:insert(Tid, {gnu, gnat}) end
    ),
    [{foo, bar}] = ets:lookup(Tid, foo),

    Pid ! halt,
    ok.

test_public_access() ->
    Self = self(),
    Pid = spawn_opt(fun() -> test_access_fun(Self, [public]) end, []),

    Pid ! get_table,
    Tid =
        receive
            {table, T} ->
                T
        after 1000 ->
            error(timeout_wait_for_table)
        end,

    true = ets:insert(Tid, {gnu, gnat}),
    [{foo, bar}] = ets:lookup(Tid, foo),

    Pid ! halt,
    ok.

test_access_fun(Pid, Options) ->
    Tid = ets:new(test, Options),

    true = ets:insert(Tid, {foo, bar}),

    receive
        get_table ->
            Pid ! {table, Tid}
    end,

    receive
        halt ->
            ok
    end.

run_test(Fun, Options) ->
    Self = self(),
    spawn_opt(fun() -> Fun(Self, Options) end, []),
    wait_for_test_result().

wait_for_test_result() ->
    receive
        Result ->
            Result
    after 1000 ->
        {error, timeout_waiting_for_test_result}
    end.

expect_failure(Fun) ->
    expect_failure(Fun, error, badarg).

expect_failure(Fun, Class, Error) ->
    try
        Fun(),
        fail
    catch
        Class:Error ->
            ok;
        OtherClass:OtherError ->
            {fail, OtherClass, OtherError}
    end.

sleep(Ms) ->
    receive
    after Ms ->
        ok
    end.

member(_Element, []) ->
    false;
member(Element, [Element | _]) ->
    true;
member(Element, [_ | Tail]) ->
    member(Element, Tail).

echo_server() ->
    receive
        {echo, Term, Pid} ->
            Pid ! Term,
            echo_server();
        halt ->
            ok
    end.

echo(Term) ->
    EchoServer = whereis(echo),
    EchoServer ! {echo, Term, self()},
    receive
        T ->
            T
    end.

test_lookup_element() ->
    Tid = ets:new(test_lookup_element, []),
    true = ets:insert(Tid, {foo, tapas}),
    foo = ets:lookup_element(Tid, foo, 1),
    tapas = ets:lookup_element(Tid, foo, 2),
    expect_failure(fun() -> ets:lookup_element(Tid, bar, 1) end),
    expect_failure(fun() -> ets:lookup_element(Tid, foo, 3) end),
    expect_failure(fun() -> ets:lookup_element(Tid, foo, 0) end),
    ok.

test_insert_list() ->
    Tid = ets:new(test_insert_list, []),
    true = ets:insert(Tid, [{foo, tapas}, {batat, batat}, {patat, patat}]),
    true = ets:insert(Tid, [{foo, tapas}, {batat, batat}, {patat, patat}]),
    [{patat, patat}] = ets:lookup(Tid, patat),
    [{batat, batat}] = ets:lookup(Tid, batat),
    true = ets:insert(Tid, []),
    expect_failure(fun() -> ets:insert(Tid, [{foo, tapas} | {patat, patat}]) end),
    expect_failure(fun() -> ets:insert(Tid, [{foo, tapas}, {batat, batat}, {patat, patat}, {}]) end),
    expect_failure(fun() ->
        ets:insert(Tid, [{foo, tapas}, pararara, {batat, batat}, {patat, patat}])
    end),
    expect_failure(fun() -> ets:insert(Tid, [{}]) end),
    ok.

test_update_counter() ->
    Tid = ets:new(test_lookup_element, []),
    true = ets:insert(Tid, {foo, 1, 2, 3}),
    3 = ets:update_counter(Tid, foo, 2),
    expect_failure(fun() -> ets:update_counter(Tid, tapas, 2) end),
    5 = ets:update_counter(Tid, tapas, 2, {batat, 3}),
    [] = ets:lookup(Tid, batat),
    [{tapas, 5}] = ets:lookup(Tid, tapas),
    0 = ets:update_counter(Tid, foo, {3, -2}),
    expect_failure(fun() -> ets:update_counter(Tid, foo, {-3, -2}) end),
    expect_failure(fun() -> ets:update_counter(Tid, foo, {30, -2}) end),
    expect_failure(fun() -> ets:update_counter(Tid, patatas, {3, -2}, {cow, 1}) end),
    0 = ets:update_counter(Tid, patatas, {3, -2}, {cow, 1, 2, 3}),
    0 = ets:update_counter(Tid, patatas, {3, -2, 0, 0}),
    10 = ets:update_counter(Tid, patatas, {3, 10, 10, 0}),
    0 = ets:update_counter(Tid, patatas, {3, 10, 10, 0}),
    ok.

test_delete_table() ->
    Tid = ets:new(test_delete_table, []),
    true = ets:delete(Tid),
    ok = expect_failure(
        fun() -> ets:insert(Tid, {gnu, gnat}) end
    ),
    Ntid = ets:new(test_delete_table, []),
    true = ets:delete(Ntid),
    ok = expect_failure(fun() -> ets:delete(Ntid) end),
    ok = expect_failure(fun() -> ets:delete(Tid) end),
    ok = expect_failure(fun() -> ets:delete(non_existent) end),
    ok.
