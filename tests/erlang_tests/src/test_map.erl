%
% This file is part of AtomVM.
%
% Copyright 2021 Fred Dushin <fred@dushin.net>
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

-module(test_map).

-export([start/0]).

start() ->
    ok = test_is_map_bif(),
    ok = test_map_size_bif(),
    ok = test_is_map_key_bif(),
    ok = test_map_get_bif(),
    ok = test_literal_map(),
    ok = test_extend_map(),
    ok = test_exact_map(),
    ok = test_generate_map(),
    ok = test_compare(),
    ok = test_match_case(),
    ok = test_match_clause(),
    ok = test_external_terms(),
    0.

test_is_map_bif() ->
    ok =
        case id(#{a => 1, b => 2}) of
            X when is_map(X) ->
                true = is_map(X),
                ok;
            _ ->
                fail
        end,
    ok =
        case id(foo) of
            Y when is_map(Y) ->
                fail;
            _ ->
                ok
        end.

test_map_size_bif() ->
    ok =
        case id(#{a => 1, b => 2}) of
            X when map_size(X) > 0 ->
                2 = map_size(X),
                ok;
            _ ->
                fail
        end,
    ok =
        case id(#{}) of
            Y when map_size(Y) > 0 ->
                0 = map_size(Y),
                fail;
            _ ->
                ok
        end,
    ok =
        try
            map_size(foo),
            fail
        catch
            _:{badmap, _} ->
                ok
        end.

%% OTP 21 or later
-ifdef(OTP_RELEASE).
test_is_map_key_bif() ->
    ok =
        case id(#{a => 1, b => 2}) of
            X when is_map_key(b, X) ->
                true = is_map_key(a, X),
                true = is_map_key(b, X),
                false = is_map_key(c, X),
                ok;
            _ ->
                fail
        end,
    ok =
        try
            case is_map_key(b, id(foo)) of
                true -> fail;
                false -> fail_as_well
            end
        catch
            _:{badmap, _} ->
                ok
        end,
    ok =
        try
            NotAMap = id(foo),
            if
                is_map_key(b, NotAMap) -> fail;
                true -> ok
            end
        catch
            _:{badmap, _} ->
                fail
        end.

test_map_get_bif() ->
    ok =
        case id(#{a => 1, b => 2}) of
            X when map_get(b, X) =:= 2 ->
                1 = map_get(a, X),
                2 = map_get(b, X),
                false = is_map_key(c, X),
                try
                    _ = map_get(c, X),
                    fail
                catch
                    _:{badkey, _} ->
                        ok;
                    _:_ ->
                        fail_catch
                end;
            _ ->
                fail
        end,
    ok =
        try
            _ = map_get(b, id(foo)),
            fail
        catch
            _:{badmap, _} ->
                ok
        end.
-else.
test_is_map_key_bif() -> ok.

test_map_get_bif() -> ok.
-endif.

test_literal_map() ->
    Map = #{a => 1, b => 2},
    #{a := A, b := B} = Map,
    A = 1,
    B = 2,
    %% No such c
    try
        #{c := _C} = Map,
        fail
    catch
        _:_E ->
            ok
    end.

test_extend_map() ->
    Map = #{a => 1, b => id(2)},
    #{a := A, b := B} = Map,
    A = 1,
    B = 2,
    Map2 = Map#{c => 3},
    #{c := C2} = Map2,
    C2 = 3,
    Map3 = Map2#{c => bar, d => tapas},
    #{c := C3, d := D3} = Map3,
    C3 = bar,
    D3 = tapas,
    %% No such c
    try
        #{e := _} = Map3,
        fail
    catch
        _:_E ->
            ok
    end.

test_exact_map() ->
    Map = #{a => 1, b => 2},
    #{a := A, b := B} = Map,
    A = 1,
    B = 2,
    Map2 = Map#{a := id(foo)},
    #{a := A2, b := B} = Map2,
    A2 = foo,
    %% No such c
    try
        _ = id(Map2#{id(c) := id(bar)}),
        fail
    catch
        _:_E ->
            ok
    end.

test_generate_map() ->
    ok = test_entries([]),
    ok = test_entries([{a, 1}, {b, 2}, {c, 3}]),
    ok = test_entries([{[foo, bar], {tapas, gnu}}, {gnu, #{gnat => top}}]),
    ok = test_entries(generate_random_entries(32)).

test_entries(KVList) ->
    Map = build_map(KVList),
    test_entries(Map, remove_duplicates(KVList)).

test_entries(_Map, []) ->
    ok;
test_entries(Map, [{K, V} | T]) ->
    #{K := V1} = Map,
    case V1 of
        V ->
            ok;
        SomethingElse ->
            erlang:display({expected, V, got, SomethingElse}),
            true = id(false)
    end,
    test_entries(Map, T).

test_match_case() ->
    Map1 = #{a => id(1), b => id(2), c => id(3)},
    Map2 = #{a => id(1), b => id(2), c => id(3)},
    ok =
        case Map1 of
            Map2 -> ok;
            _ -> fail
        end,
    Map3 = #{foo => id(bar), bar => id(tapas), tapas => id(yum)},
    ok =
        case Map3 of
            #{foo := bar} ->
                ok;
            _ ->
                fail
        end,
    ok =
        case Map3 of
            #{foo := baz} ->
                fail;
            _ ->
                ok
        end.

test_match_clause() ->
    ok = match_clause(#{a => id(1), b => id(2), c => id(3)}),
    fail = match_clause(#{a => id(foo), b => id(2), c => id(3)}),
    ok.

match_clause(#{a := 1, b := 2}) ->
    ok;
match_clause(_) ->
    fail.

test_compare() ->
    Map1 = #{a => id(1), b => id(2), c => id(3)},
    Map2 = #{a => id(1), b => id(2), c => id(3)},
    Map3 = #{foo => id(bar), bar => id(tapas), tapas => id(yum)},
    true = Map1 == Map2,
    true = Map1 =:= Map2,
    false = Map1 =/= Map2,
    false = Map2 == Map3,
    false = Map2 =:= Map3,
    false = Map1 < Map2,
    true = Map2 < Map3,
    false = Map3 < Map2,
    true = #{} < build_map(generate_random_entries(8)),
    true = #{a => id(1), b => id(2)} < #{a => id(1), b => id(2), c => id(3)},
    true = #{a => id(1), b => id(2)} < #{a => id(1), b => id(tapas)},
    true = #{a => id(1), b => id(2)} < #{foo => id(bar), bar => id(tapas)},
    true = #{a => id(1), b => id(2)} < #{a => id(1), b => id(3)},
    true = create_test_map([{a, 1}, {b, 2}, {c, 3}]) == create_test_map([{a, 1}, {b, 2}, {c, 3}]),
    true = create_test_map([{b, 2}, {a, 1}, {c, 3}]) == create_test_map([{a, 1}, {c, 3}, {b, 2}]),
    ok.

test_external_terms() ->
    Map = build_map(generate_random_entries(8)),
    Bin = term_to_binary(Map),
    Map2 = binary_to_term(Bin),
    true = Map =:= Map2,
    ok.

build_map(KVList) ->
    build_map(KVList, #{}).

build_map([], Accum) ->
    Accum;
build_map([{K, V} | T], Accum) ->
    NewAccum = Accum#{K => V},
    build_map(T, NewAccum).

generate_random_entries(Size) ->
    generate_random_entries(Size, []).

generate_random_entries(0, Accum) ->
    Accum;
generate_random_entries(N, Accum) ->
    generate_random_entries(N - 1, [generate_random_entry() | Accum]).

generate_random_entry() ->
    {random_term(), random_term()}.

random_term() ->
    case random_uniform(5) of
        1 ->
            random_atom();
        2 ->
            random_int();
        3 ->
            random_tuple();
        4 ->
            random_binary();
        5 ->
            random_list()
    end.

random_atom() ->
    case random_uniform(3) of
        1 -> foo;
        2 -> bar;
        3 -> tapas
    end.

random_int() ->
    random_uniform(256).

random_tuple() ->
    case random_uniform(3) of
        1 ->
            {random_atom()};
        2 ->
            {random_atom(), random_int()};
        3 ->
            {random_atom(), random_int(), random_int()}
    end.

random_binary() ->
    Size = random_uniform(3) * 50,
    rand_bytes(Size).

random_list() ->
    binary_to_list(random_binary()).

remove_duplicates(KVList) ->
    reverse(remove_duplicates(reverse(KVList), [])).

remove_duplicates([], Accum) ->
    Accum;
remove_duplicates([{K, _V} = E | T], Accum) ->
    case has_key(Accum, K) of
        true ->
            remove_duplicates(T, Accum);
        _ ->
            remove_duplicates(T, [E | Accum])
    end.

has_key([], _K) -> false;
has_key([{K, _V} | _], K) -> true;
has_key([_ | T], K) -> has_key(T, K).

reverse(L) -> reverse(L, []).

reverse([], A) ->
    A;
reverse([H | T], Accum) ->
    reverse(T, [H | Accum]).

create_test_map(List) ->
    create_test_map(List, #{}).

create_test_map([], Accum) ->
    Accum;
create_test_map([{K, V} | T], Accum) ->
    create_test_map(T, Accum#{K => V}).

id(X) -> X.

rand_bytes(I) ->
    case erlang:system_info(machine) of
        "BEAM" -> crypto:strong_rand_bytes(I);
        _ -> atomvm:rand_bytes(I)
    end.

random_uniform(N) ->
    case erlang:system_info(machine) of
        "BEAM" -> rand:uniform(N);
        _ -> (abs(atomvm:random()) rem N) + 1
    end.
