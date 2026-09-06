%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(maps_nifs).

-export([start/0, fact/1, make_value/1, get_expected/1, get_list/1]).

start() ->
    ok = test_maps_from_keys(),
    ok = test_maps_from_keys_dedup_boxed_terms(),
    0.

test_maps_from_keys() ->
    M1 = #{1 => [], 5 => [], 6 => [], 8 => [], 9 => [], 24 => [], 43 => [], 100 => []},
    M1 = maps:from_keys(?MODULE:get_list(1), []),

    M2 = ?MODULE:get_expected(2),
    M2 = maps:from_keys(?MODULE:get_list(2), ?MODULE:make_value(0)),

    M3 = maps:from_keys(?MODULE:get_list(3), true),
    4 = map_size(M3),
    M3 = ?MODULE:get_expected(3),

    M4 = maps:from_keys(?MODULE:get_list(4), true),
    5 = map_size(M4),
    M4 = ?MODULE:get_expected(4),

    M5 = maps:from_keys(?MODULE:get_list(5), ok),
    0 = map_size(M5),
    M5 = ?MODULE:get_expected(5),

    ok.

% maps:from_keys/2 must deduplicate keys using structural equality, not
% identity, even for boxed terms such as tuples. Each {1, 1} below is built
% by a separate recursive call to build_pairs/1, so the five tuples are
% guaranteed to be five separate heap allocations rather than one
% constant-folded literal shared by the compiler.
test_maps_from_keys_dedup_boxed_terms() ->
    List = build_pairs(5),

    M = maps:from_keys(List, true),
    1 = map_size(M),

    ok.

build_pairs(0) ->
    [];
build_pairs(N) ->
    [{1, 1} | build_pairs(N - 1)].

fact(0) ->
    1;
fact(N) ->
    fact(N - 1) * N.

make_value(N) ->
    #{
        a => {fact(N), "hello"},
        b => {test, "world", fact(N + 5)},
        c => {<<"a">>, fact(N + 4), <<"c">>}
    }.

get_list(1) ->
    [100, ?MODULE:fact(1), 9, ?MODULE:fact(3), 8, ?MODULE:fact(4), 43, 5];
get_list(2) ->
    get_list(1);
get_list(3) ->
    [5, 1, 2, fact(0), 4];
get_list(4) ->
    [5, 1, 2, fact(0), 4, 5, 1, fact(1), 9];
get_list(5) ->
    [].

get_expected(2) ->
    #{
        1 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            },
        5 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            },
        6 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            },
        8 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            },
        9 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            },
        24 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            },
        43 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            },
        100 =>
            #{
                c => {<<"a">>, 24, <<"c">>},
                a => {1, "hello"},
                b => {test, "world", 120}
            }
    };
get_expected(3) ->
    #{1 => true, 2 => true, 4 => true, 5 => true};
get_expected(4) ->
    #{1 => true, 2 => true, 4 => true, 5 => true, 9 => true};
get_expected(5) ->
    #{}.
