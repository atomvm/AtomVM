%
% This file is part of AtomVM.
%
% Copyright 2024 Tomasz Sobkiewicz <tomasz.sobkiewt>
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

-module(is_record).

-record(person, {id, name, age}).

-export([start/0, id/1, is_person/1, fail_with_badarg/1]).

-define(ID(Arg), ?MODULE:id(Arg)).

start() ->
    Mike = #person{
        id = 1,
        name = "Mike",
        age = 32
    },
    IsRecord = ?ID(fun erlang:is_record/2),
    true = IsRecord(?ID({person, 1, 2, 3}), ?ID(person)),
    true = erlang:is_record(?ID({person, 1, 2, 3}), ?ID(person)),
    true = erlang:is_record(?ID({person}), ?ID(person)),
    true = erlang:is_record(?ID(Mike), ?ID(person)),
    true = ?MODULE:is_person(?ID(Mike)),

    false = ?MODULE:is_person(?ID({tuple, 1, 2})),
    false = erlang:is_record(?ID(Mike), ?ID(foo)),
    false = erlang:is_record(?ID({person, 1, 2, 3}), ?ID(foo)),
    false = erlang:is_record(?ID({}), ?ID(person)),
    false = erlang:is_record(?ID([]), ?ID(person)),
    ok = fail_with_badarg(fun() -> erlang:is_record(?ID(Mike), ?ID(1)) end),
    ok = fail_with_badarg(fun() -> erlang:is_record(?ID({}), ?ID(1)) end),
    0.

id(T) ->
    T.

is_person(T) when is_record(T, person) -> true;
is_person(_X) -> false.

fail_with_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.
