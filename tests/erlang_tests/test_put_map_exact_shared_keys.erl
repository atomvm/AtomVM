%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%% Regression test: put_map_exact on a map sharing its keys tuple must not
%% write the (equal but not identical) key operand into the shared keys
%% tuple. Doing so mutates the source map's keys in place; with generational
%% GC enabled the young key term leaks into the old generation without a
%% write barrier and dangles after the next minor collection.
%%
%% Minor collections are triggered by allocation churn:
%% erlang:garbage_collect/0 cannot be used here as it always performs a full
%% sweep, which collapses the old generation this test relies on.
-module(test_put_map_exact_shared_keys).

-export([start/0, id/1]).

start() ->
    {Pid, Ref} = spawn_opt(fun() -> run(20) end, [monitor, {fullsweep_after, 65535}]),
    receive
        {'DOWN', Ref, process, Pid, normal} -> 0
    after 30000 -> 1
    end.

run(0) ->
    ok;
run(N) ->
    K = {a, id(1)},
    M0 = #{K => id(0), id(b) => id(0)},
    % Enough garbage for at least two minor collections: the first leaves M0
    % below the high water mark, the second promotes it (and its keys tuple)
    % to the old generation.
    churn(id(200)),
    % An equal-but-not-identical young key: the exact update must reuse the
    % existing key, not store this young term into the shared keys tuple.
    K2 = {a, id(1)},
    M1 = M0#{K2 := id(1)},
    % More minor collections: if K2 leaked into the shared keys tuple it now
    % dangles into freed and reused memory.
    churn(id(200)),
    1 = map_get({a, id(1)}, M1),
    0 = map_get({a, id(1)}, M0),
    0 = map_get(id(b), M0),
    0 = map_get(id(b), M1),
    run(N - 1).

churn(0) ->
    ok;
churn(N) ->
    Garbage = seq(id(50)),
    50 = count(Garbage, 0),
    churn(N - 1).

seq(0) -> [];
seq(N) -> [{x, N} | seq(N - 1)].

count([], Acc) -> Acc;
count([_ | T], Acc) -> count(T, Acc + 1).

id(X) ->
    X.
