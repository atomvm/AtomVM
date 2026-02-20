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

-module(test_has_map_fields).

-export([start/0]).

start() ->
    %% Test has_map_fields with 2+ keys to exercise the multi-key loop.
    M1 = id(#{a => 1, b => 2, c => 3}),
    ok = test_two_keys(M1),
    ok = test_three_keys(M1),
    ok = test_missing_key(M1),
    ok = test_partial_keys(M1),
    0.

test_two_keys(M) ->
    case M of
        #{a := _, b := _} -> ok;
        _ -> error
    end.

test_three_keys(M) ->
    case M of
        #{a := _, b := _, c := _} -> ok;
        _ -> error
    end.

test_missing_key(M) ->
    case M of
        #{a := _, d := _} -> error;
        _ -> ok
    end.

test_partial_keys(M) ->
    case M of
        #{a := _, b := _, d := _} -> error;
        _ -> ok
    end.

id(X) -> X.
