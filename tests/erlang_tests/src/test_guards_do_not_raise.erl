%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_guards_do_not_raise).

-export([start/0, id/1]).

start() ->
    ok = test_gc_bif1(),
    ok = test_gc_bif2(),
    ok = test_gc_bif3(),
    ok = test_bif1(),
    ok = test_bif2(),
    0.

test_gc_bif1() ->
    NotAList = id(not_a_list),
    if
        length(NotAList) < 42 -> fail;
        true -> ok
    end.

test_gc_bif2() ->
    NotAnInteger = id(not_an_integer),
    if
        NotAnInteger rem 42 < 20 -> fail;
        true -> ok
    end.

test_gc_bif3() ->
    case erlang:function_exported(erlang, binary_part, 3) of
        true ->
            NotABin = id(not_a_bin),
            ok =
                if
                    binary_part(NotABin, 0, 2) =:= <<"he">> -> fail;
                    true -> ok
                end;
        false ->
            erlang:display({warning, erlang, binary_part, 3, unimplemented}),
            ok
    end.

test_bif1() ->
    NotAList = id(not_a_list),
    if
        tl(NotAList) > 1 -> fail;
        true -> ok
    end.

test_bif2() ->
    NotAMap = id(not_a_map),
    if
        is_map_key(b, NotAMap) -> fail;
        true -> ok
    end.

id(X) ->
    X.
