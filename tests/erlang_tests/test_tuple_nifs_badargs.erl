%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(test_tuple_nifs_badargs).

-export([
    start/0,
    make_tuple2/2,
    insert_element3/3,
    delete_element2/2,
    setelement3/3,
    tuple_to_list1/1
]).

start() ->
    make_tuple2([], []) + insert_element3(0, {1, 2}, element) +
        insert_element3(1, <<"test">>, element) * 4 + insert_element3(4, {1, 2}, element) * 16 +
        delete_element2(0, {1, 2}) + delete_element2(1, <<"test">>) * 4 +
        setelement3(0, {1, 2}, element) + setelement3(1, <<"test">>, element) * 4 +
        setelement3(3, {1, 2}, element) * 16 + tuple_to_list1(<<"test">>) +
        make_tuple2(-1, test) * 1048576.

make_tuple2(A, B) ->
    try erlang:make_tuple(A, B) of
        Result -> Result
    catch
        error:badarg -> -1;
        _:_ -> -2
    end.

insert_element3(A, B, C) ->
    try erlang:insert_element(A, B, C) of
        Result -> Result
    catch
        error:badarg -> -4;
        _:_ -> -8
    end.

delete_element2(A, B) ->
    try erlang:delete_element(A, B) of
        Result -> Result
    catch
        error:badarg -> -256;
        _:_ -> -512
    end.

setelement3(A, B, C) ->
    try erlang:setelement(A, B, C) of
        Result -> Result
    catch
        error:badarg -> -4096;
        _:_ -> -8192
    end.

tuple_to_list1(A) ->
    try erlang:tuple_to_list(A) of
        Result -> Result
    catch
        error:badarg -> -262144;
        _:_ -> -524288
    end.
