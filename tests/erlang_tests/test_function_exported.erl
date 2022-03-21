%
% This file is part of AtomVM.
%
% Copyright 2021 Davide Bettio <davide@uninstall.it>
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

-module(test_function_exported).

-export([start/0, c/2, fail/3]).

start() ->
    fail(id(erlang), id(5), 1) +
        fail(id(5), id(display), 1) +
        fail(id(erlang), id(display), foo) +
        c(erlang:function_exported(erlang, display, 1), true) +
        c(erlang:function_exported(erlang, display, 2), false) +
        c(erlang:function_exported(erlang, displayz, 1), false) +
        c(erlang:function_exported(foo, display, 1), false) +
        c(erlang:function_exported(?MODULE, c, 1), false) +
        c(erlang:function_exported(?MODULE, c, 2), true) +
        c(erlang:function_exported(?MODULE, foo, 1), false).

c(A, A) when is_boolean(A) ->
    1;
c(A, B) when is_boolean(A) andalso is_boolean(B) ->
    0.

fail(Module, Function, Arity) ->
    try erlang:function_exported(Module, Function, Arity) of
        _A -> 1
    catch
        error:badarg -> 0
    end.

id(A) ->
    A.
