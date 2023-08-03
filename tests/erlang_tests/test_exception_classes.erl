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

-module(test_exception_classes).

-export([start/0]).

start() ->
    ok = test_exit(),
    ok = test_throw(),
    ok = test_error(),
    ok = test_inner_exit(),
    ok = test_after_catch_throw(),
    ok = test_after_throw(),
    ok = test_raise_error(),
    ok = test_raise_exit(),
    ok = test_raise_badarg(),
    0.

test_exit() ->
    try
        exit(foo)
    catch
        exit:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_throw() ->
    try
        throw(foo)
    catch
        throw:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_error() ->
    try
        error(foo)
    catch
        error:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_inner_exit() ->
    try
        try
            exit(foo)
        catch
            error:foo -> {unexpected, ?LINE}
        end
    catch
        exit:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_after_catch_throw() ->
    try
        try
            exit(foo)
        after
            try
                throw(foo)
            catch
                throw:foo -> ok
            end
        end
    catch
        exit:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_after_throw() ->
    try
        try
            exit(foo)
        after
            throw(foo)
        end
    catch
        throw:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_raise_error() ->
    try
        erlang:raise(error, foo, [])
    catch
        error:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_raise_exit() ->
    try
        erlang:raise(exit, foo, [])
    catch
        exit:foo -> ok;
        C:V -> {unexpected, ?LINE, C, V}
    end.

test_raise_badarg() ->
    try
        badarg = erlang:raise(bar, foo, []),
        ok
    catch
        C:V -> {unexpected, ?LINE, C, V}
    end.
