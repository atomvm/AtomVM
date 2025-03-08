%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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
-module(test_stacktrace).

-export([start/0, maybe_crash/1]).
-export([
    throw_with_file_and_line/0,
    throw_with_other_file_and_line/0,
    throw_with_other_file_and_line_large_value/0
]).

-include("test_stacktrace.hrl").

start() ->
    ok = test_local_throw(),
    ok = test_local_error(),
    ok = test_badmatch(),
    ok = test_apply(),
    ok = test_fun(),
    ok = test_remote_throw(),
    ok = test_tail_recursive_throw(),
    ok = test_body_recursive_throw(),
    ok = test_spawned_throw(),
    ok = test_catch(),
    ok = maybe_test_filelineno(),
    ok = maybe_test_filelineno_other_file(),
    ok = maybe_test_filelineno_large(),
    0.

test_local_throw() ->
    ok =
        try
            maybe_crash(throw_me),
            fail
        catch
            throw:throw_me:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_crash, 1},
                        {?MODULE, test_local_throw, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

test_local_error() ->
    ok =
        try
            maybe_crash(error_me),
            fail
        catch
            error:error_me:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_crash, 1},
                        {?MODULE, test_local_error, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

test_badmatch() ->
    ok =
        try
            maybe_badmatch(crash_me),
            fail
        catch
            error:{badmatch, crash_me}:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_badmatch, 1},
                        {?MODULE, test_badmatch, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

test_remote_throw() ->
    ok =
        try
            ?MODULE:maybe_crash(throw_me),
            fail
        catch
            throw:throw_me:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_crash, 1},
                        {?MODULE, test_remote_throw, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

test_apply() ->
    ok =
        try
            erlang:apply(?MODULE, maybe_crash, [throw_me]),
            fail
        catch
            throw:throw_me:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_crash, 1},
                        {?MODULE, test_apply, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

test_fun() ->
    ok =
        try
            F = fun() -> maybe_crash(throw_me) end,
            F(),
            fail
        catch
            throw:throw_me:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_crash, 1},
                        {?MODULE, test_fun, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

a_tail_recursive_function(0, Msg) ->
    maybe_crash(Msg);
a_tail_recursive_function(I, Msg) ->
    a_tail_recursive_function(I - 1, Msg).

test_tail_recursive_throw() ->
    ok =
        try
            a_tail_recursive_function(5, throw_me),
            fail
        catch
            throw:throw_me:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_crash, 1},
                        {?MODULE, test_tail_recursive_throw, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

a_body_recursive_function(0, _Msg) ->
    ok;
a_body_recursive_function(I, Msg) ->
    case a_body_recursive_function(I - 1, Msg) of
        ok ->
            maybe_crash(Msg);
        _ ->
            error
    end.

test_body_recursive_throw() ->
    ok =
        try
            a_body_recursive_function(5, throw_me),
            fail
        catch
            throw:throw_me:Stacktrace ->
                expect_stacktrace(
                    Stacktrace,
                    [
                        {?MODULE, maybe_crash, 1},
                        {?MODULE, a_body_recursive_function, 2},
                        {?MODULE, test_body_recursive_throw, 0},
                        {?MODULE, start, 0}
                    ]
                )
        end.

test_spawned_throw() ->
    Self = self(),
    spawn_opt(
        fun() ->
            try
                do_some_stuff(blah),
                a_tail_recursive_function(5, throw_me),
                do_some_stuff(blah),
                Self ! fail
            catch
                throw:throw_me:Stacktrace ->
                    Result = expect_stacktrace(
                        Stacktrace,
                        [
                            {?MODULE, maybe_crash, 1},
                            {?MODULE, '-test_spawned_throw/0-fun-0-', 1}
                        ]
                    ),
                    Self ! Result
            end
        end,
        []
    ),
    receive
        Result ->
            erlang:display(Result),
            Result
    end.

test_catch() ->
    {'EXIT', {error_me, Stacktrace}} = (catch maybe_crash(error_me)),
    Result = expect_stacktrace(
        Stacktrace,
        [
            {?MODULE, maybe_crash, 1},
            {?MODULE, test_catch, 0},
            {?MODULE, start, 0}
        ]
    ),
    do_some_stuff(Result),
    Result.

maybe_test_filelineno(Fun) ->
    ok =
        try
            ?MODULE:Fun(),
            fail
        catch
            throw:{File, Line}:Stacktrace ->
                [Frame | _] = Stacktrace,
                {?MODULE, Fun, 0, AuxData} = Frame,
                case {get_value(file, AuxData), get_value(line, AuxData)} of
                    {undefined, undefined} ->
                        ok;
                    {F, L} ->
                        Ef =
                            case is_binary(F) of
                                true ->
                                    erlang:binary_to_list(F);
                                _ ->
                                    F
                            end,
                        case File == Ef andalso Line == L of
                            true ->
                                ok;
                            _ ->
                                {unexpected_file_line, F, L}
                        end
                end
        end.

maybe_test_filelineno() ->
    maybe_test_filelineno(throw_with_file_and_line).

maybe_test_filelineno_other_file() ->
    maybe_test_filelineno(throw_with_other_file_and_line).

% This test actually succeeds even if large line numbers are not supported
% because all line numbers are then disabled.
maybe_test_filelineno_large() ->
    maybe_test_filelineno(throw_with_other_file_and_line_large_value).

get_value(_Key, []) ->
    undefined;
get_value(Key, [{Key, Value} | _]) ->
    Value;
get_value(Key, [_ | T]) ->
    get_value(Key, T).

throw_with_file_and_line() ->
    throw({?FILE, ?LINE}).

maybe_crash(Term) ->
    case Term of
        ok ->
            ok;
        throw_me ->
            throw(Term);
        error_me ->
            error(Term)
    end.

maybe_badmatch(Term) ->
    ok = Term.

do_some_stuff(_) ->
    ok.

expect_stacktrace(Stacktrace, Expect) ->
    % erlang:display({stacktrace, [{M, F, A} || {M, F, A, _} <- Stacktrace]}),
    % erlang:display({stacktrace, Stacktrace}),
    % erlang:display({expect, Expect}),
    MFAStacktrace = [{M, F, A} || {M, F, A, _} <- Stacktrace],
    case contains_in_order(Expect, remove_duplicates(MFAStacktrace, [])) of
        true ->
            ok;
        _ ->
            error(shit)
    end.

remove_duplicates([], Accum) ->
    reverse(Accum, []);
remove_duplicates([H, H | T], Accum) ->
    remove_duplicates([H | T], Accum);
remove_duplicates([H | T], Accum) ->
    remove_duplicates(T, [H | Accum]).

reverse([], Accum) -> Accum;
reverse([H | T], Accum) -> reverse(T, [H | Accum]).

contains_in_order([], _) ->
    true;
contains_in_order([H | T], E) ->
    case find(H, E) of
        not_found ->
            false;
        E2 ->
            contains_in_order(T, E2)
    end.

find(_H, []) ->
    not_found;
find(H, [H | T]) ->
    T;
find(H, [_ | T]) ->
    find(H, T).
