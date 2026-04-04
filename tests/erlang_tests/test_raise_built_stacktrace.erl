%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_raise_built_stacktrace).

-export([start/0, id/1, reraise_with/3]).

start() ->
    ok = test_raise_direct(),
    ok = test_raise_dynamic(),
    ok = test_raise_preserves_stacktrace_direct(),
    ok = test_raise_preserves_stacktrace_dynamic(),
    ok = test_raise_exit_direct(),
    ok = test_raise_exit_dynamic(),
    ok = test_raise_throw_direct(),
    ok = test_raise_throw_dynamic(),
    ok = test_raise_empty_stacktrace_direct(),
    ok = test_raise_empty_stacktrace_dynamic(),
    0.

%% Tests the raw_raise opcode path (erlang:raise/3 compiled to raw_raise).
test_raise_direct() ->
    try
        try
            do_raise_direct()
        catch
            throw:_ -> should_not_happen
        end
    catch
        error:badarg -> ok
    end.

%% Tests the NIF path (dynamic apply bypasses raw_raise opcode).
test_raise_dynamic() ->
    try
        try
            do_raise_dynamic()
        catch
            throw:_ -> should_not_happen
        end
    catch
        error:badarg -> ok
    end.

do_raise_direct() ->
    try
        erlang:error(badarg)
    catch
        error:badarg:Stacktrace ->
            erlang:raise(error, badarg, Stacktrace)
    end.

do_raise_dynamic() ->
    try
        erlang:error(badarg)
    catch
        error:badarg:Stacktrace ->
            apply(?MODULE:id(erlang), ?MODULE:id(raise), [error, badarg, Stacktrace])
    end.

id(X) -> X.

%% The compiler compiles erlang:raise/3 to raw_raise. When called with
%% a built stacktrace (list) from another function, this exercises the
%% OP_RAW_RAISE wrapping fix.
reraise_with(Class, Reason, Stacktrace) ->
    erlang:raise(Class, Reason, Stacktrace).

%% Returns a built stacktrace list. The compiler inserts build_stacktrace
%% because ST is returned as a value, not used directly in erlang:raise/3.
capture_built_stacktrace() ->
    try
        erlang:error(badarg)
    catch
        error:badarg:ST -> ST
    end.

%% Verify stacktrace content is preserved through re-raise (opcode path).
%% reraise_with/3 compiles erlang:raise/3 to raw_raise, receiving a built list.
test_raise_preserves_stacktrace_direct() ->
    OriginalST = capture_built_stacktrace(),
    true = is_list(OriginalST),
    true = OriginalST =/= [],
    try
        try
            ?MODULE:reraise_with(error, badarg, OriginalST)
        catch
            throw:_ -> should_not_happen
        end
    catch
        error:badarg:CaughtST ->
            case CaughtST of
                OriginalST -> ok;
                _ -> error
            end
    end.

%% Verify stacktrace content is preserved through re-raise (NIF path).
test_raise_preserves_stacktrace_dynamic() ->
    OriginalST = capture_built_stacktrace(),
    true = is_list(OriginalST),
    true = OriginalST =/= [],
    try
        try
            apply(?MODULE:id(erlang), ?MODULE:id(raise), [error, badarg, OriginalST])
        catch
            throw:_ -> should_not_happen
        end
    catch
        error:badarg:CaughtST ->
            case CaughtST of
                OriginalST -> ok;
                _ -> error
            end
    end.

%% Tests exit class through raw_raise opcode (via reraise_with/3).
test_raise_exit_direct() ->
    try
        try
            do_raise_exit_direct()
        catch
            error:_ -> should_not_happen
        end
    catch
        exit:some_reason -> ok
    end.

do_raise_exit_direct() ->
    try
        erlang:exit(some_reason)
    catch
        exit:some_reason:Stacktrace ->
            ?MODULE:reraise_with(exit, some_reason, Stacktrace)
    end.

%% Tests exit class through NIF path.
test_raise_exit_dynamic() ->
    try
        try
            do_raise_exit_dynamic()
        catch
            error:_ -> should_not_happen
        end
    catch
        exit:some_reason -> ok
    end.

do_raise_exit_dynamic() ->
    try
        erlang:exit(some_reason)
    catch
        exit:some_reason:Stacktrace ->
            apply(?MODULE:id(erlang), ?MODULE:id(raise), [exit, some_reason, Stacktrace])
    end.

%% Tests throw class through raw_raise opcode (via reraise_with/3).
test_raise_throw_direct() ->
    try
        try
            do_raise_throw_direct()
        catch
            error:_ -> should_not_happen
        end
    catch
        throw:some_reason -> ok
    end.

do_raise_throw_direct() ->
    try
        erlang:throw(some_reason)
    catch
        throw:some_reason:Stacktrace ->
            ?MODULE:reraise_with(throw, some_reason, Stacktrace)
    end.

%% Tests throw class through NIF path.
test_raise_throw_dynamic() ->
    try
        try
            do_raise_throw_dynamic()
        catch
            error:_ -> should_not_happen
        end
    catch
        throw:some_reason -> ok
    end.

do_raise_throw_dynamic() ->
    try
        erlang:throw(some_reason)
    catch
        throw:some_reason:Stacktrace ->
            apply(?MODULE:id(erlang), ?MODULE:id(raise), [throw, some_reason, Stacktrace])
    end.

%% Tests that raise with empty stacktrace [] preserves it (opcode path).
test_raise_empty_stacktrace_direct() ->
    try
        try
            ?MODULE:reraise_with(error, badarg, [])
        catch
            throw:_ -> should_not_happen
        end
    catch
        error:badarg:ST ->
            case ST of
                [] -> ok;
                _ -> error
            end
    end.

%% Tests that raise with empty stacktrace [] preserves it (NIF path).
test_raise_empty_stacktrace_dynamic() ->
    try
        try
            apply(?MODULE:id(erlang), ?MODULE:id(raise), [error, badarg, []])
        catch
            throw:_ -> should_not_happen
        end
    catch
        error:badarg:ST ->
            case ST of
                [] -> ok;
                _ -> error
            end
    end.
