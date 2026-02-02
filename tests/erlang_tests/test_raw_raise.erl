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

-module(test_raw_raise).

-export([start/0, do_raise/0, fail_with_atom/1, do_raise_not_error/0]).

start() ->
    do_catch() + do_catch2().

do_catch() ->
    try ?MODULE:do_raise() of
        _X -> 1
    catch
        error:{badarith, new_reason}:ST ->
            % TODO: verify if undefined is an acceptable value or an AtomVM only extension
            % See also issue #1247
            case ST of
                L when is_list(L) -> 0;
                undefined -> 0;
                _ -> 2
            end;
        _:_ ->
            3
    end.

do_raise() ->
    try ?MODULE:fail_with_atom(ciao) of
        X -> X
    catch
        error:Reason:ST ->
            erlang:raise(id(error), {Reason, new_reason}, ST)
    end.

do_catch2() ->
    try ?MODULE:do_raise_not_error() of
        _ -> 0
    catch
        _:_ -> 1
    end.

do_raise_not_error() ->
    try ?MODULE:fail_with_atom(ciao) of
        X -> X
    catch
        error:Reason:ST ->
            erlang:raise(id(not_error), {Reason, new_reason}, ST)
    end.

fail_with_atom(Atom) ->
    Atom + 1.

id(X) ->
    X.
