%
% This file is part of AtomVM.
%
% Copyright 2025 Software Mansion
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

-module(test_reraise).

-export([start/0, do_error/0]).

start() ->
    start(?OTP_RELEASE).

start(OTPVersion) when OTPVersion >= 23 ->
    test_reraise_raise_3_nif(),
    test_reraise_preserves_stacktrace(),
    0;
start(_) ->
    0.

test_reraise_raise_3_nif() ->
    try
        reraise_reraiser:reraise_error()
    catch
        Class:Reason:Stacktrace ->
            error = Class,
            "foo" = Reason,

            [
                {reraise_raiser, raise_error, 0, _Meta1},
                {reraise_reraiser, reraise_error, 0, _Meta2}
                | _Rest
            ] = Stacktrace
    end.

test_reraise_preserves_stacktrace() ->
    try
        try
            ?MODULE:do_error()
        catch
            throw:_ -> should_not_happen
        end
    catch
        error:my_error:ST ->
            case ST of
                L when is_list(L) ->
                    true = has_do_error(L),
                    ok;
                _ ->
                    %% Stacktraces disabled
                    ok
            end
    end.

do_error() ->
    erlang:error(my_error).

has_do_error([{?MODULE, do_error, _, _} | _]) -> true;
has_do_error([_ | T]) -> has_do_error(T);
has_do_error([]) -> false.
