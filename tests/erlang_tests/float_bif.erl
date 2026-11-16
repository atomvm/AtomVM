%
% This file is part of AtomVM.
%
% Copyright 2025 Jakub Gonet <jakub.gonet@swmansion.com>
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

-module(float_bif).

-export([start/0]).
% For remote function calls
-export([id/1, check_one/1]).

-define(ID(Arg), ?MODULE:id(Arg)).

start() ->
    true = 1.0 =:= float(?ID(1)),
    true = 1.0 =:= float(?ID(1.0)),
    ok =
        try float(?ID("1")) of
            _ ->
                unreachable
        catch
            error:badarg ->
                ok
        end,
    ok = ?MODULE:check_one(?ID(1)),
    ok = ?MODULE:check_one(?ID(1.0)),
    error = ?MODULE:check_one(?ID(atom)),
    ok = test_rounding(),
    0.

test_rounding() ->
    Size =
        case erlang:system_info(machine) of
            "BEAM" -> 8;
            "ATOM" -> erlang:system_info(avm_floatsize)
        end,
    case Size of
        4 ->
            ok;
        8 ->
            Base = 1 bsl 96,
            Half = 1 bsl 43,
            %% Below halfway, an even tie, above halfway, and an odd tie.
            ok = check_rounding(Base + Half - 1, 7.922816251426434e28),
            ok = check_rounding(Base + Half, 7.922816251426434e28),
            ok = check_rounding(Base + Half + 1, 7.922816251426436e28),
            ok = check_rounding(Base + 3 * Half, 7.922816251426437e28),
            %% Rounding can carry into a new digit, even at the integer limit.
            ok = check_rounding(Base - 1, 7.922816251426434e28),
            MaxInteger = (1 bsl 255) + ((1 bsl 255) - 1),
            ok = check_rounding(MaxInteger, 1.157920892373162e77),
            %% Older OTP releases double-round this conversion. Always check
            %% AtomVM, including when the test was compiled with older OTP.
            case ?OTP_RELEASE >= 29 orelse erlang:system_info(machine) =:= "ATOM" of
                true ->
                    ok = check_rounding(
                        303948059319153011862742238357535483, 3.0394805931915302e35
                    ),
                    3.290035811513358e-36 = ?ID(1) / ?ID(303948059319153011862742238357535483),
                    ok;
                false ->
                    ok
            end
    end.

check_rounding(Integer, Expected) ->
    Expected = float(?ID(Integer)),
    Negative = -Expected,
    Negative = float(?ID(-Integer)),
    case ?ID(Integer) of
        Value when float(Value) =:= Expected -> ok
    end.

id(X) ->
    X.

% Tests float/1 in guards
check_one(T) when float(T) =:= 1.0 ->
    ok;
check_one(_T) ->
    error.
