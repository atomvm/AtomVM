%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP string interface.
%%
%% This module implements a strict susbset of the Erlang/OTP string
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(string).

-export([to_upper/1, split/2, split/3, trim/1, trim/2]).

to_upper(S) when is_list(S) ->
    [upper_char(C) || C <- S];

to_upper(C) when is_integer(C) ->
    upper_char(C).

upper_char(C) when is_integer(C) andalso C >= $a andalso C =< $z ->
    C - 32;

upper_char(C) when is_integer(C) ->
    C.

split(String, Pattern) ->
    split(String, Pattern, leading).

split(String, Pattern, Where) ->
    split(String, Pattern, Where, [], []).

%% @private
split([], _Pattern, _Where, Token, Accum) ->
    lists:reverse([lists:reverse(Token)|Accum]);
split(String, Pattern, Where, Token, Accum) ->
    case prefix_match(String, Pattern) of
        {ok, Rest} ->
            case Where of
                leading ->
                    [lists:reverse(Token), Rest];
                all ->
                    split(Rest, Pattern, Where, [], [lists:reverse(Token)|Accum])
            end;
        no ->
            [Char|Rest] = String,
            split(Rest, Pattern, Where, [Char|Token], Accum)
    end.

%% @private
prefix_match(Rest, []) ->
    {ok, Rest};
prefix_match([Char|Rest], [Char|PRest]) ->
    prefix_match(Rest, PRest);
prefix_match(_String, _Pattern) ->
    no.

trim(String) ->
    trim(String, both).

trim(String, leading) ->
    triml(String);
trim(String, trailing) ->
    lists:reverse(triml(lists:reverse(String)));
trim(String, both) ->
    lists:reverse(triml(lists:reverse(triml(String)))).

%% @private
triml([$\s|R]) ->
    triml(R);
triml(R) ->
    R.
