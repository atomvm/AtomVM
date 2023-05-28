%
% This file is part of AtomVM.
%
% Copyright 2018-2023 Davide Bettio <davide@uninstall.it>
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
%% This module implements a strict subset of the Erlang/OTP string
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(string).

-export([to_upper/1, split/2, split/3, trim/1, trim/2]).

%%-----------------------------------------------------------------------------
%% @param Input a string or character to convert
%% @returns a Character or string
%% @doc Convert string or character to uppercase.
%%
%% The specified string or character is case-converted. Notice that the supported character
%% set is ISO/IEC 8859-1 (also called Latin 1); all values outside this set are unchanged
%% @end
%%-----------------------------------------------------------------------------
-spec to_upper(Input :: string() | char()) -> string() | char().
to_upper(S) when is_list(S) ->
    [upper_char(C) || C <- S];
to_upper(C) when is_integer(C) ->
    upper_char(C).

upper_char(C) when is_integer(C) andalso C >= $a andalso C =< $z ->
    C - 32;
upper_char(C) when is_integer(C) ->
    C.

%%-----------------------------------------------------------------------------
%% @equiv split(String, Pattern, leading)
%% @param String a string to split
%% @param Pattern the search pattern to split at
%% @returns chardata
%% @end
%%-----------------------------------------------------------------------------
-spec split(String :: string(), Pattern :: string()) -> string() | char().
split(String, Pattern) ->
    split(String, Pattern, leading).

%%-----------------------------------------------------------------------------
%% @param String a string to split
%% @param Pattern the search pattern to split at
%% @param Where position to split (leading, trailing, or all)
%% @returns chardata
%% @doc Splits String where SearchPattern is encountered and return the remaining parts.
%%
%% Where, default leading, indicates whether the leading, the trailing or all encounters of SearchPattern will split String.
%%
%% Example:
%% ```0> string:split("ab..bc..cd", "..").
%% ["ab","bc..cd"]
%% 1> string:split(<<"ab..bc..cd">>, "..", trailing).
%% [<<"ab..bc">>,<<"cd">>]
%% 2> string:split(<<"ab..bc....cd">>, "..", all).
%% [<<"ab">>,<<"bc">>,<<>>,<<"cd">>]'''
%% @end
%%-----------------------------------------------------------------------------
-spec split(String :: string(), Pattern :: string() | char(), Where :: atom()) -> string() | char().
split(String, Pattern, Where) ->
    split(String, Pattern, Where, [], []).

%% @private
split([], _Pattern, _Where, Token, Accum) ->
    lists:reverse([lists:reverse(Token) | Accum]);
split(String, Pattern, Where, Token, Accum) ->
    case prefix_match(String, Pattern) of
        {ok, Rest} ->
            case Where of
                leading ->
                    [lists:reverse(Token), Rest];
                all ->
                    split(Rest, Pattern, Where, [], [lists:reverse(Token) | Accum])
            end;
        no ->
            [Char | Rest] = String,
            split(Rest, Pattern, Where, [Char | Token], Accum)
    end.

%% @private
prefix_match(Rest, []) ->
    {ok, Rest};
prefix_match([Char | Rest], [Char | PRest]) ->
    prefix_match(Rest, PRest);
prefix_match(_String, _Pattern) ->
    no.

%%-----------------------------------------------------------------------------
%% @equiv trim(String, both)
%% @param String a string or character to trim whitespace
%% @returns a Character or string
%% @end
%%-----------------------------------------------------------------------------
-spec trim(String :: string()) -> string() | char().
trim(String) ->
    trim(String, both).

%%-----------------------------------------------------------------------------
%% @param String a string or character to trim
%% @param Direction an atom indicating the direction from which to remove whitespace
%% @returns a Character or string
%% @doc Returns a string, where leading or trailing, or both, whitespace has been removed.
%%
%% If omitted, Direction is both.
%%
%% Example:
%% ```1> string:trim("\t  Hello  \n").
%% "Hello"
%% 2> string:trim(<<"\t  Hello  \n">>, leading).
%% <<"Hello  \n">>
%% 3> string:trim(<<".Hello.\n">>, trailing, "\n.").
%% <<".Hello">>'''
%% @end
%%-----------------------------------------------------------------------------
-spec trim(String :: string, Direction :: atom()) -> string() | char().
trim(String, leading) ->
    triml(String);
trim(String, trailing) ->
    lists:reverse(triml(lists:reverse(String)));
trim(String, both) ->
    lists:reverse(triml(lists:reverse(triml(String)))).

%% @private
triml([$\s | R]) ->
    triml(R);
triml(R) ->
    R.
