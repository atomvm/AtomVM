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

-export([
    to_upper/1,
    to_lower/1,
    split/2, split/3,
    trim/1, trim/2,
    find/2, find/3,
    length/1,
    jaro_similarity/2
]).

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
%% @param Input a string or character to convert
%% @returns a Character or string
%% @doc Convert string or character to uppercase.
%%
%% The specified string or character is case-converted. Notice that the supported character
%% set is ISO/IEC 8859-1 (also called Latin 1); all values outside this set are unchanged
%% @end
%%-----------------------------------------------------------------------------
-spec to_lower(Input :: string() | char()) -> string() | char().
to_lower(S) when is_list(S) ->
    [lower_char(C) || C <- S];
to_lower(C) when is_integer(C) ->
    lower_char(C).

lower_char(C) when is_integer(C) andalso C >= $A andalso C =< $Z ->
    C + 32;
lower_char(C) when is_integer(C) ->
    C.

%%-----------------------------------------------------------------------------
%% @equiv split(String, Pattern, leading)
%% @param String a string to split
%% @param Pattern the search pattern to split at
%% @returns chardata
%% @end
%%-----------------------------------------------------------------------------
-spec split(String :: unicode:chardata(), Pattern :: unicode:chardata()) -> [unicode:chardata()].
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
-spec split(
    String :: unicode:chardata(), Pattern :: unicode:chardata(), Where :: leading | trailing | all
) -> [unicode:chardata()].
split(String, Pattern, Where) when is_binary(String) andalso is_list(Pattern) ->
    split_binary(String, unicode:characters_to_binary(Pattern), Where);
split(String, Pattern, Where) when is_binary(String) andalso is_binary(Pattern) ->
    split_binary(String, Pattern, Where);
split(String, Pattern, Where) when is_list(String) andalso is_binary(Pattern) ->
    split_list(String, unicode:characters_to_list(Pattern), Where);
split(String, Pattern, Where) when is_list(String) andalso is_list(Pattern) ->
    split_list(String, Pattern, Where).

%% @private
split_binary(String, Pattern, leading) ->
    binary:split(String, Pattern);
split_binary(String, Pattern, all) ->
    binary:split(String, Pattern, [global]);
split_binary(String, Pattern, trailing) ->
    case find_binary(String, Pattern, trailing) of
        nomatch ->
            [String];
        Rest ->
            [binary:part(String, 0, byte_size(String) - byte_size(Rest) - byte_size(Pattern)), Rest]
    end.

%% @private
split_list(String, Pattern, Where) ->
    split_list(String, Pattern, Where, [], []).

%% @private
split_list([], _Pattern, _Where, Token, Accum) ->
    lists:reverse([lists:reverse(Token) | Accum]);
split_list(String, Pattern, Where, Token, Accum) ->
    case prefix_match(String, Pattern) of
        {ok, Rest} ->
            case Where of
                leading ->
                    [lists:reverse(Token), Rest];
                all ->
                    split_list(Rest, Pattern, Where, [], [lists:reverse(Token) | Accum])
            end;
        no ->
            [Char | Rest] = String,
            split_list(Rest, Pattern, Where, [Char | Token], Accum)
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

%%-----------------------------------------------------------------------------
%% @equiv find(String, SearchPattern, leading)
%% @param String string to search in
%% @param SearchPattern pattern to search
%% @returns remainder of String starting from first occurrence of SearchPattern
%% or `nomatch' if SearchPattern cannot be found in String
%% @end
%%-----------------------------------------------------------------------------
-spec find(String :: unicode:chardata(), SearchPattern :: unicode:chardata()) ->
    unicode:chardata() | nomatch.
find(String, SearchPattern) ->
    find(String, SearchPattern, leading).

%%-----------------------------------------------------------------------------
%% @param String string to search in
%% @param SearchPattern pattern to search
%% @param Direction direction to search, `leading' or `trailing'
%% @returns remainder of String starting from first or last occurrence of
%% SearchPattern or `nomatch' if SearchPattern cannot be found in String
%% @end
%%-----------------------------------------------------------------------------
-spec find(
    String :: unicode:chardata(),
    SearchPattern :: unicode:chardata(),
    Direction :: leading | trailing
) -> unicode:chardata() | nomatch.
find(String, "", _Direction) ->
    String;
find(String, <<>>, _Direction) ->
    String;
find(String, SearchPattern, Direction) when is_binary(String) andalso is_list(SearchPattern) ->
    find_binary(String, unicode:characters_to_binary(SearchPattern), Direction);
find(String, SearchPattern, Direction) when is_binary(String) andalso is_binary(SearchPattern) ->
    find_binary(String, SearchPattern, Direction);
find(String, SearchPattern, Direction) when is_list(String) andalso is_binary(SearchPattern) ->
    find_list(String, unicode:characters_to_list(SearchPattern), Direction);
find(String, SearchPattern, Direction) when is_list(String) andalso is_list(SearchPattern) ->
    find_list(String, SearchPattern, Direction).

%% @private
find_binary(<<_C, Rest/binary>> = String, SearchPattern, leading) when
    byte_size(String) >= byte_size(SearchPattern)
->
    case binary:part(String, 0, byte_size(SearchPattern)) =:= SearchPattern of
        true -> String;
        false -> find_binary(Rest, SearchPattern, leading)
    end;
find_binary(_Sring, _SearchPattern, leading) ->
    nomatch.

%% @private
find_list([_C | Rest] = String, SearchPattern, leading) ->
    case prefix_match(String, SearchPattern) of
        {ok, _Rest} -> String;
        no -> find_list(Rest, SearchPattern, leading)
    end;
find_list([], _SearchPattern, leading) ->
    nomatch.

%%-----------------------------------------------------------------------------
%% @param String string to compute the length of
%% @doc Return the length of the string in characters.
%% @end
%%-----------------------------------------------------------------------------
-spec length(String :: unicode:chardata()) -> non_neg_integer().
length(String) when is_list(String) ->
    erlang:length(String);
length(String) when is_binary(String) ->
    erlang:length(unicode:characters_to_list(String)).

%%-----------------------------------------------------------------------------
%% @param String1 first string to compare
%% @param String2 second string to compare
%% @doc Calculate the Jaro similarity between two strings (stub implementation).
%%
%% This is a stub implementation that always returns 0.0, used for compatibility.
%% @end
%%-----------------------------------------------------------------------------
-spec jaro_similarity(String1 :: unicode:chardata(), String2 :: unicode:chardata()) -> float().
jaro_similarity(_String1, _String2) ->
    0.0.
