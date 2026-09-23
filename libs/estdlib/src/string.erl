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
    to_integer/1,
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
%% @param String a chardata value possibly beginning with an integer
%% @returns `{Int, Rest}' where `Rest' is the unconsumed chardata suffix, or
%%          `{error, no_integer}' if it does not begin with an integer, or
%%          `{error, badarg}' if malformed chardata or invalid UTF-8 is
%%          encountered while parsing
%% @doc Parse a leading (optionally signed) integer from a `unicode:chardata()'
%% value. Digits and sign are ASCII. The remainder keeps binary form when the
%% original argument is a binary.
%%
%% Matching OTP, the maximal leading run of ASCII `+', `-', and digits is
%% validated before the integer grammar is applied, so leftover signs in that
%% run are examined for chardata errors even when they become part of `Rest'.
%% @end
%%-----------------------------------------------------------------------------
-spec to_integer(String :: unicode:chardata()) ->
    {integer(), unicode:chardata()} | {error, no_integer | badarg}.
to_integer(String) ->
    try to_integer_cd(String) of
        {error, _} = Err ->
            Err;
        {Int, Rest} ->
            {Int, Rest}
    catch
        error:badarg ->
            {error, badarg}
    end.

%% @private
to_integer_cd(Bin) when is_binary(Bin) ->
    case take_int_bin(Bin) of
        no_integer ->
            {error, no_integer};
        {Int, Rest} ->
            {Int, Rest}
    end;
to_integer_cd(List) when is_list(List) ->
    case take_int_cd(List) of
        no_integer ->
            {error, no_integer};
        {Int, Rest} ->
            {Int, Rest}
    end;
to_integer_cd(_) ->
    {error, badarg}.

%% Take the maximal leading run of ASCII + - digits (OTP string:take/2 set),
%% validate the first codepoint after that run, then apply integer grammar.
%% @private
take_int_bin(Bin) ->
    {PrefLen, Tail} = take_set_bin(Bin, 0),
    case ensure_utf8_boundary(Tail) of
        ok when PrefLen =:= 0 ->
            no_integer;
        ok ->
            Pref = binary:part(Bin, 0, PrefLen),
            case parse_int_bin(Pref) of
                no_integer ->
                    no_integer;
                {Int, RestPref} ->
                    {Int, <<RestPref/binary, Tail/binary>>}
            end;
        error ->
            erlang:error(badarg)
    end.

%% @private
take_set_bin(<<C, Rest/binary>>, N) when
    C =:= $+ orelse C =:= $- orelse (C >= $0 andalso C =< $9)
->
    take_set_bin(Rest, N + 1);
take_set_bin(Rest, N) ->
    {N, Rest}.

%% @private
parse_int_bin(<<$+, Rest/binary>>) ->
    parse_int_digits_bin(Rest, 1);
parse_int_bin(<<$-, Rest/binary>>) ->
    parse_int_digits_bin(Rest, -1);
parse_int_bin(Bin) ->
    parse_int_digits_bin(Bin, 1).

%% @private
parse_int_digits_bin(Bin, Sign) ->
    {N, Rest} = take_digits_bin(Bin, 0),
    case N of
        0 ->
            no_integer;
        _ ->
            Digits = binary:part(Bin, 0, N),
            {Sign * binary_to_integer(Digits), Rest}
    end.

%% @private
take_digits_bin(<<C, Rest/binary>>, N) when C >= $0 andalso C =< $9 ->
    take_digits_bin(Rest, N + 1);
take_digits_bin(Rest, N) ->
    {N, Rest}.

%% @private
ensure_utf8_boundary(<<>>) ->
    ok;
ensure_utf8_boundary(<<C, _/binary>>) when C =< 16#7F ->
    ok;
ensure_utf8_boundary(<<_/utf8, _/binary>>) ->
    ok;
ensure_utf8_boundary(_) ->
    error.

%% @private
take_int_cd(CD) ->
    {HeadChars, Tail0} = take_set_cd(CD, []),
    Tail = normalize_empty_cd(Tail0),
    case HeadChars of
        [] ->
            no_integer;
        _ ->
            case parse_int_chars(HeadChars) of
                no_integer ->
                    no_integer;
                {Int, RestChars} ->
                    {Int, append_cd(RestChars, Tail)}
            end
    end.

%% Collect leading + - digit codepoints; stop before first non-member.
%% @private
take_set_cd(CD, Acc) ->
    case next_cp(CD) of
        empty ->
            {lists:reverse(Acc), CD};
        {C, Rest} when C =:= $+ orelse C =:= $- orelse (C >= $0 andalso C =< $9) ->
            take_set_cd(Rest, [C | Acc]);
        {_C, _Rest} ->
            {lists:reverse(Acc), CD}
    end.

%% @private
parse_int_chars([$+ | Rest]) ->
    parse_int_digits_chars(Rest, 1);
parse_int_chars([$- | Rest]) ->
    parse_int_digits_chars(Rest, -1);
parse_int_chars(Chars) ->
    parse_int_digits_chars(Chars, 1).

%% @private
parse_int_digits_chars([C | Rest], Sign) when C >= $0 andalso C =< $9 ->
    parse_int_more_chars(Rest, Sign, [C]);
parse_int_digits_chars(_Rest, _Sign) ->
    no_integer.

%% @private
parse_int_more_chars([C | Rest], Sign, Acc) when C >= $0 andalso C =< $9 ->
    parse_int_more_chars(Rest, Sign, [C | Acc]);
parse_int_more_chars(Rest, Sign, Acc) ->
    {Sign * list_to_integer(lists:reverse(Acc)), Rest}.

%% @private
append_cd([], Tail) ->
    Tail;
append_cd(RestChars, Tail) ->
    RestChars ++ Tail.

%% @private
normalize_empty_cd(CD) ->
    case is_empty_cd(CD) of
        true ->
            [];
        false ->
            CD
    end.

%% @private
is_empty_cd([]) ->
    true;
is_empty_cd(<<>>) ->
    true;
is_empty_cd([H | T]) ->
    is_empty_cd(H) andalso is_empty_cd(T);
is_empty_cd(_) ->
    false.

%% next_cp(Chardata) -> {Codepoint, Rest} | empty
%% Raises error:badarg on malformed UTF-8 / non-chardata, including improper
%% list continuations that are not a list or binary.
%% @private
next_cp([]) ->
    empty;
next_cp(<<>>) ->
    empty;
next_cp([H | T]) when is_integer(H) ->
    if
        H >= 0 andalso H =< 16#10FFFF ->
            {H, ensure_cd_cont(T)};
        true ->
            erlang:error(badarg)
    end;
next_cp([H | T]) when is_binary(H) ->
    case next_cp_bin(H) of
        empty ->
            next_cp(ensure_cd_cont(T));
        {C, RestBin} ->
            {C, stack_rest(RestBin, T)}
    end;
next_cp([H | T]) when is_list(H) ->
    case next_cp(H) of
        empty ->
            next_cp(ensure_cd_cont(T));
        {C, RestH} ->
            {C, stack_rest(RestH, T)}
    end;
next_cp(Bin) when is_binary(Bin) ->
    next_cp_bin(Bin);
next_cp(_) ->
    erlang:error(badarg).

%% @private
next_cp_bin(<<>>) ->
    empty;
next_cp_bin(<<C/utf8, Rest/binary>>) ->
    {C, Rest};
next_cp_bin(_) ->
    erlang:error(badarg).

%% @private
stack_rest([], T) ->
    ensure_cd_cont(T);
stack_rest(<<>>, T) ->
    ensure_cd_cont(T);
stack_rest(H, []) ->
    H;
stack_rest(H, T) ->
    [H | ensure_cd_cont(T)].

%% A chardata continuation must be [] | binary() | list().
%% @private
ensure_cd_cont([]) ->
    [];
ensure_cd_cont(T) when is_binary(T) ->
    T;
ensure_cd_cont(T) when is_list(T) ->
    T;
ensure_cd_cont(_) ->
    erlang:error(badarg).

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
-spec trim(String :: string(), Direction :: atom()) -> string() | char().
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
