%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Fred Dushin <fred@dushin.net>
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
%% @doc An implementation of the Erlang/OTP io_lib interface.
%%
%% This module implements a strict subset of the Erlang/OTP io_lib
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(io_lib).

-export([format/2]).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format argument
%% @returns string
%% @doc     Format string and data to a string.
%%          Approximates features of OTP io_lib:format/2, but
%%          only supports ~p and ~n format specifiers.
%%          Raises `badarg' error if the number of format specifiers
%%          does not match the length of the Args.
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format :: string(), Args :: list()) -> string().
format(Format, Args) ->
    {FormatTokens, Instr} = split(Format),
    case length(FormatTokens) == length(Args) + 1 of
        true ->
            interleave(FormatTokens, Instr, Args, []);
        false ->
            error(badarg)
    end.

%%
%% internal operations
%%

-record(format, {
    field_width = undefined :: number() | undefined,
    precision = undefined :: number() | undefined,
    pad = undefined :: char() | undefined,
    mod = undefined :: atom() | undefined,
    control :: atom() | undefined
}).

%% @private
split(Format) ->
    split(Format, [], [], []).

%% @private
split([], Cur, Accum, Instr) ->
    {lists:reverse([lists:reverse(Cur) | Accum]), lists:reverse(Instr)};
split([$~ | Tail], Cur, Accum, Instr) ->
    {FormatSpec, Rest} = parse_format(Tail),
    case FormatSpec of
        {literal, Lit} ->
            split(Rest, [Lit | Cur], Accum, Instr);
        Format = #format{} ->
            split(Rest, [], [lists:reverse(Cur) | Accum], [
                fun(T) -> format_term(Format, T) end | Instr
            ]);
        ignore ->
            split(Rest, [], [lists:reverse(Cur) | Accum], [fun(_T) -> [] end | Instr])
    end;
split([Char | Rest], Cur, Accum, Instr) ->
    split(Rest, [Char | Cur], Accum, Instr).

%% @private
parse_format([$i | Rest]) ->
    {ignore, Rest};
parse_format([$~ | Rest]) ->
    {{literal, $~}, Rest};
parse_format([$n | Rest]) ->
    {{literal, $\n}, Rest};
parse_format(String) ->
    Format = #format{},
    parse_format_field_width(String, Format).

%% @private
parse_format_field_width([$. | Rest], Format) ->
    parse_format_precision(Rest, Format);
parse_format_field_width([C | _] = String, Format0) when
    C =:= $- orelse (C >= $0 andalso C =< $9)
->
    {Value, Rest0} = parse_integer(String),
    Format1 = Format0#format{field_width = Value},
    case Rest0 of
        [$. | Rest1] ->
            parse_format_precision(Rest1, Format1);
        _ ->
            parse_format_mod(Rest0, Format1)
    end;
parse_format_field_width(String, Format) ->
    parse_format_mod(String, Format).

%% @private
parse_format_precision([$. | Rest], Format) ->
    parse_format_pad(Rest, Format);
parse_format_precision([C | _] = String, Format) when C =:= $- orelse (C >= $0 andalso C =< $9) ->
    {Value, Rest} = parse_integer(String),
    parse_format_precision(Rest, Format#format{precision = Value});
parse_format_precision(String, Format) ->
    parse_format_mod(String, Format).

%% @private
parse_format_pad([Pad | Rest], Format) ->
    parse_format_mod(Rest, Format#format{pad = Pad}).

%% @private
parse_format_mod([$t | Rest], Format) ->
    parse_format_control(Rest, Format#format{mod = t});
parse_format_mod([$l | Rest], Format) ->
    parse_format_control(Rest, Format#format{mod = l});
parse_format_mod(String, Format) ->
    parse_format_control(String, Format).

%% @private
parse_format_control([$s | Rest], Format) -> {Format#format{control = s}, Rest};
parse_format_control([$p | Rest], Format) -> {Format#format{control = p}, Rest};
parse_format_control([$w | Rest], Format) -> {Format#format{control = w}, Rest};
parse_format_control([$c | Rest], Format) -> {Format#format{control = c}, Rest};
parse_format_control([$B | Rest], Format) -> {Format#format{control = 'B'}, Rest};
parse_format_control([$b | Rest], Format) -> {Format#format{control = b}, Rest};
parse_format_control([$# | Rest], Format) -> {Format#format{control = '#'}, Rest};
parse_format_control([$+ | Rest], Format) -> {Format#format{control = '+'}, Rest};
parse_format_control([$e | Rest], Format) -> {Format#format{control = e}, Rest};
parse_format_control([$f | Rest], Format) -> {Format#format{control = f}, Rest};
parse_format_control([$g | Rest], Format) -> {Format#format{control = g}, Rest};
parse_format_control(_String, _Format) -> error({badarg, _String}).

%% @private
parse_integer([$- | Tail]) ->
    {Val, Rest} = parse_integer0(Tail, 0),
    {-Val, Rest};
parse_integer(Str) ->
    parse_integer0(Str, 0).

%% @private
parse_integer0([C | Tail], Acc) when C >= $0 andalso C =< $9 ->
    parse_integer0(Tail, Acc * 10 + C - $0);
parse_integer0(Str, Acc) ->
    {Acc, Str}.

%% @private
interleave([LastToken], _Instr, [], Accum) ->
    lists:reverse([LastToken | Accum]);
interleave([Token | Tokens], [Formatter | Instr], [Arg | Args], Accum) ->
    interleave(Tokens, Instr, Args, [Formatter(Arg), Token | Accum]).

%% @private
format_term(#format{control = C} = Format, T) when C =:= s orelse C =:= p orelse C =:= w ->
    trunc_or_pad(Format, format_string(Format, T));
format_term(#format{control = C} = Format, T) when
    C =:= 'B' orelse C =:= b orelse C =:= '#' orelse C =:= '+'
->
    trunc_or_pad(Format, format_integer(Format, T));
format_term(#format{control = C} = Format, T) when C =:= e orelse C =:= f orelse C =:= g ->
    trunc_or_pad(Format, format_float(Format, T));
format_term(#format{control = 'c'} = Format, T) ->
    trunc_or_pad(Format, format_char(Format, T)).

%% @private
trunc_or_pad(#format{field_width = undefined, pad = undefined}, Str) ->
    Str;
trunc_or_pad(#format{field_width = Width, pad = Pad0}, Str0) when Width =/= undefined ->
    Str = lists:flatten(Str0),
    AbsWidth = abs(Width),
    Pad =
        case Pad0 of
            undefined -> 32;
            _ -> Pad0
        end,
    Len = length(Str),
    if
        Len > AbsWidth ->
            lists:duplicate(AbsWidth, $*);
        Len =:= AbsWidth ->
            Str;
        Width < 0 ->
            [Str, lists:duplicate(AbsWidth - Len, Pad)];
        true ->
            [lists:duplicate(AbsWidth - Len, Pad), Str]
    end.

%% @private
format_string(
    #format{control = s, precision = Precision, field_width = FieldWidth} = Format, T
) when Precision =/= undefined orelse FieldWidth =/= undefined ->
    Str0 = format_spw(Format, T),
    Str = lists:flatten(Str0),
    TruncSize =
        if
            Precision =:= undefined -> abs(FieldWidth);
            true -> Precision
        end,
    lists:sublist(Str, TruncSize);
format_string(Format, T) ->
    format_spw(Format, T).

%% @private
format_spw(#format{control = s}, T) when is_atom(T) ->
    erlang:atom_to_list(T);
format_spw(_Format, T) when is_atom(T) ->
    AtomStr = erlang:atom_to_list(T),
    case atom_requires_quotes(T, AtomStr) of
        false -> AtomStr;
        true -> [$', AtomStr, $']
    end;
format_spw(#format{control = s, mod = t} = Format, T) when is_binary(T) ->
    case unicode:characters_to_list(T, utf8) of
        L when is_list(L) -> L;
        E when is_tuple(E) ->
            format_spw(Format#format{mod = undefined}, T)
    end;
format_spw(#format{control = s, mod = undefined}, T) when is_binary(T) ->
    erlang:binary_to_list(T);
format_spw(#format{control = Control, mod = t} = Format, T) when is_binary(T) ->
    case unicode:characters_to_list(T, utf8) of
        L when is_list(L) ->
            FormattedStr =
                case {Control, test_string_class(L)} of
                    {p, latin1_printable} -> format_p_string(L, []);
                    {p, unicode} -> [format_p_string(L, []), "/utf8"];
                    _ -> lists:join($,, [integer_to_list(B) || B <- L])
                end,
            [$<, $<, FormattedStr, $>, $>];
        E when is_tuple(E) ->
            format_spw(Format#format{mod = undefined}, T)
    end;
format_spw(#format{control = Control, mod = undefined}, T) when is_binary(T) ->
    L = erlang:binary_to_list(T),
    FormattedStr =
        case {Control, test_string_class(L)} of
            {p, latin1_printable} -> format_p_string(L, []);
            _ -> lists:join($,, [integer_to_list(B) || B <- L])
        end,
    [$<, $<, FormattedStr, $>, $>];
format_spw(#format{control = s, mod = Mod}, L) when is_list(L) ->
    Flatten = lists:flatten(L),
    case {Mod, test_string_class(Flatten)} of
        {_, not_a_string} -> error(badarg);
        {undefined, unicode} -> error(badarg);
        {_, _} -> Flatten
    end;
format_spw(#format{control = p} = Format, L) when is_list(L) ->
    case test_string_class(L) of
        latin1_printable -> format_p_string(L, []);
        _ -> [$[, lists:join($,, [format_spw(Format, E) || E <- L]), $]]
    end;
format_spw(#format{control = w} = Format, L) when is_list(L) ->
    [$[, lists:join($,, [format_spw(Format, E) || E <- L]), $]];
format_spw(#format{control = s}, _) ->
    error(badarg);
format_spw(_Format, T) when is_integer(T) ->
    erlang:integer_to_list(T);
format_spw(_Format, T) when is_float(T) ->
    erlang:float_to_list(T);
format_spw(_Format, T) when is_pid(T) ->
    erlang:pid_to_list(T);
format_spw(_Format, T) when is_reference(T) ->
    erlang:ref_to_list(T);
format_spw(_Format, T) when is_function(T) ->
    erlang:fun_to_list(T);
format_spw(Format, T) when is_tuple(T) ->
    [${, lists:join($,, [format_spw(Format, E) || E <- tuple_to_list(T)]), $}];
format_spw(Format, T) when is_map(T) ->
    [
        $#,
        ${,
        lists:join($,, [
            [format_spw(Format, K), " => ", format_spw(Format, V)]
         || {K, V} <- maps:to_list(T)
        ]),
        $}
    ].

%% We will probably need to add 'maybe' with OTP 27
-define(RESERVED_KEYWORDS, [
    'after',
    'and',
    'andalso',
    'band',
    'begin',
    'bnot',
    'bor',
    'bsl',
    'bsr',
    'bxor',
    'case',
    'catch',
    'cond',
    'div',
    'end',
    'fun',
    'if',
    'let',
    'not',
    'of',
    'or',
    'orelse',
    'receive',
    'rem',
    'try',
    'when',
    'xor'
]).

%% @private
atom_requires_quotes(Atom, AtomStr) ->
    case lists:member(Atom, ?RESERVED_KEYWORDS) of
        true -> true;
        false -> atom_requires_quotes0(AtomStr)
    end.

atom_requires_quotes0([C | _T]) when C < $a orelse C > $z -> true;
atom_requires_quotes0([_C | T]) -> atom_requires_quotes1(T).

atom_requires_quotes1([]) -> false;
atom_requires_quotes1([$@ | T]) -> atom_requires_quotes1(T);
atom_requires_quotes1([$_ | T]) -> atom_requires_quotes1(T);
atom_requires_quotes1([C | T]) when C >= $A andalso C =< $Z -> atom_requires_quotes1(T);
atom_requires_quotes1([C | T]) when C >= $0 andalso C =< $9 -> atom_requires_quotes1(T);
atom_requires_quotes1([C | T]) when C >= $a andalso C =< $z -> atom_requires_quotes1(T);
atom_requires_quotes1(_) -> true.

%% @private
format_integer(#format{control = C, precision = Precision0}, T0) when
    is_integer(T0) andalso (C =:= '#' orelse C =:= '+')
->
    Base =
        case Precision0 of
            undefined -> 10;
            _ -> Precision0
        end,
    {Sign, T} =
        if
            T0 < 0 ->
                {"-", -T0};
            true ->
                {[], T0}
        end,
    Str0 = integer_to_list(T, Base),
    Str =
        case C of
            '#' -> Str0;
            '+' -> string:to_lower(Str0)
        end,
    [Sign, integer_to_list(Base), "#", Str];
format_integer(#format{precision = undefined}, T) when is_integer(T) ->
    integer_to_list(T);
format_integer(#format{control = 'B', precision = Base}, T) when is_integer(T) ->
    integer_to_list(T, Base);
format_integer(#format{control = b, precision = Base}, T) when is_integer(T) ->
    string:to_lower(integer_to_list(T, Base));
format_integer(_Format, _) ->
    error(badarg).

%% @private
format_float(#format{control = f, precision = undefined}, T) when is_float(T) ->
    float_to_list(T, [{decimals, 6}]);
format_float(#format{control = f, precision = Precision}, T) when is_float(T) ->
    float_to_list(T, [{decimals, Precision}]);
format_float(#format{control = g, precision = undefined}, T) when
    is_float(T) andalso T >= 0.1 andalso T < 10000.0
->
    float_to_list(T, [{decimals, 5}]);
format_float(#format{control = g, precision = Precision}, T) when
    is_float(T) andalso T >= 0.1 andalso T < 10000.0
->
    float_to_list(T, [{decimals, Precision - 1}]);
format_float(#format{control = C, precision = undefined}, T) when
    is_float(T) andalso (C =:= e orelse C =:= g)
->
    format_scientific(T, 6, 0);
format_float(#format{control = C, precision = Precision}, T) when
    is_float(T) andalso (C =:= e orelse C =:= g)
->
    format_scientific(T, Precision, 0);
format_float(_Format, _) ->
    error(badarg).

%% @private
format_scientific(T, Precision, E) when (T < 1 andalso T > 0) orelse (T > -1 andalso T < 0) ->
    format_scientific(T * 10, Precision, E - 1);
format_scientific(T, Precision, E) when T >= 10 orelse T =< -10 ->
    format_scientific(T / 10, Precision, E + 1);
format_scientific(T, Precision, E) when E >= 0 ->
    [float_to_list(T, [{decimals, Precision - 1}]), "e+", integer_to_list(E)];
format_scientific(T, Precision, E) ->
    [float_to_list(T, [{decimals, Precision - 1}]), "e", integer_to_list(E)].

%% @private
format_char(#format{field_width = FieldWidth, precision = undefined} = Format, T) when
    FieldWidth =/= undefined
->
    format_char(Format#format{field_width = undefined, precision = FieldWidth}, T);
format_char(#format{mod = undefined, precision = undefined}, T) when is_integer(T) ->
    [T band 16#FF];
% TODO: check T is valid unicode char
format_char(#format{mod = t, precision = undefined}, T) when is_integer(T) -> [T];
format_char(#format{precision = Precision} = Format, T) when Precision =/= undefined ->
    [Ch] = format_char(Format#format{field_width = undefined, precision = undefined}, T),
    lists:duplicate(Precision, Ch);
format_char(_, _) ->
    error(badarg).

%% @private
%% String classes:
%% latin1_printable
%% latin1_unprintable
%% unicode
%% io_lib doesn't distinguish between valid unicode and invalid unicode
%% characters. This is done with io, though, when actually writing the string.
%% Compare:
%% ```
%% io_lib:format("~tc", [16#D800]).
%% io:format("~tc", [16#D800]).
%% ```
test_string_class(Str) ->
    test_string_class(Str, latin1_printable).

test_string_class([H | T], Class) when is_integer(H) andalso H >= 0 ->
    NewClass =
        case {Class, char_class(H)} of
            {_, latin1_printable} -> Class;
            {latin1_printable, CharClass} -> CharClass;
            {_, latin1_unprintable} -> Class;
            {latin1_unprintable, CharClass} -> CharClass;
            _ -> unicode
        end,
    test_string_class(T, NewClass);
test_string_class([], Class) ->
    Class;
test_string_class(_String, _Class) ->
    not_a_string.

char_class(H) when H >= 0 andalso H < 8 -> latin1_unprintable;
char_class(27) -> latin1_printable;
char_class(H) when H >= 14 andalso H < 32 -> latin1_unprintable;
char_class(H) when H < 256 -> latin1_printable;
char_class(_H) -> unicode.

%% @private
format_p_string([], Acc) ->
    [$", lists:reverse(Acc), $"];
format_p_string([8 | T], Acc) ->
    format_p_string(T, ["\\b" | Acc]);
format_p_string([9 | T], Acc) ->
    format_p_string(T, ["\\t" | Acc]);
format_p_string([10 | T], Acc) ->
    format_p_string(T, ["\\n" | Acc]);
format_p_string([11 | T], Acc) ->
    format_p_string(T, ["\\v" | Acc]);
format_p_string([12 | T], Acc) ->
    format_p_string(T, ["\\f" | Acc]);
format_p_string([13 | T], Acc) ->
    format_p_string(T, ["\\r" | Acc]);
format_p_string([27 | T], Acc) ->
    format_p_string(T, ["\\e" | Acc]);
format_p_string([H | T], Acc) ->
    format_p_string(T, [H | Acc]).
